open Gil_syntax
open Monadic

module Perm = struct
  type t = Compcert.Memtype.permission =
    | Freeable
    | Writable
    | Readable
    | Nonempty

  let to_int = function
    | Freeable -> 4
    | Writable -> 3
    | Readable -> 2
    | Nonempty -> 1

  let pp fmt = function
    | Freeable -> Fmt.pf fmt "Freeable"
    | Writable -> Fmt.pf fmt "Writable"
    | Readable -> Fmt.pf fmt "Readable"
    | Nonempty -> Fmt.pf fmt "Nonempty"

  module Infix = struct
    let ( >% ), ( <% ), ( <=% ), ( >=% ), ( =% ) =
      let make op a b = op (to_int a) (to_int b) in
      (make ( > ), make ( < ), make ( <= ), make ( >= ), make ( = ))
  end
end

type err =
  | UseAfterFree
  | BufferOverrun
  | InsufficientPermission of { required : Perm.t; actual : Perm.t }
  | Unhandled              of string

let pp_err fmt = function
  | UseAfterFree -> Fmt.pf fmt "Use After Free"
  | BufferOverrun -> Fmt.pf fmt "Buffer Overrun"
  | InsufficientPermission { required; actual } ->
      Fmt.pf fmt "Insufficient Permision: Got %a but required %a" Perm.pp
        required Perm.pp actual
  | Unhandled e -> Fmt.pf fmt "Unhandled error with message : %s" e

let err_equal a b =
  match (a, b) with
  | UseAfterFree, UseAfterFree -> true
  | BufferOverrun, BufferOverrun -> true
  | ( InsufficientPermission { required = ra; actual = aa },
      InsufficientPermission { required = rb; actual = ab } ) ->
      let open Perm.Infix in
      ra =% rb && aa =% ab
  | Unhandled a, Unhandled b -> String.equal a b
  | _ -> false

type 'a or_error = ('a, err) result

module Range = struct
  type t = Expr.t * Expr.t

  let pp fmt (a, b) = Fmt.pf fmt "@[<h>[%a; %a[@]" Expr.pp a Expr.pp b

  let make low high = (low, high)

  let from_low_and_chunk low chunk =
    let open Expr.Infix in
    let len = Expr.num (float_of_int (Chunk.size chunk)) in
    (low, low +. len)

  let is_equal (la, ha) (lb, hb) =
    let open Formula.Infix in
    la #== lb #&& (ha #== hb)

  let equal ~pc (la, ha) (lb, hb) =
    let ( = ) = FOSolver.is_equal ~pc in
    la = lb && ha = hb

  let is_inside (la, ha) (lb, hb) =
    let open Formula.Infix in
    lb #<= la #&& (ha #<= hb)

  let inside ~pc (la, ha) (lb, hb) =
    let ( <= ) = FOSolver.is_less_or_equal ~pc in
    lb <= la && ha <= hb

  let split_at (l, h) x = ((l, x), (x, h))

  let extract (il, ih) (ol, oh) = ((ol, il), (il, ih), (ih, oh))
end

module Node = struct
  type t =
    | Hole
    | AllocatedUnkown
    | Zero
    | Single          of { chunk : Compcert.AST.memory_chunk; value : SVal.t }

  let pp fmt = function
    | Hole                    -> Fmt.pf fmt "HOLE"
    | AllocatedUnkown         -> Fmt.pf fmt "ALLOCATED UNKOWN"
    | Zero                    -> Fmt.pf fmt "ZERO"
    | Single { chunk; value } ->
        Fmt.pf fmt "(%a : %a)" SVal.pp value Chunk.pp chunk

  let equal a b =
    match (a, b) with
    | Hole, Hole -> true
    | AllocatedUnkown, AllocatedUnkown -> true
    | Zero, Zero -> true
    | Single { chunk = ca; value = va }, Single { chunk = cb; value = vb } ->
        Chunk.equal ca cb && SVal.equal va vb
    | _ -> false

  let split = function
    (* TODO: Improve this *)
    | Hole -> (Hole, Hole)
    | AllocatedUnkown -> (AllocatedUnkown, AllocatedUnkown)
    | Zero -> (Zero, Zero)
    | Single _ -> (AllocatedUnkown, AllocatedUnkown)

  let decode ~chunk t =
    match t with
    | Hole | AllocatedUnkown -> SVal.SUndefined
    | Zero -> SVal.zero_of_chunk chunk
    | Single { chunk = m_chunk; value } ->
        if Chunk.equal m_chunk chunk then value else SUndefined

  let encode ~(chunk : Chunk.t) (sval : SVal.t) =
    match (sval, chunk) with
    | ( SVint _,
        (Mint8signed | Mint8unsigned | Mint16signed | Mint16unsigned | Mint32) )
    | SVlong _, Mint64
    | SVsingle _, Mfloat32
    | SVfloat _, Mfloat64 -> Single { chunk; value = sval }
    | Sptr _, c when Chunk.equal c Chunk.ptr -> Single { chunk; value = sval }
    | _ -> Single { chunk; value = SUndefined }
end

module Tree = struct
  type t = { node : Node.t; span : Range.t; children : (t * t) option }

  let rec pp fmt t =
    let pp_children fmt (l, r) = Fmt.pf fmt "@ %a@ %a" pp l pp r in
    let pp_aux fmt { node; span; children } =
      Fmt.pf fmt "%a@ %a%a" Range.pp span Node.pp node (Fmt.option pp_children)
        children
    in
    (Fmt.parens (Fmt.vbox pp_aux)) fmt t

  let rec equal ~pc a b =
    Node.equal a.node b.node
    && Range.equal ~pc a.span b.span
    &&
    match (a.children, b.children) with
    | None, None -> true
    | Some (a1, a2), Some (b1, b2) -> equal ~pc a1 b1 && equal ~pc a2 b2
    | _ -> false

  let make ~node ~span ?children () = { node; span; children }

  let with_children t ~left ~right = { t with children = Some (left, right) }

  let with_node t node = { t with node }

  let allocatedUnkown span = make ~node:AllocatedUnkown ~span ()

  let rec split ~pc ~range t : (t * t, 'a) SatResults.t =
    (* We're assuming that range is inside old_span *)
    let open Formula.Infix in
    let open SatResults.Syntax in
    let old_span = t.span in
    let ol, oh = old_span in
    let nl, nh = range in
    SatResults.branch_on_sat ~pc ol #== nl
      ~then_branch:(fun pc ->
        let left_node, right_node = Node.split t.node in
        let left_span, right_span = Range.split_at old_span nh in
        let left = make ~node:left_node ~span:left_span () in
        let right = make ~node:right_node ~span:right_span () in
        SatResults.return ~pc (left, right))
      ~else_branch:(fun pc ->
        SatResults.branch_on_sat ~pc oh #== nh
          ~then_branch:(fun pc ->
            let left_node, right_node = Node.split t.node in
            let left_span, right_span = Range.split_at old_span nh in
            let left = make ~node:left_node ~span:left_span () in
            let right = make ~node:right_node ~span:right_span () in
            SatResults.return ~pc (left, right))
          ~else_branch:(fun pc ->
            (* We're first splitting on the left then splitting again on the right *)
            let left_node, right_node = Node.split t.node in
            let left_span, right_span = Range.split_at old_span nl in
            let left = make ~node:left_node ~span:left_span () in
            let full_right = make ~node:right_node ~span:right_span () in
            let** (right_left, right_right), pc = split ~pc ~range full_right in
            let right =
              with_children full_right ~left:right_left ~right:right_right
            in
            SatResults.return ~pc (left, right)))

  let rec frame_single ~pc t low chunk =
    let range = Range.from_low_and_chunk low chunk in
    SatResults.branch_on_sat ~pc
      (Range.is_equal range t.span)
      ~then_branch:(fun pc -> SatResults.return ~pc t)
      ~else_branch:(fun pc ->
        let open SatResults.Syntax in
        match t.children with
        | Some (left, right) ->
            SatResults.branch_on_sat ~pc
              (Range.is_inside range left.span)
              ~then_branch:(fun pc ->
                let++ left = frame_single ~pc left low chunk in
                with_children t ~left ~right)
              ~else_branch:(fun pc ->
                SatResults.branch_on_sat ~pc
                  (Range.is_inside range right.span)
                  ~then_branch:(fun pc ->
                    let++ right = frame_single ~pc right low chunk in
                    with_children t ~left ~right)
                  ~else_branch:(fun pc ->
                    SatResults.terminate ~pc
                      (Unhandled
                         "Children don't seem to be a partition of parent")))
        | None               ->
            let++ left, right = split ~pc ~range t in
            with_children t ~left ~right)

  let rec get_framed ~pc t low chunk =
    let range_eq = Range.equal ~pc in
    let range_ins = Range.inside ~pc in
    let range = Range.from_low_and_chunk low chunk in
    if range_eq t.span range then Ok (Node.decode ~chunk t.node)
    else
      match t.children with
      | None               -> Error
                                (Unhandled
                                   "Value should have been framed beforhand")
      | Some (left, right) ->
          if range_ins range left.span then get_framed ~pc left low chunk
          else if range_ins range right.span then get_framed ~pc right low chunk
          else Error (Unhandled "Children not in partition in get_framed")

  let rec set_framed ~pc t low chunk sval =
    let ( let+ ) x f = Result.map f x in
    let range_eq = Range.equal ~pc in
    let range_ins = Range.inside ~pc in
    let range = Range.from_low_and_chunk low chunk in
    if range_eq t.span range then
      let node = Node.encode ~chunk sval in
      Ok (with_node t node)
    else
      match t.children with
      | None               -> Error
                                (Unhandled
                                   "Value should have been framed beforhand")
      | Some (left, right) ->
          if range_ins range left.span then
            let+ left = set_framed ~pc left low chunk sval in
            with_children t ~left ~right
          else if range_ins range right.span then
            let+ right = set_framed ~pc right low chunk sval in
            with_children t ~left ~right
          else Error (Unhandled "Children not in partition in get_framed")
end

type t = Freed | Tree of { perm : Perm.t; span : Range.t; root : Tree.t }

let pp fmt = function
  | Freed                     -> Fmt.pf fmt "FREED"
  | Tree { perm; span; root } ->
      let pp_aux fmt (perm, span, root) =
        Fmt.pf fmt "%a@ %a@ %a" Range.pp span Perm.pp perm Tree.pp root
      in
      (Fmt.parens (Fmt.vbox pp_aux)) fmt (perm, span, root)

let equal ?(pc = Pc.empty) a b =
  match (a, b) with
  | Freed, Freed -> true
  | ( Tree { perm = pa; span = sa; root = ra },
      Tree { perm = pb; span = sb; root = rb } ) ->
      (let open Perm.Infix in
      pa =% pb)
      && Range.equal ~pc sa sb && Tree.equal ~pc ra rb
  | _ -> false

let get_root = function
  | Freed  -> Error UseAfterFree
  | Tree x -> Ok x.root

let get_perm = function
  | Freed  -> Error UseAfterFree
  | Tree x -> Ok x.perm

let get_span = function
  | Freed  -> Error UseAfterFree
  | Tree x -> Ok x.span

let with_root t root =
  match t with
  | Freed  -> Error UseAfterFree
  | Tree x -> Ok (Tree { x with root })

let alloc low high =
  let span = Range.make low high in
  Tree { perm = Freeable; root = Tree.allocatedUnkown span; span }

let drop_perm ~pc t low high new_perm =
  let ( let*+ ) x f = SatResults.bind (SatResults.of_res ~pc x) f in
  let ( === ) = FOSolver.is_equal ~pc in
  let open Perm.Infix in
  let*+ (cl, hl), _ = get_span t in
  if not (cl === low && hl === high) then
    SatResults.terminate ~pc
      (Unhandled "Modifying permission on range different from object span")
  else
    let*+ curr_perm, _ = get_perm t in
    if new_perm >% curr_perm then
      SatResults.terminate ~pc
        (InsufficientPermission { required = new_perm; actual = curr_perm })
    else
      let*+ root, _ = get_root t in
      SatResults.return ~pc
        (Tree { root; span = Range.make cl hl; perm = new_perm })

let free ~pc t low high =
  let ( let*+ ) x f = SatResults.bind (SatResults.of_res ~pc x) f in
  let terminate = SatResults.terminate ~pc in
  let return = SatResults.return ~pc in
  let ( === ) = FOSolver.is_equal ~pc in
  let open Perm.Infix in
  let*+ (cl, hl), _ = get_span t in
  if not (cl === low && hl === high) then
    terminate (Unhandled "Freeing only part of an object")
  else
    let*+ curr_perm, _ = get_perm t in
    if Freeable >% curr_perm then
      terminate
        (InsufficientPermission { required = Freeable; actual = curr_perm })
    else return Freed

let get ~pc t low chunk =
  let open SatResults.Syntax in
  let range = Range.from_low_and_chunk low chunk in
  let** span, pc = SatResults.of_res ~pc (get_span t) in
  SatResults.branch_on_sat ~pc
    (Range.is_inside range span)
    ~then_branch:(fun pc ->
      let** root, pc = SatResults.of_res ~pc (get_root t) in
      let** root_framed, pc = Tree.frame_single ~pc root low chunk in
      let** value, pc =
        SatResults.of_res ~pc (Tree.get_framed ~pc root_framed low chunk)
      in
      let** wroot, pc = SatResults.of_res ~pc (with_root t root_framed) in
      SatResults.return ~pc (value, wroot))
    ~else_branch:(fun pc -> SatResults.terminate ~pc BufferOverrun)

let set ~pc t low chunk sval =
  let open SatResults.Syntax in
  let range = Range.from_low_and_chunk low chunk in
  let** span, pc = SatResults.of_res ~pc (get_span t) in
  SatResults.branch_on_sat ~pc
    (Range.is_inside range span)
    ~else_branch:(fun pc -> SatResults.terminate ~pc BufferOverrun)
    ~then_branch:(fun pc ->
      let++ root = SatResults.of_res ~pc (get_root t) in
      let** root_framed, pc = Tree.frame_single ~pc root low chunk in
      let** root_set, pc =
        SatResults.of_res ~pc (Tree.set_framed ~pc root_framed low chunk sval)
      in
      SatResults.return ~pc (with_root t root_set))
