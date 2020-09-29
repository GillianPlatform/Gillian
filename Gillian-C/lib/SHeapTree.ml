open Gil_syntax
open Monadic
module DR = Delayed_result
module DO = Delayed_option
module SS = Utils.Containers.SS

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

  let opt_to_int = function
    | None   -> 0
    | Some x -> to_int x

  let pp fmt = function
    | Freeable -> Fmt.pf fmt "Freeable"
    | Writable -> Fmt.pf fmt "Writable"
    | Readable -> Fmt.pf fmt "Readable"
    | Nonempty -> Fmt.pf fmt "Nonempty"

  module Infix = struct
    let ( >% ), ( <% ), ( <=% ), ( >=% ), ( =% ) =
      let make op a b = op (to_int a) (to_int b) in
      (make ( > ), make ( < ), make ( <= ), make ( >= ), make ( = ))

    let ( >%? ), ( <%? ), ( <=%? ), ( >=%? ), ( =%? ) =
      let make op a b = op (opt_to_int a) (opt_to_int b) in
      (make ( > ), make ( < ), make ( <= ), make ( >= ), make ( = ))
  end
end

type err =
  | UseAfterFree
  | BufferOverrun
  | InsufficientPermission of { required : Perm.t; actual : Perm.t option }
  | InvalidAlignment       of { alignment : int; offset : Expr.t }
  | MissingResource
  | Unhandled              of string

let pp_err fmt = function
  | UseAfterFree -> Fmt.pf fmt "Use After Free"
  | BufferOverrun -> Fmt.pf fmt "Buffer Overrun"
  | InsufficientPermission { required; actual } ->
      Fmt.pf fmt "Insufficient Permision: Got %a but required %a" Perm.pp
        required
        (Fmt.option ~none:(Fmt.any "None") Perm.pp)
        actual
  | InvalidAlignment { alignment; offset } ->
      Fmt.pf fmt "Invalid alignment: %d should divide %a" alignment Expr.pp
        offset
  | MissingResource -> Fmt.pf fmt "MissingResource"
  | Unhandled e -> Fmt.pf fmt "Unhandled error with message : %s" e

let err_equal a b =
  match (a, b) with
  | MissingResource, MissingResource -> true
  | UseAfterFree, UseAfterFree -> true
  | BufferOverrun, BufferOverrun -> true
  | ( InsufficientPermission { required = ra; actual = aa },
      InsufficientPermission { required = rb; actual = ab } ) ->
      let open Perm.Infix in
      ra =% rb && aa =%? ab
  | Unhandled a, Unhandled b -> String.equal a b
  | _ -> false

type 'a or_error = ('a, err) result

module Range = struct
  type t = Expr.t * Expr.t

  let pp fmt (a, b) = Fmt.pf fmt "@[<h>[%a; %a[@]" Expr.pp a Expr.pp b

  let make low high = (low, high)

  let of_low_and_chunk low chunk =
    let open Expr.Infix in
    let len = Expr.num (float_of_int (Chunk.size chunk)) in
    (low, low +. len)

  let is_equal (la, ha) (lb, hb) =
    let open Formula.Infix in
    la #== lb #&& (ha #== hb)

  let sure_equal ~pc (la, ha) (lb, hb) =
    let ( = ) = FOSolver.is_equal ~pc in
    la = lb && ha = hb

  let is_inside (la, ha) (lb, hb) =
    let open Formula.Infix in
    lb #<= la #&& (ha #<= hb)

  let split_at (l, h) x = ((l, x), (x, h))

  let extract (il, ih) (ol, oh) = ((ol, il), (il, ih), (ih, oh))

  let lvars (a, b) = SS.union (Expr.lvars a) (Expr.lvars b)
end

module Node = struct
  type t =
    | NotOwned
    | Hole
    | AllocatedUnkown
    | Zero
    | Single          of { chunk : Compcert.AST.memory_chunk; value : SVal.t }

  let pp fmt = function
    | NotOwned                -> Fmt.pf fmt "NOT OWNED"
    | Hole                    -> Fmt.pf fmt "HOLE"
    | AllocatedUnkown         -> Fmt.pf fmt "ALLOCATED UNKOWN"
    | Zero                    -> Fmt.pf fmt "ZERO"
    | Single { chunk; value } ->
        Fmt.pf fmt "(%a : %a)" SVal.pp value Chunk.pp chunk

  let equal a b =
    match (a, b) with
    | Hole, Hole
    | AllocatedUnkown, AllocatedUnkown
    | Zero, Zero
    | NotOwned, NotOwned -> true
    | Single { chunk = ca; value = va }, Single { chunk = cb; value = vb } ->
        Chunk.equal ca cb && SVal.equal va vb
    | _ -> false

  let split = function
    (* TODO: Improve this *)
    | NotOwned -> (NotOwned, NotOwned)
    | Hole -> (Hole, Hole)
    | AllocatedUnkown -> (AllocatedUnkown, AllocatedUnkown)
    | Zero -> (Zero, Zero)
    | Single _ -> (AllocatedUnkown, AllocatedUnkown)

  let decode ~chunk t =
    match t with
    | NotOwned -> Error MissingResource
    | Hole | AllocatedUnkown -> Ok SVal.SUndefined
    | Zero -> Ok (SVal.zero_of_chunk chunk)
    | Single { chunk = m_chunk; value } ->
        Ok (if Chunk.equal m_chunk chunk then value else SUndefined)

  let encode ~(chunk : Chunk.t) (sval : SVal.t) =
    match (sval, chunk) with
    | ( SVint _,
        (Mint8signed | Mint8unsigned | Mint16signed | Mint16unsigned | Mint32) )
    | SVlong _, Mint64
    | SVsingle _, Mfloat32
    | SVfloat _, Mfloat64 -> Single { chunk; value = sval }
    | Sptr _, c when Chunk.equal c Chunk.ptr -> Single { chunk; value = sval }
    | _ -> Single { chunk; value = SUndefined }

  let lvars = function
    | NotOwned | Hole | AllocatedUnkown | Zero -> SS.empty
    | Single { value = e; _ } -> SVal.lvars e
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
    && Range.sure_equal ~pc a.span b.span
    &&
    match (a.children, b.children) with
    | None, None -> true
    | Some (a1, a2), Some (b1, b2) -> equal ~pc a1 b1 && equal ~pc a2 b2
    | _ -> false

  let make ~node ~span ?children () = { node; span; children }

  let with_children t ~left ~right = { t with children = Some (left, right) }

  let with_node t node = { t with node }

  let allocatedUnkown span = make ~node:AllocatedUnkown ~span ()

  let rec split ~range t : (t * t) Delayed.t =
    (* We're assuming that range is inside old_span *)
    let open Formula.Infix in
    let open Delayed.Syntax in
    let old_span = t.span in
    let ol, oh = old_span in
    let nl, nh = range in
    if%sat ol #== nl then
      let left_node, right_node = Node.split t.node in
      let left_span, right_span = Range.split_at old_span nh in
      let left = make ~node:left_node ~span:left_span () in
      let right = make ~node:right_node ~span:right_span () in
      Delayed.return (left, right)
    else
      if%sat oh #== nh then
        let left_node, right_node = Node.split t.node in
        let left_span, right_span = Range.split_at old_span nh in
        let left = make ~node:left_node ~span:left_span () in
        let right = make ~node:right_node ~span:right_span () in
        Delayed.return (left, right)
      else
        (* We're first splitting on the left then splitting again on the right *)
        let left_node, right_node = Node.split t.node in
        let left_span, right_span = Range.split_at old_span nl in
        let left = make ~node:left_node ~span:left_span () in
        let full_right = make ~node:right_node ~span:right_span () in
        let* right_left, right_right = split ~range full_right in
        let right =
          with_children full_right ~left:right_left ~right:right_right
        in
        Delayed.return (left, right)

  let rec frame_single (t : t) (low : Expr.t) (chunk : Chunk.t) : (t, err) DR.t
      =
    let range = Range.of_low_and_chunk low chunk in
    if%sat Range.is_equal range t.span then DR.ok t
    else
      let open DR.Syntax in
      match t.children with
      | Some (left, right) ->
          if%sat Range.is_inside range left.span then
            let++ t = frame_single left low chunk in
            with_children t ~left ~right
          else
            if%sat Range.is_inside range right.span then
              let++ right = frame_single right low chunk in
              with_children t ~left ~right
            else
              DR.error
                (Unhandled "Children don't seem to be a partition of parent")
      | None               ->
          let open Delayed.Syntax in
          let+ left, right = split ~range t in
          Ok (with_children t ~left ~right)

  let rec get_framed t low chunk =
    let range = Range.of_low_and_chunk low chunk in
    if%ent Range.is_equal t.span range then
      DR.of_result (Node.decode ~chunk t.node)
    else
      match t.children with
      | None               -> DR.error
                                (Unhandled
                                   "Value should have been framed beforhand")
      | Some (left, right) ->
          if%ent Range.is_inside range left.span then get_framed left low chunk
          else
            if%ent Range.is_inside range right.span then
              get_framed right low chunk
            else DR.error (Unhandled "Children not in partition in get_framed")

  let rec set_framed t low chunk sval =
    let open DR.Syntax in
    let range = Range.of_low_and_chunk low chunk in
    if%ent Range.is_equal t.span range then
      let node = Node.encode ~chunk sval in
      DR.ok (with_node t node)
    else
      match t.children with
      | None               -> DR.error
                                (Unhandled
                                   "Value should have been framed beforhand")
      | Some (left, right) ->
          if%ent Range.is_inside range left.span then
            let++ left = set_framed left low chunk sval in
            with_children t ~left ~right
          else
            if%ent Range.is_inside range right.span then
              let++ right = set_framed right low chunk sval in
              with_children t ~left ~right
            else DR.error (Unhandled "Children not in partition in get_framed")

  let rec lvars { node; span; children } =
    let node_lvars = Node.lvars node in
    let span_lvars = Range.lvars span in
    let children_lvars =
      match children with
      | Some (a, b) -> SS.union (lvars a) (lvars b)
      | None        -> SS.empty
    in
    SS.union (SS.union node_lvars span_lvars) children_lvars
end

type t = Freed | Tree of { perm : Perm.t; span : Range.t; root : Tree.t }

let pp fmt = function
  | Freed                     -> Fmt.pf fmt "FREED"
  | Tree { perm; span; root } ->
      let pp_aux fmt (perm, span, root) =
        Fmt.pf fmt "%a@ %a@ %a" Range.pp span Perm.pp perm Tree.pp root
      in
      (Fmt.parens (Fmt.vbox pp_aux)) fmt (perm, span, root)

(* let equal ?(pc = Pc.empty) a b =
  match (a, b) with
  | Freed, Freed -> true
  | ( Tree { perm = pa; span = sa; root = ra },
      Tree { perm = pb; span = sb; root = rb } ) ->
      (let open Perm.Infix in
      pa =% pb)
      && Range.equal sa sb && Tree.equal ~pc ra rb
  | _ -> false *)

let get_root = function
  | Freed  -> Error UseAfterFree
  | Tree x -> Ok x.root

let get_perm = function
  | Freed  -> None
  | Tree x -> Some x.perm

let get_perm_at ofs = function
  | Freed  -> DO.none ()
  | Tree x ->
      let open Formula.Infix in
      let l, h = x.span in
      if%sat l #<= ofs #&& (ofs #< h) then DO.some x.perm else DO.none ()

let get_min_perm_between low high = function
  | Freed  -> DO.none ()
  | Tree x ->
      if%sat Range.is_inside (low, high) x.span then DO.some x.perm
      else DO.none ()

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

let drop_perm t low high new_perm =
  let open Perm.Infix in
  let open Formula.Infix in
  let open DR.Syntax in
  let** cl, hl = DR.of_result (get_span t) in
  if%ent cl #== low #&& (hl #== high) then
    let** curr_perm = DR.of_option ~none:BufferOverrun (get_perm t) in
    if new_perm >% curr_perm then
      DR.error
        (InsufficientPermission { required = new_perm; actual = Some curr_perm })
    else
      let++ root = DR.of_result (get_root t) in
      Tree { root; span = Range.make cl hl; perm = new_perm }
  else
    DR.error
      (Unhandled "Modifying permission on range different from object span")

let free t low high =
  let open Perm.Infix in
  let open DR.Syntax in
  let open Formula.Infix in
  let** cl, hl = DR.of_result (get_span t) in
  if%ent cl #== low #&& (hl #== high) then
    let** curr_perm = DR.of_option ~none:BufferOverrun (get_perm t) in
    if curr_perm <% Freeable then
      DR.error
        (InsufficientPermission { required = Freeable; actual = Some curr_perm })
    else DR.ok Freed
  else
    DR.error
      (Unhandled
         "Freeing only part of an object (this might need fixing in the MM)")

let get t low chunk =
  let open DR.Syntax in
  let range = Range.of_low_and_chunk low chunk in
  let** span = DR.of_result (get_span t) in
  if%sat Range.is_inside range span then
    let** root = DR.of_result (get_root t) in
    let** root_framed = Tree.frame_single root low chunk in
    let** value = Tree.get_framed root_framed low chunk in
    let++ wroot = DR.of_result (with_root t root_framed) in
    (value, wroot)
  else DR.error BufferOverrun

let set t low chunk sval =
  let open DR.Syntax in
  let range = Range.of_low_and_chunk low chunk in
  let** span = DR.of_result (get_span t) in
  if%sat Range.is_inside range span then
    let** root = DR.of_result (get_root t) in
    let** root_framed = Tree.frame_single root low chunk in
    let+* root_set = Tree.set_framed root_framed low chunk sval in
    with_root t root_set
  else DR.error BufferOverrun

let check_valid_access t chunk ofs perm =
  let open Delayed.Syntax in
  let open Perm.Infix in
  let open Formula.Infix in
  let open Expr.Infix in
  let sz = Expr.num (float_of_int (Chunk.size chunk)) in
  let al = Chunk.align chunk in
  let al_expr = Expr.num (float_of_int al) in
  let high = ofs +. sz in
  let* cur_perm = get_min_perm_between ofs high t in
  let has_perm = cur_perm >=%? Some perm in
  if has_perm then
    let divides x y =
      Expr.(y #== (num 0.) #|| ((BinOp (y, FMod, x)) #== (Expr.num 0.)))
    in
    if%sat divides al_expr ofs then DR.ok ()
    else DR.error (InvalidAlignment { alignment = al; offset = ofs })
  else DR.error (InsufficientPermission { actual = cur_perm; required = perm })

let load t chunk ofs =
  let open DR.Syntax in
  let** () = check_valid_access t chunk ofs Readable in
  get t ofs chunk

let store t chunk ofs value =
  let open DR.Syntax in
  let** () = check_valid_access t chunk ofs Readable in
  set t ofs chunk value

let lvars = function
  | Freed                  -> SS.empty
  | Tree { span; root; _ } -> SS.union (Range.lvars span) (Tree.lvars root)
