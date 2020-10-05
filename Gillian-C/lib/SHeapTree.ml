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
  | RemovingNotOwned
  | HoleNotUndefined

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
  | RemovingNotOwned -> Fmt.pf fmt "Removing not owned"
  | HoleNotUndefined -> Fmt.pf fmt "Hole not undefined"

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

  let is_before (_, ha) (lb, _) =
    let open Formula.Infix in
    ha #<= lb

  let is_after (la, _) (_, hb) =
    let open Formula.Infix in
    hb #<= la

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
  type qty = Totally | Partially

  let str_qty = function
    | Totally   -> "TOTALLY"
    | Partially -> "PARTIALLY"

  type t =
    | NotOwned of qty
    | Undef
    | Zero
    | Single   of { chunk : Compcert.AST.memory_chunk; value : SVal.t }

  let pp fmt = function
    | NotOwned qty            -> Fmt.pf fmt "%s NOT OWNED" (str_qty qty)
    | Undef                   -> Fmt.pf fmt "UNDEF"
    | Zero                    -> Fmt.pf fmt "ZERO"
    | Single { chunk; value } ->
        Fmt.pf fmt "(%a : %a)" SVal.pp value Chunk.pp chunk

  let equal a b =
    match (a, b) with
    | Undef, Undef | Zero, Zero -> true
    | NotOwned x, NotOwned y -> x == y
    | Single { chunk = ca; value = va }, Single { chunk = cb; value = vb } ->
        Chunk.equal ca cb && SVal.equal va vb
    | _ -> false

  let split = function
    (* TODO: Improve this *)
    | NotOwned Totally -> (NotOwned Totally, NotOwned Totally)
    | Undef -> (Undef, Undef)
    | Zero -> (Zero, Zero)
    | Single _ -> (Undef, Undef)
    | NotOwned Partially -> failwith "Should never split a partially owned node"

  let merge ~left:a ~right:b =
    (* In the future, this can get more precise, for example composing single values *)
    match (a, b) with
    | NotOwned Totally, NotOwned Totally -> NotOwned Totally
    | NotOwned _, _ | _, NotOwned _ -> NotOwned Partially
    | Zero, Zero -> Zero
    | _, _ -> Undef

  let decode ~chunk t =
    match t with
    | NotOwned _ -> Error MissingResource
    | Undef -> Ok SVal.SUndefined
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
    | NotOwned _ | Undef | Zero -> SS.empty
    | Single { value = e; _ }   -> SVal.lvars e
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

  let undefined span = make ~node:Undef ~span ()

  let create_root range =
    { children = None; span = range; node = NotOwned Totally }

  let is_entirely_owned_between ~tree low high =
    let range = (low, high) in
    let rec check_child child range =
      match child.node with
      | NotOwned Totally   -> Delayed.return false
      | NotOwned Partially ->
          let left, right = Option.get tree.children in
          if%sat Range.is_inside range left.span then check_child left range
          else check_child right range
      | _                  -> Delayed.return true
      (* The difference is that we don't check the root first *)
    in
    match tree.node with
    | NotOwned Totally -> Delayed.return false
    | _                ->
        if%sat Range.is_inside (low, high) tree.span then
          match tree.node with
          | NotOwned Partially ->
              let left, right = Option.get tree.children in
              if%sat Range.is_inside range left.span then check_child left range
              else check_child right range
          | _                  -> Delayed.return true
        else Delayed.return false

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
        let left_span, right_span = Range.split_at old_span nl in
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

  let extend_if_needed t range =
    if%sat Range.is_before range t.span then
      let new_left_span = (fst range, fst t.span) in
      let new_left_tree =
        { node = NotOwned Totally; span = new_left_span; children = None }
      in
      let new_range = (fst range, snd t.span) in
      let new_children = Some (new_left_tree, t) in
      Delayed.return
        { node = NotOwned Partially; span = new_range; children = new_children }
    else
      if%sat Range.is_after range t.span then
        let new_right_span = (snd t.span, snd range) in
        let new_right_tree =
          { node = NotOwned Totally; span = new_right_span; children = None }
        in
        let new_range = (fst t.span, snd range) in
        let new_children = Some (t, new_right_tree) in
        Delayed.return
          {
            node = NotOwned Partially;
            span = new_range;
            children = new_children;
          }
      else Delayed.return t

  let frame_range (t : t) (range : Range.t) : (t, err) DR.t =
    let open DR.Syntax in
    let rec frame_inside (t : t) (range : Range.t) : (t, err) DR.t =
      if%sat Range.is_equal range t.span then DR.ok t
      else
        match t.children with
        | Some (left, right) ->
            if%sat Range.is_inside range left.span then
              let++ left = frame_inside left range in
              with_children t ~left ~right
            else
              let++ right = frame_inside right range in
              with_children t ~left ~right
        | None               ->
            let open Delayed.Syntax in
            let+ left, right = split ~range t in
            Ok (with_children t ~left ~right)
    in
    let open Delayed.Syntax in
    let* root = extend_if_needed t range in
    frame_inside root range

  let frame_single (t : t) (low : Expr.t) (chunk : Chunk.t) : (t, err) DR.t =
    let range = Range.of_low_and_chunk low chunk in
    frame_range t range

  let rec get_framed_node t range =
    if%ent Range.is_equal t.span range then DR.ok t.node
    else
      match t.children with
      | None               -> DR.error
                                (Unhandled
                                   "Value should have been framed beforhand")
      | Some (left, right) ->
          if%ent Range.is_inside range left.span then get_framed_node left range
          else
            if%ent Range.is_inside range right.span then
              get_framed_node right range
            else DR.error (Unhandled "Children not in partition in get_framed")

  let get_framed_single t low chunk =
    let open DR.Syntax in
    let range = Range.of_low_and_chunk low chunk in
    let** node = get_framed_node t range in
    DR.of_result (Node.decode ~chunk node)

  let of_children ~left ~right =
    let span = (fst left.span, snd right.span) in
    let children = Some (left, right) in
    let node = Node.merge ~left:left.node ~right:right.node in
    { span; children; node }

  let rec set_node_framed t range node =
    let open DR.Syntax in
    if%ent Range.is_equal t.span range then DR.ok (with_node t node)
    else
      match t.children with
      | None               -> DR.error
                                (Unhandled
                                   "Value should have been framed beforhand")
      | Some (left, right) ->
          if%ent Range.is_inside range left.span then
            let++ left = set_node_framed left range node in
            of_children ~left ~right
          else
            if%ent Range.is_inside range right.span then
              let++ right = set_node_framed right range node in
              of_children ~left ~right
            else DR.error (Unhandled "Children not in partition in get_framed")

  let set_single_framed t low chunk sval =
    let range = Range.of_low_and_chunk low chunk in
    let node = Node.encode ~chunk sval in
    set_node_framed t range node

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

type t =
  | Freed
  | Tree  of { perm : Perm.t; bounds : Range.t option; root : Tree.t option }

let pp fmt = function
  | Freed -> Fmt.pf fmt "FREED"
  | Tree { perm; bounds; root } ->
      let pp_aux fmt (perm, bounds, root) =
        Fmt.pf fmt "%a@ %a@ %a"
          (Fmt.option ~none:(Fmt.any "NO BOUNDS") Range.pp)
          bounds Perm.pp perm
          (Fmt.option ~none:(Fmt.any "EMPTY") Tree.pp)
          root
      in
      (Fmt.parens (Fmt.vbox pp_aux)) fmt (perm, bounds, root)

let empty () =
  let bounds = None in
  let perm = Perm.Freeable in
  let root = None in
  Delayed.return (Tree { bounds; perm; root })

let lvars = function
  | Freed                    -> SS.empty
  | Tree { bounds; root; _ } ->
      SS.union
        (Option.fold ~none:SS.empty ~some:Range.lvars bounds)
        (Option.fold ~none:SS.empty ~some:Tree.lvars root)

let get_root = function
  | Freed  -> Error UseAfterFree
  | Tree x -> Ok x.root

let get_perm_opt t =
  match t with
  | Freed  -> None
  | Tree x -> Some x.perm

let get_perm_res t =
  match t with
  | Freed  -> Error UseAfterFree
  | Tree x -> Ok x.perm

let set_perm t perm =
  match t with
  | Freed  -> Error UseAfterFree
  | Tree x -> Ok (Tree { x with perm })

let is_in_bounds range bounds =
  match bounds with
  | None        -> Formula.True
  | Some bounds -> Range.is_inside range bounds

let get_min_perm_between low high = function
  | Freed  -> DO.none ()
  | Tree x ->
      if%sat is_in_bounds (low, high) x.bounds then
        match x.root with
        | None      -> DO.none ()
        | Some tree ->
            let open Delayed.Syntax in
            let* owned = Tree.is_entirely_owned_between ~tree low high in
            if owned then DO.some x.perm else DO.none ()
      else DO.none ()

let get_perm_at ofs =
  let open Expr.Infix in
  get_min_perm_between ofs (ofs +. Expr.num 1.)

let get_bounds = function
  | Freed  -> Error UseAfterFree
  | Tree x -> Ok x.bounds

let set_bounds t bounds =
  match t with
  | Freed  -> Error UseAfterFree
  | Tree x -> Ok (Tree { x with bounds })

let with_root_opt t root =
  match t with
  | Freed  -> Error UseAfterFree
  | Tree x -> Ok (Tree { x with root })

let with_root t root = with_root_opt t (Some root)

let alloc low high =
  let bounds = Range.make low high in
  Tree
    {
      perm = Freeable;
      root = Some (Tree.undefined bounds);
      bounds = Some bounds;
    }

let drop_perm t _low _high new_perm =
  match t with
  | Freed  -> Error UseAfterFree
  | Tree x -> (
      match x.perm with
      | Freeable -> Ok (Tree { x with perm = new_perm })
      | _        ->
          Error
            (InsufficientPermission
               { required = Freeable; actual = Some x.perm }) )

let free t low high =
  let open DR.Syntax in
  let open Delayed.Syntax in
  let** bounds = DR.of_result (get_bounds t) in
  match t with
  (* Can't free something already freed *)
  | Freed -> DR.error UseAfterFree
  | Tree tree -> (
      (* Can only free if entirely freeable *)
      match tree.perm with
      | Freeable -> (
          (* We need to own the entire object to free it, including what's outside of the bounds *)
          match bounds with
          | None        -> DR.error MissingResource
          | Some bounds ->
              if%ent Range.is_equal (low, high) bounds then
                match tree.root with
                | None      -> DR.error MissingResource
                | Some root ->
                    let* owned =
                      Tree.is_entirely_owned_between ~tree:root low high
                    in
                    if owned then DR.ok Freed else DR.error MissingResource
              else
                DR.error
                  (Unhandled
                     "Freeing only part of an object (this might need fixing \
                      in the MM)") )
      | _        ->
          DR.error
            (InsufficientPermission
               { required = Freeable; actual = Some tree.perm }) )

let get_single t low chunk =
  let open DR.Syntax in
  let range = Range.of_low_and_chunk low chunk in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None      -> DR.error MissingResource
    | Some root ->
        let** root_framed = Tree.frame_single root low chunk in
        let** value = Tree.get_framed_single root_framed low chunk in
        let++ wroot = DR.of_result (with_root t root_framed) in
        (value, wroot)
  else DR.error BufferOverrun

let set_single t low chunk sval =
  let open DR.Syntax in
  let range = Range.of_low_and_chunk low chunk in
  let** root = DR.of_result (get_root t) in
  let** root_set =
    match root with
    | None      -> Tree.set_single_framed (Tree.create_root range) low chunk sval
    | Some root ->
        let** root_framed = Tree.frame_single root low chunk in
        Tree.set_single_framed root_framed low chunk sval
  in
  let** bounds = DR.of_result (get_bounds t) in
  let learned =
    match bounds with
    | None        -> []
    | Some bounds -> [ Range.is_inside range bounds ]
  in
  DR.of_result ~learned (with_root t root_set)

let rem_single t low chunk =
  let open DR.Syntax in
  let range = Range.of_low_and_chunk low chunk in
  let** root = DR.of_result (get_root t) in
  let** root_set =
    match root with
    | None      -> DR.ok None
    | Some root ->
        let** root_framed = Tree.frame_single root low chunk in
        let** root_set =
          Tree.set_node_framed root_framed range (NotOwned Totally)
        in
        DR.ok (Some root_set)
  in
  let** bounds = DR.of_result (get_bounds t) in
  let learned =
    match bounds with
    | None        -> []
    | Some bounds -> [ Range.is_inside range bounds ]
  in
  DR.of_result ~learned (with_root_opt t root_set)

let get_hole t low high =
  let open DR.Syntax in
  let range = (low, high) in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None      -> DR.error MissingResource
    | Some root ->
        let** root_framed = Tree.frame_range root range in
        let** node = Tree.get_framed_node root_framed range in
        let res =
          match node with
          | Undef      -> Ok ()
          | NotOwned _ -> Error MissingResource
          | _          -> Error HoleNotUndefined
        in
        let++ wroot =
          DR.of_result (Result.bind res (fun () -> with_root t root_framed))
        in
        wroot
  else DR.error BufferOverrun

let set_hole t low high =
  let open DR.Syntax in
  let range = (low, high) in
  let** root = DR.of_result (get_root t) in
  let** root_set =
    match root with
    | None      -> Tree.set_node_framed (Tree.create_root range) range Undef
    | Some root ->
        let** root_framed = Tree.frame_range root range in
        Tree.set_node_framed root_framed range Undef
  in
  let** bounds = DR.of_result (get_bounds t) in
  let learned =
    match bounds with
    | None        -> []
    | Some bounds -> [ Range.is_inside range bounds ]
  in
  DR.of_result ~learned (with_root t root_set)

let rem_hole t low high =
  let open DR.Syntax in
  let range = (low, high) in
  let** root = DR.of_result (get_root t) in
  let** root_set =
    match root with
    | None      -> DR.ok None
    | Some root ->
        let** root_framed = Tree.frame_range root range in
        let** root_set =
          Tree.set_node_framed root_framed range (NotOwned Totally)
        in
        DR.ok (Some root_set)
  in
  let** bounds = DR.of_result (get_bounds t) in
  let learned =
    match bounds with
    | None        -> []
    | Some bounds -> [ Range.is_inside range bounds ]
  in
  DR.of_result ~learned (with_root_opt t root_set)

let check_valid_access t chunk ofs perm =
  let open Delayed.Syntax in
  let open Perm.Infix in
  let open Expr.Infix in
  let sz = Expr.num (float_of_int (Chunk.size chunk)) in
  let al = Chunk.align chunk in
  let al_expr = Expr.num (float_of_int al) in
  let high = ofs +. sz in
  let* cur_perm = get_min_perm_between ofs high t in
  let has_perm = cur_perm >=%? Some perm in
  if has_perm then
    let divides x y =
      let open Formula.Infix in
      Expr.(y #== (num 0.)) #|| ((Expr.fmod y x) #== (Expr.num 0.))
    in
    if%sat divides al_expr ofs then DR.ok ()
    else DR.error (InvalidAlignment { alignment = al; offset = ofs })
  else DR.error (InsufficientPermission { actual = cur_perm; required = perm })

let load t chunk ofs =
  let open DR.Syntax in
  let** () = check_valid_access t chunk ofs Readable in
  get_single t ofs chunk

let store t chunk ofs value =
  let open DR.Syntax in
  let** () = check_valid_access t chunk ofs Readable in
  set_single t ofs chunk value
