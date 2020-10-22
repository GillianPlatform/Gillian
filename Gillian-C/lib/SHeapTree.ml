open Gil_syntax
open Monadic
module DR = Delayed_result
module DO = Delayed_option
module SS = Utils.Containers.SS

type err =
  | UseAfterFree
  | BufferOverrun
  | InsufficientPermission of { required : Perm.t; actual : Perm.t }
  | InvalidAlignment       of { alignment : int; offset : Expr.t }
  | MissingResource
  | Unhandled              of string
  | RemovingNotOwned
  | HoleNotUndefined
  | MemoryNotFreed

exception FatalErr of err

let pp_err fmt = function
  | UseAfterFree -> Fmt.pf fmt "Use After Free"
  | BufferOverrun -> Fmt.pf fmt "Buffer Overrun"
  | InsufficientPermission { required; actual } ->
      Fmt.pf fmt "Insufficient Permision: Got %a but required %a" Perm.pp
        required Perm.pp actual
  | InvalidAlignment { alignment; offset } ->
      Fmt.pf fmt "Invalid alignment: %d should divide %a" alignment Expr.pp
        offset
  | MissingResource -> Fmt.pf fmt "MissingResource"
  | Unhandled e -> Fmt.pf fmt "Unhandled error with message : %s" e
  | RemovingNotOwned -> Fmt.pf fmt "Removing not owned"
  | HoleNotUndefined -> Fmt.pf fmt "Hole not undefined"
  | MemoryNotFreed -> Fmt.pf fmt "MemoryNotFreed"

let err_equal a b =
  match (a, b) with
  | MissingResource, MissingResource -> true
  | UseAfterFree, UseAfterFree -> true
  | BufferOverrun, BufferOverrun -> true
  | ( InsufficientPermission { required = ra; actual = aa },
      InsufficientPermission { required = rb; actual = ab } ) ->
      let open Perm.Infix in
      ra =% rb && aa =% ab
  | Unhandled a, Unhandled b -> String.equal a b
  | _ -> false

type 'a or_error = ('a, err) Result.t

type 'a d_or_error = ('a, err) DR.t

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

  (* let sure_equal ~pc (la, ha) (lb, hb) =
     let ( = ) = FOSolver.is_equal ~pc in
     la = lb && ha = hb *)

  let is_inside (la, ha) (lb, hb) =
    let open Formula.Infix in
    lb #<= la #&& (ha #<= hb)

  let split_at (l, h) x = ((l, x), (x, h))

  let lvars (a, b) = SS.union (Expr.lvars a) (Expr.lvars b)

  let substitution ~le_subst (a, b) = (le_subst a, le_subst b)
end

module Node = struct
  type qty = Totally | Partially

  let str_qty = function
    | Totally   -> "TOTALLY"
    | Partially -> "PARTIALLY"

  type mem_val =
    | Undef  of qty
    | Single of { chunk : Compcert.AST.memory_chunk; value : SVal.t }

  type t =
    | NotOwned of qty
    | MemVal   of {
        min_perm : Perm.t;
        exact_perm : Perm.t option;
        mem_val : mem_val;
      }

  let drop_perm_exn ~perm = function
    | NotOwned _            ->
        raise (FatalErr (Unhandled "Inconsistent permissions in the tree"))
    | MemVal { mem_val; _ } ->
        MemVal { min_perm = perm; exact_perm = Some perm; mem_val }

  let update_parent_perm t ~left ~right =
    match (t, left, right) with
    | ( MemVal { mem_val; _ },
        MemVal { exact_perm = epl; min_perm = mpl; _ },
        MemVal { exact_perm = epr; min_perm = mpr; _ } ) ->
        let exact_perm =
          match (epr, epl) with
          | Some r, Some l when r == l -> Some r
          | _ -> None
        in
        let min_perm = Perm.min mpl mpr in
        MemVal { mem_val; exact_perm; min_perm }
    | _ -> t

  let undefined ~perm =
    MemVal { mem_val = Undef Totally; min_perm = perm; exact_perm = Some perm }

  let not_owned = NotOwned Totally

  let pp fmt = function
    | NotOwned qty -> Fmt.pf fmt "%s NOT OWNED" (str_qty qty)
    | MemVal { exact_perm; mem_val; _ } -> (
        match mem_val with
        | Undef qty               ->
            Fmt.pf fmt "%s UNDEF (%a)" (str_qty qty)
              (Fmt.option ~none:(Fmt.any "None") Perm.pp)
              exact_perm
        | Single { chunk; value } ->
            Fmt.pf fmt "(%a : %a) (%a)" SVal.pp value Chunk.pp chunk
              (Fmt.option ~none:(Fmt.any "None") Perm.pp)
              exact_perm )

  let check_perm required node =
    match required with
    | None          -> Ok ()
    | Some required -> (
        match node with
        | NotOwned _ -> Error MissingResource
        | MemVal { min_perm = actual; _ } ->
            let open Perm.Infix in
            if actual >=% required then Ok ()
            else Error (InsufficientPermission { actual; required }) )

  let exact_perm = function
    | NotOwned Partially -> `KeepLooking
    | NotOwned Totally -> `StopLooking (Error MissingResource)
    | MemVal { exact_perm = None; _ } -> `KeepLooking
    | MemVal { exact_perm = Some x; _ } -> `StopLooking (Ok x)

  (* let equal a b =
     match (a, b) with
     | NotOwned x, NotOwned y -> x == y
     | ( MemVal { min_perm = min_perma; exact_perm = ex_perma; mem_val = vala },
         MemVal { min_perm = min_permb; exact_perm = ex_permb; mem_val = valb } )
       -> (
         min_perma == min_permb && ex_perma == ex_permb
         &&
         match (vala, valb) with
         | Undef x, Undef y -> x == y
         | Single { chunk = ca; value = va }, Single { chunk = cb; value = vb }
           -> Chunk.equal ca cb && SVal.equal va vb
         | _ -> false )
     | _ -> false *)

  let split = function
    (* TODO: Improve this *)
    | NotOwned Totally -> (NotOwned Totally, NotOwned Totally)
    | NotOwned Partially -> failwith "Should never split a partially owned node"
    | MemVal { exact_perm; min_perm; mem_val } -> (
        let mk mem_val = MemVal { min_perm; exact_perm; mem_val } in
        match mem_val with
        | Undef Totally   -> (mk (Undef Totally), mk (Undef Totally))
        | Single _        -> (mk (Undef Totally), mk (Undef Totally))
        | Undef Partially ->
            failwith "Should never split a partially undef node" )

  let merge ~left:a ~right:b =
    (* In the future, this can get more precise, for example composing single values *)
    match (a, b) with
    | NotOwned Totally, NotOwned Totally -> NotOwned Totally
    | NotOwned _, _ | _, NotOwned _ -> NotOwned Partially
    | ( MemVal { exact_perm = ex_perma; min_perm = min_perma; mem_val = vala },
        MemVal { exact_perm = ex_permb; min_perm = min_permb; mem_val = valb } )
      -> (
        let min_perm = Perm.min min_perma min_permb in
        let exact_perm =
          match (ex_perma, ex_permb) with
          | Some pa, Some pb when pa == pb -> Some pa
          | _, _ -> None
        in
        let mk mem_val = MemVal { min_perm; mem_val; exact_perm } in
        match (vala, valb) with
        | Undef Totally, Undef Totally -> mk (Undef Totally)
        | _, _ -> mk (Undef Partially) )

  let decode ~chunk t =
    match t with
    | NotOwned _ -> Error MissingResource
    | MemVal { mem_val = Undef _; exact_perm; _ } ->
        Ok (SVal.SUndefined, exact_perm)
    | MemVal { mem_val = Single { chunk = m_chunk; value }; exact_perm; _ } ->
        Ok
          ( if Chunk.equal m_chunk chunk then (value, exact_perm)
          else (SUndefined, exact_perm) )

  let encode ~(perm : Perm.t) ~(chunk : Chunk.t) (sval : SVal.t) =
    let mem_val =
      match (sval, chunk) with
      | ( SVint _,
          (Mint8signed | Mint8unsigned | Mint16signed | Mint16unsigned | Mint32)
        )
      | SVlong _, Mint64
      | SVsingle _, Mfloat32
      | SVfloat _, Mfloat64 -> Single { chunk; value = sval }
      | Sptr _, c when Chunk.equal c Chunk.ptr -> Single { chunk; value = sval }
      | _ -> Single { chunk; value = SUndefined }
    in
    MemVal { exact_perm = Some perm; min_perm = perm; mem_val }

  let lvars = function
    | MemVal { mem_val = Single { value = e; _ }; _ } -> SVal.lvars e
    | _ -> SS.empty

  let substitution ~sval_subst n =
    let smv = function
      | Single s -> Single { s with value = sval_subst s.value }
      | u        -> u
    in
    match n with
    | MemVal mv -> MemVal { mv with mem_val = smv mv.mem_val }
    | no        -> no
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

  let is_empty { node; _ } =
    match node with
    | NotOwned Totally -> true
    | _                -> false

  (* let rec equal ~pc a b =
     Node.equal a.node b.node
     && Range.sure_equal ~pc a.span b.span
     &&
     match (a.children, b.children) with
     | None, None -> true
     | Some (a1, a2), Some (b1, b2) -> equal ~pc a1 b1 && equal ~pc a2 b2
     | _ -> false *)

  let make ~node ~span ?children () = { node; span; children }

  let with_children t ~left ~right = { t with children = Some (left, right) }

  let of_children _ ~left ~right =
    let span = (fst left.span, snd right.span) in
    let node = Node.merge ~left:left.node ~right:right.node in
    let children = Some (left, right) in
    { span; children; node }

  let update_parent_perm t ~left ~right =
    let { node; span; _ } = t in
    let new_node =
      Node.update_parent_perm node ~left:left.node ~right:right.node
    in
    { node = new_node; span; children = Some (left, right) }

  let sval_leaf ~low ~perm ~value ~chunk =
    let node = Node.encode ~perm ~chunk value in
    let span = Range.of_low_and_chunk low chunk in
    make ~node ~span ()

  let undefined ?(perm = Perm.Freeable) span =
    make ~node:(Node.undefined ~perm) ~span ()

  let create_root range =
    { children = None; span = range; node = NotOwned Totally }

  let rec split ~range t : (Node.t * t * t) Delayed.t =
    (* this function splits a tree and returns the node in the given range *)
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
      Delayed.return (left_node, left, right)
    else
      if%sat oh #== nh then
        let left_node, right_node = Node.split t.node in
        let left_span, right_span = Range.split_at old_span nl in
        let left = make ~node:left_node ~span:left_span () in
        let right = make ~node:right_node ~span:right_span () in
        Delayed.return (left_node, left, right)
      else
        (* We're first splitting on the left then splitting again on the right *)
        let left_node, right_node = Node.split t.node in
        let left_span, right_span = Range.split_at old_span nl in
        let left = make ~node:left_node ~span:left_span () in
        let full_right = make ~node:right_node ~span:right_span () in
        let* node, right_left, right_right = split ~range full_right in
        let right =
          with_children full_right ~left:right_left ~right:right_right
        in
        Delayed.return (node, left, right)

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

  let frame_range (t : t) ~replace_node ~rebuild_parent (range : Range.t) :
      (Node.t * t, err) DR.t =
    let open DR.Syntax in
    let rec frame_inside (t : t) (range : Range.t) =
      if%sat Range.is_equal range t.span then
        match replace_node t with
        | Ok new_tree -> DR.ok (t.node, new_tree)
        | Error err   -> DR.error err
      else
        match t.children with
        | Some (left, right) ->
            if%sat Range.is_inside range left.span then
              let++ node, left = frame_inside left range in
              (node, rebuild_parent t ~left ~right)
            else
              if%sat Range.is_inside range right.span then
                let++ node, right = frame_inside right range in
                (node, rebuild_parent t ~left ~right)
              else DR.error (Unhandled "wrong pre-cut")
        | None               ->
            let open Delayed.Syntax in
            let+ node, left, right = split ~range t in
            Ok (node, rebuild_parent t ~left ~right)
    in
    let open Delayed.Syntax in
    let* root = extend_if_needed t range in
    frame_inside root range

  let get_node (t : t) range : (Node.t * t, err) DR.t =
    let replace_node x = Ok x in
    let rebuild_parent = with_children in
    frame_range t ~replace_node ~rebuild_parent range

  let set_node (t : t) range node : (t, err) DR.t =
    let open DR.Syntax in
    let replace_node _ = Ok (make ~node ~span:range ()) in
    let rebuild_parent = of_children in
    let++ _, t = frame_range t ~replace_node ~rebuild_parent range in
    t

  let get_single (t : t) (low : Expr.t) (chunk : Chunk.t) :
      (SVal.t * Perm.t option * t, err) DR.t =
    let open DR.Syntax in
    let replace_node x = Ok x in
    let rebuild_parent = with_children in
    let range = Range.of_low_and_chunk low chunk in
    let+* node, tree = frame_range t ~replace_node ~rebuild_parent range in
    Result.map
      (fun (sval, perm) -> (sval, perm, tree))
      (Node.decode ~chunk node)

  let set_single
      (t : t) (low : Expr.t) (chunk : Chunk.t) (sval : SVal.t) (perm : Perm.t) :
      (t, err) DR.t =
    let open DR.Syntax in
    let replace_node _ = Ok (sval_leaf ~low ~chunk ~value:sval ~perm) in
    let rebuild_parent = of_children in
    let range = Range.of_low_and_chunk low chunk in
    let++ _, t = frame_range t ~replace_node ~rebuild_parent range in
    t

  let load (t : t) (low : Expr.t) (chunk : Chunk.t) : (SVal.t * t, err) DR.t =
    let open DR.Syntax in
    let open Perm.Infix in
    let range = Range.of_low_and_chunk low chunk in
    let replace_node node =
      match node.node with
      | Node.NotOwned _        -> Error MissingResource
      | MemVal { min_perm; _ } ->
          if min_perm >=% Readable then Ok node
          else
            Error
              (InsufficientPermission { required = Readable; actual = min_perm })
    in
    let rebuild_parent = with_children in
    let+* node, tree = frame_range t ~replace_node ~rebuild_parent range in
    Result.map (fun (sval, _) -> (sval, tree)) (Node.decode ~chunk node)

  let store (t : t) (low : Expr.t) (chunk : Chunk.t) (sval : SVal.t) :
      (t, err) DR.t =
    let open DR.Syntax in
    let open Perm.Infix in
    let range = Range.of_low_and_chunk low chunk in
    let replace_node node =
      match node.node with
      | NotOwned _             -> Error MissingResource
      | MemVal { min_perm; _ } ->
          if min_perm >=% Writable then
            Ok (sval_leaf ~low ~chunk ~value:sval ~perm:min_perm)
          else
            Error
              (InsufficientPermission { required = Writable; actual = min_perm })
    in
    let rebuild_parent = of_children in
    let++ _, tree = frame_range t ~replace_node ~rebuild_parent range in
    tree

  let get_perm_at (tree : t) (ofs : Expr.t) : (Perm.t, err) DR.t =
    let range =
      let open Expr.Infix in
      (ofs, ofs +. Expr.num 1.)
    in
    let { span; _ } = tree in
    let rec rec_call treep =
      match Node.exact_perm treep.node with
      | `StopLooking r -> DR.of_result r
      | `KeepLooking   ->
          let left, right = Option.get treep.children in
          if%sat Range.is_inside range left.span then rec_call left
          else rec_call right
    in
    if%sat Range.is_inside range span then rec_call tree
    else DR.error MissingResource

  let drop_perm (t : t) (low : Expr.t) (high : Expr.t) (perm : Perm.t) :
      (t, err) DR.t =
    let rec rec_set_perm { node; span; children } =
      let node = Node.drop_perm_exn ~perm node in
      let children =
        Option.map (fun (a, b) -> (rec_set_perm a, rec_set_perm b)) children
      in
      { node; span; children }
    in
    let open DR.Syntax in
    let range = Range.make low high in
    let replace_node node =
      match node.node with
      | NotOwned _ -> Error MissingResource
      | MemVal { min_perm = Freeable; _ } -> Ok (rec_set_perm node)
      | MemVal { min_perm; _ } ->
          Error
            (InsufficientPermission { required = Freeable; actual = min_perm })
    in
    let rebuild_parent = update_parent_perm in
    let++ _, t = frame_range t ~replace_node ~rebuild_parent range in
    t

  let rec lvars { node; span; children } =
    let node_lvars = Node.lvars node in
    let span_lvars = Range.lvars span in
    let children_lvars =
      match children with
      | Some (a, b) -> SS.union (lvars a) (lvars b)
      | None        -> SS.empty
    in
    SS.union (SS.union node_lvars span_lvars) children_lvars

  let rec assertions ~loc { node; span; children } =
    match node with
    | NotOwned Totally -> []
    | NotOwned Partially | MemVal { mem_val = Undef Partially; _ } ->
        let left, right = Option.get children in
        assertions ~loc left @ assertions ~loc right
    | MemVal { mem_val = Undef Totally; exact_perm = perm; _ } ->
        let low, high = span in
        [ Constr.hole ~loc ~low ~high ~perm ]
    | MemVal { mem_val = Single { chunk; value }; exact_perm = perm; _ } ->
        let sval, types = SVal.to_gil_expr value in
        let types =
          List.map
            (let open Formula.Infix in
            fun (x, t) -> Asrt.Pure (Expr.typeof x) #== (Lit (Type t)))
            types
        in
        Constr.single ~loc ~ofs:(fst span) ~chunk ~sval ~perm :: types

  let rec substitution ~sval_subst ~le_subst { node; span; children } =
    let node = Node.substitution ~sval_subst node in
    let span = Range.substitution ~le_subst span in
    let children =
      Option.map
        (fun (left, right) ->
          let f = substitution ~sval_subst ~le_subst in
          (f left, f right))
        children
    in
    { node; span; children }
end

type t = Freed | Tree of { bounds : Range.t option; root : Tree.t option }

let pp fmt = function
  | Freed                 -> Fmt.pf fmt "FREED"
  | Tree { bounds; root } ->
      let pp_aux fmt (bounds, root) =
        Fmt.pf fmt "%a@ %a"
          (Fmt.option ~none:(Fmt.any "NO BOUNDS") Range.pp)
          bounds
          (Fmt.option ~none:(Fmt.any "EMPTY") Tree.pp)
          root
      in
      (Fmt.parens (Fmt.vbox pp_aux)) fmt (bounds, root)

let empty =
  let bounds = None in
  let root = None in
  Tree { bounds; root }

let is_empty t =
  match t with
  | Freed                 -> false
  | Tree { bounds; root } ->
      Option.is_none bounds
      && Option.fold ~none:true ~some:(fun root -> Tree.is_empty root) root

let freed = Freed

let lvars = function
  | Freed                 -> SS.empty
  | Tree { bounds; root } ->
      SS.union
        (Option.fold ~none:SS.empty ~some:Range.lvars bounds)
        (Option.fold ~none:SS.empty ~some:Tree.lvars root)

let get_root = function
  | Freed  -> Error UseAfterFree
  | Tree x -> Ok x.root

let is_in_bounds range bounds =
  match bounds with
  | None        -> Formula.True
  | Some bounds -> Range.is_inside range bounds

let get_perm_at t ofs =
  let open DR.Syntax in
  match t with
  | Freed                 -> DR.ok None
  | Tree { bounds; root } ->
      let is_in_bounds =
        let open Expr.Infix in
        is_in_bounds (ofs, ofs +. Expr.num 1.) bounds
      in
      if%sat is_in_bounds then
        match root with
        | None      -> DR.error MissingResource
        | Some root ->
            let++ perm = Tree.get_perm_at root ofs in
            Some perm
      else DR.ok None

let get_bounds = function
  | Freed  -> Error UseAfterFree
  | Tree x -> Ok x.bounds

let set_bounds t bounds =
  match t with
  | Freed  -> Error UseAfterFree
  | Tree x -> Ok (Tree { x with bounds })

let rem_bounds t =
  match t with
  | Freed  -> Error UseAfterFree
  | Tree x -> Ok (Tree { x with bounds = None })

let with_root_opt t root =
  match t with
  | Freed  -> Error UseAfterFree
  | Tree x -> Ok (Tree { x with root })

let with_root t root = with_root_opt t (Some root)

let alloc low high =
  let bounds = Range.make low high in
  Tree { root = Some (Tree.undefined bounds); bounds = Some bounds }

let drop_perm t low high new_perm =
  let open DR.Syntax in
  match t with
  | Freed                 -> DR.error UseAfterFree
  | Tree { bounds; root } -> (
      match root with
      | None      -> DR.error MissingResource
      | Some tree ->
          let++ new_root = Tree.drop_perm tree low high new_perm in
          Tree { bounds; root = Some new_root } )

let free t low high =
  let open DR.Syntax in
  let** bounds = DR.of_result (get_bounds t) in
  match t with
  (* Can't free something already freed *)
  | Freed -> DR.error UseAfterFree
  | Tree tree -> (
      (* Can only free if entirely freeable *)
      match bounds with
      | None        -> DR.error MissingResource
      | Some bounds ->
          if%ent Range.is_equal (low, high) bounds then
            match tree.root with
            | None      -> DR.error MissingResource
            | Some root ->
                let+* node, _ = Tree.get_node root (low, high) in
                Result.map
                  (fun () -> Freed)
                  (Node.check_perm (Some Freeable) node)
          else
            DR.error
              (Unhandled
                 "Freeing only part of an object (this might need fixing in \
                  the MM)") )

let get_single t low chunk =
  let open DR.Syntax in
  let range = Range.of_low_and_chunk low chunk in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None      -> DR.error MissingResource
    | Some root ->
        let** value, perm, root_framed = Tree.get_single root low chunk in
        let++ wroot = DR.of_result (with_root t root_framed) in
        (value, perm, wroot)
  else DR.error BufferOverrun

let set_single t low chunk sval perm =
  let open DR.Syntax in
  let range = Range.of_low_and_chunk low chunk in
  let** root = DR.of_result (get_root t) in
  let root = Option.value root ~default:(Tree.create_root range) in
  let** root_set = Tree.set_single root low chunk sval perm in
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
        let** root_set = Tree.set_node root range Node.not_owned in
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
        let** node, root_framed = Tree.get_node root range in
        let res =
          match node with
          | MemVal { mem_val = Undef Totally; exact_perm = perm; _ } -> Ok perm
          | NotOwned _ -> Error MissingResource
          | _ -> Error HoleNotUndefined
        in
        let++ wroot =
          DR.of_result
            (Result.bind res (fun perm ->
                 Result.map (fun mem -> (mem, perm)) (with_root t root_framed)))
        in
        wroot
  else DR.error BufferOverrun

let set_hole t low high perm =
  let open DR.Syntax in
  let range = (low, high) in
  let** root = DR.of_result (get_root t) in
  let root = Option.value ~default:(Tree.create_root range) root in
  let** root_set = Tree.set_node root range (Node.undefined ~perm) in
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
        let** root_set = Tree.set_node root range Node.not_owned in
        DR.ok (Some root_set)
  in
  let** bounds = DR.of_result (get_bounds t) in
  let learned =
    match bounds with
    | None        -> []
    | Some bounds -> [ Range.is_inside range bounds ]
  in
  DR.of_result ~learned (with_root_opt t root_set)

let get_freed t =
  match t with
  | Freed -> Ok ()
  | _     -> Error MemoryNotFreed

let check_valid_alignment chunk ofs =
  let al = Chunk.align chunk in
  let al_expr = Expr.num (float_of_int al) in
  let divides x y =
    let open Formula.Infix in
    Expr.(y #== (num 0.)) #|| ((Expr.fmod y x) #== (Expr.num 0.))
  in
  if%sat divides al_expr ofs then DR.ok ()
  else DR.error (InvalidAlignment { offset = ofs; alignment = al })

let load t chunk ofs =
  let open DR.Syntax in
  let** () = check_valid_alignment chunk ofs in
  let range = Range.of_low_and_chunk ofs chunk in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None      -> DR.error MissingResource
    | Some root ->
        let** value, root = Tree.load root ofs chunk in
        let++ wroot = DR.of_result (with_root t root) in
        (value, wroot)
  else DR.error BufferOverrun

let store t chunk ofs value =
  let open DR.Syntax in
  let** () = check_valid_alignment chunk ofs in
  let range = Range.of_low_and_chunk ofs chunk in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None      -> DR.error MissingResource
    | Some root ->
        let** root = Tree.store root ofs chunk value in
        DR.of_result (with_root t root)
  else DR.error BufferOverrun

let assertions ~loc t =
  let loc = Expr.loc_from_loc_name loc in
  match t with
  | Freed  -> [ Constr.freed ~loc ]
  | Tree x ->
      let bounds = Constr.bounds_opt ~loc ~bounds:x.bounds in
      let tree =
        match x.root with
        | None      -> []
        | Some root -> Tree.assertions ~loc root
      in
      bounds :: tree

(* let merge old new_ =
  (* the new tree has priority over the old tree. *)
  match (new_, old) with
  | Freed, Freed -> Freed
  | Tree new_tree, Tree old_tree ->
    let def_bounds = (
      match new_tree.bounds with
      | Some bounds -> Some bounds
      | None -> old_tree.bounds
    ) in
    let def_permission = new_tree.perm in
    let def_root = 
  | _ -> failwith "Incompatible trees to merge"
     *)

let substitution ~le_subst ~sval_subst t =
  match t with
  | Freed                 -> Freed
  | Tree { bounds; root } ->
      let bounds = Option.map (Range.substitution ~le_subst) bounds in
      let root = Option.map (Tree.substitution ~sval_subst ~le_subst) root in
      Tree { bounds; root }
