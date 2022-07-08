open Gil_syntax
open Monadic
module DR = Delayed_result
module DO = Delayed_option
module SS = Utils.Containers.SS
module NSVal = SVal
module SVal = MonadicSVal
module SVArr = SVal.SVArray
module CoreP = Constr.Core

let log_string s = Logging.verbose (fun fmt -> fmt "SHEAPTREE CHECKING: %s" s)

type err =
  | UseAfterFree
  | BufferOverrun
  | InsufficientPermission of { required : Perm.t; actual : Perm.t }
  | InvalidAlignment of { alignment : int; offset : Expr.t }
  | MissingResource
  | Unhandled of string
  | RemovingNotOwned
  | WrongMemVal
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
  | WrongMemVal -> Fmt.pf fmt "WrongMemVal"
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

module PathTaken = struct
  (** Going through a tree can become quite expensive.
      Remover is always called right after the getter, so we take note of the last path taken and on remove we just go for it directly.  *)

  type t = Left | Right | Here [@@deriving yojson]
end

module Range = struct
  type t = Expr.t * Expr.t [@@deriving yojson]

  let pp fmt (a, b) = Fmt.pf fmt "@[<h>[%a; %a[@]" Expr.pp a Expr.pp b
  let make low high = (low, high)

  module Lift = struct
    open Debugger.DebuggerTypes

    let as_variables
        ~(make_node :
           name:string ->
           value:string ->
           ?children:variable list ->
           unit ->
           variable)
        (low, high) =
      let str = Fmt.to_to_string (Fmt.hbox Expr.pp) in
      let from = make_node ~name:"From" ~value:(str low) () in
      let to_ = make_node ~name:"To" ~value:(str high) () in
      [ from; to_ ]
  end

  let of_low_and_chunk low chunk =
    let open Expr.Infix in
    let len = Expr.int (Chunk.size chunk) in
    (low, low + len)

  let of_low_chunk_and_size low chunk size =
    let open Expr.Infix in
    let sz_chunk = Expr.int (Chunk.size chunk) in
    (low, low + (sz_chunk * size))

  let is_equal (la, ha) (lb, hb) =
    let open Formula.Infix in
    la #== lb #&& (ha #== hb)

  let is_inside (la, ha) (lb, hb) =
    let open Formula.Infix in
    lb #<= la #&& (ha #<= hb)

  let size (a, b) = Expr.Infix.( - ) b a

  let point_strictly_inside x (l, h) =
    let open Formula.Infix in
    l #< x #&& (x #< h)

  let split_at (l, h) x = ((l, x), (x, h))
  let lvars (a, b) = SS.union (Expr.lvars a) (Expr.lvars b)
  let alocs (a, b) = SS.union (Expr.alocs a) (Expr.alocs b)
  let substitution ~le_subst (a, b) = (le_subst a, le_subst b)
end

module Node = struct
  type qty = Totally | Partially [@@deriving yojson]

  let str_qty = function
    | Totally -> "TOTALLY"
    | Partially -> "PARTIALLY"

  type mem_val =
    | Zeros
    | Undef of qty
    | Single of { chunk : Chunk.t; value : SVal.t }
    | Array of { chunk : Chunk.t; values : SVArr.t }
  [@@deriving yojson]

  let eq_mem_val ma mb =
    match (ma, mb) with
    | Zeros, Zeros
    | Undef Totally, Undef Totally
    | Undef Partially, Undef Partially -> true
    | ( Single { chunk = chunka; value = valuea },
        Single { chunk = chunkb; value = valueb } )
      when Chunk.equal chunka chunkb && SVal.equal valuea valueb -> true
    | ( Array { chunk = chunka; values = valuesa },
        Array { chunk = chunkb; values = valuesb } )
      when Chunk.equal chunka chunkb && SVArr.equal valuesa valuesb -> true
    | _ -> false

  type t =
    | NotOwned of qty
    | MemVal of {
        min_perm : Perm.t;
        exact_perm : Perm.t option;
        mem_val : mem_val;
      }
  [@@deriving yojson]

  let make_owned ~mem_val ~perm =
    MemVal { mem_val; min_perm = perm; exact_perm = Some perm }

  let drop_perm_exn ~perm = function
    | NotOwned _ ->
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

  let undefined ~perm = make_owned ~perm ~mem_val:(Undef Totally)
  let not_owned = NotOwned Totally

  let pp fmt = function
    | NotOwned qty -> Fmt.pf fmt "%s NOT OWNED" (str_qty qty)
    | MemVal { exact_perm; mem_val; _ } -> (
        match mem_val with
        | Zeros -> Fmt.pf fmt "ZEROS (%a)" (Fmt.Dump.option Perm.pp) exact_perm
        | Undef qty ->
            Fmt.pf fmt "%s UNDEF (%a)" (str_qty qty) (Fmt.Dump.option Perm.pp)
              exact_perm
        | Single { chunk; value } ->
            Fmt.pf fmt "(%a : %a) (%a)" SVal.pp value Chunk.pp chunk
              (Fmt.Dump.option Perm.pp) exact_perm
        | Array { chunk; values } ->
            Fmt.pf fmt "(%a : many %a) (%a)" SVArr.pp values Chunk.pp chunk
              (Fmt.Dump.option Perm.pp) exact_perm)

  let check_perm required node =
    match required with
    | None -> Ok ()
    | Some required -> (
        match node with
        | NotOwned _ -> Error MissingResource
        | MemVal { min_perm = actual; _ } ->
            let open Perm.Infix in
            if actual >=% required then Ok ()
            else Error (InsufficientPermission { actual; required }))

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

  let split ~span:(low, high) ~at node =
    Logging.tmi (fun m ->
        m "ABOUT TO SPLIT NODE THAT HAS SPAN %a AT %a" Range.pp (low, high)
          Expr.pp at);
    let open Delayed.Syntax in
    match node with
    | NotOwned Totally -> Delayed.return (NotOwned Totally, NotOwned Totally)
    | NotOwned Partially -> failwith "Should never split a partially owned node"
    | MemVal { exact_perm; min_perm; mem_val } -> (
        let mk mem_val = MemVal { min_perm; exact_perm; mem_val } in
        let make_pair left right = Delayed.return (mk left, mk right) in
        match mem_val with
        | Zeros -> make_pair Zeros Zeros
        | Undef Totally -> make_pair (Undef Totally) (Undef Totally)
        | Single _ -> make_pair (Undef Totally) (Undef Totally)
        | Array { chunk; values } ->
            let open Expr.Infix in
            let mk_arr ~chunk values =
              let+ value = SVArr.to_single_value ~chunk values in
              match value with
              | Some value -> mk (Single { chunk; value })
              | None -> mk (Array { chunk; values })
            in
            let sz = Expr.int (Chunk.size chunk) in
            let len_left = (at - low) / sz in
            let len_right = (high - at) / sz in
            let left_arr = SVArr.array_sub values (Expr.int 0) len_left in
            let right_arr = SVArr.array_sub values len_left len_right in
            let* left = mk_arr ~chunk left_arr in
            let+ right = mk_arr ~chunk right_arr in
            (left, right)
        | Undef Partially ->
            failwith "Should never split a partially undef node")

  let merge ~left ~right =
    let open SVal.Infix in
    let ret = Delayed.return in
    let a, size_a = left in
    let b, size_b = right in
    match (a, b) with
    | NotOwned Totally, NotOwned Totally -> ret (NotOwned Totally)
    | NotOwned _, _ | _, NotOwned _ -> ret (NotOwned Partially)
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
        | Zeros, Zeros -> ret (mk Zeros)
        | Undef Totally, Undef Totally -> ret (mk (Undef Totally))
        | ( Single { chunk = chunk_l; value = value_l },
            Single { chunk = chunk_r; value = value_r } )
          when Chunk.equal chunk_l chunk_r -> (
            match value_l ^: SVArr.singleton value_r with
            | Some values -> ret (mk (Array { chunk = chunk_l; values }))
            | None -> ret (mk (Undef Partially)))
        | ( Single { chunk = chunk_l; value = value_l },
            Array { chunk = chunk_r; values = values_r } )
          when Chunk.equal chunk_l chunk_r -> (
            match value_l ^: values_r with
            | Some values -> ret (mk (Array { chunk = chunk_l; values }))
            | None -> ret (mk (Undef Partially)))
        | ( Array { chunk = chunk_l; values = values_l },
            Single { chunk = chunk_r; value = value_r } )
          when Chunk.equal chunk_l chunk_r -> (
            match values_l @: SVArr.singleton value_r with
            | Some values -> ret (mk (Array { chunk = chunk_l; values }))
            | None -> ret (mk (Undef Partially)))
        | ( Array { chunk = chunk_l; values = values_l },
            Array { chunk = chunk_r; values = values_r } )
          when Chunk.equal chunk_l chunk_r ->
            let size_l, size_r =
              let open Expr.Infix in
              let size_chunk = Chunk.size_expr chunk_l in
              (size_a / size_chunk, size_b / size_chunk)
            in
            Delayed.map
              (SVArr.concat_knowing_size (values_l, size_l) (values_r, size_r))
              (fun values -> mk (Array { chunk = chunk_l; values }))
        | Array { chunk; values }, Undef _ ->
            let size_l, size_r =
              let open Expr.Infix in
              let size_chunk = Chunk.size_expr chunk in
              (size_a / size_chunk, size_b / size_chunk)
            in
            Delayed.map
              (SVArr.concat_knowing_size (values, size_l) (AllUndef, size_r))
              (fun values -> mk (Array { chunk; values }))
        | Array { chunk; values }, Zeros ->
            let size_l, size_r =
              let open Expr.Infix in
              let size_chunk = Chunk.size_expr chunk in
              (size_a / size_chunk, size_b / size_chunk)
            in
            Delayed.map
              (SVArr.concat_knowing_size (values, size_l) (AllZeros, size_r))
              (fun values -> mk (Array { chunk; values }))
        | Undef _, Array { chunk; values } ->
            let size_l, size_r =
              let open Expr.Infix in
              let size_chunk = Chunk.size_expr chunk in
              (size_a / size_chunk, size_b / size_chunk)
            in
            Delayed.map
              (SVArr.concat_knowing_size (AllUndef, size_r) (values, size_l))
              (fun values -> mk (Array { chunk; values }))
        | Zeros, Array { chunk; values } ->
            let size_l, size_r =
              let open Expr.Infix in
              let size_chunk = Chunk.size_expr chunk in
              (size_a / size_chunk, size_b / size_chunk)
            in
            Delayed.map
              (SVArr.concat_knowing_size (AllZeros, size_r) (values, size_l))
              (fun values -> mk (Array { chunk; values }))
        | _, _ -> ret (mk (Undef Partially)))

  let decode_bytes_to_unsigned_int ~chunk arr size =
    let open Delayed.Syntax in
    let* values = SVArr.reduce arr in
    match values with
    | AllZeros -> Delayed.return (SVal.zero_of_chunk chunk)
    | Arr e ->
        let two_pow_8 i = Int.shift_left 1 (8 * i) in
        let open Expr.Infix in
        let open Formula.Infix in
        (* FIXME: This assumes big endian *)
        if%sat (Expr.list_length e) #== (Expr.int size) then
          let bytes = List.init size (fun i -> Expr.list_nth e i) in
          let _, v =
            List.fold_left
              (fun (i, acc) v ->
                (Int.pred i, (v * Expr.int (two_pow_8 i)) + acc))
              (Int.pred size, Expr.int 0)
              bytes
          in
          let learned =
            List.filter_map
              (function
                | Expr.Lit Undefined -> None
                | byte ->
                    Some byte #>= (Expr.int 0) #&& (byte #<= (Expr.int 255)))
              bytes
          in
          let* v = SVal.of_chunk_and_expr chunk v in
          Delayed.return ~learned v
        else Delayed.return SVal.SUndefined
    | _ -> Delayed.return SVal.SUndefined

  let decode ~chunk t =
    let open Delayed.Syntax in
    match t with
    | NotOwned _ -> DR.error MissingResource
    | MemVal { mem_val = Zeros; exact_perm; _ } ->
        DR.ok (SVal.zero_of_chunk chunk, exact_perm)
    | MemVal { mem_val = Undef _; exact_perm; _ } ->
        DR.ok (SVal.SUndefined, exact_perm)
    | MemVal { mem_val = Single { chunk = m_chunk; value }; exact_perm; _ } ->
        DR.ok
          (if Chunk.equal m_chunk chunk then (value, exact_perm)
          else (SUndefined, exact_perm))
    | MemVal
        { mem_val = Array { chunk = Mint8unsigned; values }; exact_perm; _ }
      when Chunk.equal chunk Mint16unsigned ->
        let+ decoded = decode_bytes_to_unsigned_int ~chunk values 2 in
        Ok (decoded, exact_perm)
    | MemVal
        { mem_val = Array { chunk = Mint8unsigned; values }; exact_perm; _ }
      when Chunk.equal chunk Mint32 ->
        let+ decoded = decode_bytes_to_unsigned_int ~chunk values 4 in
        Ok (decoded, exact_perm)
    | MemVal
        { mem_val = Array { chunk = Mint8unsigned; values }; exact_perm; _ }
      when Chunk.equal chunk Mint64 ->
        let+ decoded = decode_bytes_to_unsigned_int ~chunk values 8 in
        Ok (decoded, exact_perm)
    | MemVal { mem_val = Array { chunk = chunk_b; values }; exact_perm; _ }
      when Chunk.equal chunk chunk_b ->
        let* values = SVArr.reduce values in
        if Chunk.equal chunk Chunk.ptr then
          match values with
          | AllZeros -> DR.ok (SVal.zero_of_chunk chunk, exact_perm)
          | Arr (EList [ a ]) -> (
              let obj = SVal.Patterns.obj in
              let integer = SVal.Patterns.integer in
              match%ent a with
              | integer ->
                  let v =
                    if Compcert.Archi.ptr64 then SVal.SVlong a else SVal.SVint a
                  in
                  DR.ok (v, exact_perm)
              | obj -> (
                  let* value = SVal.of_gil_expr a in
                  match value with
                  | Some value -> DR.ok (value, exact_perm)
                  | None -> failwith "Look here, something's wrong")
              | _ -> DR.ok (SVal.SUndefined, exact_perm))
          | _ -> DR.ok (SVal.SUndefined, exact_perm)
        else
          let+ single =
            DO.value ~default:SVal.SUndefined
              (SVArr.to_single_value ~chunk values)
          in
          Ok (single, exact_perm)
    | MemVal { mem_val = Array { chunk = _; _ }; exact_perm; _ } ->
        DR.ok (SVal.SUndefined, exact_perm)

  let decode_arr ~size ~chunk t =
    let open Delayed.Syntax in
    let split_array_in ~size ~amount arr =
      match arr with
      | SVArr.AllUndef -> List.init amount (fun _ -> SVArr.AllUndef)
      | AllZeros -> List.init amount (fun _ -> SVArr.AllZeros)
      | Arr e ->
          let i f = Expr.int f in
          List.init amount (fun k ->
              let values =
                Expr.list_sub ~lst:e ~start:(i (k * size)) ~size:(i size)
              in
              SVArr.Arr values)
    in
    let decode_several_unsigned_ints_of_bytes ~amount ~chunk arr =
      let arrs = split_array_in ~size:(Chunk.size chunk) ~amount arr in
      let size = Chunk.size chunk in
      let rec get_values = function
        | [] -> Fmt.failwith "EMPTY ARRAY ??"
        | [ one_value ] ->
            let+ that_value =
              decode_bytes_to_unsigned_int ~chunk one_value size
            in
            SVArr.singleton that_value
        | a :: r ->
            let* that_value = decode_bytes_to_unsigned_int ~chunk a size in
            let+ rest = get_values r in
            Option.get @@ SVArr.array_cons that_value rest
      in
      get_values arrs
    in
    match t with
    | NotOwned _ -> DR.error MissingResource
    | MemVal { mem_val = Zeros; exact_perm; _ } ->
        DR.ok (SVArr.AllZeros, exact_perm)
    | MemVal { mem_val = Undef _; exact_perm; _ } ->
        DR.ok (SVArr.AllUndef, exact_perm)
    | MemVal { mem_val = Single { chunk = m_chunk; value }; exact_perm; _ } ->
        DR.ok
          (if Chunk.equal m_chunk chunk then (SVArr.singleton value, exact_perm)
          else (AllUndef, exact_perm))
    | MemVal
        { mem_val = Array { chunk = Mint8unsigned; values }; exact_perm; _ }
      when Chunk.equal chunk Mint64 -> (
        match size with
        | Expr.Lit (Int amount) ->
            let amount = Z.to_int amount in
            let+ arr =
              decode_several_unsigned_ints_of_bytes ~amount ~chunk values
            in
            Ok (arr, exact_perm)
        | _ -> DR.ok (SVArr.AllUndef, exact_perm))
    | MemVal { mem_val = Array { chunk = m_chunk; values }; exact_perm; _ }
      when Chunk.equal m_chunk chunk ->
        let* () = SVArr.learn_chunk ~chunk ~size values in
        DR.ok (values, exact_perm)
    | MemVal { mem_val = Array _; exact_perm; _ } ->
        DR.ok (SVArr.AllUndef, exact_perm)

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

  let encode_arr ~(perm : Perm.t) ~(chunk : Chunk.t) (sarr : SVArr.t) =
    (* FIXME: this is probably wrong *)
    let mem_val = Array { chunk; values = sarr } in
    MemVal { exact_perm = Some perm; min_perm = perm; mem_val }

  let lvars = function
    | MemVal { mem_val = Single { value = e; _ }; _ } -> SVal.lvars e
    | _ -> SS.empty

  let alocs = function
    | MemVal { mem_val = Single { value = e; _ }; _ } -> SVal.alocs e
    | MemVal { mem_val = Array { values = Arr e; _ }; _ } -> Expr.alocs e
    | _ -> SS.empty

  let substitution ~sval_subst ~svarr_subst n =
    let smv = function
      | Single s -> Single { s with value = sval_subst s.value }
      | Array a -> Array { a with values = svarr_subst a.values }
      | u -> u
    in
    match n with
    | MemVal mv -> MemVal { mv with mem_val = smv mv.mem_val }
    | no -> no
end

module Tree = struct
  type t = {
    node : Node.t;
    span : Range.t;
    children : (t * t) option;
    last_path : PathTaken.t option; [@ignore]
  }
  [@@deriving yojson]

  module Lift = struct
    open Debugger.DebuggerTypes

    let rec as_variable
        ~(make_node :
           name:string ->
           value:string ->
           ?children:variable list ->
           unit ->
           variable)
        (tree : t) : variable =
      let as_variable = as_variable ~make_node in
      let str pp = Fmt.to_to_string (Fmt.hbox pp) in
      let name = (str Range.pp) tree.span in
      let value = (str Node.pp) tree.node in
      let children =
        Option.map
          (fun (a, b) -> [ as_variable a; as_variable b ])
          tree.children
      in
      make_node ~name ~value ?children ()
  end

  let box_range_and_node span node =
    let open PrintBox in
    frame
    @@ hlist
         [
           hpad 2 @@ text (Fmt.to_to_string Range.pp @@ span);
           hpad 1 @@ text (Fmt.to_to_string Node.pp @@ node);
         ]

  let box_full t =
    let open PrintBox in
    let make { node; span; children; _ } =
      let node = box_range_and_node span node in
      let children =
        match children with
        | None -> []
        | Some (a, b) -> [ a; b ]
      in
      (node, children)
    in
    mk_tree make t

  let pp_full fmt t = PrintBox_text.pp fmt (box_full t)

  let is_empty { node; _ } =
    match node with
    | NotOwned Totally -> true
    | _ -> false

  let make ~node ~span ?children () = { node; span; children; last_path = None }

  (* Used to change the position of a tree. The start of the tree is going to be [start], but the spans don't change. *)
  let rec realign t start =
    let open Expr.Infix in
    let reduce e = Engine.Reduction.reduce_lexpr e in
    let l, h = t.span in
    let span = (start, reduce (start + h - l)) in
    let children =
      Option.map
        (fun (left, right) ->
          let left = realign left start in
          let _, m = left.span in
          let right = realign right m in
          (left, right))
        t.children
    in
    make ~node:t.node ~span ?children ()

  let with_children t ~left ~right =
    Delayed.return { t with children = Some (left, right); last_path = None }

  let of_children_s ~left ~right =
    let open Delayed.Syntax in
    let span = (fst left.span, snd right.span) in
    let+ node =
      Node.merge
        ~left:(left.node, Range.size left.span)
        ~right:(right.node, Range.size right.span)
    in
    let children =
      match node with
      | NotOwned Totally
      | MemVal { exact_perm = Some _; mem_val = Zeros | Undef Totally; _ } ->
          None
      | _ -> Some (left, right)
    in
    { span; children; node; last_path = None }

  let of_children _ ~left ~right = of_children_s ~left ~right

  let update_parent_perm t ~left ~right =
    let { node; span; _ } = t in
    let new_node =
      Node.update_parent_perm node ~left:left.node ~right:right.node
    in
    Delayed.return
      { node = new_node; span; children = Some (left, right); last_path = None }

  let sval_leaf ~low ~perm ~value ~chunk =
    let node = Node.encode ~perm ~chunk value in
    let span = Range.of_low_and_chunk low chunk in
    make ~node ~span ()

  let sarr_leaf ~low ~perm ~size ~array ~chunk =
    let node = Node.encode_arr ~perm ~chunk array in
    let span = Range.of_low_chunk_and_size low chunk size in
    make ~node ~span ()

  let undefined ?(perm = Perm.Freeable) span =
    make ~node:(Node.undefined ~perm) ~span ()

  let create_root range =
    {
      children = None;
      span = range;
      node = NotOwned Totally;
      last_path = Some Here;
    }

  let rec split ~range t : (Node.t * t * t) Delayed.t =
    (* this function splits a tree and returns the node in the given range *)
    (* We're assuming that range is inside old_span *)
    let open Formula.Infix in
    let open Delayed.Syntax in
    let old_span = t.span in
    let ol, oh = old_span in
    let nl, nh = range in
    if%sat
      log_string "ol #== nl";
      ol #== nl
    then
      let at = nh in
      let* left_node, right_node = Node.split ~span:old_span ~at t.node in
      let left_span, right_span = Range.split_at old_span at in
      let left = make ~node:left_node ~span:left_span () in
      let right = make ~node:right_node ~span:right_span () in
      Delayed.return (left_node, left, right)
    else
      if%sat
        log_string "oh #== nh";
        oh #== nh
      then
        let at = nl in
        let* left_node, right_node = Node.split ~span:old_span ~at t.node in
        let left_span, right_span = Range.split_at old_span nl in
        let left = make ~node:left_node ~span:left_span () in
        let right = make ~node:right_node ~span:right_span () in
        Delayed.return (right_node, left, right)
      else
        (* We're first splitting on the left then splitting again on the right *)
        let* left_node, right_node = Node.split ~span:old_span ~at:nl t.node in
        let left_span, right_span = Range.split_at old_span nl in
        let left = make ~node:left_node ~span:left_span () in
        let full_right = make ~node:right_node ~span:right_span () in
        let* node, right_left, right_right = split ~range full_right in
        let* right =
          with_children full_right ~left:right_left ~right:right_right
        in
        Delayed.return (node, left, right)

  let extend_if_needed t range =
    let open Formula.Infix in
    let open Delayed.Syntax in
    let rl, rh = range in
    let sl, sh = t.span in
    let* t_with_left =
      if%sat rl #< sl then
        let new_left_tree = make ~node:(NotOwned Totally) ~span:(rl, sl) () in
        let children = (new_left_tree, t) in
        Delayed.return
          (make ~node:(NotOwned Partially) ~span:(rl, sh) ~children ())
      else Delayed.return t
    in
    let sl, _ = t_with_left.span in
    let* result =
      if%sat rh #> sh then
        let new_right_tree = make ~node:(NotOwned Totally) ~span:(sh, rh) () in
        let children = (t_with_left, new_right_tree) in
        Delayed.return
          (make ~node:(NotOwned Partially) ~span:(sl, rh) ~children ())
      else Delayed.return t_with_left
    in
    Delayed.return result

  let frame_range (t : t) ~replace_node ~rebuild_parent (range : Range.t) :
      (t * t, err) DR.t =
    let open DR.Syntax in
    let open Delayed.Syntax in
    let rec extract (t : t) (range : Range.t) : (t * t option) Delayed.t =
      (* First result is the extracted tree, second is the remain *)
      let open Delayed in
      let open Syntax in
      if%sat
        log_string "EXTRACT range is equal span";
        Range.is_equal range t.span
      then return (t, None)
      else
        let left, right = Option.get t.children in
        if%sat
          log_string "EXTRACT range inside left";
          Range.is_inside range left.span
        then
          let* extracted, new_left = extract left range in
          let+ new_self =
            match new_left with
            | Some left -> of_children_s ~right ~left
            | None -> Delayed.return right
          in
          (extracted, Some new_self)
        else
          let* extracted, new_right = extract right range in
          let+ new_self =
            match new_right with
            | Some right -> of_children_s ~right ~left
            | None -> Delayed.return left
          in
          (extracted, Some new_self)
    in
    let rec add_to_the_right t addition : t Delayed.t =
      match t.children with
      | None -> of_children_s ~left:t ~right:addition
      | Some (left, right) ->
          let* new_right = add_to_the_right right addition in
          of_children_s ~left ~right:new_right
    in
    let rec frame_inside ~replace_node ~rebuild_parent (t : t) (range : Range.t)
        =
      Logging.verbose (fun fmt ->
          fmt "STARTING FRAME INSIDE WITH: %a" pp_full t);
      if%sat
        log_string "range equals span";
        Range.is_equal range t.span
      then (
        log_string "Range does equal span, replacing.";
        match replace_node t with
        | Ok new_tree -> DR.ok (t, { new_tree with last_path = Some Here })
        | Error err -> DR.error err)
      else
        match t.children with
        | Some (left, right) ->
            let _, mid = left.span in
            if%sat
              log_string "mid strictly in range";
              Range.point_strictly_inside mid range
            then
              let _, h = range in
              let upper_range = (mid, h) in
              let dont_replace_node t = Ok t in
              let** _, right =
                frame_inside ~replace_node:dont_replace_node
                  ~rebuild_parent:with_children right upper_range
              in
              let* extracted, right_opt = extract right upper_range in
              let* left = add_to_the_right left extracted in
              let* new_self =
                match right_opt with
                | Some right -> of_children_s ~left ~right
                | None -> Delayed.return left
              in
              frame_inside ~replace_node ~rebuild_parent new_self range
            else
              if%sat
                log_string "range inside left";
                Range.is_inside range left.span
              then
                let** node, left =
                  frame_inside ~replace_node ~rebuild_parent left range
                in
                let+ new_parent = rebuild_parent t ~left ~right in
                Ok (node, { new_parent with last_path = Some Left })
              else
                if%sat
                  log_string "range inside right";
                  Range.is_inside range right.span
                then
                  let** node, right =
                    frame_inside ~replace_node ~rebuild_parent right range
                  in
                  let+ new_parent = rebuild_parent t ~left ~right in
                  Ok (node, { new_parent with last_path = Some Right })
                else (
                  Logging.verbose (fun fmt ->
                      fmt
                        "ABOUT TO SAY PRECUT:\nLEFT: %a\nRIGHT: %a\n RANGE: %a"
                        Range.pp left.span Range.pp right.span Range.pp range);
                  DR.error (Unhandled "wrong pre-cut"))
        | None ->
            let open Delayed.Syntax in
            let* _, left, right = split ~range t in
            let* new_self = with_children t ~left ~right in
            Logging.verbose (fun fmt ->
                fmt "AFTER SPLITTING FOR %a: %a" Range.pp range pp_full new_self);
            frame_inside ~replace_node ~rebuild_parent new_self range
    in
    let open Delayed.Syntax in
    let* root = extend_if_needed t range in
    frame_inside ~replace_node ~rebuild_parent root range

  let get_node (t : t) range : (Node.t * t, err) DR.t =
    let open DR.Syntax in
    let replace_node x = Ok x in
    let rebuild_parent = with_children in
    let++ framed, rest = frame_range t ~replace_node ~rebuild_parent range in
    (framed.node, rest)

  let set_node (t : t) range node : (t, err) DR.t =
    let open DR.Syntax in
    let replace_node _ = Ok (make ~node ~span:range ()) in
    let rebuild_parent = of_children in
    let++ _, t = frame_range t ~replace_node ~rebuild_parent range in
    t

  let rem_last_get (t : t) : t =
    let rebuild
        ({ span = lspan; node = lnode; _ } as left)
        ({ span = rspan; node = rnode; _ } as right) =
      let span = (fst lspan, snd rspan) in
      match (lnode, rnode) with
      | NotOwned Totally, NotOwned Totally ->
          make ~node:(NotOwned Totally) ~span ()
      | _ -> make ~node:(NotOwned Partially) ~span ~children:(left, right) ()
    in
    let rec loop { span; children; last_path; _ } =
      match last_path with
      | None -> failwith "Imposible, removing last get, but can't follow a path"
      | Some Here -> make ~node:Node.not_owned ~span ()
      | Some Left ->
          let left, right = Option.get children in
          let left = loop left in
          rebuild left right
      | Some Right ->
          let left, right = Option.get children in
          let right = loop right in
          rebuild left right
    in
    loop t

  let get_array (t : t) (low : Expr.t) (chunk : Chunk.t) (size : Expr.t) :
      (SVArr.t * Perm.t option * t, err) DR.t =
    let open DR.Syntax in
    let open Delayed.Syntax in
    let* size = Delayed.reduce size in
    let replace_node x = Ok x in
    let rebuild_parent = with_children in
    let range = Range.of_low_chunk_and_size low chunk size in
    let** framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    let+* arr, perm = Node.decode_arr ~size ~chunk framed.node in
    Ok (arr, perm, tree)

  let set_array
      (t : t)
      (low : Expr.t)
      (size : Expr.t)
      (chunk : Chunk.t)
      (array : SVArr.t)
      (perm : Perm.t) : (t, err) DR.t =
    let open DR.Syntax in
    let open Delayed.Syntax in
    let replace_node _ = Ok (sarr_leaf ~low ~chunk ~array ~size ~perm) in
    let rebuild_parent = of_children in
    let range = Range.of_low_chunk_and_size low chunk size in
    let** _, t = frame_range t ~replace_node ~rebuild_parent range in
    let+ () = SVArr.learn_chunk ~chunk ~size array in
    Ok t

  let get_single (t : t) (low : Expr.t) (chunk : Chunk.t) :
      (SVal.t * Perm.t option * t, err) DR.t =
    let open DR.Syntax in
    let replace_node x = Ok x in
    let rebuild_parent = with_children in
    let range = Range.of_low_and_chunk low chunk in
    let** framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    let node = framed.node in
    Logging.tmi (fun m ->
        m "GET_SINGLE GOT THE FOLLOWING NODE: %a" Node.pp node);
    let++ sval, perm = Node.decode ~chunk node in
    (sval, perm, tree)

  let set_single
      (t : t)
      (low : Expr.t)
      (chunk : Chunk.t)
      (sval : SVal.t)
      (perm : Perm.t) : (t, err) DR.t =
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
      | Node.NotOwned _ -> Error MissingResource
      | MemVal { min_perm; _ } ->
          if min_perm >=% Readable then Ok node
          else
            Error
              (InsufficientPermission { required = Readable; actual = min_perm })
    in
    let rebuild_parent = with_children in
    let** framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    let++ sval, _ = Node.decode ~chunk framed.node in
    (sval, tree)

  let store (t : t) (low : Expr.t) (chunk : Chunk.t) (sval : SVal.t) :
      (t, err) DR.t =
    let open DR.Syntax in
    let open Perm.Infix in
    let range = Range.of_low_and_chunk low chunk in
    let replace_node node =
      match node.node with
      | NotOwned _ -> Error MissingResource
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
      (ofs, ofs + Expr.int 1)
    in
    let { span; _ } = tree in
    let rec rec_call treep =
      match Node.exact_perm treep.node with
      | `StopLooking r -> DR.of_result r
      | `KeepLooking ->
          let left, right = Option.get treep.children in
          if%sat Range.is_inside range left.span then rec_call left
          else rec_call right
    in
    if%sat Range.is_inside range span then rec_call tree
    else DR.error MissingResource

  let weak_valid_pointer (tree : t) (ofs : Expr.t) : (bool, err) DR.t =
    let open Delayed.Syntax in
    let open Perm.Infix in
    let open Expr.Infix in
    let* at_ofs = get_perm_at tree ofs in
    match at_ofs with
    | Ok p when p >=% Nonempty -> DR.ok true
    | _ ->
        let+ at_ofs_minus_one = get_perm_at tree (ofs - Expr.int 1) in
        at_ofs_minus_one |> Result.map (fun p -> p >=% Nonempty)

  let drop_perm (t : t) (low : Expr.t) (high : Expr.t) (perm : Perm.t) :
      (t, err) DR.t =
    let rec rec_set_perm { node; span; children; last_path } =
      let node = Node.drop_perm_exn ~perm node in
      let children =
        Option.map (fun (a, b) -> (rec_set_perm a, rec_set_perm b)) children
      in
      { node; span; children; last_path }
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

  let rec lvars { node; span; children; _ } =
    let node_lvars = Node.lvars node in
    let span_lvars = Range.lvars span in
    let children_lvars =
      match children with
      | Some (a, b) -> SS.union (lvars a) (lvars b)
      | None -> SS.empty
    in
    SS.union (SS.union node_lvars span_lvars) children_lvars

  let rec alocs { node; span; children; _ } =
    let node_lvars = Node.alocs node in
    let span_lvars = Range.alocs span in
    let children_lvars =
      match children with
      | Some (a, b) -> SS.union (alocs a) (alocs b)
      | None -> SS.empty
    in
    SS.union (SS.union node_lvars span_lvars) children_lvars

  let rec assertions ~loc { node; span; children; _ } =
    let low, high = span in
    match node with
    | NotOwned Totally -> []
    | NotOwned Partially | MemVal { mem_val = Undef Partially; _ } ->
        let left, right = Option.get children in
        assertions ~loc left @ assertions ~loc right
    | MemVal { mem_val = Undef Totally; exact_perm = perm; _ } ->
        [ CoreP.hole ~loc ~low ~high ~perm ]
    | MemVal { mem_val = Zeros; exact_perm = perm; _ } ->
        [ CoreP.zeros ~loc ~low ~high ~perm ]
    | MemVal { mem_val = Single { chunk; value }; exact_perm = perm; _ } ->
        let sval, types = NSVal.to_gil_expr value in
        let types =
          List.map
            (let open Formula.Infix in
            fun (x, t) -> Asrt.Pure (Expr.typeof x) #== (Expr.type_ t))
            types
        in
        CoreP.single ~loc ~ofs:low ~chunk ~sval ~perm :: types
    | MemVal { mem_val = Array { chunk; values }; exact_perm = perm; _ } -> (
        let chksize = Chunk.size_expr chunk in
        let total_size =
          let open Expr.Infix in
          (high - low) / chksize
        in
        match values with
        | AllUndef -> [ CoreP.hole ~loc ~low ~high ~perm ]
        | AllZeros -> [ CoreP.zeros ~loc ~low ~high ~perm ]
        | array ->
            let e, learned =
              SVArr.to_gil_expr_undelayed ~range:span array ~chunk
            in
            let learned = List.map (fun x -> Asrt.Pure x) learned in
            CoreP.array ~loc ~ofs:low ~perm ~chunk ~size:total_size ~sval_arr:e
            :: learned)

  let rec substitution
      ~svarr_subst
      ~sval_subst
      ~le_subst
      { node; span; children; last_path } =
    let node = Node.substitution ~sval_subst ~svarr_subst node in
    let span = Range.substitution ~le_subst span in
    let children =
      Option.map
        (fun (left, right) ->
          let f = substitution ~sval_subst ~le_subst ~svarr_subst in
          (f left, f right))
        children
    in
    { node; span; children; last_path }

  let box t =
    let rec flatten_tree { node; span; children; _ } =
      match node with
      | NotOwned Partially | MemVal { mem_val = Undef Partially; _ } ->
          let left, right = Option.get children in
          flatten_tree left @ flatten_tree right
      | node -> [ (span, node) ]
    in
    let open PrintBox in
    frame @@ vlist_map (fun (x, y) -> box_range_and_node x y) (flatten_tree t)

  let pp fmt tree = PrintBox_text.pp fmt (box tree)
end

type t = Freed | Tree of { bounds : Range.t option; root : Tree.t option }
[@@deriving yojson]

let pp_full fmt = function
  | Freed -> Fmt.pf fmt "FREED"
  | Tree { bounds; root } ->
      let pp_aux fmt (bounds, root) =
        Fmt.pf fmt "%a@ %a"
          (Fmt.option ~none:(Fmt.any "NO BOUNDS") Range.pp)
          bounds
          (Fmt.option ~none:(Fmt.any "EMPTY") Tree.pp_full)
          root
      in
      (Fmt.parens (Fmt.vbox pp_aux)) fmt (bounds, root)

let pp fmt t =
  match t with
  | Freed -> Fmt.pf fmt "FREED"
  | Tree { bounds; root } ->
      Fmt.pf fmt "%a@ %a"
        (Fmt.option ~none:(Fmt.any "NO BOUNDS") Range.pp)
        bounds
        (Fmt.option ~none:(Fmt.any "EMPTY") Tree.pp)
        root

let empty =
  let bounds = None in
  let root = None in
  Tree { bounds; root }

let is_empty t =
  match t with
  | Freed -> false
  | Tree { bounds; root } ->
      Option.is_none bounds
      && Option.fold ~none:true ~some:(fun root -> Tree.is_empty root) root

let freed = Freed

let lvars = function
  | Freed -> SS.empty
  | Tree { bounds; root } ->
      SS.union
        (Option.fold ~none:SS.empty ~some:Range.lvars bounds)
        (Option.fold ~none:SS.empty ~some:Tree.lvars root)

let alocs = function
  | Freed -> SS.empty
  | Tree { bounds; root } ->
      SS.union
        (Option.fold ~none:SS.empty ~some:Range.alocs bounds)
        (Option.fold ~none:SS.empty ~some:Tree.alocs root)

let get_root = function
  | Freed -> Error UseAfterFree
  | Tree x -> Ok x.root

let is_in_bounds range bounds =
  match bounds with
  | None -> Formula.True
  | Some bounds -> Range.is_inside range bounds

let get_perm_at t ofs =
  let open DR.Syntax in
  match t with
  | Freed -> DR.ok None
  | Tree { bounds; root } ->
      let is_in_bounds =
        let open Expr.Infix in
        is_in_bounds (ofs, ofs + Expr.int 1) bounds
      in
      if%sat is_in_bounds then
        match root with
        | None -> DR.error MissingResource
        | Some root ->
            let++ perm = Tree.get_perm_at root ofs in
            Some perm
      else DR.ok None

let weak_valid_pointer (t : t) (ofs : Expr.t) : (bool, err) DR.t =
  let is_sure_false bounds ofs =
    let open Formula.Infix in
    match bounds with
    | None -> Formula.False
    | Some (low, high) -> ofs #< low #|| (ofs #> high)
  in
  match t with
  | Freed -> DR.ok false
  | Tree { bounds; root } -> (
      if%sat is_sure_false bounds ofs then DR.ok false
      else
        match root with
        | None -> DR.error MissingResource
        | Some root -> Tree.weak_valid_pointer root ofs)

let get_bounds = function
  | Freed -> Error UseAfterFree
  | Tree x -> Ok x.bounds

let set_bounds t bounds =
  match t with
  | Freed -> Error UseAfterFree
  | Tree x -> Ok (Tree { x with bounds })

let rem_bounds t =
  match t with
  | Freed -> Error UseAfterFree
  | Tree x -> Ok (Tree { x with bounds = None })

let with_root_opt t root =
  match t with
  | Freed -> Error UseAfterFree
  | Tree x -> Ok (Tree { x with root })

let with_root t root = with_root_opt t (Some root)

let alloc low high =
  let bounds = Range.make low high in
  Tree { root = Some (Tree.undefined bounds); bounds = Some bounds }

let drop_perm t low high new_perm =
  let open DR.Syntax in
  match t with
  | Freed -> DR.error UseAfterFree
  | Tree { bounds; root } -> (
      match root with
      | None -> DR.error MissingResource
      | Some tree ->
          let++ new_root = Tree.drop_perm tree low high new_perm in
          Tree { bounds; root = Some new_root })

let free t low high =
  let open DR.Syntax in
  let** bounds = DR.of_result (get_bounds t) in
  match t with
  (* Can't free something already freed *)
  | Freed -> DR.error UseAfterFree
  | Tree tree -> (
      (* Can only free if entirely freeable *)
      match bounds with
      | None -> DR.error MissingResource
      | Some bounds ->
          if%ent Range.is_equal (low, high) bounds then
            match tree.root with
            | None -> DR.error MissingResource
            | Some root ->
                let+* node, _ = Tree.get_node root (low, high) in
                Result.map
                  (fun () -> Freed)
                  (Node.check_perm (Some Freeable) node)
          else
            DR.error
              (Unhandled
                 "Freeing only part of an object (this might need fixing in \
                  the MM)"))

let get_single t low chunk =
  let open DR.Syntax in
  let range = Range.of_low_and_chunk low chunk in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None -> DR.error MissingResource
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
    | None -> []
    | Some bounds -> [ Range.is_inside range bounds ]
  in
  DR.of_result ~learned (with_root t root_set)

let rem_last_get t =
  match t with
  | Tree { root = Some root; bounds } ->
      Tree { bounds; root = Some (Tree.rem_last_get root) }
  | _ -> failwith "Impossible, removing last get with something absurd"

let get_array t low size chunk =
  let open DR.Syntax in
  let range = Range.of_low_chunk_and_size low chunk size in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None -> DR.error MissingResource
    | Some root ->
        let** array, perm, root_framed = Tree.get_array root low chunk size in
        let++ wroot = DR.of_result (with_root t root_framed) in
        (array, perm, wroot)
  else DR.error BufferOverrun

let set_array t low size chunk array perm =
  let open DR.Syntax in
  let range = Range.of_low_chunk_and_size low chunk size in
  let** root = DR.of_result (get_root t) in
  let root = Option.value root ~default:(Tree.create_root range) in
  let** root_set = Tree.set_array root low size chunk array perm in
  let** bounds = DR.of_result (get_bounds t) in
  let learned =
    match bounds with
    | None -> []
    | Some bounds -> [ Range.is_inside range bounds ]
  in
  DR.of_result ~learned (with_root t root_set)

let get_simple_mem_val ~expected_mem_val t low high =
  let open DR.Syntax in
  let range = (low, high) in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None -> DR.error MissingResource
    | Some root ->
        let** node, root_framed = Tree.get_node root range in
        let res =
          match node with
          | MemVal { mem_val; exact_perm = perm; _ }
            when Node.eq_mem_val mem_val expected_mem_val -> Ok perm
          | NotOwned _ -> Error MissingResource
          | _ -> Error WrongMemVal
        in
        let++ wroot =
          DR.of_result
            (Result.bind res (fun perm ->
                 Result.map (fun mem -> (mem, perm)) (with_root t root_framed)))
        in
        wroot
  else DR.error BufferOverrun

let set_simple_mem_val ~mem_val t low high perm =
  let open DR.Syntax in
  let range = (low, high) in
  let** root = DR.of_result (get_root t) in
  let root = Option.value ~default:(Tree.create_root range) root in
  let** root_set = Tree.set_node root range (Node.make_owned ~perm ~mem_val) in
  let** bounds = DR.of_result (get_bounds t) in
  let learned =
    match bounds with
    | None -> []
    | Some bounds -> [ Range.is_inside range bounds ]
  in
  DR.of_result ~learned (with_root t root_set)

let get_hole = get_simple_mem_val ~expected_mem_val:(Undef Totally)
let set_hole = set_simple_mem_val ~mem_val:(Undef Totally)
let get_zeros = get_simple_mem_val ~expected_mem_val:Zeros
let set_zeros = set_simple_mem_val ~mem_val:Zeros

let get_freed t =
  match t with
  | Freed -> Ok ()
  | _ -> Error MemoryNotFreed

let _check_valid_alignment chunk ofs =
  let al = Chunk.align chunk in
  let al_expr = Expr.int al in
  let divides x y =
    let open Formula.Infix in
    Expr.(y #== (int 0)) #|| ((Expr.imod y x) #== (Expr.int 0))
  in
  if%sat divides al_expr ofs then DR.ok ()
  else DR.error (InvalidAlignment { offset = ofs; alignment = al })

let load t chunk ofs =
  let open DR.Syntax in
  (* FIXME: this should be reestablished asap *)
  (* let** () = check_valid_alignment chunk ofs in *)
  let range = Range.of_low_and_chunk ofs chunk in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None -> DR.error MissingResource
    | Some root ->
        let** value, root = Tree.load root ofs chunk in
        let++ wroot = DR.of_result (with_root t root) in
        (value, wroot)
  else DR.error BufferOverrun

let store t chunk ofs value =
  let open DR.Syntax in
  (* let** () = check_valid_alignment chunk ofs in *)
  let range = Range.of_low_and_chunk ofs chunk in
  let** span = DR.of_result (get_bounds t) in
  if%sat is_in_bounds range span then
    let** root = DR.of_result (get_root t) in
    match root with
    | None -> DR.error MissingResource
    | Some root ->
        let** root = Tree.store root ofs chunk value in
        DR.of_result (with_root t root)
  else DR.error BufferOverrun

let move dst_tree dst_ofs src_tree src_ofs size =
  let open DR.Syntax in
  let dst_range, src_range =
    let open Expr.Infix in
    ((dst_ofs, dst_ofs + size), (src_ofs, src_ofs + size))
  in
  let** src_span = DR.of_result (get_bounds src_tree) in
  if%sat is_in_bounds src_range src_span then
    let** src_root = DR.of_result (get_root src_tree) in
    match src_root with
    | None -> DR.error MissingResource
    | Some src_root ->
        let** framed, _ =
          Tree.frame_range src_root
            ~replace_node:(fun x -> Ok x)
            ~rebuild_parent:(fun t ~left:_ ~right:_ -> Delayed.return t)
            src_range
        in
        let** () =
          match framed.node with
          | NotOwned _ -> DR.error MissingResource
          | _ -> DR.ok ()
        in
        let** dst_span = DR.of_result (get_bounds dst_tree) in
        if%sat is_in_bounds dst_range dst_span then
          let** dst_root = DR.of_result (get_root dst_tree) in
          match dst_root with
          | None -> DR.error MissingResource
          | Some dst_root ->
              let** _, new_dst_root =
                Tree.frame_range dst_root
                  ~replace_node:(fun current ->
                    match current.node with
                    | NotOwned _ -> Error MissingResource
                    | _ -> Ok (Tree.realign framed dst_ofs))
                  ~rebuild_parent:Tree.of_children dst_range
              in
              DR.of_result (with_root dst_tree new_dst_root)
        else DR.error BufferOverrun
  else DR.error BufferOverrun

let assertions ~loc t =
  let loc = Expr.loc_from_loc_name loc in
  match t with
  | Freed -> [ CoreP.freed ~loc ]
  | Tree x ->
      let bounds =
        Option.fold ~none:[]
          ~some:(fun (low, high) -> [ CoreP.bounds ~loc ~low ~high ])
          x.bounds
      in
      let tree =
        match x.root with
        | None -> []
        | Some root -> Tree.assertions ~loc root
      in
      bounds @ tree

let merge ~old_tree ~new_tree =
  let open DR.Syntax in
  Logging.verbose (fun m -> m "OLD TREE:@\n%a" pp old_tree);
  Logging.verbose (fun m -> m "NEW TREE:@\n%a" pp new_tree);
  if is_empty old_tree then DR.ok new_tree
  else if is_empty new_tree then DR.ok old_tree
  else
    match (old_tree, new_tree) with
    | Freed, _ | _, Freed ->
        failwith "merging a non-empty tree with a freed block"
    | Tree new_tree, Tree old_tree ->
        let def_bounds =
          match new_tree.bounds with
          | Some bounds -> Some bounds
          | None -> old_tree.bounds
        in
        let rec get_owned_nodes (t : Tree.t) : Tree.t list =
          match t.node with
          | NotOwned Totally -> []
          | NotOwned Partially ->
              let left, right = Option.get t.children in
              get_owned_nodes left @ get_owned_nodes right
          | _ -> [ t ]
        in
        let++ def_root =
          match (old_tree.root, new_tree.root) with
          | None, None -> DR.ok None
          | None, Some d | Some d, None -> DR.ok (Some d)
          | Some d, Some o when Tree.is_empty o -> DR.ok (Some d)
          | Some o, Some d when Tree.is_empty o -> DR.ok (Some d)
          | Some old_root, Some new_root ->
              let new_owned_nodes = get_owned_nodes new_root in
              Logging.verbose (fun fmt ->
                  fmt "There are %d new owned nodes"
                    (List.length new_owned_nodes));
              let++ tree =
                List.fold_left
                  (fun acc (tree_node : Tree.t) ->
                    let** acc in
                    let replace_node _ = Ok tree_node in
                    let rebuild_parent = Tree.of_children in
                    let++ _, tree =
                      Tree.frame_range acc ~replace_node ~rebuild_parent
                        tree_node.span
                    in
                    tree)
                  (DR.ok old_root) new_owned_nodes
              in
              Some tree
        in
        Logging.verbose (fun m ->
            m "TREE AFTER MERGE:@\n%a" (Fmt.Dump.option Tree.pp) def_root);
        Tree { bounds = def_bounds; root = def_root }

let substitution ~le_subst ~sval_subst ~svarr_subst t =
  match t with
  | Freed -> Freed
  | Tree { bounds; root } ->
      let bounds = Option.map (Range.substitution ~le_subst) bounds in
      let root =
        Option.map (Tree.substitution ~sval_subst ~le_subst ~svarr_subst) root
      in
      Tree { bounds; root }

module Lift = struct
  open Debugger.DebuggerTypes

  let get_variable
      ~(make_node :
         name:string ->
         value:string ->
         ?children:variable list ->
         unit ->
         variable)
      ~loc
      t : variable =
    match t with
    | Freed -> make_node ~name:loc ~value:"Freed" ()
    | Tree { bounds; root } ->
        let bounds =
          match bounds with
          | None -> make_node ~name:"Bounds" ~value:"Not owned" ()
          | Some bounds ->
              make_node ~name:"Bounds" ~value:""
                ~children:(Range.Lift.as_variables ~make_node bounds)
                ()
        in
        let root =
          match root with
          | None -> make_node ~name:"Tree" ~value:"Not owned" ()
          | Some root -> Tree.Lift.as_variable ~make_node root
        in
        make_node ~name:loc ~value:"Allocated" ~children:[ bounds; root ] ()
end
