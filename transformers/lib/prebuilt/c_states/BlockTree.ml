open Gil_syntax
open Monadic
module Subst = Gillian.Symbolic.Subst
module DR = Delayed_result
module DO = Delayed_option
module SS = Utils.Containers.SS
module CoreP = Constr.Core
module MyAsrt = States.MyAsrt

(* Import from Cgil lib: *)
module CConstants = Cgil_lib.CConstants
module Perm = Cgil_lib.Perm
module ValueTranslation = Cgil_lib.ValueTranslation
module Chunk = Cgil_lib.Chunk
module NSVal = Cgil_lib.SVal
module SVal = Cgil_lib.MonadicSVal
module SVArr = SVal.SVArray

let log_string s = Logging.verbose (fun fmt -> fmt "SHEAPTREE CHECKING: %s" s)

type missingResourceType =
  | Unfixable
  | Fixable of { is_store : bool; low : Expr.t; chunk : Chunk.t }
[@@deriving show, yojson]

type err =
  | BufferOverrun
  | InsufficientPermission of { required : Perm.t; actual : Perm.t }
  | InvalidAlignment of { alignment : int; offset : Expr.t }
  | MissingResource of missingResourceType
  | Unhandled of string
  | WrongMemVal
[@@deriving show, yojson]

exception FatalErr of err

let sval_is_concrete = function
  | SVal.SUndefined -> true
  | Sptr (_, e) | SVint e | SVlong e | SVsingle e | SVfloat e ->
      Expr.is_concrete e

let svarr_is_concrete = function
  | SVArr.Arr e -> Expr.is_concrete e
  | AllUndef | AllZeros -> true

module Range = struct
  type t = Expr.t * Expr.t [@@deriving yojson]

  let pp fmt (a, b) = Fmt.pf fmt "@[<h>[%a; %a[@]" Expr.pp a Expr.pp b
  let make low high = (low, high)

  module Lift = struct
    open Gillian.Debugger.Utils

    let as_variables
        ~(make_node :
           name:string ->
           value:string ->
           ?children:Variable.t list ->
           unit ->
           Variable.t)
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
  let is_concrete (a, b) = Expr.is_concrete a && Expr.is_concrete b
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

  let is_concrete = function
    | NotOwned _ -> true
    | MemVal { mem_val; _ } -> (
        match mem_val with
        | Zeros -> true
        | Undef _ -> true
        | Single { value; _ } -> sval_is_concrete value
        | Array { values; _ } -> svarr_is_concrete values)

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
        | NotOwned _ -> Error (MissingResource Unfixable)
        | MemVal { min_perm = actual; _ } ->
            let open Perm.Infix in
            if actual >=% required then Ok ()
            else Error (InsufficientPermission { actual; required }))

  let exact_perm = function
    | NotOwned Partially -> `KeepLooking
    | NotOwned Totally -> `StopLooking (Error (MissingResource Unfixable))
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
        | Array { chunk; values }, Zeros ->
            let size_l, size_r =
              let open Expr.Infix in
              let size_chunk = Chunk.size_expr chunk in
              (size_a / size_chunk, size_b / size_chunk)
            in
            Delayed.map
              (SVArr.concat_knowing_size (values, size_l) (AllZeros, size_r))
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

  let decode ~low ~chunk t =
    let open Delayed.Syntax in
    match t with
    | NotOwned Totally ->
        DR.error (MissingResource (Fixable { is_store = false; low; chunk }))
    | NotOwned Partially ->
        Logging.verbose (fun fmt ->
            fmt
              "SHeapTree Decode Error: Memory Partially Not Owned (Currently \
               Unsupported)");
        DR.error (MissingResource Unfixable)
    | MemVal { mem_val = Zeros; exact_perm; _ } ->
        DR.ok (SVal.zero_of_chunk chunk, exact_perm)
    | MemVal { mem_val = Undef _; exact_perm; _ } ->
        DR.ok (SVal.SUndefined, exact_perm)
    | MemVal { mem_val = Single { chunk = m_chunk; value }; exact_perm; _ } ->
        DR.ok
          (if Chunk.phy_equal m_chunk chunk then (value, exact_perm)
           else (SUndefined, exact_perm))
    | MemVal
        { mem_val = Array { chunk = Mint8unsigned; values }; exact_perm; _ }
      when Chunk.equal chunk Mint16unsigned ->
        let+ decoded = decode_bytes_to_unsigned_int ~chunk values 2 in
        Ok (decoded, exact_perm)
    | MemVal
        { mem_val = Array { chunk = Mint8unsigned; values }; exact_perm; _ }
      when Chunk.equal chunk Mint32
           || (Chunk.equal chunk Mptr && not Compcert.Archi.ptr64) ->
        let+ decoded = decode_bytes_to_unsigned_int ~chunk values 4 in
        Ok (decoded, exact_perm)
    | MemVal
        { mem_val = Array { chunk = Mint8unsigned; values }; exact_perm; _ }
      when Chunk.equal chunk Mint64
           || (Chunk.equal chunk Mptr && Compcert.Archi.ptr64) ->
        let+ decoded = decode_bytes_to_unsigned_int ~chunk values 8 in
        Ok (decoded, exact_perm)
    | MemVal { mem_val = Array { chunk = chunk_b; values }; exact_perm; _ }
      when Chunk.equal chunk chunk_b ->
        let* values = SVArr.reduce values in
        if Chunk.could_be_ptr chunk then
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
    | MemVal { mem_val = Array { chunk = c; values }; exact_perm; _ }
      when Chunk.size c < Chunk.size chunk == SVArr.sure_is_all_zeros values ->
        DR.ok (SVal.zero_of_chunk chunk, exact_perm)
    | MemVal { mem_val = Array { chunk = _; _ }; exact_perm; _ } ->
        DR.ok (SVal.SUndefined, exact_perm)

  let decode_arr ~low:_ ~size ~chunk t =
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
    | NotOwned _ ->
        Logging.verbose (fun fmt ->
            fmt
              "Right now, we can't fix array errors easily without adding more \
               fixes. Right now we can only fix single accesses.\n\
              \           We should at least be able to fix it for concrete \
               arrays");
        DR.error (MissingResource Unfixable)
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
      when Chunk.equal chunk Mint64
           || (Chunk.equal chunk Mptr && Compcert.Archi.ptr64) -> (
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
      | SVlong _, Mptr when Compcert.Archi.ptr64 ->
          Single { chunk; value = sval }
      | SVint _, Mptr when not Compcert.Archi.ptr64 ->
          Single { chunk; value = sval }
      | Sptr _, c when Chunk.could_be_ptr c -> Single { chunk; value = sval }
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
  type t = { node : Node.t; span : Range.t; children : (t * t) option }
  [@@deriving yojson]

  module Lift = struct
    open Gillian.Debugger.Utils

    let rec as_variable
        ~(make_node :
           name:string ->
           value:string ->
           ?children:Variable.t list ->
           unit ->
           Variable.t)
        (tree : t) : Variable.t =
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

  let rec is_concrete { node; span; children } =
    if not (Node.is_concrete node) then false
    else if not (Range.is_concrete span) then false
    else
      match children with
      | None -> true
      | Some (a, b) -> is_concrete a && is_concrete b

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

  let make ~node ~span ?children () = { node; span; children }

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
    Delayed.return { t with children = Some (left, right) }

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
    { span; children; node }

  let of_children _ ~left ~right = of_children_s ~left ~right

  let update_parent_perm t ~left ~right =
    let { node; span; _ } = t in
    let new_node =
      Node.update_parent_perm node ~left:left.node ~right:right.node
    in
    Delayed.return { node = new_node; span; children = Some (left, right) }

  let remove_node x = Ok (make ~node:(NotOwned Totally) ~span:x.span ())

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
    { children = None; span = range; node = NotOwned Totally }

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
    let rec add_to_the_left t addition : t Delayed.t =
      match t.children with
      | None -> of_children_s ~left:addition ~right:t
      | Some (left, right) ->
          let* new_left = add_to_the_left left addition in
          of_children_s ~left:new_left ~right
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
        | Ok new_tree -> DR.ok (t, new_tree)
        | Error err -> DR.error err)
      else
        match t.children with
        | Some (left, right) ->
            let _, mid = left.span in
            if%sat
              log_string "mid strictly in range";
              Range.point_strictly_inside mid range
            then
              let l, h = range in
              let upper_range = (mid, h) in
              let dont_replace_node = Result.ok in
              if%sat
                (* High-range already good *)
                Range.is_equal upper_range right.span
              then
                let lower_range = (l, mid) in
                let** _, left =
                  frame_inside ~replace_node:dont_replace_node
                    ~rebuild_parent:with_children left lower_range
                in
                let* extracted, left_opt = extract left lower_range in
                let* right = add_to_the_left right extracted in
                let* new_self =
                  match left_opt with
                  | Some left -> of_children_s ~left ~right
                  | None -> Delayed.return right
                in
                frame_inside ~replace_node ~rebuild_parent new_self range
              else
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
                Ok (node, new_parent)
              else
                if%sat
                  log_string "range inside right";
                  Range.is_inside range right.span
                then
                  let** node, right =
                    frame_inside ~replace_node ~rebuild_parent right range
                  in
                  let+ new_parent = rebuild_parent t ~left ~right in
                  Ok (node, new_parent)
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

  let cons_node (t : t) range : (Node.t * t, err) DR.t =
    let open DR.Syntax in
    let replace_node x = remove_node x in
    let rebuild_parent = of_children in
    let++ framed, rest = frame_range t ~replace_node ~rebuild_parent range in
    (framed.node, rest)

  let prod_node (t : t) range node : (t, err) DR.t =
    let open DR.Syntax in
    let replace_node _ = Ok (make ~node ~span:range ()) in
    let rebuild_parent = of_children in
    let++ _, t = frame_range t ~replace_node ~rebuild_parent range in
    t

  let cons_array (t : t) (low : Expr.t) (chunk : Chunk.t) (size : Expr.t) :
      (SVArr.t * Perm.t option * t, err) DR.t =
    let open DR.Syntax in
    let open Delayed.Syntax in
    let* size = Delayed.reduce size in
    let replace_node = remove_node in
    let rebuild_parent = of_children in
    let range = Range.of_low_chunk_and_size low chunk size in
    let** framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    let+* arr, perm = Node.decode_arr ~size ~low ~chunk framed.node in
    Ok (arr, perm, tree)

  let prod_array
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

  let cons_single (t : t) (low : Expr.t) (chunk : Chunk.t) :
      (SVal.t * Perm.t option * t, err) DR.t =
    let open DR.Syntax in
    let replace_node x = remove_node x in
    let rebuild_parent = of_children in
    let range = Range.of_low_and_chunk low chunk in
    let** framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    let node = framed.node in
    let++ sval, perm = Node.decode ~low ~chunk node in
    (sval, perm, tree)

  let prod_single
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
      | Node.NotOwned Totally ->
          Error (MissingResource (Fixable { is_store = false; low; chunk }))
      | Node.NotOwned Partially ->
          Logging.verbose (fun fmt ->
              fmt
                "SHeapTree Load Error: Memory Partially Not Owned (Currently \
                 Unsupported)");
          Error (MissingResource Unfixable)
      | MemVal { min_perm; _ } ->
          if min_perm >=% Readable then Ok node
          else
            Error
              (InsufficientPermission { required = Readable; actual = min_perm })
    in
    let rebuild_parent = with_children in
    let** framed, tree = frame_range t ~replace_node ~rebuild_parent range in
    let++ sval, _ = Node.decode ~low ~chunk framed.node in
    (sval, tree)

  let store (t : t) (low : Expr.t) (chunk : Chunk.t) (sval : SVal.t) :
      (t, err) DR.t =
    let open DR.Syntax in
    let open Perm.Infix in
    let range = Range.of_low_and_chunk low chunk in
    let replace_node node =
      match node.node with
      | NotOwned Totally ->
          Error (MissingResource (Fixable { is_store = true; low; chunk }))
      | NotOwned Partially ->
          Logging.verbose (fun fmt ->
              fmt
                "SHeapTree Store Error: Memory Partially Not Owned (Currently \
                 Unsupported)");
          Error (MissingResource Unfixable)
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
    else DR.error (MissingResource Unfixable)

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
      | NotOwned Totally ->
          Error (MissingResource Unfixable) (* No chunk available to fix *)
      | NotOwned Partially ->
          Logging.verbose (fun fmt ->
              fmt
                "SHeapTree Drop Permission Error: Memory Partially Not Owned \
                 (Currently Unsupported)");
          Error (MissingResource Unfixable)
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

  let rec assertions { node; span; children; _ } =
    let low, high = span in
    match node with
    | NotOwned Totally -> []
    | NotOwned Partially | MemVal { mem_val = Undef Partially; _ } ->
        let left, right = Option.get children in
        assertions left @ assertions right
    | MemVal { mem_val = Undef Totally; exact_perm = perm; _ } ->
        [ CoreP.hole ~low ~high ~perm ]
    | MemVal { mem_val = Zeros; exact_perm = perm; _ } ->
        [ CoreP.zeros ~low ~high ~perm ]
    | MemVal { mem_val = Single { chunk; value }; exact_perm = perm; _ } ->
        let sval, _ = NSVal.to_gil_expr value in
        [ CoreP.single ~ofs:low ~chunk ~sval ~perm ]
    | MemVal { mem_val = Array { chunk; values }; exact_perm = perm; _ } -> (
        let chksize = Chunk.size_expr chunk in
        let total_size =
          let open Expr.Infix in
          (high - low) / chksize
        in
        match values with
        | AllUndef -> [ CoreP.hole ~low ~high ~perm ]
        | AllZeros -> [ CoreP.zeros ~low ~high ~perm ]
        | array ->
            let e, _ = SVArr.to_gil_expr_undelayed ~range:span array ~chunk in
            [ CoreP.array ~ofs:low ~perm ~chunk ~size:total_size ~sval_arr:e ])

  let rec assertions_others { node; span; children; _ } =
    match node with
    | NotOwned Totally -> []
    | NotOwned Partially | MemVal { mem_val = Undef Partially; _ } ->
        let left, right = Option.get children in
        assertions_others left @ assertions_others right
    | MemVal { mem_val = Undef Totally; _ } -> []
    | MemVal { mem_val = Zeros; _ } -> []
    | MemVal { mem_val = Single { value; _ }; _ } ->
        let _, types = NSVal.to_gil_expr value in
        List.map
          (let open Formula.Infix in
           fun (x, t) -> Asrt.Pure (Expr.typeof x) #== (Expr.type_ t))
          types
    | MemVal { mem_val = Array { chunk; values }; _ } -> (
        match values with
        | AllUndef | AllZeros -> []
        | array ->
            let _, learned =
              SVArr.to_gil_expr_undelayed ~range:span array ~chunk
            in
            List.map (fun x -> Asrt.Pure x) learned)

  let rec substitution
      ~svarr_subst
      ~sval_subst
      ~le_subst
      { node; span; children } =
    let node = Node.substitution ~sval_subst ~svarr_subst node in
    let span = Range.substitution ~le_subst span in
    let children =
      Option.map
        (fun (left, right) ->
          let f = substitution ~sval_subst ~le_subst ~svarr_subst in
          (f left, f right))
        children
    in
    { node; span; children }

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

module M = struct
  open LActions

  type err_t = err [@@deriving show, yojson]

  type t = { bounds : Range.t option; root : Tree.t option }
  [@@deriving show, yojson]

  type action = ac
  type pred = ga

  let action_to_str = str_ac
  let action_from_str s = try Some (ac_from_str s) with _ -> None
  let pred_to_str = str_ga
  let pred_from_str s = try Some (ga_from_str s) with _ -> None

  let list_actions _ =
    [
      (DropPerm, [ "?" ], [ "?" ]);
      (GetCurPerm, [ "?" ], [ "?" ]);
      (WeakValidPointer, [ "?" ], [ "?" ]);
      (Store, [ "?" ], [ "?" ]);
      (Load, [ "?" ], [ "?" ]);
    ]

  let list_preds _ =
    [
      (LActions.Single, [ "?" ], [ "?" ]);
      (LActions.Array, [ "?" ], [ "?" ]);
      (LActions.Hole, [ "?" ], [ "?" ]);
      (LActions.Zeros, [ "?" ], [ "?" ]);
      (LActions.Bounds, [ "?" ], [ "?" ]);
    ]

  let pp fmt { bounds; root } =
    Fmt.pf fmt "%a@ %a"
      (Fmt.option ~none:(Fmt.any "NO BOUNDS") Range.pp)
      bounds
      (Fmt.option ~none:(Fmt.any "EMPTY") Tree.pp)
      root

  let empty () = { bounds = None; root = None }

  let is_empty { bounds; root } =
    Option.is_none bounds && Option.fold ~none:true ~some:Tree.is_empty root

  let is_concrete { bounds; root } =
    Option.fold ~none:true ~some:Range.is_concrete bounds
    && Option.fold ~none:true ~some:Tree.is_concrete root

  let lvars { bounds; root } =
    SS.union
      (Option.fold ~none:SS.empty ~some:Range.lvars bounds)
      (Option.fold ~none:SS.empty ~some:Tree.lvars root)

  let alocs { bounds; root } =
    SS.union
      (Option.fold ~none:SS.empty ~some:Range.alocs bounds)
      (Option.fold ~none:SS.empty ~some:Tree.alocs root)

  let is_in_bounds range bounds =
    match bounds with
    | None -> Formula.True
    | Some bounds -> Range.is_inside range bounds

  let get_perm_at { bounds; root } ofs =
    let open DR.Syntax in
    let is_in_bounds =
      let open Expr.Infix in
      is_in_bounds (ofs, ofs + Expr.int 1) bounds
    in
    if%sat is_in_bounds then
      match root with
      | None -> DR.error (MissingResource Unfixable)
      | Some root ->
          let++ perm = Tree.get_perm_at root ofs in
          Some perm
    else DR.ok None

  let weak_valid_pointer ({ bounds; root } : t) (ofs : Expr.t) :
      (bool, err_t) DR.t =
    let is_sure_false bounds ofs =
      let open Formula.Infix in
      match bounds with
      | None -> Formula.False
      | Some (low, high) -> ofs #< low #|| (ofs #> high)
    in
    if%sat is_sure_false bounds ofs then DR.ok false
    else
      match root with
      | None -> DR.error (MissingResource Unfixable)
      | Some root -> Tree.weak_valid_pointer root ofs

  let cons_bounds x = Ok (x.bounds, { x with bounds = None })
  let prod_bounds x bounds = Ok { x with bounds }
  let with_root_opt x root = Ok { x with root }
  let with_root t root = with_root_opt t (Some root)

  let drop_perm { bounds; root } low high new_perm =
    let open DR.Syntax in
    match root with
    | None -> DR.error (MissingResource Unfixable)
    | Some tree ->
        let++ new_root = Tree.drop_perm tree low high new_perm in
        { bounds; root = Some new_root }

  let cons_single t low chunk =
    let open DR.Syntax in
    let { bounds; root } = t in
    let range = Range.of_low_and_chunk low chunk in
    if%sat is_in_bounds range bounds then
      match root with
      (* TODO: What should the offset be in this case *)
      | None ->
          DR.error (MissingResource (Fixable { is_store = false; low; chunk }))
      | Some root ->
          let** value, perm, root_framed = Tree.cons_single root low chunk in
          let++ wroot = DR.of_result (with_root t root_framed) in
          (value, perm, wroot)
    else DR.error BufferOverrun

  let prod_single t low chunk sval perm =
    let open DR.Syntax in
    let { bounds; root } = t in
    let range = Range.of_low_and_chunk low chunk in
    let root = Option.value root ~default:(Tree.create_root range) in
    let** root_set = Tree.prod_single root low chunk sval perm in
    let learned =
      match bounds with
      | None -> []
      | Some bounds -> [ Range.is_inside range bounds ]
    in
    DR.of_result ~learned (with_root t root_set)

  let cons_array t low size chunk =
    let open DR.Syntax in
    let range = Range.of_low_chunk_and_size low chunk size in
    let { bounds; root } = t in
    if%sat is_in_bounds range bounds then
      match root with
      | None ->
          DR.error (MissingResource (Fixable { is_store = false; low; chunk }))
      | Some root ->
          let** array, perm, root_framed =
            Tree.cons_array root low chunk size
          in
          let++ wroot = DR.of_result (with_root t root_framed) in
          (array, perm, wroot)
    else DR.error BufferOverrun

  let prod_array t low size chunk array perm =
    let open DR.Syntax in
    let range = Range.of_low_chunk_and_size low chunk size in
    let { bounds; root } = t in
    let root = Option.value root ~default:(Tree.create_root range) in
    let** root_set = Tree.prod_array root low size chunk array perm in
    let learned =
      match bounds with
      | None -> []
      | Some bounds -> [ Range.is_inside range bounds ]
    in
    DR.of_result ~learned (with_root t root_set)

  let cons_simple_mem_val ~expected_mem_val t low high =
    let open DR.Syntax in
    let range = (low, high) in
    let { bounds; root } = t in
    if%sat is_in_bounds range bounds then
      match root with
      | None -> DR.error (MissingResource Unfixable)
      | Some root ->
          let** node, root_framed = Tree.cons_node root range in
          let res =
            match node with
            | MemVal { mem_val; exact_perm = perm; _ }
              when Node.eq_mem_val mem_val expected_mem_val -> Ok perm
            | NotOwned Totally -> Error (MissingResource Unfixable)
            | NotOwned Partially ->
                Logging.verbose (fun fmt ->
                    fmt
                      "SHeapTree Get Simple Memory Value Error: Memory \
                       Partially Not Owned (Currently Unsupported)");
                Error (MissingResource Unfixable)
            | _ -> Error WrongMemVal
          in
          let++ wroot =
            DR.of_result
              (Result.bind res (fun perm ->
                   Result.map (fun mem -> (mem, perm)) (with_root t root_framed)))
          in
          wroot
    else DR.error BufferOverrun

  let prod_simple_mem_val ~mem_val t low high perm =
    let open DR.Syntax in
    let range = (low, high) in
    let { bounds; root } = t in
    let root = Option.value ~default:(Tree.create_root range) root in
    let** root_set =
      Tree.prod_node root range (Node.make_owned ~perm ~mem_val)
    in
    let learned =
      match bounds with
      | None -> []
      | Some bounds -> [ Range.is_inside range bounds ]
    in
    DR.of_result ~learned (with_root t root_set)

  let cons_hole = cons_simple_mem_val ~expected_mem_val:(Undef Totally)
  let prod_hole = prod_simple_mem_val ~mem_val:(Undef Totally)
  let cons_zeros = cons_simple_mem_val ~expected_mem_val:Zeros
  let prod_zeros = prod_simple_mem_val ~mem_val:Zeros

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
    let { bounds; root } = t in
    if%sat is_in_bounds range bounds then
      match root with
      | None ->
          DR.error
            (MissingResource (Fixable { is_store = false; low = ofs; chunk }))
      | Some root ->
          let** value, root = Tree.load root ofs chunk in
          let++ wroot = DR.of_result (with_root t root) in
          (value, wroot)
    else DR.error BufferOverrun

  let store t chunk ofs value =
    let open DR.Syntax in
    (* let** () = check_valid_alignment chunk ofs in *)
    let range = Range.of_low_and_chunk ofs chunk in
    let { bounds; root } = t in
    if%sat is_in_bounds range bounds then
      match root with
      | None ->
          DR.error
            (MissingResource (Fixable { is_store = true; low = ofs; chunk }))
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
    let { bounds = src_bounds; root = src_root } = src_tree in
    let { bounds = dst_bounds; root = dst_root } = dst_tree in
    if%sat is_in_bounds src_range src_bounds then
      match src_root with
      | None -> DR.error (MissingResource Unfixable)
      | Some src_root ->
          let** framed, _ =
            Tree.frame_range src_root
              ~replace_node:(fun x -> Ok x)
              ~rebuild_parent:(fun t ~left:_ ~right:_ -> Delayed.return t)
              src_range
          in
          let** () =
            match framed.node with
            | NotOwned _ -> DR.error (MissingResource Unfixable)
            | _ -> DR.ok ()
          in
          if%sat is_in_bounds dst_range dst_bounds then
            match dst_root with
            | None -> DR.error (MissingResource Unfixable)
            | Some dst_root ->
                let** _, new_dst_root =
                  Tree.frame_range dst_root
                    ~replace_node:(fun current ->
                      match current.node with
                      | NotOwned _ -> Error (MissingResource Unfixable)
                      | _ -> Ok (Tree.realign framed dst_ofs))
                    ~rebuild_parent:Tree.of_children dst_range
                in
                DR.of_result (with_root dst_tree new_dst_root)
          else DR.error BufferOverrun
    else DR.error BufferOverrun

  let assertions x =
    let bounds =
      Option.fold ~none:[]
        ~some:(fun (low, high) -> [ CoreP.bounds ~low ~high ])
        x.bounds
    in
    let tree =
      match x.root with
      | None -> []
      | Some root -> Tree.assertions root
    in
    bounds @ tree

  let assertions_others { root; _ } =
    match root with
    | None -> []
    | Some root -> Tree.assertions_others root

  let merge ~old_tree ~new_tree =
    let open DR.Syntax in
    Logging.verbose (fun m -> m "OLD TREE:@\n%a" pp old_tree);
    Logging.verbose (fun m -> m "NEW TREE:@\n%a" pp new_tree);
    if is_empty old_tree then DR.ok new_tree
    else if is_empty new_tree then DR.ok old_tree
    else
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
                fmt "There are %d new owned nodes" (List.length new_owned_nodes));
            let++ tree =
              List.fold_left
                (fun acc (tree_node : Tree.t) ->
                  let** acc = acc in
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
      { bounds = def_bounds; root = def_root }

  let substitution ~le_subst ~sval_subst ~svarr_subst { bounds; root } =
    let bounds = Option.map (Range.substitution ~le_subst) bounds in
    let root =
      Option.map (Tree.substitution ~sval_subst ~le_subst ~svarr_subst) root
    in
    { bounds; root }

  let execute_action a s args =
    let open Delayed.Syntax in
    let open DR.Syntax in
    match (a, args) with
    | GetCurPerm, [ ofs ] ->
        let** perm = get_perm_at s ofs in
        let perm_string =
          Expr.Lit (String (ValueTranslation.string_of_permission_opt perm))
        in
        DR.ok (s, [ perm_string ])
    | WeakValidPointer, [ ofs ] ->
        let** bool = weak_valid_pointer s ofs in
        let res = Expr.bool bool in
        DR.ok (s, [ res ])
    | DropPerm, [ low; high; Expr.Lit (String perm_string) ] ->
        let perm = ValueTranslation.permission_of_string perm_string in
        let++ s' = drop_perm s low high perm in
        (s', [])
    | Store, [ Expr.Lit (String chunk_name); ofs; value ] ->
        let* sval = SVal.of_gil_expr_exn value in
        let chunk = ValueTranslation.chunk_of_string chunk_name in
        let++ s' = store s chunk ofs sval in
        (s', [])
    | Load, [ Expr.Lit (String chunk_name); ofs ] ->
        let chunk = ValueTranslation.chunk_of_string chunk_name in
        let** value, s' = load s chunk ofs in
        let* gil_value = SVal.to_gil_expr value in
        DR.ok (s', [ gil_value ])
    | _, _ ->
        failwith
          (Fmt.str "Invalid action %s with args %a" (action_to_str a)
             Fmt.Dump.(list Expr.pp)
             args)

  let consume pred s ins =
    let open Delayed.Syntax in
    let open DR.Syntax in
    match (pred, ins) with
    | Single, [ ofs; Expr.Lit (String chunk_string) ] ->
        let chunk = ValueTranslation.chunk_of_string chunk_string in
        let** sval, perm, s' = cons_single s ofs chunk in
        let* sval_e = SVal.to_gil_expr sval in
        let perm_string = ValueTranslation.string_of_permission_opt perm in
        DR.ok (s', [ sval_e; Expr.Lit (String perm_string) ])
    | Array, [ ofs; size; Expr.Lit (String chunk_string) ] ->
        let chunk = ValueTranslation.chunk_of_string chunk_string in
        let** array, perm, s' = cons_array s ofs size chunk in
        let range = Range.of_low_chunk_and_size ofs chunk size in
        let* array_e = SVArr.to_gil_expr ~chunk ~range array in
        let perm_string = ValueTranslation.string_of_permission_opt perm in
        DR.ok (s', [ array_e; Expr.Lit (String perm_string) ])
    | Hole, [ low; high ] ->
        let** s', perm = cons_hole s low high in
        let perm_e =
          Expr.string (ValueTranslation.string_of_permission_opt perm)
        in
        DR.ok (s', [ perm_e ])
    | Zeros, [ low; high ] ->
        let** s', perm = cons_zeros s low high in
        let perm_e =
          Expr.string (ValueTranslation.string_of_permission_opt perm)
        in
        DR.ok (s', [ perm_e ])
    | Bounds, [] ->
        let++ bounds, s' = cons_bounds s |> DR.of_result in
        let bounds_e =
          match bounds with
          | None -> Expr.Lit Null
          | Some (low, high) -> Expr.EList [ low; high ]
        in
        (s', [ bounds_e ])
    | _, _ -> failwith "Invalid consume call"

  let produce pred s insouts =
    let open Delayed.Syntax in
    let filter_errors dr =
      Delayed.bind dr (fun res ->
          match res with
          | Ok res -> Delayed.return res
          | Error err ->
              Logging.tmi (fun m -> m "Filtering error branch: %a" pp_err err);
              Delayed.vanish ())
    in
    match (pred, insouts) with
    | ( Single,
        [
          ofs;
          Expr.Lit (String chunk_string);
          sval_e;
          Expr.Lit (String perm_string);
        ] ) ->
        let perm = ValueTranslation.permission_of_string perm_string in
        let chunk = ValueTranslation.chunk_of_string chunk_string in
        let* sval = SVal.of_gil_expr_exn sval_e in
        prod_single s ofs chunk sval perm |> filter_errors
    | ( Array,
        [
          ofs;
          size;
          Expr.Lit (String chunk_string);
          arr_e;
          Expr.Lit (String perm_string);
        ] ) ->
        let perm = ValueTranslation.permission_of_string perm_string in
        let chunk = ValueTranslation.chunk_of_string chunk_string in
        let arr = SVArr.of_gil_expr_exn arr_e in
        prod_array s ofs size chunk arr perm |> filter_errors
    | Hole, [ low; high; Expr.Lit (String perm_string) ] ->
        let perm = ValueTranslation.permission_of_string perm_string in
        prod_hole s low high perm |> filter_errors
    | Zeros, [ low; high; Expr.Lit (String perm_string) ] ->
        let perm = ValueTranslation.permission_of_string perm_string in
        prod_zeros s low high perm |> filter_errors
    | Bounds, [ bounds_e ] ->
        let bounds =
          match bounds_e with
          | Expr.EList [ low; high ] -> Some (low, high)
          | Lit (LList [ low; high ]) -> Some (Lit low, Lit high)
          | Lit Null -> None
          | _ -> failwith "set_bounds wrong param"
        in
        prod_bounds s bounds |> DR.of_result |> filter_errors
    | _, _ -> failwith "Invalid produce call"

  let compose s1 s2 =
    let open Delayed.Syntax in
    let* res = merge ~old_tree:s1 ~new_tree:s2 in
    match res with
    | Ok s' -> Delayed.return s'
    | Error e ->
        Logging.verbose (fun fmt ->
            fmt "Vanishing on compose error: %a" pp_err_t e);
        Delayed.vanish ()

  let is_exclusively_owned { root; bounds } e =
    let open Delayed.Syntax in
    match (root, bounds, e) with
    | Some root, Some bounds, [ low; high ] ->
        if%ent Range.is_equal (low, high) bounds then
          let+ res = Tree.cons_node root (low, high) in
          match res with
          | Ok (node, _) -> Node.check_perm (Some Freeable) node |> Result.is_ok
          | Error _ -> false
        else Delayed.return false
    | _ -> Delayed.return false

  let substitution_in_place subst s =
    let le_subst = Subst.subst_in_expr subst ~partial:true in
    let sval_subst = SVal.substitution ~le_subst in
    let svarr_subst = SVal.SVArray.subst ~le_subst in
    substitution ~le_subst ~sval_subst ~svarr_subst s |> Delayed.return

  let get_recovery_tactic _ = Gillian.General.Recovery_tactic.none

  let instantiate = function
    | [ low; high ] ->
        let bounds = Range.make low high in
        ({ root = Some (Tree.undefined bounds); bounds = Some bounds }, [])
    | _ -> failwith "BlockTree: Invalid instantiate arguments"

  let can_fix = function
    | MissingResource _ -> true
    | _ -> false

  let get_fixes = function
    | MissingResource (Fixable { is_store; low = ofs; chunk }) ->
        let open CConstants.VTypes in
        let freeable_perm =
          ValueTranslation.string_of_permission Perm.Freeable |> Expr.string
        in
        let chunk_as_expr =
          ValueTranslation.string_of_chunk chunk |> Expr.string
        in
        let fixes =
          match chunk with
          | Mptr ->
              let new_var1 = Expr.LVar (LVar.alloc ()) in
              let new_var2 = Expr.LVar (LVar.alloc ()) in
              let value = Expr.EList [ new_var1; new_var2 ] in
              let null_typ =
                if Compcert.Archi.ptr64 then Expr.string long_type
                else Expr.string int_type
              in
              let null_ptr = Expr.EList [ null_typ; Expr.int 0 ] in
              [
                [
                  MyAsrt.CorePred
                    (Single, [ ofs; chunk_as_expr ], [ value; freeable_perm ]);
                  MyAsrt.Types
                    [ (new_var1, Type.ObjectType); (new_var2, Type.IntType) ];
                ];
                [
                  MyAsrt.CorePred
                    (Single, [ ofs; chunk_as_expr ], [ null_ptr; freeable_perm ]);
                ];
              ]
          | _ ->
              let type_str, type_gil =
                match chunk with
                | Chunk.Mfloat32 -> (single_type, Type.NumberType)
                | Chunk.Mfloat64 -> (float_type, Type.NumberType)
                | Chunk.Mint64 -> (long_type, Type.IntType)
                | _ -> (int_type, Type.IntType)
              in
              let new_var = Expr.LVar (LVar.alloc ()) in
              let value = Expr.EList [ Expr.string type_str; new_var ] in
              [
                [
                  MyAsrt.CorePred
                    (Single, [ ofs; chunk_as_expr ], [ value; freeable_perm ]);
                  MyAsrt.Types [ (new_var, type_gil) ];
                ];
              ]
        in
        (* Additional fix for store operation to handle case of unitialized memory *)
        if is_store then
          let offset_by_chunk low chunk =
            let open Expr.Infix in
            let len = Expr.int (Chunk.size chunk) in
            low + len
          in
          let uninit =
            [
              [
                MyAsrt.CorePred
                  (Hole, [ ofs; offset_by_chunk ofs chunk ], [ freeable_perm ]);
              ];
            ]
          in
          uninit @ fixes
        else fixes
    | MissingResource Unfixable -> []
    | _ -> failwith "BlockTree: Invalid get_fixes arguments"

  let allocated_function : t =
    {
      bounds = Some (Expr.zero_i, Expr.one_i);
      root =
        Some
          {
            node = Node.make_owned ~perm:Nonempty ~mem_val:(Undef Totally);
            span = (Expr.zero_i, Expr.one_i);
            children = None;
          };
    }
end

module MT : States.MyMonadicSMemory.S = M
