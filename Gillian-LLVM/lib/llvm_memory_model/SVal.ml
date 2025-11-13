open Gil_syntax
open Monadic
open Delayed.Syntax
module DO = Delayed_option
module DR = Delayed_result

module SVal = struct
  (* A symbolic value is just a value with its chunk, so we know how to interpret it. *)
  type t = { value : Expr.t; chunk : Chunk.t } [@@deriving yojson]

  let leak_chunk t = t.chunk
  let make ~chunk ~value = { value; chunk }
  let alocs v = Expr.alocs v.value
  let lvars v = Expr.lvars v.value
  let substitution ~le_subst v = { v with value = le_subst v.value }
  let leak t = (t.chunk, t.value)
  let is_concrete v = Expr.is_concrete v.value

  let vanish_or_fail_on_none f e fail_string =
    match e with
    | Some e -> Delayed.return (f e)
    | None ->
        if !Gillian.Utils.Config.under_approximation then Delayed.vanish ()
        else failwith fail_string

  let create_sval e =
    let open Delayed.Syntax in
    let* runtimetype = LLVMRuntimeTypes.type_of_expr e in
    vanish_or_fail_on_none
      (fun runtimetype ->
        (* XX(tnytown): create_sval is called by the store action with the tagged value, so we need to extract the raw value here *)
        let value = Expr.list_nth e 1 in
        make ~chunk:(LLVMRuntimeTypes.type_to_chunk runtimetype) ~value)
      runtimetype
      (Format.asprintf "Expression is not a valid symbolic value: %a" Expr.pp e)

  let reduce v =
    let open Delayed.Syntax in
    let+ value = Delayed.reduce v.value in
    { v with value }

  let unsign_int ~bit_size (e : Expr.t) =
    let open Expr.Infix in
    if%sat Expr.zero_i <= e then Delayed.return e
    else
      let two_power_size = Z.(one lsl bit_size) in
      let open Expr.Infix in
      Delayed.return (e + Expr.int_z two_power_size)

  let sign_int ~bit_size (e : Expr.t) =
    let open Expr.Infix in
    let two_power_size = Z.(one lsl bit_size) in
    let imax = Expr.int_z Z.((two_power_size asr 1) - one) in
    if%sat e <= imax then Delayed.return e
    else
      let open Expr.Infix in
      Delayed.return (e - Expr.int_z two_power_size)

  let syntactic_equal a b =
    Chunk.equal a.chunk b.chunk && Expr.equal a.value b.value

  let sure_is_zero { chunk; value } =
    if Chunk.is_int chunk then
      match value with
      | Expr.Lit (Int z) -> Z.equal z Z.zero
      | _ -> false
    else
      match value with
      | Lit (Num 0.) -> true
      | _ -> false

  let pp ft t = Fmt.pf ft "(%a : %s)" Expr.pp t.value (Chunk.to_string t.chunk)

  let to_gil_expr ~chunk t =
    if Chunk.equal chunk t.chunk then
      let type_expr e1 ty =
        let open Expr.Infix in
        Expr.BinOp (Expr.typeof e1, Equal, Expr.type_ ty)
      in
      let* learned, _ =
        let act_value = t.value in
        match chunk with
        | IntegerChunk w ->
            let learned = [ type_expr act_value (Type.BvType w) ] in
            Delayed.return (learned, LLVMRuntimeTypes.Int w)
        | IntegerOrPtrChunk ->
            let* rtype = LLVMRuntimeTypes.type_of_expr t.value in
            vanish_or_fail_on_none
              (fun runtimetype ->
                let learned =
                  [
                    type_expr act_value
                      (LLVMRuntimeTypes.rtype_to_gil_type runtimetype);
                  ]
                in
                (learned, runtimetype))
              rtype
              (Format.asprintf "Expression is not a valid symbolic value: %a"
                 Expr.pp t.value)
        | F32 ->
            let learned = [ type_expr act_value Type.NumberType ] in
            Delayed.return (learned, LLVMRuntimeTypes.F32)
        | F64 ->
            let learned = [ type_expr act_value Type.NumberType ] in
            Delayed.return (learned, LLVMRuntimeTypes.F64)
      in
      Delayed.return ~learned t.value
    else
      Fmt.failwith "to_gil_expr: chunk mismatch: %s vs %s"
        (Chunk.to_string chunk) (Chunk.to_string t.chunk)

  let zero_of_chunk (chunk : Chunk.t) =
    let make value = make ~chunk ~value in
    let make_bv w =
      make
        (Expr.list
           [
             Expr.string
               (LLVMRuntimeTypes.type_to_string (LLVMRuntimeTypes.Int w));
             Expr.zero_bv w;
           ])
    in
    match chunk with
    | IntegerChunk w -> make_bv w
    | IntegerOrPtrChunk -> make_bv (Llvmconfig.ptr_width ())
    | F32 -> make (Lit (Num 0.))
    | F64 -> make (Lit (Num 0.))

  let any_of_chunk_reified (chunk : Chunk.t) :
      (Expr.t * (string * Type.t) list) list =
    let types = LLVMRuntimeTypes.chunk_to_type chunk in
    let make_branch (ty : LLVMRuntimeTypes.t) =
      let lvar = LVar.alloc () in
      let learned_types = [ (lvar, LLVMRuntimeTypes.rtype_to_gil_type ty) ] in
      let expr =
        Expr.list
          [ Expr.string (LLVMRuntimeTypes.type_to_string ty); Expr.LVar lvar ]
      in
      (expr, learned_types)
    in
    List.map make_branch types

  let any_of_chunk (chunk : Chunk.t) : t Delayed.t =
    let make value = make ~chunk ~value in
    let branches =
      any_of_chunk_reified chunk
      |> List.map (fun (exprs, learned_types) ->
             let learned = [] in
             Delayed.return ~learned_types ~learned (make exprs))
    in
    Delayed.branches branches

  let reencode ~(chunk : Chunk.t) (v : t) =
    let open Delayed.Syntax in
    match (Chunk.to_components v.chunk, Chunk.to_components chunk) with
    | Ptr, Ptr -> Delayed.return v
    | Ptr, Float _ | Float _, Ptr ->
        failwith "Trying to convert between ptr and float, unhandled"
    | Int { bit_width = w }, Ptr | Ptr, Int { bit_width = w } ->
        if Int.equal w (Llvmconfig.ptr_width ()) then (
          Logging.normal (fun m ->
              m "Warning: over-approximating ptr to int type punning");
          any_of_chunk chunk)
        else failwith "Trying to convert between non pointer sized int and Ptr"
    | Int _, Float _ | Float _, Int _ ->
        if sure_is_zero v then Delayed.return (zero_of_chunk chunk)
        else
          let () =
            Logging.normal (fun m ->
                m "Warning: over-approximating float-int type punning")
          in
          any_of_chunk chunk
    | Int { bit_width = size_from }, Int { bit_width = size_to } ->
        if size_from != size_to then
          failwith "Error: sval reencode size mismatch, shouldn't happen"
        else Delayed.return v
    | Float { bit_width = size_from }, Float { bit_width = size_to } ->
        if size_from != size_to then
          failwith "Error: sval float reencode size mismatch, shouldn't happen";
        Delayed.return v

  (** Returns the value represented as an array of small-endian bytes *)
  let to_raw_bytes_se (sval : t) : Expr.t list Delayed.t =
    if not (Chunk.is_int sval.chunk) then
      Fmt.failwith "Unhandled: byte_array of float value";
    let size = Chunk.size sval.chunk in
    (* We can't just Seq.init, because it would recreate a LVar every time *)
    let array = List.init size (fun _ -> LVar.alloc ()) in
    let learned_types = List.map (fun lvar -> (lvar, Type.BvType 8)) array in
    let exprs = List.map (fun lvar -> Expr.LVar lvar) array in
    let learned =
      List.init size (fun i ->
          let target_lvar = List.nth exprs i in
          let lb = i * 8 in
          let ub = (i + 1) * 8 in
          let extracted =
            Expr.BVExprIntrinsic
              ( BVOps.BVExtract,
                [
                  (* bvextract's bounds are inclusive *)
                  Expr.Literal (ub - 1);
                  Expr.Literal lb;
                  Expr.BvExpr (sval.value, size * 8);
                ],
                Some 8 )
          in
          Expr.BinOp (target_lvar, Equal, extracted))
    in
    let result =
      match !Llvmconfig.endianness with
      | `LittleEndian -> exprs
      | `BigEndian -> List.rev exprs
    in
    Delayed.return ~learned_types ~learned result

  (** Takes an array of small-endian bytes and builds a value *)
  let of_raw_bytes_se ~chunk (bytes : Expr.t list) : t Delayed.t =
    let open Delayed.Syntax in
    if not (Chunk.is_int chunk) then
      Fmt.failwith "Unhandled: byte_array of float value";
    let bytes =
      match !Llvmconfig.endianness with
      | `LittleEndian -> bytes
      | `BigEndian -> List.rev bytes
    in
    let expr = Expr.bv_concat bytes in
    make ~chunk ~value:expr |> Delayed.return

  let assertions_others t =
    let open Expr.Infix in
    Option.fold (Chunk.type_of t.chunk) ~none:[] ~some:(fun x ->
        List.fold_left
          (fun curr ty ->
            Expr.BinOp (curr, BinOp.Or, Expr.typeof t.value == Expr.type_ ty))
          Expr.true_ x
        |> fun x -> [ Asrt.Pure x ])
end

module SVArray = struct
  type t = { values : Expr.t; chunk : Chunk.t } [@@deriving yojson]

  let is_concrete v = Expr.is_concrete v.values
  let make ~chunk ~values = { values; chunk }
  let alocs v = Expr.alocs v.values
  let lvars v = Expr.lvars v.values
  let leak t = (t.chunk, t.values)
  let leak_chunk t = t.chunk

  let reduce t =
    let open Delayed.Syntax in
    let+ values = Delayed.reduce t.values in
    { t with values }

  let pp ft t =
    Fmt.pf ft "(%a: [%s])" Expr.pp t.values (Chunk.to_string t.chunk)

  (* To be sound, this should be only ever called
     if the size given is guaranteed to be
      the size of the array *)
  let concretize_with_size ~size ({ chunk; values } : t) =
    List.init size (fun i ->
        let value = Expr.list_nth values i in
        SVal.make ~chunk ~value)

  let sure_is_all_zeros { chunk; values } =
    if Chunk.is_int chunk then
      match values with
      | Expr.EList l ->
          List.for_all
            (function
              | Expr.Lit (Int z) -> Z.equal z Z.zero
              | _ -> false)
            l
      | Expr.Lit (LList l) ->
          List.for_all
            (function
              | Literal.Int z -> Z.equal z Z.zero
              | _ -> false)
            l
      | _ -> false
    else
      match values with
      | Expr.EList l ->
          List.for_all
            (function
              | Expr.Lit (Num 0.) -> true
              | _ -> false)
            l
      | Expr.Lit (LList l) ->
          List.for_all
            (function
              | Literal.Num 0. -> true
              | _ -> false)
            l
      | _ -> false

  let syntactic_equal arr_a arr_b =
    Chunk.equal arr_a.chunk arr_b.chunk && Expr.equal arr_a.values arr_b.values

  let concat_same_chunk left right =
    if Chunk.equal left.chunk right.chunk then
      Some
        { chunk = left.chunk; values = Expr.list_cat left.values right.values }
    else None

  let singleton SVal.{ chunk; value } = { chunk; values = Expr.EList [ value ] }

  (** This assumes chunks are properly respected outside of the call of this
      function *)
  let cons_same_chunk (el : SVal.t) (arr : t) =
    concat_same_chunk (singleton el) arr

  let append_same_chunk arr el = concat_same_chunk arr (singleton el)

  let of_two_svals_same_chunk (first : SVal.t) (second : SVal.t) =
    if Chunk.equal first.chunk second.chunk then
      Some
        {
          chunk = first.chunk;
          values = Expr.EList [ first.value; second.value ];
        }
    else None

  let make_zeros ~chunk ~size : t Delayed.t =
    let return ?learned ?learned_types values =
      Delayed.return ?learned ?learned_types { chunk; values }
    in
    let size = Engine.Reduction.reduce_lexpr size in
    match size with
    | Lit (Int n) when Z.(n <= Z.of_int 512) ->
        (* We chose an arbitrary size limit, because we don't want to allocate
           a huge list of data for specs like <mem_zeros>(p, 2^32).
           If it gets bigger, the alternative is still sound. *)
        Logging.verbose (fun fmt -> fmt "Zeros pf: Concrete: %a" Expr.pp size);
        let values =
          Expr.EList (List.init (Z.to_int n) (fun _ -> Expr.zero_i))
        in
        return values
    | _ ->
        let open Expr.Infix in
        Logging.verbose (fun fmt ->
            fmt "Zeros pf: not as concrete: %a" Expr.pp size);
        let values_var = LVar.alloc () in
        let values = Expr.LVar values_var in
        let i = LVar.alloc () in
        let i_e = Expr.LVar i in
        let zero = Expr.zero_i in
        let learned_types = [ (values_var, Type.ListType) ] in
        let correct_length = Expr.list_length values == size in
        let all_zero =
          forall
            [ (i, Some IntType) ]
            ((zero <= i_e && i_e < size)
            ==> (Expr.list_nth_e values i_e == zero))
        in
        return ~learned:[ correct_length; all_zero ] ~learned_types values

  let byte_array_of_sval (sval : SVal.t) : t Delayed.t =
    let open Delayed.Syntax in
    let+ result = SVal.to_raw_bytes_se sval in
    (* At this point we know that the chunk is: *)
    (* IntegerChunk | IntegerOrPtrChunk *)
    (* Unconditionally decompose into an IntegerChunk of ptr width, matching the *)
    (* array element type *)
    let result_expr = Expr.EList result in
    let chunk = Chunk.IntegerChunk (Llvmconfig.ptr_width ()) in
    { chunk; values = result_expr }

  let decode_sval_into ~chunk (sval : SVal.t) =
    let open Delayed.Syntax in
    match (Chunk.to_components sval.chunk, Chunk.to_components chunk) with
    | Ptr, Ptr -> Delayed.return (singleton sval)
    | Ptr, Float _ | Float _, Ptr ->
        failwith "Trying to convert between ptr and float, unhandled"
    | Int { bit_width = from }, Ptr | Ptr, Int { bit_width = from } ->
        if Int.equal from (Llvmconfig.ptr_width ()) then
          let () =
            Logging.normal (fun m ->
                m "Warning: over-approximating ptr to int type punning")
          in
          let+ sval = SVal.any_of_chunk chunk in
          singleton sval
        else failwith "Trying to convert between non pointer sized int and Ptr"
    | Float { bit_width = from }, Float { bit_width = into } ->
        if from < into then
          failwith
            "Error: decomposing one smaller float into a list of bigger ones"
        else if from == into then Delayed.return (singleton sval)
        else
          (* We're in the case where we're decoding one F64 into 2 F32s.
             Gillian can't really handle that, so we're over-approximating here. *)
          let () =
            assert (Chunk.equal chunk F32 && Chunk.equal sval.chunk F64)
          in
          let first = LVar.alloc () in
          let second = LVar.alloc () in
          let learned_types =
            [ (first, Type.NumberType); (second, Type.NumberType) ]
          in
          let values = Expr.EList [ Expr.LVar first; Expr.LVar second ] in
          let array = make ~chunk ~values in
          Delayed.return ~learned_types array
    | Float { bit_width = from }, Int { bit_width = into; _ }
    | Int { bit_width = from; _ }, Float { bit_width = into } ->
        (* Float-int type punning. I can't possibly model that, I'm going to over-approximate. *)
        if from < into then
          failwith
            "Error: type-punning one smaller float/ints into a list of bigger \
             ones"
        else if from mod into != 0 then
          failwith "decomposition size doesn't match (float/int punning)"
        else
          let amount = from / into in
          let+ values =
            Seq.fold_left
              (fun acc _ ->
                let* acc = acc in
                let+ sval = SVal.any_of_chunk chunk in
                sval.value :: acc)
              (Delayed.return []) (Seq.init amount Fun.id)
          in
          make ~chunk ~values:(Expr.EList values)
    | Int { bit_width = size_from }, Int { bit_width = size_into } ->
        if size_from < size_into then
          failwith
            "Trying to build an array of elements smaller than the total size \
             of the array"
        else
          let num_elems = size_from / size_into in
          let each_elem =
            List.init num_elems (fun i ->
                Expr.bv_extract (i + size_into) i sval.value)
          in
          make ~chunk ~values:(Expr.list each_elem) |> Delayed.return

  let decode_as_sval ~chunk arr =
    let get_exactly_one arr =
      SVal.{ chunk; value = Expr.list_nth arr.values 0 }
    in
    let open Delayed.Syntax in
    match (Chunk.to_components arr.chunk, Chunk.to_components chunk) with
    | Ptr, Ptr -> Delayed.return (get_exactly_one arr)
    | Ptr, Float _ | Float _, Ptr ->
        failwith "Trying to convert between ptr and float, unhandled"
    | Int { bit_width = from }, Ptr | Ptr, Int { bit_width = from } ->
        if Int.equal from (Llvmconfig.ptr_width ()) then
          let () =
            Logging.normal (fun m ->
                m "Warning: over-approximating ptr to int type punning")
          in
          SVal.any_of_chunk chunk
        else
          failwith
            "Trying to convert between non pointer sized int we dont currently \
             support this and Ptr"
    | Int _, Float _ | Float _, Int _ ->
        if sure_is_all_zeros arr then Delayed.return (SVal.zero_of_chunk chunk)
        else
          let () =
            Logging.normal (fun m ->
                m "Warning: over-approximating float-int type punning (array)")
          in
          SVal.any_of_chunk chunk
    | Int { bit_width = size_from }, Int { bit_width = size_to } ->
        if size_from == size_to then
          let selem = get_exactly_one arr in
          Delayed.return
            SVal.
              {
                chunk;
                value = Expr.bv_extract_between_sz size_to size_from selem.value;
              }
        else
          (* Same size conversion so just concat everything *)
          let ln = size_to / size_from in
          let bts =
            List.init ln (fun i ->
                let elem = Expr.list_nth arr.values i in
                (*let elem' = try
                    Engine.Reduction.reduce_lexpr elem
                  with
                    _ as err ->
                    Logging.tmi (fun m -> m "Attempted to simplify decode_as_sval elem but: %s" (Printexc.to_string err));
                    elem
                  in*)
                Logging.tmi (fun m ->
                    m "decode_as_sval i=%d expr=@,%a" i Expr.pp elem);
                elem)
          in
          let built = Expr.bv_concat bts in
          Delayed.return SVal.{ chunk; value = built }
    | Float { bit_width = size_from }, Float { bit_width = size_to } ->
        if size_from > size_to then
          failwith
            "Error: reencoding an array of a float type to a single element of \
             smaller type";
        if size_from < size_to then
          failwith "unhandled: reencoding 2 F32 as 1 F64";
        Delayed.return (get_exactly_one arr)

  let reencode ~chunk arr : t Delayed.t =
    if Chunk.equal chunk arr.chunk then Delayed.return arr
    else failwith "unimplemented: decoding an array as another one"

  let array_sub ~arr ~start ~size : t =
    let e = Expr.list_sub ~lst:arr.values ~start ~size in
    let values =
      try Engine.Reduction.reduce_lexpr e
      with _ as err ->
        Logging.tmi (fun m ->
            m "Attempted to simplify array expression but: %s"
              (Printexc.to_string err));
        e
    in
    { arr with values }

  let split_at_offset ~at arr : t * t =
    let len_bv =
      match Expr.list_length arr.values with
      | Expr.Lit (Literal.Int x) -> Expr.Lit (LBitvector (x, 64))
      | _ -> failwith "unexpected value in list length"
    in
    let size_right = Expr.bv_sub len_bv at in
    ( array_sub ~arr ~start:(Expr.zero_bv 64) ~size:at,
      array_sub ~arr ~start:at ~size:size_right )

  let split_at_byte ~at arr : (t * t) Delayed.t =
    let chunk_size = Expr.bv_z (Z.of_int (Chunk.size arr.chunk)) 64 in
    let can_keep_chunk =
      let open Expr.Infix in
      Expr.imod at chunk_size == Expr.zero_bv 64
    in
    if%ent can_keep_chunk then
      Delayed.return (split_at_offset ~at:(Expr.bv_udiv at chunk_size) arr)
    else failwith "Unhandled: split_at_byte that doesn't preserve chunk"

  (* let split_array_in ~size ~amount arr =
     let i f = Expr.int f in
     List.init amount (fun k ->
         let values =
           Expr.list_sub ~lst:e ~start:(i (k * size)) ~size:(i size)
         in
         SVArr.Arr values) *)

  (* It's unclear what I'm going to do with
     this. I don't know how to ensure that size is always the right hing.*)

  let assertions_others ~(low : Expr.t) ~(high : Expr.t) (arr : t) =
    let chunk_size = Chunk.size arr.chunk |> Expr.int in
    let open Expr.Infix in
    let size = (high - low) / chunk_size in
    [
      Asrt.Pure (Expr.list_length arr.values == size);
      Asrt.Pure (Expr.typeof arr.values == Expr.type_ Type.ListType);
    ]

  let to_gil_expr ~size:_ ~chunk t =
    if Chunk.equal t.chunk chunk then t.values
    else
      Fmt.failwith "Chunk mismatch: %s vs %s" (Chunk.to_string t.chunk)
        (Chunk.to_string chunk)

  let subst ~le_subst t = { t with values = le_subst t.values }
end
