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

  let reduce v =
    let open Delayed.Syntax in
    let+ value = Delayed.reduce v.value in
    { v with value }

  let unsign_int ~bit_size (e : Expr.t) =
    let open Formula.Infix in
    if%sat Expr.zero_i #<= e then Delayed.return e
    else
      let two_power_size = Z.(one lsl bit_size) in
      let open Expr.Infix in
      Delayed.return (e + Expr.int_z two_power_size)

  let sign_int ~bit_size (e : Expr.t) =
    let open Formula.Infix in
    let two_power_size = Z.(one lsl bit_size) in
    let imax = Expr.int_z Z.((two_power_size asr 1) - one) in
    if%sat e #<= imax then Delayed.return e
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
    if Chunk.equal chunk t.chunk then t.value
    else
      Fmt.failwith "to_gil_expr: chunk mismatch: %s vs %s"
        (Chunk.to_string chunk) (Chunk.to_string t.chunk)

  let zero_of_chunk (chunk : Chunk.t) =
    let make value = make ~chunk ~value in
    match chunk with
    | I8 | I16 | I32 | I64 | I128 | U8 | U16 | U32 | U64 | U128 ->
        make Expr.zero_i
    | F32 -> make (Lit (Num 0.))
    | F64 -> make (Lit (Num 0.))

  let any_of_chunk (chunk : Chunk.t) : t Delayed.t =
    let make value = make ~chunk ~value in
    let lvar = LVar.alloc () in
    let lvar_e = Expr.LVar lvar in
    let learned_types, learned =
      match chunk with
      | I8 | I16 | I32 | I64 | I128 | U8 | U16 | U32 | U64 | U128 ->
          let learned_types = [ (lvar, Type.IntType) ] in
          let learned =
            match Chunk.bounds chunk with
            | Some (low, high) ->
                let open Formula.Infix in
                [ lvar_e #>= (Expr.int_z low); lvar_e #<= (Expr.int_z high) ]
            | None -> []
          in
          (learned_types, learned)
      | F32 | F64 ->
          let learned_types = [ (lvar, Type.NumberType) ] in
          let learned = [] in
          (learned_types, learned)
    in
    Delayed.return ~learned_types ~learned (make lvar_e)

  let reencode ~(chunk : Chunk.t) (v : t) =
    let open Delayed.Syntax in
    match (Chunk.to_components v.chunk, Chunk.to_components chunk) with
    | Int _, Float _ | Float _, Int _ ->
        if sure_is_zero v then Delayed.return (zero_of_chunk chunk)
        else
          let () =
            Logging.normal (fun m ->
                m "Warning: over-approximating float-int type punning")
          in
          any_of_chunk chunk
    | ( Int { bit_width = size_from; signed = signed_from },
        Int { bit_width = size_to; signed = signed_to } ) ->
        if size_from != size_to then
          failwith "Error: sval reencode size mismatch, shouldn't happen";
        if signed_from == signed_to then Delayed.return v
        else if signed_from then
          let+ value = unsign_int ~bit_size:size_from v.value in
          { value; chunk }
        else
          let+ value = sign_int ~bit_size:size_from v.value in
          { value; chunk }
    | Float { bit_width = size_from }, Float { bit_width = size_to } ->
        if size_from != size_to then
          failwith "Error: sval float reencode size mismatch, shouldn't happen";
        Delayed.return v

  (** Returns the value represented as an array of small-endian bytes *)
  let to_raw_bytes_se (sval : t) : Expr.t list Delayed.t =
    if not (Chunk.is_int sval.chunk) then
      Fmt.failwith "Unhandled: byte_array of float value";
    let signed, size = Chunk.int_chunk_to_signed_and_size sval.chunk in
    let* unsigned_value =
      if signed then unsign_int ~bit_size:size sval.value
      else Delayed.return sval.value
    in
    (* We can't just Seq.init, because it would recreate a LVar every time *)
    let array = List.init size (fun _ -> LVar.alloc ()) in
    let learned_types = List.map (fun lvar -> (lvar, Type.IntType)) array in
    let exprs = List.map (fun lvar -> Expr.LVar lvar) array in
    let all_bytes =
      List.map
        (fun lv ->
          let open Formula.Infix in
          Expr.zero_i #<= lv #&& (lv #<= (Expr.int 255)))
        exprs
    in
    (* We take the bytes from small to big *)
    let add_to_sval =
      let total_sum_bytes =
        List.fold_left
          (fun (i, acc) lvar ->
            let res_expr =
              let open Expr.Infix in
              acc + (lvar * Expr.int_z i)
            in
            (Z.shift_left i 8, res_expr))
          (Z.one, Expr.zero_i) exprs
      in
      let open Formula.Infix in
      unsigned_value #== (snd total_sum_bytes)
    in
    let learned = add_to_sval :: all_bytes in
    let result =
      match !Kconfig.endianness with
      | `LittleEndian -> exprs
      | `BigEndian -> List.rev exprs
    in
    Delayed.return ~learned_types ~learned result

  (** Takes an array of small-endian bytes and builds a value *)
  let of_raw_bytes_se ~chunk (bytes : Expr.t list) : t Delayed.t =
    let open Delayed.Syntax in
    if not (Chunk.is_int chunk) then
      Fmt.failwith "Unhandled: byte_array of float value";
    let signed, byte_size = Chunk.int_chunk_to_signed_and_size chunk in
    let bytes =
      match !Kconfig.endianness with
      | `LittleEndian -> bytes
      | `BigEndian -> List.rev bytes
    in
    let expr, _ =
      List.fold_left
        (fun (acc, factor) byte ->
          let next = Z.shift_left factor 8 in
          let factor = Expr.int_z factor in
          let open Expr.Infix in
          (acc + (byte * factor), next))
        (Expr.zero_i, Z.one) bytes
    in
    let+ expr =
      if signed then sign_int ~bit_size:(byte_size * 8) expr
      else Delayed.return expr
    in
    make ~chunk ~value:expr
end

module SVArray = struct
  type t = { values : Expr.t; chunk : Chunk.t } [@@deriving yojson]

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

  (** This assumes chunks are properly respected outside of the call of this function *)
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
    let open Formula.Infix in
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
        Logging.verbose (fun fmt ->
            fmt "Zeros pf: not as concrete: %a" Expr.pp size);
        let values_var = LVar.alloc () in
        let values = Expr.LVar values_var in
        let is_zero e = e #== (Expr.int 0) in
        let i = LVar.alloc () in
        let i_e = Expr.LVar i in
        let zero = Expr.zero_i in
        let learned_types = [ (values_var, Type.ListType) ] in
        let correct_length = (Expr.list_length values) #== size in
        let all_zero =
          forall
            [ (i, Some IntType) ]
            zero #<= i_e #&& (i_e #< size)
            #=> (is_zero (Expr.list_nth_e values i_e))
        in
        return ~learned:[ correct_length; all_zero ] ~learned_types values

  let byte_array_of_sval (sval : SVal.t) : t Delayed.t =
    let open Delayed.Syntax in
    let+ result = SVal.to_raw_bytes_se sval in
    { chunk = sval.chunk; values = Expr.EList result }

  let decode_sval_into ~chunk (sval : SVal.t) =
    let open Delayed.Syntax in
    match (Chunk.to_components sval.chunk, Chunk.to_components chunk) with
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
    | ( Int { bit_width = size_from; signed = signed_from },
        Int { bit_width = size_into; signed = signed_into } ) ->
        if size_from < size_into then
          failwith
            "Error: decomposing one smaller int into a list of bigger ones"
        else if size_from == size_into then
          (* Callng reencode is maybe a bit slower here since a lot
             of checks will be done again, but it's nicer to centralise
             this logic. *)
          let+ sval = SVal.reencode ~chunk sval in
          singleton sval
        else if size_from mod size_into != 0 then
          failwith "decomposition size doesn't match"
        else
          (* Otherwise, our best effort for now is to simply decompose the value
             as an array of bytes, and then recompose the bytes into the right form. *)
          let* raw_bytes_se = SVal.to_raw_bytes_se sval in
          (* On a big endian architecture where a U32 would be represented as ABCD,
             the bytes are now [D, C, B, A]. *)
          let packed_bytes =
            Kutils.batch_list ~batch_size:size_into raw_bytes_se
          in
          (* For example if we're building a list of U16 from a U32, from example above,
             we now have [[D, C], [B, A]]*)
          let+ values =
            List.fold_left
              (fun acc bytes_se ->
                let* acc = acc in
                let+ value = SVal.of_raw_bytes_se ~chunk bytes_se in
                value.value :: acc)
              (Delayed.return []) packed_bytes
          in
          (* We have now built each value and reversed the order,
             so values are [AB, CD]. However, that's the wrong order for a
             small-endian architecture.*)
          let values =
            match !Kconfig.endianness with
            | `BigEndian -> values
            | `LittleEndian -> List.rev values
          in
          make ~chunk ~values:(Expr.EList values)

  let decode_as_sval ~chunk arr =
    let get_exactly_one arr =
      SVal.{ chunk; value = Expr.list_nth arr.values 0 }
    in
    let open Delayed.Syntax in
    match (Chunk.to_components arr.chunk, Chunk.to_components chunk) with
    | Int _, Float _ | Float _, Int _ ->
        if sure_is_all_zeros arr then Delayed.return (SVal.zero_of_chunk chunk)
        else
          let () =
            Logging.normal (fun m ->
                m "Warning: over-approximating float-int type punning (array)")
          in
          SVal.any_of_chunk chunk
    | ( Int { bit_width = size_from; signed = signed_from },
        Int { bit_width = size_to; signed = signed_to } ) ->
        if size_from > size_to then
          failwith
            "Error: reencoding an array of a type to a single element of \
             smaller type";
        if size_from == size_to then
          let sval = get_exactly_one arr in
          if signed_from == signed_to then Delayed.return sval
          else if signed_from then
            let+ unsigned = SVal.unsign_int ~bit_size:size_from sval.value in
            { sval with value = unsigned }
          else
            let+ signed = SVal.sign_int ~bit_size:size_from sval.value in
            { sval with value = signed }
        else
          (* We did our best effort to preserve abstraction, and there's maybe still
             improvements to make. But until then, we just turn everything into an array of
             bytes and then reencode it.
             We know the array size exactly, because we know the size of each element
             and the total size. *)
          let array_size = size_to / size_from in
          let concrete_array = concretize_with_size ~size:array_size arr in
          let* raw_bytes_se =
            List.fold_left
              (fun acc sval ->
                let* acc = acc in
                let+ sval_bytes = SVal.to_raw_bytes_se sval in
                List.rev_append sval_bytes acc)
              (Delayed.return []) concrete_array
          in
          SVal.of_raw_bytes_se ~chunk raw_bytes_se
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
    { arr with values = Expr.list_sub ~lst:arr.values ~start ~size }

  let split_at_offset ~at arr : t * t =
    let size_right =
      let open Expr.Infix in
      Expr.list_length arr.values - at
    in
    ( array_sub ~arr ~start:Expr.zero_i ~size:at,
      array_sub ~arr ~start:at ~size:size_right )

  let split_at_byte ~at arr : (t * t) Delayed.t =
    let chunk_size = Expr.int (Chunk.size arr.chunk) in
    let can_keep_chunk =
      let open Formula.Infix in
      (Expr.imod at chunk_size) #== Expr.zero_i
    in
    if%ent can_keep_chunk then
      Delayed.return (split_at_offset ~at:Expr.Infix.(at / chunk_size) arr)
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
  let to_gil_expr ~size:_ ~chunk t =
    if Chunk.equal t.chunk chunk then t.values
    else
      Fmt.failwith "Chunk mismatch: %s vs %s" (Chunk.to_string t.chunk)
        (Chunk.to_string chunk)

  let subst ~le_subst t = { t with values = le_subst t.values }
end
