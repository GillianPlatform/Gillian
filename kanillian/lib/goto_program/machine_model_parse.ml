open Machine_model

module Builder = struct
  type b = {
    mutable alignment : int option;
    mutable bool_width : int option;
    mutable char_is_unsigned : bool option;
    mutable char_width : int option;
    mutable double_width : int option;
    mutable int_width : int option;
    mutable is_big_endian : bool option;
    mutable long_double_width : int option;
    mutable long_int_width : int option;
    mutable long_long_int_width : int option;
    mutable memory_operand_size : int option;
    mutable null_is_zero : bool option;
    mutable pointer_width : int option;
    mutable short_int_width : int option;
    mutable single_width : int option;
    mutable wchar_t_is_unsigned : bool option;
    mutable wchar_t_width : int option;
    mutable word_size : int option;
  }
  [@@deriving make]

  let get ~field o =
    match o with
    | Some o -> o
    | None ->
        failwith
          (Printf.sprintf "Couldn't build field %s, not initialized" field)

  let build (b : b) : t =
    {
      alignment = get ~field:"alignment" b.alignment;
      bool_width = get ~field:"bool_width" b.bool_width;
      char_is_unsigned = get ~field:"char_is_unsigned" b.char_is_unsigned;
      char_width = get ~field:"char_width" b.char_width;
      double_width = get ~field:"double_width" b.double_width;
      int_width = get ~field:"int_width" b.int_width;
      is_big_endian = get ~field:"is_big_endian" b.is_big_endian;
      long_double_width = get ~field:"long_double_width" b.long_double_width;
      long_int_width = get ~field:"long_int_width" b.long_int_width;
      long_long_int_width =
        get ~field:"long_long_int_width" b.long_long_int_width;
      memory_operand_size =
        get ~field:"memory_operand_size" b.memory_operand_size;
      null_is_zero = get ~field:"null_is_zero" b.null_is_zero;
      pointer_width = get ~field:"pointer_width" b.pointer_width;
      short_int_width = get ~field:"short_int_width" b.short_int_width;
      single_width = get ~field:"single_width" b.single_width;
      wchar_t_is_unsigned =
        get ~field:"wchar_t_is_unsigned" b.wchar_t_is_unsigned;
      wchar_t_width = get ~field:"wchar_t_width" b.wchar_t_width;
      word_size = get ~field:"word_size" b.word_size;
    }
end

open Irep.Infix

(* Once again, a bit hacky. This has been said somewhere else, but maybe
   constant lifting should be done in exactly one place and then used in other files. *)

let as_int (sym : Symbol.t) =
  sym.value $ Value
  |> Irep.as_just_bitpattern ~width:32 ~signed:false
  |> Z.to_int

let as_bool sym =
  match as_int sym with
  | 1 -> true
  | 0 -> false
  | _ ->
      Gerror.unexpected ~irep:sym.value
        "Invalid boolean when building machine model!"

let as_string (sym : Symbol.t) =
  let ops = sym.value.sub in
  List.nth ops 1 $ Value |> Irep.as_just_string

(** Modifies the symtab in-place,
  removes every field corresponding to the machine model,
  return the built machine model.
  Will fail if the symtab is incomplete *)
let consume_from_symtab (symtab : Symtab.t) =
  let open Builder in
  let builder = make_b () in
  symtab
  |> Hashtbl.filter_map_inplace (fun n sym ->
         match n with
         | "__CPROVER_architecture_NULL_is_zero" ->
             builder.null_is_zero <- Some (as_bool sym);
             None
         | "__CPROVER_architecture_os" | "__CPROVER_architecture_arch" ->
             (* We don't use that *)
             None
         | "__CPROVER_architecture_endianness" ->
             let be =
               match as_int sym with
               | 2 -> true
               | 1 -> false
               | _ -> Gerror.unexpected ~irep:sym.value "Invalid endianness"
             in
             builder.is_big_endian <- Some be;
             None
         | "__CPROVER_architecture_alignment" ->
             builder.alignment <- Some (as_int sym);
             None
         | "__CPROVER_architecture_wchar_t_width" ->
             builder.wchar_t_width <- Some (as_int sym);
             None
         | "__CPROVER_architecture_long_double_width" ->
             builder.long_double_width <- Some (as_int sym);
             None
         | "__CPROVER_architecture_double_width" ->
             builder.double_width <- Some (as_int sym);
             None
         | "__CPROVER_architecture_wchar_t_is_unsigned" ->
             builder.wchar_t_is_unsigned <- Some (as_bool sym);
             None
         | "__CPROVER_architecture_char_is_unsigned" ->
             builder.char_is_unsigned <- Some (as_bool sym);
             None
         | "__CPROVER_architecture_pointer_width" ->
             builder.pointer_width <- Some (as_int sym);
             None
         | "__CPROVER_architecture_short_int_width" ->
             builder.short_int_width <- Some (as_int sym);
             None
         | "__CPROVER_architecture_memory_operand_size" ->
             builder.memory_operand_size <- Some (as_int sym);
             None
         | "__CPROVER_architecture_word_size" ->
             builder.word_size <- Some (as_int sym);
             None
         | "__CPROVER_architecture_int_width" ->
             builder.int_width <- Some (as_int sym);
             None
         | "__CPROVER_architecture_long_int_width" ->
             builder.long_int_width <- Some (as_int sym);
             None
         | "__CPROVER_architecture_bool_width" ->
             builder.bool_width <- Some (as_int sym);
             None
         | "__CPROVER_architecture_char_width" ->
             builder.char_width <- Some (as_int sym);
             None
         | "__CPROVER_architecture_long_long_int_width" ->
             builder.long_long_int_width <- Some (as_int sym);
             None
         | "__CPROVER_architecture_single_width" ->
             builder.single_width <- Some (as_int sym);
             None
         | _ -> Some sym);
  build builder
