type t = I_bool | I_char | I_int | I_size_t | I_ssize_t
[@@deriving show { with_path = false }, eq]

module Bv_encoding = struct
  type int_type = t
  type t = { signed : bool; width : int } [@@deriving eq]

  let encode ~(machine : Machine_model.t) = function
    | I_int -> { signed = true; width = machine.int_width }
    | I_size_t -> { signed = false; width = machine.pointer_width }
    | I_char ->
        { signed = not machine.char_is_unsigned; width = machine.char_width }
    | I_ssize_t -> { signed = true; width = machine.pointer_width }
    | I_bool ->
        Gerror.code_error
          "using encode for I_bool, this should be unreachable - Kani doesn't \
           encode booleans as bitvectors"
end

let which_int_type_opt ~machine ~signed ~width =
  List.find_opt
    (fun it ->
      Bv_encoding.equal { signed; width } (Bv_encoding.encode ~machine it))
    [ I_int; I_char; I_size_t; I_ssize_t ]
