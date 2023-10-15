module Internal_functions = struct
  let malloc = "i__malloc"
  let calloc = "i__calloc"
  let memmove = "i__memmove"
  let memcpy = "i__memcpy"
  let memset = "i__memset"
  let memcmp = "i__memcmp"
  let store_zeros = "i__store_zeros"
  let loadv = "i__loadv"
  let storev = "i__storev"
  let get_function_name = "i__get_function_name"
  let ef_memcpy = "i__ef_memcpy"
  let val_of_bool = "i__value_of_bool"
  let bool_of_val = "i__bool_of_value"
  let glob_set_fun = "i__glob_set_fun"

  module Rust = struct
    let rust_alloc = "i__rust_alloc"
    let rust_alloc_zeroed = "i__rust_alloc_zeroed"
    let rust_dealloc = "i__rust_dealloc"
    let rust_realloc = "i__rust_realloc"
  end

  let hook = function
    | "malloc" -> Some malloc
    | "calloc" -> Some calloc
    | "memmove" -> Some memmove
    | "memcpy" -> Some memcpy
    | "memset" -> Some memset
    | "memcmp" -> Some memcmp
    | "__rust_alloc" -> Some Rust.rust_alloc
    | "__rust_alloc_zeroed" -> Some Rust.rust_alloc_zeroed
    | "__rust_dealloc" -> Some Rust.rust_dealloc
    | "__rust_realloc" -> Some Rust.rust_realloc
    | _ -> None
end

module CBMC_names = struct
  let initialize = "__CPROVER_initialize"
  let start = "__CPROVER__start"
end

module Kanillian_names = struct
  let return_by_copy_name = "i___ret"
  let ret_label = "ret"
end

module Unop_functions = struct
  let object_size = "i__unop_object_size"
end

module Binop_functions = struct
  let eq_maybe_ptr = "i__binop_equal_maybe_ptr"
  let neq_maybe_ptr = "i__binop_notequal_maybe_ptr"
  let gt_maybe_ptr = "i__binop_greaterthan_maybe_ptr"
  let lt_maybe_ptr = "i__binop_lowerthan_maybe_ptr"
  let geq_maybe_ptr = "i__binop_greatereq_maybe_ptr"
  let leq_maybe_ptr = "i__binop_lesseq_maybe_ptr"
  let add_maybe_ptr = "i__binop_add_maybe_ptr"
  let sub_maybe_ptr = "i__binop_sub_maybe_ptr"
  let mod_maybe_ptr = "i__binop_mod_maybe_ptr"
  let overflow_plus_maybe_ptr = "i__binop_overflow_plus_maybe_ptr"
end

module Cast_functions = struct
  let unsign_int_same_size = "i__cast_unsign_int_same_size"
  let sign_int_same_size = "i__cast_sign_int_same_size"
end

module Internal_Predicates = struct
  let _prefix = "i__"
  let is_int = _prefix ^ "is_int"
  let is_ptr_to_0 = _prefix ^ "is_ptr_to_0"
  let is_ptr = _prefix ^ "is_ptr"
  let is_ptr_to_0_opt = _prefix ^ "is_ptr_to_0_opt"
  let is_ptr_opt = _prefix ^ "is_ptr_opt"
  let is_ptr_to_int_opt = _prefix ^ "is_ptr_to_int_opt"
  let is_ptr_to_float_opt = _prefix ^ "is_ptr_to_float_opt"
  let is_ptr_to_long_opt = _prefix ^ "is_ptr_to_long_opt"
  let is_ptr_to_single_opt = _prefix ^ "is_ptr_to_single_opt"
  let is_long = _prefix ^ "is_long"
  let is_single = _prefix ^ "is_single"
  let is_float = _prefix ^ "is_float"

  (** Internal value getters *)
  let ptr_to_0_get = _prefix ^ "ptr_to_0"

  let ptr_get = _prefix ^ "ptr"
  let int_get = _prefix ^ "int"
  let single_get = _prefix ^ "single"
  let long_get = _prefix ^ "long"
  let float_get = _prefix ^ "float"

  (** global_env *)
  let global_env = _prefix ^ "global_env"

  let glob_fun = _prefix ^ "glob_fun"
  let glob_var_unallocated = _prefix ^ "glob_var_unallocated"
  let glob_var_unallocated_loc = _prefix ^ "glob_var_unallocated_loc"
  let fun_ptr = _prefix ^ "function_ptr"

  (* Arrays *)

  let malloced = _prefix ^ "malloced"
  let zeros_ptr_size = _prefix ^ "zeros_ptr_size"
  let undefs_ptr_size = _prefix ^ "undefs_ptr_size"
  let array_ptr = _prefix ^ "array_ptr"

  (* Pointer arithmetic *)

  let ptr_add = _prefix ^ "ptr_add"
end
