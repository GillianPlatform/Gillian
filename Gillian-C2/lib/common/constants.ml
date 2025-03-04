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

  let names =
    [
      malloc;
      calloc;
      memmove;
      memcpy;
      memset;
      memcmp;
      store_zeros;
      loadv;
      storev;
      get_function_name;
      ef_memcpy;
      val_of_bool;
      bool_of_val;
      glob_set_fun;
      Rust.rust_alloc;
      Rust.rust_alloc_zeroed;
      Rust.rust_dealloc;
      Rust.rust_realloc;
    ]
end

module CBMC_names = struct
  let initialize = "__CPROVER_initialize"
  let start = "__CPROVER__start"
end

module Gillian_C2_names = struct
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

module Prefix = struct
  let internal_pred = "i__"
  let generated_pred = "p__"
  let location = "$l_"
end

module Internal_Predicates = struct
  let is_int = Prefix.internal_pred ^ "is_int"
  let is_ptr_to_0 = Prefix.internal_pred ^ "is_ptr_to_0"
  let is_ptr = Prefix.internal_pred ^ "is_ptr"
  let is_ptr_to_0_opt = Prefix.internal_pred ^ "is_ptr_to_0_opt"
  let is_ptr_opt = Prefix.internal_pred ^ "is_ptr_opt"
  let is_ptr_to_int_opt = Prefix.internal_pred ^ "is_ptr_to_int_opt"
  let is_ptr_to_float_opt = Prefix.internal_pred ^ "is_ptr_to_float_opt"
  let is_ptr_to_long_opt = Prefix.internal_pred ^ "is_ptr_to_long_opt"
  let is_ptr_to_single_opt = Prefix.internal_pred ^ "is_ptr_to_single_opt"
  let is_long = Prefix.internal_pred ^ "is_long"
  let is_single = Prefix.internal_pred ^ "is_single"
  let is_float = Prefix.internal_pred ^ "is_float"

  (** Internal value getters *)
  let ptr_to_0_get = Prefix.internal_pred ^ "ptr_to_0"

  let ptr_get = Prefix.internal_pred ^ "ptr"
  let int_get = Prefix.internal_pred ^ "int"
  let single_get = Prefix.internal_pred ^ "single"
  let long_get = Prefix.internal_pred ^ "long"
  let float_get = Prefix.internal_pred ^ "float"

  (** global_env *)
  let global_env = Prefix.internal_pred ^ "global_env"

  let glob_fun = Prefix.internal_pred ^ "glob_fun"
  let glob_var_unallocated = Prefix.internal_pred ^ "glob_var_unallocated"

  let glob_var_unallocated_loc =
    Prefix.internal_pred ^ "glob_var_unallocated_loc"

  let fun_ptr = Prefix.internal_pred ^ "function_ptr"

  (* Arrays *)

  let malloced = Prefix.internal_pred ^ "malloced"
  let zeros_ptr_size = Prefix.internal_pred ^ "zeros_ptr_size"
  let undefs_ptr_size = Prefix.internal_pred ^ "undefs_ptr_size"
  let array_ptr = Prefix.internal_pred ^ "array_ptr"

  (* Pointer arithmetic *)

  let ptr_add = Prefix.internal_pred ^ "ptr_add"
end
