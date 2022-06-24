module ExecMode = Gillian.Utils.ExecMode

module Architecture = struct
  type t = Arch64 | Arch32

  let a64 = [ Arch64 ]
  let a32 = [ Arch32 ]
  let any_arch = [ Arch32; Arch64 ]
end

module GEnvConfig = struct
  type t = Allocated_functions | Unallocated_functions

  let any_genv_config = [ Allocated_functions; Unallocated_functions ]
  let allocated = [ Allocated_functions ]
  let unallocated = [ Unallocated_functions ]

  let current_genv_config () =
    if !Config.allocated_functions then Allocated_functions
    else Unallocated_functions
end

module Imports = struct
  open ExecMode
  open Architecture
  open GEnvConfig

  let env_path_var = "GILLIAN_C_RUNTIME_PATH"

  type t = {
    file : string;
    arch : Architecture.t list;
    exec : ExecMode.t list;
    genv_config : GEnvConfig.t list;
  }

  (** All imports, should not be used as such, imports should be selected using the [import] function *)
  let all_imports =
    [
      (* Common *)
      {
        file = "unops_common.gil";
        arch = any_arch;
        exec = all_exec;
        genv_config = any_genv_config;
      };
      {
        file = "internals.gil";
        arch = any_arch;
        exec = all_exec;
        genv_config = any_genv_config;
      };
      {
        file = "global_environment_common.gil";
        arch = any_arch;
        exec = all_exec;
        genv_config = any_genv_config;
      };
      {
        file = "binops_common.gil";
        arch = any_arch;
        exec = all_exec;
        genv_config = any_genv_config;
      };
      {
        file = "logic_common.gil";
        arch = any_arch;
        exec = exec_with_preds;
        genv_config = any_genv_config;
      };
      {
        file = "string.gil";
        arch = any_arch;
        exec = all_exec;
        genv_config = any_genv_config;
      };
      (* Arch64 specific *)
      {
        file = "stdlib_archi64.gil";
        arch = a64;
        exec = all_exec;
        genv_config = any_genv_config;
      };
      {
        file = "stdlib_archi64_verif.gil";
        arch = a64;
        exec = ver_exec;
        genv_config = any_genv_config;
      };
      {
        file = "stdlib_archi64_non_verif.gil";
        arch = a64;
        exec = non_ver_exec;
        genv_config = any_genv_config;
      };
      {
        file = "global_environment_archi64.gil";
        arch = a64;
        exec = all_exec;
        genv_config = any_genv_config;
      };
      {
        file = "logic_archi64.gil";
        arch = a64;
        exec = exec_with_preds;
        genv_config = any_genv_config;
      };
      {
        file = "binops_archi64_all_exec.gil";
        arch = a64;
        exec = all_exec;
        genv_config = any_genv_config;
      };
      {
        file = "binops_archi64_non_bi.gil";
        arch = a64;
        exec = non_bi_exec;
        genv_config = any_genv_config;
      };
      {
        file = "binops_archi64_bi_exec.gil";
        arch = a64;
        exec = bi_exec;
        genv_config = any_genv_config;
      };
      (* Arch32 specific *)
      {
        file = "stdlib_archi32.gil";
        arch = a32;
        exec = all_exec;
        genv_config = any_genv_config;
      };
      {
        file = "stdlib_archi32_verif.gil";
        arch = a32;
        exec = ver_exec;
        genv_config = any_genv_config;
      };
      {
        file = "stdlib_archi32_non_verif.gil";
        arch = a32;
        exec = non_ver_exec;
        genv_config = any_genv_config;
      };
      {
        file = "global_environment_archi32.gil";
        arch = a32;
        exec = all_exec;
        genv_config = any_genv_config;
      };
      {
        file = "logic_archi32.gil";
        arch = a32;
        exec = exec_with_preds;
        genv_config = any_genv_config;
      };
      {
        file = "binops_archi32_all_exec.gil";
        arch = a32;
        exec = all_exec;
        genv_config = any_genv_config;
      };
      {
        file = "binops_archi32_non_bi.gil";
        arch = a32;
        exec = non_bi_exec;
        genv_config = any_genv_config;
      };
      {
        file = "binops_archi32_bi_exec.gil";
        arch = a32;
        exec = bi_exec;
        genv_config = any_genv_config;
      };
      (* Global functions *)
      {
        file = "genv_allocated_functions.gil";
        arch = any_arch;
        exec = all_exec;
        genv_config = allocated;
      };
      {
        file = "genv_unallocated_functions.gil";
        arch = any_arch;
        exec = all_exec;
        genv_config = unallocated;
      };
    ]

  let imports arch exec_mode genv_config =
    let select x =
      List.mem arch x.arch && List.mem exec_mode x.exec
      && List.mem genv_config x.genv_config
    in
    List.map (fun imp -> (imp.file, false)) (List.filter select all_imports)
end

module Internal_Functions = struct
  let initialize_genv = "i__initialize_genv"
  let malloc = "i__malloc"
  let calloc = "i__calloc"
  let memmove = "i__memmove"
  let memcpy = "i__memcpy"
  let ef_memcpy = "i__ef_memcpy"
  let memset = "i__memset"
  let strcmp = "i__strcmp"
  let strlen = "i__strlen"
  let strcpy = "i__strcpy"
  let rand = "i__rand"
  let free = "i__free"
  let loadv = "i__loadv"
  let storev = "i__storev"
  let free_list = "i__free_list"
  let not_implemented = "i__not_implemented"
  let get_function_name = "i__get_function_name"
  let glob_set_fun = "i__glob_set_fun"
  let glob_set_var = "i__glob_set_var"
  let bool_of_val = "i__bool_of_value"
  let printf = "EXTERN_printf"
end

module Builtin_Functions = struct
  let assert_f = "ASSERT"
  let assume_f = "ASSUME"
end

module BinOp_Functions = struct
  let add = "i__binop_add"
  let sub = "i__binop_sub"
  let div = "i__binop_div"
  let and_ = "i__binop_and"
  let xor = "i__binop_xor"
  let or_ = "i__binop_or"
  let shl = "i__binop_shl"
  let shr = "i__binop_shr"
  let mod_ = "i__binop_mod"
  let shru = "i__binop_shru"
  let mul = "i__binop_mul"
  let addl = "i__binop_addl"
  let subl = "i__binop_subl"
  let mull = "i__binop_mull"
  let divl = "i__binop_divl"
  let andl = "i__binop_andl"
  let shrlu = "i__binop_shrlu"
  let shll = "i__binop_shll"
  let orl = "i__binop_orl"
  let xorl = "i__binop_xorl"
  let divlu = "i__binop_divlu"
  let modlu = "i__binop_modlu"
  let addf = "i__binop_addf"
  let mulfs = "i__binop_mulfs"
  let divf = "i__binop_divf"
  let addfs = "i__binop_addfs"
  let subfs = "i__binop_subfs"
  let cmpl_le = "i__binop_cmpl_le"
  let cmpl_ge = "i__binop_cmpl_ge"
  let cmpl_lt = "i__binop_cmpl_lt"
  let cmpl_eq = "i__binop_cmpl_eq"
  let cmpu_le = "i__binop_cmpu_le"
  let cmpu_gt = "i__binop_cmpu_gt"
  let cmpu_ne = "i__binop_cmpu_ne"
  let cmpu_eq = "i__binop_cmpu_eq"
  let cmpu_ge = "i__binop_cmpu_ge"
  let cmplu_le = "i__binop_cmplu_le"
  let cmplu_eq = "i__binop_cmplu_eq"
  let cmplu_ne = "i__binop_cmplu_ne"
  let cmplu_ge = "i__binop_cmplu_ge"
  let cmplu_lt = "i__binop_cmplu_lt"
  let cmplu_gt = "i__binop_cmplu_gt"
  let cmpfs_ge = "i__binop_cmpfs_ge"
  let cmpfs_le = "i__binop_cmpfs_le"
  let cmp_gt = "i__binop_cmp_gt"
  let cmp_ge = "i__binop_cmp_ge"
  let cmp_lt = "i__binop_cmp_lt"
  let cmp_le = "i__binop_cmp_le"
  let cmp_eq = "i__binop_cmp_eq"
  let cmp_ne = "i__binop_cmp_ne"
end

module UnOp_Functions = struct
  let cast8signed = "i__unop_cast8signed"
  let cast8unsigned = "i__unop_cast8unsigned"
  let cast16signed = "i__unop_cast16signed"
  let cast16unsigned = "i__unop_cast16unsigned"
  let longofint = "i__unop_longofint"
  let longofsingle = "i__unop_longofsingle"
  let longuofsingle = "i__unop_longuofsingle"
  let intoflong = "i__unop_intoflong"
  let intoffloat = "i__unop_intoffloat"
  let intofsingle = "i__unop_intofsingle"
  let longofintu = "i__unop_longofintu"
  let longoffloat = "i__unop_longoffloat"
  let floatofint = "i__unop_floatofint"
  let floatofintu = "i__unop_floatofintu"
  let floatofsingle = "i__unop_floatofsingle"
  let singleoflongu = "i__unop_singleoflongu"
  let singleofint = "i__unop_singleofint"
  let singleoffloat = "i__unop_singleoffloat"
  let negl = "i__unop_negl"
  let negint = "i__unop_negint"
end

module VTypes = struct
  let int_type = "int"
  let long_type = "long"
  let single_type = "single"
  let float_type = "float"
end

module Prefix = struct
  let gvar = "gvar__"
  let uvar = "uvar__"
  let lvar = "#lvar"
  let then_lab = "then"
  let else_lab = "else"
  let endif_lab = "endif"
  let endswitch_lab = "endswitch"
  let end_block_lab = "blockend"
  let loop_lab = "loop"
  let user_lab = "userlab_"
  let default_lab = "default"
  let switch_lab = "switch"
  let case_lab = "case"
  let loc = "$l" (* This should rather be imported from Gillian directly *)

  let internal_preds = "i__"
  let generated_preds = "p__"
  let lloc = "#loc"
end

module Internal_Predicates = struct
  let is_int = Prefix.internal_preds ^ "is_int"
  let is_ptr_to_0 = Prefix.internal_preds ^ "is_ptr_to_0"
  let is_ptr = Prefix.internal_preds ^ "is_ptr"
  let is_ptr_to_0_opt = Prefix.internal_preds ^ "is_ptr_to_0_opt"
  let is_ptr_opt = Prefix.internal_preds ^ "is_ptr_opt"
  let is_ptr_to_int_opt = Prefix.internal_preds ^ "is_ptr_to_int_opt"
  let is_ptr_to_float_opt = Prefix.internal_preds ^ "is_ptr_to_float_opt"
  let is_ptr_to_long_opt = Prefix.internal_preds ^ "is_ptr_to_long_opt"
  let is_ptr_to_single_opt = Prefix.internal_preds ^ "is_ptr_to_single_opt"
  let is_long = Prefix.internal_preds ^ "is_long"
  let is_single = Prefix.internal_preds ^ "is_single"
  let is_float = Prefix.internal_preds ^ "is_float"

  (** Internal value getters *)
  let ptr_to_0_get = Prefix.internal_preds ^ "ptr_to_0"

  let ptr_get = Prefix.internal_preds ^ "ptr"
  let int_get = Prefix.internal_preds ^ "int"
  let single_get = Prefix.internal_preds ^ "single"
  let long_get = Prefix.internal_preds ^ "long"
  let float_get = Prefix.internal_preds ^ "float"

  (** global_env *)
  let global_env = Prefix.internal_preds ^ "global_env"

  let glob_fun = Prefix.internal_preds ^ "glob_fun"
  let glob_var_unallocated = Prefix.internal_preds ^ "glob_var_unallocated"

  let glob_var_unallocated_loc =
    Prefix.internal_preds ^ "glob_var_unallocated_loc"

  let fun_ptr = Prefix.internal_preds ^ "function_ptr"

  (* Arrays *)

  let malloced = Prefix.internal_preds ^ "malloced"
  let zeros_ptr_size = Prefix.internal_preds ^ "zeros_ptr_size"
  let undefs_ptr_size = Prefix.internal_preds ^ "undefs_ptr_size"
  let array_ptr = Prefix.internal_preds ^ "array_ptr"

  (* Pointer arithmetic *)

  let ptr_add = Prefix.internal_preds ^ "ptr_add"
end

module Symbolic_Constr = struct
  let symb_int = "symb_int"
  let symb_float = "symb_float"
  let symb_single = "symb_single"
  let symb_long = "symb_long"
end
