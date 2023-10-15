(* At some point, this should be just put actually
   set up in the environment. For now we filter them. *)
let is_cbmc_specific = function
  | "__CPROVER_assert"
  | "__CPROVER_assume"
  | "__CPROVER_architecture_NULL_is_zero"
  | "__CPROVER_architecture_os"
  | "__CPROVER_architecture_arch"
  | "__CPROVER_architecture_endianness"
  | "__CPROVER_architecture_alignment"
  | "__CPROVER_architecture_wchar_t_width"
  | "__CPROVER_architecture_long_double_width"
  | "__CPROVER_architecture_double_width"
  | "__CPROVER_deallocated"
  | "__CPROVER_next_thread_key"
  | "__CPROVER_thread_key_dtors"
  | "__CPROVER_thread_keys"
  | "__CPROVER_architecture_wchar_t_is_unsigned"
  | "__CPROVER_initialize"
  | "__CPROVER_next_thread_id"
  | "__CPROVER_architecture_char_is_unsigned"
  | "__CPROVER_pipe_count"
  | "__CPROVER_thread_id"
  | "__CPROVER_constant_infinity_uint"
  | "__CPROVER_architecture_pointer_width"
  | "__CPROVER_dead_object"
  | "__CPROVER_size_t"
  | "__CPROVER_architecture_short_int_width"
  | "__CPROVER_memory"
  | "__CPROVER_rounding_mode"
  | "__CPROVER_threads_exited"
  | "__CPROVER_architecture_memory_operand_size"
  | "__CPROVER_architecture_word_size"
  | "__CPROVER_new_object"
  | "__CPROVER_malloc_is_new_array"
  | "__CPROVER__start"
  | "__CPROVER_memory_leak"
  | "__CPROVER_alloca_object"
  | "__CPROVER_max_malloc_size"
  | "__CPROVER_architecture_int_width"
  | "__CPROVER_architecture_long_int_width"
  | "__CPROVER_architecture_bool_width"
  | "__CPROVER_architecture_char_width"
  | "__CPROVER_architecture_long_long_int_width"
  | "__CPROVER_architecture_single_width" -> true
  | _ -> false

let is_kani_specific = function
  | "__rust_alloc" | "__rust_dealloc" | "__rust_realloc" | "__rust_alloc_zeroed"
    -> true
  | _ -> false

let sanitize_symbol s =
  match s with
  | "" -> "i__empty"
  | _ -> (
      if is_cbmc_specific s || is_kani_specific s then s
      else
        let replaced = Str.global_replace (Str.regexp {|[:\.\$]|}) "_" s in
        match String.get replaced 0 with
        | 'A' .. 'Z' | 'a' .. 'z' -> replaced
        | _ -> "m__" ^ replaced)

let sanitize_symbol s =
  let new_s = sanitize_symbol s in
  new_s

let sanitizer =
  object
    inherit [unit] Visitors.map as super

    method! visit_expr_value ~ctx ~type_ value =
      match value with
      (* Rename any other symbol *)
      | Symbol s -> Symbol (sanitize_symbol s)
      | _ -> super#visit_expr_value ~ctx ~type_ value
  end

let sanitize_expr = sanitizer#visit_expr ~ctx:()
let sanitize_stmt = sanitizer#visit_stmt ~ctx:()

let sanitize_param (p : Param.t) =
  { p with identifier = Option.map sanitize_symbol p.identifier }

(** Sanitizes every variable symbol symbol. *)
let sanitize_program (prog : Program.t) =
  (* Create a second table with the new vars *)
  let new_vars = Hashtbl.create (Hashtbl.length prog.vars) in
  Hashtbl.iter
    (fun name gvar ->
      let new_name = sanitize_symbol name in
      let new_gvar =
        Program.Global_var.
          {
            type_ = gvar.type_;
            symbol = new_name;
            value = Option.map sanitize_expr gvar.value;
            location = gvar.location;
          }
      in
      Hashtbl.add new_vars new_name new_gvar)
    prog.vars;

  let new_funs = Hashtbl.create (Hashtbl.length prog.funs) in
  Hashtbl.iter
    (fun name func ->
      let new_name = sanitize_symbol name in
      let new_fun =
        Program.Func.
          {
            symbol = new_name;
            params = List.map sanitize_param func.params;
            body = Option.map sanitize_stmt func.body;
            location = func.location;
            return_type = func.return_type;
          }
      in
      Hashtbl.add new_funs new_name new_fun)
    prog.funs;

  let new_constrs = Hashtbl.create (Hashtbl.length prog.constrs) in
  Hashtbl.iter
    (fun name () ->
      let new_name = sanitize_symbol name in
      Hashtbl.add new_constrs new_name ())
    prog.constrs;
  { prog with funs = new_funs; vars = new_vars; constrs = new_constrs }
