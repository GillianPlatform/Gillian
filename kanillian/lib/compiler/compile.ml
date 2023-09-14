module KAnnot = Annot
open Gil_syntax
open Gillian.Utils.Prelude
module GExpr = Goto_lib.Expr
module GType = Goto_lib.Type
module Mem_interface = Memory_model.Interface

(* let as_pure_string_literal (e : GExpr.t) =
   match e.value with
   | AddressOf

         value =
           Index
             {
               array = { value = StringConstant str; _ };
               index = { value = IntConstant z; _ };
             }
         _;
       } ->
       let idx = Z.to_int z in
       String.sub str (Z.to_int z) (String.length str - idx)
   | StringConstant str -> str
   | _ -> Error.unexpected ("not a pure string literal: " ^ GExpr.show e) *)

(** Gillian-C utils for compilation*)

open Helpers
open Compile_expr

let set_global_function (fn : Program.Func.t) : Body_item.t Seq.t =
  let b =
    let loc = Body_item.compile_location fn.location in
    Body_item.make ~loc
  in
  let symbol = Expr.string fn.symbol in

  let target =
    match Constants.Internal_functions.hook fn.symbol with
    | Some f -> f
    | None -> fn.symbol
  in
  let target = Expr.string target in
  let glob_set_fun = Expr.string Constants.Internal_functions.glob_set_fun in
  let call =
    b @@ Cmd.Call ("u", glob_set_fun, [ symbol; target ], None, None)
  in
  Seq.return call

(* This is to be used without a current body.
   Do not call fresh_v or fresh_lv inside *)
let set_global_var ~ctx (gv : Program.Global_var.t) : Body_item.t Seq.t =
  let b =
    let loc = Body_item.compile_location gv.location in
    Body_item.make ~loc
  in
  (* If the value is a ZST, we don't even put it in the global environment.
     I'm not sure if that is the correct behaviour. *)
  if Ctx.is_zst_access ctx gv.type_ then Seq.empty
  else
    (* We start by allocating the variable *)
    let size = Ctx.size_of ctx gv.type_ in
    let loc_expr, alloc_cmd = Memory.alloc ~loc_var:"ll" ~size in
    let alloc_cmd = b alloc_cmd in
    let size = Expr.int size in
    let loc = "loc" in
    let assign_cmd = b @@ Cmd.Assignment (loc, loc_expr) in
    let loc = Expr.PVar loc in
    let store_zeros_cmd =
      let store_zeros = Constants.Internal_functions.store_zeros in
      b @@ Cmd.Call ("u", Lit (String store_zeros), [ loc; size ], None, None)
    in

    let store_value_cmds =
      match gv.value with
      | None -> []
      | Some e ->
          let v, v_init_cmds = compile_expr ~ctx e in
          let dst = Expr.EList [ loc; Expr.zero_i ] in
          let store_value =
            match v with
            | ByValue v ->
                [ b (Memory.store_scalar ~ctx ~var:"u" dst v gv.type_) ]
            | ByCompositValue { writes; _ } ->
                Memory.write_composit ~ctx ~annot:b ~dst writes
            | _ -> Error.unexpected "compile_global_var: not by value"
          in
          v_init_cmds @ store_value
    in
    let drom_perm_cmd =
      let drom_perm = Mem_interface.(str_ac (AMem DropPerm)) in
      let perm_string = Expr.Lit (String (Perm.to_string Writable)) in
      b @@ Cmd.LAction ("u", drom_perm, [ loc; Expr.zero_i; size; perm_string ])
    in
    let symexpr = Expr.Lit (String gv.symbol) in
    let set_symbol_cmd =
      let set_symbol = Mem_interface.(str_ac (AGEnv SetSymbol)) in
      b @@ Cmd.LAction ("u", set_symbol, [ symexpr; loc ])
    in
    let set_def_cmd =
      let set_def = Mem_interface.(str_ac (AGEnv SetDef)) in
      b
      @@ Cmd.LAction
           ("u", set_def, [ loc; EList [ Lit (String "variable"); symexpr ] ])
    in
    [ alloc_cmd; assign_cmd; store_zeros_cmd ]
    @ store_value_cmds
    @ [ drom_perm_cmd; set_symbol_cmd; set_def_cmd ]
    |> List.to_seq

let set_global_env_proc (ctx : Ctx.t) =
  let ctx = Ctx.with_new_generators ctx in
  let variables = Hashtbl.to_seq_values ctx.prog.vars in
  let functions = Hashtbl.to_seq_values ctx.prog.funs in
  let set_variables = Seq.concat_map (set_global_var ~ctx) variables in
  let set_functions = Seq.concat_map set_global_function functions in
  let constructor_calls =
    Seq.map
      (fun c ->
        let cmd = Cmd.Call ("u", Expr.string c, [], None, None) in
        Body_item.make cmd)
      (Hashset.to_seq ctx.prog.constrs)
  in
  let ret =
    let b = Body_item.make in
    let assign = b @@ Cmd.Assignment (Kutils.Names.return_variable, Lit Null) in
    let ret = b Cmd.ReturnNormal in
    Seq.cons assign (Seq.return ret)
  in
  let body =
    Seq.concat
      (List.to_seq [ set_variables; set_functions; constructor_calls; ret ])
  in
  let body = Array.of_seq body in
  Proc.
    {
      proc_name = Constants.CBMC_names.initialize;
      proc_source_path = None;
      proc_internal = true;
      proc_body = body;
      proc_params = [];
      proc_spec = None;
      (* TODO *)
      proc_aliases = [];
      proc_calls = [];
    }

let compile_free_locals (ctx : Ctx.t) =
  let open Kutils.Prelude in
  let locals = Hashtbl.copy ctx.locals in
  let () =
    Hashtbl.filter_map_inplace
      (fun symbol (local : Ctx.Local.t) ->
        if Ctx.in_memory ctx symbol && not (Ctx.is_zst_access ctx local.type_)
        then Some local
        else None)
      locals
  in
  Hashtbl.to_seq_values locals
  |> Seq.map (Memory.dealloc_local ~ctx)
  |> List.of_seq

let compile_alloc_params ~ctx params =
  List.concat_map
    (fun (param, type_) ->
      if Ctx.is_zst_access ctx type_ then []
      else if Ctx.representable_in_store ctx type_ && Ctx.in_memory ctx param
      then
        let ptr, cmda = Memory.alloc_ptr ~ctx type_ in
        let cmdb = Memory.store_scalar ~ctx ptr (PVar param) type_ in
        let cmdc = Cmd.Assignment (param, ptr) in
        [ cmda; cmdb; cmdc ]
      else if not (Ctx.representable_in_store ctx type_) then
        (* Passing a structure to the function. In that case, we copy it. *)
        let dst, cmda = Memory.alloc_ptr ~ctx type_ in
        let cmdb = Memory.memcpy ~ctx ~dst ~src:(PVar param) ~type_ in
        let cmdc = Cmd.Assignment (param, dst) in
        [ cmda; cmdb; cmdc ]
      else [])
    params

let compile_function ~ctx (func : Program.Func.t) : (KAnnot.t, string) Proc.t =
  let f_loc = Body_item.compile_location func.location in
  let body =
    (* If the function has no body, it's assumed to be just non-det *)
    match func.body with
    | Some b -> b
    | None ->
        if !Kcommons.Kconfig.nondet_on_missing then
          let nondet =
            GExpr.
              {
                location = func.location;
                type_ = func.return_type;
                value = Nondet;
              }
          in
          Stmt.
            {
              stmt_location = func.location;
              body = Return (Some nondet);
              comment = None;
            }
        else
          let body =
            Stmt.Assert
              {
                cond =
                  GExpr.
                    {
                      value = BoolConstant false;
                      type_ = Bool;
                      location = func.location;
                    };
                property_class = Some "missing_function";
              }
          in
          Stmt.{ body; stmt_location = func.location; comment = None }
  in

  (* Fmt.pr "FUNCTION %s:\n%a@?\n\n" func.symbol Stmt.pp body; *)
  let ctx =
    Ctx.with_entering_body ctx ~params:func.params ~body ~location:func.location
  in
  let proc_params =
    List.map
      (fun x ->
        match x.Param.identifier with
        | None -> (Ctx.fresh_v ctx, x.type_)
        | Some s -> (s, x.type_))
      func.params
  in
  let proc_spec = None in
  let free_locals = compile_free_locals ctx in
  (* We add a return undef in case the function has no return *)
  let b = Body_item.make_hloc ~loc:func.location in
  let return_undef =
    b (Assignment (Kutils.Names.return_variable, Lit Undefined))
  in
  let return_block =
    set_first_label ~annot:(b ~loop:[]) Constants.Kanillian_names.ret_label
      (free_locals @ [ b ReturnNormal ])
  in
  let alloc_params = compile_alloc_params ~ctx proc_params |> List.map b in
  let _, comp_body = compile_statement ~ctx body in
  let proc_body =
    Array.of_list (alloc_params @ comp_body @ [ return_undef ] @ return_block)
  in
  let proc_params =
    let identifiers = List.map fst proc_params in
    if Ctx.representable_in_store ctx func.return_type then identifiers
    else Constants.Kanillian_names.return_by_copy_name :: identifiers
  in
  Proc.
    {
      proc_name = func.symbol;
      proc_source_path = Some f_loc.loc_source;
      proc_internal = false;
      proc_params;
      proc_body;
      proc_spec;
      (* TODO *)
      proc_aliases = [];
      proc_calls = [];
    }

let start_for_harness ~ctx (harness : Program.Func.t) =
  let cprover_start =
    let open Program.Func in
    let stmt stmt_body =
      Stmt.
        { stmt_location = harness.location; body = stmt_body; comment = None }
    in
    let expr type_ expr_value =
      GExpr.{ location = harness.location; type_; value = expr_value }
    in
    let harness_type =
      GType.Code { params = harness.params; return_type = harness.return_type }
    in
    let params, params_decls =
      List.split
      @@ List.map
           (fun (p : Param.t) ->
             let ident =
               match p.identifier with
               | None -> Ctx.fresh_v ctx
               | Some ident -> ident
             in
             let lhs = expr p.type_ (Symbol ident) in
             let value = Some (expr p.type_ Nondet) in
             (expr p.type_ (Symbol ident), stmt @@ Stmt.Decl { lhs; value }))
           harness.params
    in
    let cprover_init_type = GType.Code { params = []; return_type = Empty } in
    {
      symbol = Constants.CBMC_names.start;
      params = [];
      return_type = Empty;
      body =
        Some
          (stmt
          @@ Block
               ([
                  (* First call __CPROVER__initialize*)
                  stmt
                  @@ SFunctionCall
                       {
                         lhs = None;
                         func =
                           expr cprover_init_type
                             (Symbol Constants.CBMC_names.initialize);
                         args = [];
                       };
                ]
               @ params_decls
               @ [
                   stmt
                   @@ SFunctionCall
                        {
                          lhs = None;
                          func = expr harness_type (Symbol harness.symbol);
                          args = params;
                        };
                   stmt @@ Return None;
                 ]));
      location = harness.location;
    }
  in
  compile_function ~ctx cprover_start

let compile (context : Ctx.t) : (KAnnot.t, string) Prog.t =
  let program = context.prog in
  let gil_prog = Prog.create () in
  let gil_prog =
    Program.fold_functions
      (fun _ f prog ->
        (* Only compile the function if it isn't hooked *)
        if Option.is_none (Constants.Internal_functions.hook f.symbol) then
          let compiled = compile_function ~ctx:context f in
          let with_fun = Prog.add_proc prog compiled in
          if Ctx.is_act context then
            Prog.add_bispec with_fun (Logics.bispec ~ctx:context ~compiled f)
          else with_fun
        else prog)
      program gil_prog
  in
  let gil_prog = Prog.add_proc gil_prog (set_global_env_proc context) in
  let gil_prog =
    Option.fold ~none:gil_prog
      ~some:(fun harness ->
        let harness =
          match Hashtbl.find_opt program.funs harness with
          | Some f -> f
          | None -> Error.user_error (Fmt.str "Missing harness %s" harness)
        in
        let gil_prog =
          Prog.add_proc gil_prog (start_for_harness ~ctx:context harness)
        in
        gil_prog)
      context.harness
  in
  assert (Machine_model.equal context.machine Machine_model.archi64);
  let imports =
    Imports.imports Arch64 !Gillian.Utils.Config.current_exec_mode
  in
  { gil_prog with imports }
