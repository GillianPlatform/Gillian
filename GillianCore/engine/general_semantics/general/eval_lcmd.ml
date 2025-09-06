module L = Logging

module Make (Impl : Impl.S) = struct
  open Impl
  open Choice

  type ctx = { state : state; prog : annot MP.prog; did_branch : bool }
  type res = (ctx, state_err list) result Seq.t

  let ( let**& ) s f =
    s
    |> Seq.concat_map @@ function
       | Ok v -> f v
       | Error e -> return (Error e)

  let eval_expr state expr =
    match eval_expr' state expr with
    | Ok e -> Ok e
    | Error (errors, _) -> Error (Error errors)

  let eval_slcmd sl_cmd ({ prog; state; _ } as ctx) =
    let open Syntaxes.List in
    let ctxs =
      let+ state = State.evaluate_slcmd prog sl_cmd state in
      match state with
      | Ok state -> Ok { ctx with state }
      | Error e -> Error [ e ]
    in
    choose_const ctxs

  let eval_assume_type e t ({ state; _ } as ctx) =
    let&** v_x = eval_expr state e in
    match State.assume_t state v_x t with
    | Some state -> return (Ok { ctx with state })
    | _ ->
        L.normal (fun m ->
            m "ERROR: AssumeType: Cannot assume type %s for expression %a."
              (Type.str t) Expr.pp e);
        vanish

  let eval_assume f ({ state; _ } as ctx) =
    let store_subst = Store.to_ssubst (State.get_store state) in
    let f' = SVal.SESubst.subst_in_expr store_subst ~partial:true f in
    let&* f'', ctx =
      (* Sacha: I don't know why something different is happening in bi-exec *)
      if Exec_mode.is_biabduction_exec !Config.current_exec_mode then
        let fos =
          let rec aux = function
            | Expr.BinOp (e1, Or, e2) -> aux e1 @ aux e2
            | e -> [ e ]
          in
          aux f'
        in
        match fos with
        | [] -> vanish
        | [ f' ] -> return (f', { ctx with state })
        | f' :: other_fos ->
            let new_fos_states =
              other_fos
              |> List.map @@ fun f'' ->
                 let state = State.copy ctx.state in
                 (f'', { ctx with state; did_branch = true })
            in
            choose_const
            @@ ((f', { ctx with state; did_branch = true }) :: new_fos_states)
      else return (f', { ctx with state })
    in
    match State.assume_a state [ f'' ] with
    | Some state -> return (Ok { ctx with state })
    | _ -> vanish

  let eval_fresh_svar x ({ state; _ } as ctx) =
    let new_svar = Generators.fresh_svar () in
    let state = State.add_spec_vars state (SS.singleton new_svar) in
    let v = Val.from_expr (LVar new_svar) |> Option.get in
    let state = update_store state x v in
    return (Ok { ctx with state })

  let eval_assert f ({ state; _ } as ctx) =
    let store_subst = Store.to_ssubst (State.get_store state) in
    let f' = SVal.SESubst.subst_in_expr store_subst ~partial:true f in
    match State.assert_a state [ f' ] with
    | true -> return (Ok { ctx with state })
    | false ->
        let err = StateErr.EPure f' in
        let failing_model = State.sat_check_f state [ Expr.Infix.not f' ] in
        L.normal (fun m ->
            m
              "Assert failed with argument @[<h>%a@].@\n\
               @[<v 2>Failing Model:@\n\
               %a@]@\n"
              Expr.pp f'
              Fmt.(option ~none:(any "CANNOT CREATE MODEL") ESubst.pp)
              failing_model);
        return (Error [ err ])

  let eval_branch fof ({ state; did_branch; _ } as ctx) =
    let state' = State.copy state in
    let left_states =
      match State.assume_a state [ fof ] with
      | Some state -> [ state ]
      | None -> []
    in
    let right_states =
      match State.assume_a state' [ Expr.Infix.not fof ] with
      | Some state -> [ state ]
      | None -> []
    in
    let states = left_states @ right_states in
    let did_branch = did_branch || List.length states > 1 in
    states
    |> List.map (fun state -> Ok { ctx with state; did_branch })
    |> choose_const

  let rec eval_macro name args ({ prog; _ } as ctx) =
    let macro =
      let open MP in
      match Macro.get prog.prog.macros name with
      | Some macro -> macro
      | None ->
          L.verbose (fun m ->
              m "@[<v 2>Current MACRO TABLE:\n%a\n@]" Macro.pp_tbl
                prog.prog.macros);
          Fmt.failwith "NO MACRO found when executing: @[<h>%a@]" LCmd.pp
            (Macro (name, args))
    in
    let lcmds =
      let params = macro.macro_params in
      let num_params = List.length params in
      let num_args = List.length args in
      if num_params <> num_args then
        Fmt.failwith
          "Macro %s called with incorrect number of parameters: %d instead of \
           %d."
          macro.macro_name num_args num_params;
      let subst = SVal.SSubst.init (List.combine params args) in
      macro.macro_definition
      |> List.map (SVal.SSubst.substitute_lcmd subst ~partial:true)
    in
    eval_lcmds' lcmds ctx

  and eval_if e lcmds_t lcmds_e ({ state; did_branch; _ } as ctx) =
    let&** ve = eval_expr state e in
    let e = Val.to_expr ve in
    match e with
    | Expr.Lit (Bool true) -> eval_lcmds' lcmds_t ctx
    | Expr.Lit (Bool false) -> eval_lcmds' lcmds_e ctx
    | _ ->
        if not (Expr.is_boolean_expr e) then
          Fmt.failwith
            "Non-boolean expression in the condition of the logical if: %a"
            Expr.pp e;
        let ne = Expr.negate e in
        let state' = State.copy state in
        let then_states =
          match State.assume_a state [ e ] with
          | Some state -> [ (state, lcmds_t) ]
          | None -> []
        in
        let else_states =
          match State.assume_a state' [ ne ] with
          | Some state -> [ (state, lcmds_e) ]
          | None -> []
        in
        let states = then_states @ else_states in
        let did_branch = List.length states > 1 || did_branch in
        states
        |> List.map (fun (state, lcmds) () ->
               eval_lcmds' lcmds { ctx with state; did_branch } ())
        |> choose

  and eval_lcmds' lcmds ctx : res =
    match lcmds with
    | [] -> return (Ok ctx)
    | lcmd :: lcmds -> (
        let&* res = eval_lcmd' lcmd ctx in
        match res with
        | Ok ctx -> eval_lcmds' lcmds ctx
        | Error e -> return (Error e))

  and eval_lcmd' (lcmd : LCmd.t) ctx : res =
    match lcmd with
    | SL sl_cmd -> eval_slcmd sl_cmd ctx
    | AssumeType (e, t) -> eval_assume_type e t ctx
    | Assume f -> eval_assume f ctx
    | FreshSVar x -> eval_fresh_svar x ctx
    | Assert f -> eval_assert f ctx
    | Branch fof -> eval_branch fof ctx
    | Macro (name, args) -> eval_macro name args ctx
    | If (e, lcmds_t, lcmds_e) -> eval_if e lcmds_t lcmds_e ctx

  let wrap_ctx f prog state =
    let ctx = { prog; state; did_branch = false } in
    let&+ r = f ctx in
    match r with
    | Ok { state; did_branch; _ } -> Ok (state, did_branch)
    | Error e -> Error e

  let eval_lcmds lcmds = wrap_ctx (eval_lcmds' lcmds)
  let eval_lcmd lcmd = wrap_ctx (eval_lcmd' lcmd)
end
