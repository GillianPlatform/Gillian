module L = Logging
open Choice
open Choice.Syntax

module Make (Impl : Impl.S) = struct
  open Impl
  open Res_list.Syntax

  type res = (int, (state * bool, state_err) result) Choice.t
  type ctx = { state : state; prog : annot MP.prog; did_branch : bool }

  let eval_expr state expr =
    match eval_expr' state expr with
    | Ok e -> Res_list.return e
    | Error (errors, _) -> Res_list.just_errors errors

  let eval_assume_type e t state =
    let** v_x = eval_expr state e in
    match State.assume_t state v_x t with
    | Some state' -> [ Ok state' ]
    | _ ->
        L.normal (fun m ->
            m "ERROR: AssumeType: Cannot assume type %s for expression %a."
              (Type.str t) Expr.pp e);
        []

  let eval_assume f state =
    let store_subst = Store.to_ssubst (State.get_store state) in
    let f' = SVal.SESubst.subst_in_expr store_subst ~partial:true f in
    let open Syntaxes.List in
    let* f'', state =
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
        | [] -> []
        | [ f' ] -> [ (f', state) ]
        | f' :: other_fos ->
            let new_fos_states =
              List.map (fun f'' -> (f'', State.copy state)) other_fos
            in
            (f', state) :: new_fos_states
      else [ (f', state) ]
    in
    match State.assume_a state [ f'' ] with
    | Some state' -> Res_list.return state'
    | _ -> Res_list.vanish

  let eval_fresh_svar x state =
    let new_svar = Generators.fresh_svar () in
    let state' = State.add_spec_vars state (SS.singleton new_svar) in
    let v = Val.from_expr (LVar new_svar) |> Option.get in
    Res_list.return (update_store state' x v)

  let eval_assert f state =
    let store_subst = Store.to_ssubst (State.get_store state) in
    let f' = SVal.SESubst.subst_in_expr store_subst ~partial:true f in
    match State.assert_a state [ f' ] with
    | true -> Res_list.return state
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
        Res_list.error_with err

  let eval_branch fof state =
    let state' = State.copy state in
    let left_states =
      match State.assume_a state [ fof ] with
      | Some state -> Res_list.return state
      | None -> Res_list.vanish
    in
    let right_states =
      match State.assume_a state' [ Expr.Infix.not fof ] with
      | Some state -> Res_list.return state
      | None -> Res_list.vanish
    in
    left_states @ right_states

  let rec eval_macro name args (ctx : ctx) =
    let macro =
      let open MP in
      match Macro.get ctx.prog.prog.macros name with
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

  and eval_if e lcmds_t lcmds_e prog state =
    let** ve = eval_expr state e in
    let e = Val.to_expr ve in
    match e with
    | Expr.Lit (Bool true) -> eval_lcmds' lcmds_t prog state
    | Expr.Lit (Bool false) -> eval_lcmds' lcmds_e prog state
    | _ ->
        if not (Expr.is_boolean_expr e) then
          Fmt.failwith
            "Non-boolean expression in the condition of the logical if: %a"
            Expr.pp e;
        let ne = Expr.negate e in
        let state' = State.copy state in
        let then_states =
          match State.assume_a state [ e ] with
          | Some state -> eval_lcmds' lcmds_t prog state
          | None -> Res_list.vanish
        in
        let else_states =
          match State.assume_a state' [ ne ] with
          | Some state -> eval_lcmds' lcmds_e prog state
          | None -> Res_list.vanish
        in
        then_states @ else_states

  and eval_lcmds' lcmds prog state : res =
    match lcmds with
    | [] -> return (Ok state)
    | lcmd :: lcmds -> (
        let&* res = eval_lcmd' lcmd prog state in
        match res with
        | Ok state -> eval_lcmds' lcmds prog state
        | Error err -> return (Error err))

  and eval_lcmd' (lcmd : LCmd.t) prog state : res =
    match lcmd with
    | AssumeType (e, t) -> eval_assume_type e t state
    | Assume f -> eval_assume f state
    | FreshSVar x -> eval_fresh_svar x state
    | Assert f -> eval_assert f state
    | Branch fof -> eval_branch fof state
    | Macro (name, args) -> eval_macro name args prog state
    | If (e, lcmds_t, lcmds_e) -> eval_if e lcmds_t lcmds_e prog state
    | SL sl_cmd -> State.evaluate_slcmd prog sl_cmd state
end
