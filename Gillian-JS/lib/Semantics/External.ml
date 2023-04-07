open Gillian.Gil_syntax.Literal
open Gillian.General
open Js2jsil_lib
module GProg = Gillian.Gil_syntax.Prog
module GProc = Gillian.Gil_syntax.Proc
module Literal = Gillian.Gil_syntax.Literal
module Annot = Gillian.Gil_syntax.Annot

(** JSIL external procedure calls *)
module M
    (Val : Val.S)
    (ESubst : Engine.ESubst.S with type vt = Val.t and type t = Val.et)
    (Store : Store.S with type vt = Val.t)
    (State : State.S
               with type vt = Val.t
                and type st = ESubst.t
                and type store_t = Store.t)
    (Call_stack : Call_stack.S with type vt = Val.t and type store_t = Store.t) =
struct
  (* module Call_stack = Call_stack.M(Val)(Store) *)

  let update_store state x v =
    let store = State.get_store state in
    let _ = Store.put store x v in
    let state' = State.set_store state store in
    state'

  (**
    JavaScript Eval

    @param prog JSIL program
    @param state Current state
    @param preds Current predicates
    @param cs Current call stack
    @param i Current index
    @param x Variable that stores the result
    @param v_args Parameters
    @param j Optional error index
    @return Resulting configuration
  *)
  let execute_eval (prog : ('a, int) GProg.t) state cs i x v_args j =
    let store = State.get_store state in
    let v_args = v_args @ [ Val.from_literal Undefined ] in
    match v_args with
    | x_scope :: x_this :: ostrict :: v_code :: _ -> (
        let opt_strict = Val.to_literal ostrict in
        match opt_strict with
        | Some (Bool strictness) -> (
            let opt_lit_code = Val.to_literal v_code in
            match opt_lit_code with
            | None ->
                raise (Failure "Eval statement argument not a literal string")
            | Some (String code) -> (
                let code =
                  Str.global_replace (Str.regexp (Str.quote "\\\"")) "\"" code
                in
                let opt_proc_eval =
                  try
                    let e_js =
                      JS_Parser.parse_string_exn ~force_strict:strictness code
                    in
                    let strictness =
                      strictness
                      || JS_Parser.Syntax.script_and_strict e_js.exp_stx
                    in
                    let cur_proc_id = Call_stack.get_cur_proc_id cs in
                    Ok
                      (JS2JSIL_Compiler.js2jsil_eval prog cur_proc_id strictness
                         e_js)
                  with
                  | JS_Parser.Error.ParserError
                      (JS_Parser.Error.FlowParser (_, error_type)) ->
                      Error error_type
                  | _ -> Error "SyntaxError"
                in
                match opt_proc_eval with
                | Ok proc_eval ->
                    let eval_v_args = [ x_scope; x_this ] in
                    let proc = GProg.get_proc prog proc_eval in
                    let proc =
                      match proc with
                      | Some proc -> proc
                      | None ->
                          raise (Failure "The eval procedure does not exist.")
                    in
                    let params = GProc.get_params proc in
                    let prmlen = List.length params in

                    let args = Array.make prmlen (Val.from_literal Undefined) in
                    let _ =
                      List.iteri
                        (fun i v_arg -> if i < prmlen then args.(i) <- v_arg)
                        eval_v_args
                    in
                    let args = Array.to_list args in

                    let new_store = Store.init (List.combine params args) in
                    let old_store = State.get_store state in
                    let state' = State.set_store state new_store in
                    let loop_ids = Call_stack.get_loop_ids cs in
                    let cs' =
                      Call_stack.push cs ~pid:proc_eval ~arguments:eval_v_args
                        ~store:old_store ~loop_ids ~ret_var:x ~call_index:i
                        ~continue_index:(i + 1) ?error_index:j ()
                    in
                    [ (state', cs', -1, 0) ]
                | Error error_type -> (
                    let error_variable =
                      match error_type with
                      | "SyntaxError" -> JS2JSIL_Helpers.var_se
                      | "ReferenceError" -> JS2JSIL_Helpers.var_re
                      | _ -> raise (Failure "Impossible error type")
                    in
                    match (Store.get store error_variable, j) with
                    | Some v, Some j ->
                        let _ = update_store state x v in
                        [ (state, cs, i, j) ]
                    | _, None ->
                        raise
                          (Failure
                             "Eval triggered an error, but no error label was \
                              provided")
                    | _, _ ->
                        raise (Failure "JavaScript error object undefined")))
            | _ ->
                let _ = update_store state x v_code in
                [ (state, cs, i, i + 1) ])
        | _ -> raise (Failure "Eval not given correct strictness"))
    | _ -> raise (Failure "Eval failed")

  (* Two arguments only, the parameters and the function body *)

  (**
    JavaScript Function Constructor

    @param prog JSIL program
    @param state Current state
    @param cs Current call stack
    @param i Current index
    @param x Variable that stores the result
    @param v_args Parameters
    @param j Optional error index
    @return Resulting configuration
  *)
  let execute_function_constructor prog state cs i x v_args j =
    let throw message =
      let _ = update_store state x (Val.from_literal (String message)) in
      [ (state, cs, i, Option.get j) ]
    in

    (* Initialise parameters and body *)
    let params : Literal.t = Option.get (Val.to_literal (List.nth v_args 0)) in
    let body : Literal.t = Option.get (Val.to_literal (List.nth v_args 1)) in

    match (params, body) with
    | String params, String code -> (
        let code =
          Str.global_replace (Str.regexp (Str.quote "\\\"")) "\"" code
        in
        let code =
          "function THISISANELABORATENAME (" ^ params ^ ") {" ^ code ^ "}"
        in

        let e_js =
          try Some (JS_Parser.parse_string_exn code) with _ -> None
        in
        match e_js with
        | None -> throw "Body not parsable."
        | Some e_js -> (
            match e_js.JS_Parser.Syntax.exp_stx with
            | Script (_, [ e ]) -> (
                match e.JS_Parser.Syntax.exp_stx with
                | JS_Parser.Syntax.Function
                    (strictness, Some "THISISANELABORATENAME", params, body) ->
                    let cur_proc_id = Call_stack.get_cur_proc_id cs in
                    let new_proc =
                      JS2JSIL_Compiler.js2jsil_function_constructor_prop prog
                        cur_proc_id params strictness body
                    in
                    let fun_name = String new_proc.proc_name in
                    let params = LList (List.map (fun x -> String x) params) in
                    let return_value = LList [ fun_name; params ] in
                    let _ =
                      update_store state x (Val.from_literal return_value)
                    in
                    [ (state, cs, i, i + 1) ]
                | _ -> throw "Body incorrectly parsed.")
            | _ -> throw "Not a script."))
    | _, _ -> throw "Body or parameters not a string."

  (**
    General External Procedure Treatment

    @param prog JSIL program
    @param state Current state
    @param cs Current call stack
    @param i Current index
    @param x Variable that stores the result
    @param pid Procedure identifier
    @param v_args Parameters
    @param j Optional error index
    @return Resulting configuration
  *)
  let execute
      (prog : ('a, int) GProg.t)
      (state : State.t)
      (cs : Call_stack.t)
      (i : int)
      (x : string)
      (pid : string)
      (v_args : Val.t list)
      (j : int option) =
    match pid with
    | "ExecuteEval" -> execute_eval prog state cs i x v_args j
    | "ExecuteFunctionConstructor" ->
        execute_function_constructor prog state cs i x v_args j
    | _ -> raise (Failure ("Unsupported external procedure call: " ^ pid))
end
