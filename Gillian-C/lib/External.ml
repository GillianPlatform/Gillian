open Gillian.Gil_syntax
open Gillian.General

(** JSIL external procedure calls *)
module M
    (Val : Val.S)
    (ESubst : ESubst.S with type vt = Val.t and type t = Val.et)
    (Store : Store.S with type vt = Val.t)
    (State : State.S
               with type vt = Val.t
                and type st = ESubst.t
                and type store_t = Store.t)
    (CallStack : CallStack.S with type vt = Val.t and type store_t = Store.t) =
struct
  let execute_printf _prog state cs i x v_args _j =
    let store = State.get_store state in
    let _ = Store.put store x (Val.from_literal Null) in
    let end_state = State.set_store state store in
    let () =
      Gillian.Logging.verbose (fun m ->
          m "C PRINTF WITH: %a" (Fmt.Dump.list Val.full_pp) v_args)
    in
    [ (end_state, cs, i, i + 1) ]

  (**
    General External Procedure Treatment
    @param prog GIL program
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
      (prog : (Annot.t, int) Prog.t)
      (state : State.t)
      (cs : CallStack.t)
      (i : int)
      (x : string)
      (pid : string)
      (v_args : Val.t list)
      (j : int option) =
    match pid with
    | "EXTERN_printf" -> execute_printf prog state cs i x v_args j
    | _ -> raise (Failure ("Unsupported external procedure call: " ^ pid))
end
