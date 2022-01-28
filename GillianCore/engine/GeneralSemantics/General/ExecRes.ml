type ('state, 'value, 'err) t =
  | RFail of string * int * 'state * 'err list
  | RSucc of Flag.t * 'value * 'state

let pp pp_state pp_value pp_err ft res =
  let open Fmt in
  match res with
  | RFail (proc, i, state, errs) ->
      pf ft
        "@[FAILURE TERMINATION: Procedure %s, Command %d@\n\
         Errors: @[<h>%a@]@\n\
         @[<v 2>FINAL STATE:@\n\
         %a@]@]"
        proc i (list ~sep:comma pp_err) errs pp_state state
  | RSucc (fl, v, state) ->
      pf ft "@[SUCCESSFUL TERMINATION: (%s, %a)@\n@[<v 2>FINAL STATE:@\n%a@]@]"
        (Flag.str fl) pp_value v pp_state state

let pp_what_exec_did pp_value pp_err ft res =
  let open Fmt in
  match res with
  | RFail (proc, i, _, errs) ->
      pf ft
        "finished its execution with failure in proc %s at command %i with \
         errors %a"
        proc i (Dump.list pp_err) errs
  | RSucc (fl, v, _) ->
      pf ft "finished its execution successfully in %s mode and returned %a"
        (Flag.str fl) pp_value v
