type ('state, 'err) rfail = {
  proc : string;
  proc_idx : int;
  error_state : 'state;
  errors : 'err list;
  loc : Location.t option;
}
[@@deriving yojson]

type ('state, 'value) rsucc = {
  flag : Flag.t;
  ret_val : 'value;
  final_state : 'state;
  last_report : Logging.Report_id.t option;
  loc : Location.t option;
}
[@@deriving yojson]

type ('state, 'value, 'err) t =
  | RFail of ('state, 'err) rfail
  | RSucc of ('state, 'value) rsucc
[@@deriving yojson]

let pp_rfail pp_state pp_err ft { proc; proc_idx; error_state; errors; loc } =
  let open Fmt in
  pf ft
    "@[FAILURE TERMINATION: Procedure %s, Command %d%a@\n\
     Errors: @[<h>%a@]@\n\
     @[<v 2>FINAL STATE:@\n\
     %a@]@]"
    proc proc_idx Location.pp_full loc (list ~sep:comma pp_err) errors pp_state
    error_state

let pp_rsucc pp_state pp_value ft { flag; ret_val; final_state; _ } =
  Fmt.pf ft "@[SUCCESSFUL TERMINATION: (%s, %a)@\n@[<v 2>FINAL STATE:@\n%a@]@]"
    (Flag.str flag) pp_value ret_val pp_state final_state

let pp pp_state pp_value pp_err ft = function
  | RFail f -> pp_rfail pp_state pp_err ft f
  | RSucc s -> pp_rsucc pp_state pp_value ft s

let pp_what_exec_did pp_value pp_err ft res =
  let open Fmt in
  match res with
  | RFail { proc; proc_idx; errors; _ } ->
      pf ft
        "finished its execution with failure in proc %s at command %i with \
         errors %a"
        proc proc_idx (Dump.list pp_err) errors
  | RSucc { flag; ret_val; _ } ->
      pf ft "finished its execution successfully in %s mode and returned %a"
        (Flag.str flag) pp_value ret_val
