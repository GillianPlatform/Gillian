type ('state, 'value, 'err) t =
  | RFail of {
      proc : string;
      proc_idx : int;
      error_state : 'state;
      errors : 'err list;
      loc : Location.t option;
    }
  | RSucc of {
      flag : Flag.t;
      ret_val : 'value;
      final_state : 'state;
      last_report : Logging.Report_id.t option;
      loc : Location.t option;
    }
[@@deriving yojson]

let pp pp_state pp_value pp_err ft res =
  let open Fmt in
  match res with
  | RFail { proc; proc_idx; error_state; errors; loc } ->
      pf ft
        "@[FAILURE TERMINATION: Procedure %s, Command %d%a@\n\
         Errors: @[<h>%a@]@\n\
         @[<v 2>FINAL STATE:@\n\
         %a@]@]"
        proc proc_idx Location.pp_full loc (list ~sep:comma pp_err) errors
        pp_state error_state
  | RSucc { flag; ret_val; final_state; _ } ->
      pf ft "@[SUCCESSFUL TERMINATION: (%s, %a)@\n@[<v 2>FINAL STATE:@\n%a@]@]"
        (Flag.str flag) pp_value ret_val pp_state final_state

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
