type ('value, 'state_err) t =
  | EProc    of 'value  (** Incorrect procedure identifier *)
  | ESt      of 'state_err  (** Memory Error *)
  | ECleanUp

let pp pp_val pp_state_err ft err =
  let open Fmt in
  match err with
  | EProc pid -> pf ft "@[<h>EProc(%a)@]" pp_val pid
  | ESt err   -> (hbox pp_state_err) ft err
  | ECleanUp  -> string ft "ECleanUp()"
