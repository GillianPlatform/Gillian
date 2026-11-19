(** @canonical Gillian.General.Exec_err *)

(** @canonical Gillian.General.Exec_err.t *)
type ('value, 'state_err) t =
  | EProc of 'value  (** Incorrect procedure identifier *)
  | EState of 'state_err  (** Memory Error *)
  | ECleanUp
  | ELeak (* Memory leak *)
  | EFailReached of {
      fail_code : string;
      fail_params : 'value list;
      fail_msg : string option;
          (** The frontend's [fail_message] for it, if any *)
    }
[@@deriving yojson]

let pp pp_val pp_state_err ft err =
  let open Fmt in
  match err with
  | EProc pid -> pf ft "@[<h>EProc(%a)@]" pp_val pid
  | EState err -> (hbox pp_state_err) ft err
  | ECleanUp -> string ft "ECleanUp()"
  | ELeak -> string ft "MemoryLeak"
  | EFailReached { fail_msg = Some msg; _ } -> string ft msg
  | EFailReached { fail_code; fail_params; fail_msg = None } ->
      pf ft "@[<h>EFailReached(%s, %a)@]" fail_code (list ~sep:comma pp_val)
        fail_params
