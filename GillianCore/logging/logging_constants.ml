(** @canonical Gillian.Logging.Logging_constants*)

(** Allowed strings for the [type_] field of a report *)
module Content_type = struct
  let debug = "debug"
  let phase = "phase"
  let proc_init = "proc_init"
  let cmd = "cmd"
  let cmd_result = "cmd_result"
  let cmd_step = "cmd_step" (* TODO: Remove *)
  let unify = "unify"
  let unify_case = "unify_case"
  let unify_result = "unify_result"
  let assertion = "assertion"
  let annotated_action = "annotated_action"
  let set_freed_info = "set_freed_info"
end

module Severity = struct
  type t = Info | Log | Success | Error | Warning [@@deriving enum, yojson]
end
