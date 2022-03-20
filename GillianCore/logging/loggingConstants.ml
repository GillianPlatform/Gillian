(** Allowed strings for the type_ field of a report *)
module ContentType = struct
  let debug = "debug"
  let phase = "phase"
  let cmd_step = "cmd_step"
  let unify_step = "unify_step"
  let annotated_action = "annotated_action"
  let set_freed_info = "set_freed_info"
end
