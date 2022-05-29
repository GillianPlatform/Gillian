(**
    Module for querying the reports stored by the database reporter.
    Queries will return None if the LogQueryer is not enabled.
*)

type relationship = LogDatabase.relationship = Child | Next

let with_enabled func = if LogDatabase.is_enabled () then func () else None

let get_report report_id =
  with_enabled (fun () -> LogDatabase.get_report report_id)

let get_previous_report_id cur_report_id =
  with_enabled (fun () -> LogDatabase.get_previous_report_id cur_report_id)

let get_next_report_id cur_report_id =
  with_enabled (fun () -> LogDatabase.get_next_report_id cur_report_id)

let get_previously_freed_annot loc =
  with_enabled (fun () -> LogDatabase.get_previously_freed_annot loc)

let get_cmd_results cmd_report_id =
  if LogDatabase.is_enabled () then LogDatabase.get_cmd_results cmd_report_id
  else []

let get_unification_for id relationship =
  with_enabled (fun () -> LogDatabase.get_unification_for id relationship)
