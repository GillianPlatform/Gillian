(**
    Module for querying the reports stored by the database reporter.
    Queries will return None if the LogQueryer is not enabled.
*)

let is_enabled = ref false

let enable () = is_enabled := true

let with_enabled func = if !is_enabled then func () else None

let get_report report_id =
  with_enabled (fun () -> LogDatabase.get_report report_id)

let get_previous_report_id cur_report_id =
  with_enabled (fun () -> LogDatabase.get_previous_report_id cur_report_id)

let get_next_report_id cur_report_id =
  with_enabled (fun () -> LogDatabase.get_next_report_id cur_report_id)

let get_previous_annot () =
  with_enabled (fun () ->
      match ReportBuilder.get_cur_parent_id () with
      | None             -> None
      | Some parent_uuid ->
          LogDatabase.get_previous_annot (Uuidm.to_string parent_uuid))

let get_previous_freed_annot loc =
  with_enabled (fun () -> LogDatabase.get_previous_freed_annot loc)
