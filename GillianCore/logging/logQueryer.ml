let get_report report_id = LogDatabase.get_report report_id

let get_previous_report_id cur_report_id =
  LogDatabase.get_previous_report_id cur_report_id

let get_next_report_id cur_report_id =
  LogDatabase.get_next_report_id cur_report_id

let get_previous_annot () =
  match ReportBuilder.get_cur_parent_id () with
  | None             -> None
  | Some parent_uuid ->
      LogDatabase.get_previous_annot (Uuidm.to_string parent_uuid)

let get_previous_freed_annot = LogDatabase.get_previous_freed_annot
