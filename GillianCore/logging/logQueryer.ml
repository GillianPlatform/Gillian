let get_report report_id = LogDatabase.get_report report_id

let get_previous_report_id cur_report_id =
  LogDatabase.get_previous_report_id cur_report_id

let get_next_report_id cur_report_id =
  LogDatabase.get_next_report_id cur_report_id
