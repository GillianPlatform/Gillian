(**
    Module for querying the reports stored by the database reporter.
    Queries will return None if the LogQueryer is not enabled.
*)

let with_enabled func = if LogDatabase.is_enabled () then func () else None

let get_report report_id =
  with_enabled (fun () -> LogDatabase.get_report report_id)

let get_previous_report_id cur_report_id =
  with_enabled (fun () -> LogDatabase.get_previous_report_id cur_report_id)

let get_next_reports id =
  with_enabled (fun () -> Some (LogDatabase.get_next_reports id))
  |> Option.value ~default:[]

let get_next_report_ids cur_report_id =
  get_next_reports cur_report_id |> List.map (fun (id, _, _) -> id)

let get_next_report_id cur_report_id =
  match get_next_report_ids cur_report_id with
  | id :: _ -> Some id
  | [] -> None

let get_previously_freed_annot loc =
  with_enabled (fun () -> LogDatabase.get_previously_freed_annot loc)

let get_children_of ?(roots_only = false) id =
  if LogDatabase.is_enabled () then LogDatabase.get_children_of roots_only id
  else []

let get_cmd_results cmd_report_id =
  get_children_of cmd_report_id
  |> List.filter_map (fun (id, type_, content) ->
         if type_ = LoggingConstants.ContentType.cmd_result then
           Some (id, content)
         else None)

let get_unify_for id =
  get_children_of id
  |> List.find_map (fun (id, type_, content) ->
         if type_ = LoggingConstants.ContentType.unify then Some (id, content)
         else None)
