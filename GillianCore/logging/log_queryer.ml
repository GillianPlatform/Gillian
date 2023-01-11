(** @canonical Gillian.Logging.Log_queryer *)

let with_enabled func = if Log_database.is_enabled () then func () else None

let get_report report_id =
  with_enabled (fun () -> Log_database.get_report report_id)

let get_previous_report_id cur_report_id =
  with_enabled (fun () -> Log_database.get_previous_report_id cur_report_id)

let get_next_reports id =
  with_enabled (fun () -> Some (Log_database.get_next_reports id))
  |> Option.value ~default:[]

let get_next_report_ids cur_report_id =
  get_next_reports cur_report_id |> List.map (fun (id, _, _) -> id)

let get_next_report_id cur_report_id =
  match get_next_report_ids cur_report_id with
  | id :: _ -> Some id
  | [] -> None

let get_previously_freed_annot loc =
  with_enabled (fun () -> Log_database.get_previously_freed_annot loc)

let get_children_of ?(roots_only = false) id =
  if Log_database.is_enabled () then Log_database.get_children_of roots_only id
  else []

let get_cmd_results cmd_report_id =
  get_children_of cmd_report_id
  |> List.filter_map (fun (id, type_, content) ->
         if type_ = Logging_constants.Content_type.cmd_result then
           Some (id, content)
         else None)

let get_unify_for id =
  get_children_of id
  |> List.find_map (fun (id, type_, content) ->
         if type_ = Logging_constants.Content_type.unify then Some (id, content)
         else None)

let rec get_unify_results id =
  get_children_of id
  |> List.concat_map (fun (id, type_, content) ->
         if type_ = Logging_constants.Content_type.unify_case then
           get_unify_results id
         else if type_ = Logging_constants.Content_type.unify_result then
           [ (id, content) ]
         else [])
