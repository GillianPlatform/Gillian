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

let get_match_for id =
  get_children_of id
  |> List.find_map (fun (id, type_, content) ->
         if type_ = Logging_constants.Content_type.match_ then Some (id, content)
         else None)

let rec get_match_results id =
  get_children_of id
  |> List.concat_map (fun (id, type_, content) ->
         if type_ = Logging_constants.Content_type.match_case then
           get_match_results id
         else if type_ = Logging_constants.Content_type.match_result then
           [ (id, content) ]
         else [])

let resolve_command_and_matches id =
  let open Logging_constants.Content_type in
  let rec aux acc id type_ =
    if type_ = cmd then (id, List.rev acc)
    else if type_ = assertion then
      let parent_id, parent_type, _ =
        match Log_database.get_parent_of id with
        | Some parent -> parent
        | None -> Fmt.failwith "Assertion %a has no parent" Report_id.pp id
      in
      let () =
        if parent_type <> match_ then
          Fmt.failwith
            "Expected type of %a (parent of assertion %a) to be match, got %s"
            Report_id.pp parent_id Report_id.pp id parent_type
      in
      let gparent_id, gparent_type, _ =
        match Log_database.get_parent_of parent_id with
        | Some gparent -> gparent
        | None -> Fmt.failwith "Match %a has no parent" Report_id.pp parent_id
      in
      aux ((id, parent_id) :: acc) gparent_id gparent_type
    else Fmt.failwith "Unknown type %s for %a" type_ Report_id.pp id
  in
  let type_ =
    match Log_database.get_report id with
    | Some (_, type_) -> type_
    | None -> Fmt.failwith "No report found for %a" Report_id.pp id
  in
  aux [] id type_
