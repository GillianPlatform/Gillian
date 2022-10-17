module ReportState = struct
  type t = { previous : ReportId.t option ref; parents : ReportId.t Stack.t }

  let make () = { previous = ref None; parents = Stack.create () }

  let clone { previous; parents } =
    { previous = ref !previous; parents = Stack.copy parents }

  let global_state = make ()
  let active_state = ref global_state
  let activate state = active_state := state

  let with_state f state =
    let prev_report_state = !active_state in
    active_state := state;
    let result = f () in
    active_state := prev_report_state;
    result
end

open ReportState

let get_previous () =
  let { previous; _ } = !active_state in
  !previous

let set_previous id =
  let { previous; _ } = !active_state in
  match id with
  | None -> ()
  | id -> previous := id

let get_parent () =
  let { parents; _ } = !active_state in
  Stack.top_opt parents

let set_parent id =
  let { previous; parents } = !active_state in
  previous := None;
  Stack.push id parents

let release_parent = function
  | None -> ()
  | Some rid as id_opt ->
      let { previous; parents } = !active_state in
      let parent_id = Stack.pop parents in
      assert (ReportId.equal rid parent_id);
      previous := id_opt

let make
    ?title
    ~(content : Loggable.t)
    ~(type_ : string)
    ?(severity = Report.Log)
    () =
  let title =
    match title with
    | None -> type_
    | Some title -> title
  in
  let report : Report.t =
    {
      id = ReportId.next ();
      title;
      elapsed_time = Sys.time ();
      previous = get_previous ();
      parent = get_parent ();
      content;
      severity;
      type_;
    }
  in
  set_previous (Some report.id);
  report

let start_phase level ?title ?severity () =
  if Mode.should_log level then (
    let report =
      make ?title
        ~content:(Loggable.make_string "*** Phase ***")
        ~type_:LoggingConstants.ContentType.phase ?severity ()
    in
    set_parent report.id;
    Some report)
  else None

let end_phase = release_parent
