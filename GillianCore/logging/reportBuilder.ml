let previous : ReportId.t option ref = ref None

let parents : ReportId.t Stack.t = Stack.create ()

let make
    ?title ~(content : Loggable.t) ~(type_ : string) ?(severity = Report.Log) ()
    =
  let title =
    match title with
    | None       -> type_
    | Some title -> title
  in
  let report : Report.t =
    {
      id = ReportId.next ();
      title;
      elapsed_time = Sys.time ();
      previous = !previous;
      parent = Stack.top_opt parents;
      content;
      severity;
      type_;
    }
  in
  previous := Some report.id;
  report

let start_phase level ?title ?severity () =
  if Mode.should_log level then (
    let report =
      make ?title
        ~content:(Loggable.make_string "*** Phase ***")
        ~type_:LoggingConstants.ContentType.phase ?severity ()
    in
    previous := None;
    Stack.push report.id parents;
    Some report)
  else None

let end_phase = function
  | None               -> ()
  | Some rid as id_opt ->
      let parent_id = Stack.pop parents in
      assert (ReportId.equal rid parent_id);
      previous := id_opt

let get_cur_parent_id () = Stack.top_opt parents
