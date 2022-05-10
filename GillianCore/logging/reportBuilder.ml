let previous : ReportId.t option ref = ref None
let parents : ReportId.t Stack.t = Stack.create ()

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
      previous = !previous;
      parent = Stack.top_opt parents;
      content;
      severity;
      type_;
    }
  in
  previous := Some report.id;
  report

let set_previous = function
  | None -> ()
  | Some _ as id -> previous := id

let get_parent () = Stack.top_opt parents

let set_parent id =
  previous := None;
  Stack.push id parents

let release_parent = function
  | None -> ()
  | Some rid as id_opt ->
      let parent_id = Stack.pop parents in
      assert (ReportId.equal rid parent_id);
      previous := id_opt

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
let get_cur_parent_id () = Stack.top_opt parents
