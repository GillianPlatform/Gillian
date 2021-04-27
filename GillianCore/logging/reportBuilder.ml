(**
    Module for building reports
*)

let seed = Random.State.make_self_init ()

let previous : Report.uuidm option ref = ref Option.none

let parents : Report.uuidm Stack.t = Stack.create ()

let make
    ?title
    ~(content : Loggable.loggable)
    ~(type_ : string)
    ?(severity = Report.Log)
    () =
  let title =
    match title with
    | None       -> type_
    | Some title -> title
  in
  let report : Report.t =
    {
      id = Uuidm.v4_gen seed ();
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
  | None            -> ()
  | Some uuid as id ->
      let parent_uuid = Stack.pop parents in
      assert (Uuidm.equal uuid parent_uuid);
      previous := id
