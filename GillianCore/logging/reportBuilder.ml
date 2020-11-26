let seed = Random.State.make_self_init ()

let previous : Report.id option ref = ref Option.none

let parents : Report.id Stack.t = Stack.create ()

let make ?title ~(content : 'a Report.content) ?(severity = Report.Log) () =
  let title =
    match title with
    | None       -> (
        match content with
        | Agnostic content -> (
            match content with
            | Debug _ -> "Debug message"
            | Phase   -> "Phase")
        | Specific _       -> "Target language specific report")
    | Some title -> title
  in
  let report : 'a Report.t =
    {
      id = (Unix.getpid (), Uuidm.v4_gen seed ());
      title;
      elapsed_time = Sys.time ();
      previous = !previous;
      parent = Stack.top_opt parents;
      content;
      severity;
    }
  in
  previous := Some report.id;
  report

let start_phase level ?title ?severity () =
  if Mode.should_log level then (
    let report = make ?title ~content:(Agnostic Phase) ?severity () in
    previous := None;
    Stack.push report.id parents;
    Default.log report;
    Some report.id)
  else None

let end_phase = function
  | None                   -> ()
  | Some (pid, uuid) as id ->
      let p, u = Stack.pop parents in
      assert (Int.equal pid p && Uuidm.equal uuid u);
      previous := id
