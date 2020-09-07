type 'a t = unit -> 'a Report.t

let parents : Uuidm.t Stack.t = Stack.create ()

let current : Uuidm.t option ref = ref Option.none

let seed = Random.State.make_self_init ()

let make ?title ~(content : 'a Report.content) ?(severity = Report.Log) () =
  let title =
    match title with
    | None       -> (
        match content with
        | Agnostic content -> (
            match content with
            | Debug _ -> "Debug message"
            | Phase   -> "Phase" )
        | Specific _       -> "Target language specific report" )
    | Some title -> title
  in
  let report : 'a Report.t =
    {
      id = Uuidm.v4_gen seed ();
      title;
      elapsed_time = Sys.time ();
      previous = !current;
      parent = Stack.top_opt parents;
      content;
      severity;
    }
  in
  current := Some report.id;
  report

let start_phase level ?title ?severity () =
  if Mode.should_log level then (
    let report = make ?title ~content:(Agnostic Phase) ?severity () in
    Stack.push report.id parents;
    current := None;
    Default.log report;
    Some report.id )
  else None

let end_phase = function
  | None    -> ()
  | Some id ->
      assert (Uuidm.equal id (Stack.pop parents));
      current := Some id
