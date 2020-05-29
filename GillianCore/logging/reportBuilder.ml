type 'a t = unit -> 'a Report.t

let active_parents : (Uuidm.t * Phase.t) Stack.t = Stack.create ()

let all_parents : Phase.t Stack.t = Stack.create ()

let current : Uuidm.t option ref = ref Option.none

let seed = Random.State.make_self_init ()

let make ?title ~(content : 'a Report.content) ?(severity = Report.Log) () =
  let title =
    match title with
    | None       -> (
        match content with
        | Agnostic content -> (
            match content with
            | Debug _     -> "Debug message"
            | Phase phase -> Printf.sprintf "%s phase" phase )
        | Specific _       -> "Target language specific report" )
    | Some title -> title
  in
  let report : 'a Report.t =
    {
      id = Uuidm.v4_gen seed ();
      title;
      elapsed_time = Sys.time ();
      previous = !current;
      parent = Option.map fst @@ Stack.top_opt active_parents;
      content;
      severity;
    }
  in
  current := Some report.id;
  report

let start_phase level ?title ?severity phase =
  if Mode.enabled () then (
    if Mode.should_log level then (
      let report = make ?title ~content:(Agnostic (Phase phase)) ?severity () in
      Stack.push (report.id, phase) active_parents;
      current := None;
      Default.log report );
    Stack.push phase all_parents )

let end_phase phase =
  if Mode.enabled () then
    match Stack.top_opt active_parents with
    | Some (_, p) when String.equal p phase ->
        current := Stack.pop active_parents |> fst |> Option.some;
        assert (Stack.pop all_parents |> String.equal phase)
    | None | Some _ -> assert (Stack.pop all_parents |> String.equal phase)
