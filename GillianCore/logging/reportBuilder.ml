type 'a t = unit -> 'a Report.t

let parents : Uuidm.t Stack.t = Stack.create ()

let current : Uuidm.t option ref = ref Option.none

let seed = Random.State.make_self_init ()

let make ~title ~content ~severity () =
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

let info title content = make ~title ~content ~severity:Info

let log title content = make ~title ~content ~severity:Log

let success title content = make ~title ~content ~severity:Success

let error title content = make ~title ~content ~severity:Error

let warning title content = make ~title ~content ~severity:Warning

let enter_node () =
  Stack.push (Option.get !current) parents;
  current := Option.none

let exit_node () = current := Option.some @@ Stack.pop parents
