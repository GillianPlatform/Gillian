let reporters : (module Reporter.S) list =
  let module DR = DatabaseReporter in
  let module FR = FileReporter in
  [ (module DR); (module FR) ]

let initialize () =
  List.iter
    (fun (reporter : (module Reporter.S)) ->
      let (module R) = reporter in
      R.initialize ())
    reporters

let log report =
  List.iter
    (fun (reporter : (module Reporter.S)) ->
      let (module R) = reporter in
      R.log report)
    reporters

let wrap_up () =
  List.iter
    (fun (reporter : (module Reporter.S)) ->
      let (module R) = reporter in
      R.wrap_up ())
    reporters
