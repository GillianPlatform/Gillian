let reporters =
  [ (new FileReporter.t :> Reporter.t); (new DatabaseReporter.t :> Reporter.t) ]

let log report = List.iter (fun reporter -> reporter#log report) reporters

let wrap_up () = List.iter (fun reporter -> reporter#wrap_up) reporters
