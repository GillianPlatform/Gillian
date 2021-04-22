let file_reporter =
  object
    inherit FileReporter.t
  end

let database_reporter =
  object
    inherit DatabaseReporter.t
  end

let reporters =
  [ (file_reporter :> Reporter.t); (database_reporter :> Reporter.t) ]

let log report =
  List.iter (fun (reporter : Reporter.t) -> reporter#log report) reporters

let wrap_up () = List.iter (fun reporter -> reporter#wrap_up) reporters
