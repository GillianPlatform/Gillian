type t

let file_reporter =
  object
    inherit [t] FileReporter.t
  end

let database_reporter =
  object
    inherit [t] DatabaseReporter.t

    method private specific_serializer _ = `Null
  end

let reporters =
  [ (file_reporter :> t Reporter.t); (database_reporter :> t Reporter.t) ]

let log report =
  List.iter (fun (reporter : t Reporter.t) -> reporter#log report) reporters

let log_specific loggable report =
  List.iter
    (fun (reporter : t Reporter.t) -> reporter#log_specific loggable report)
    reporters

let wrap_up () = List.iter (fun reporter -> reporter#wrap_up) reporters
