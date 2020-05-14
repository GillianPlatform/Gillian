type t

let file_reporter =
  object
    inherit [t] FileReporter.t

    method private target_lang _ = ()
  end

let database_reporter =
  object
    inherit [t] DatabaseReporter.t

    method private serialize_target_lang _ = ""
  end

let reporters =
  [ (file_reporter :> t Reporter.t); (database_reporter :> t Reporter.t) ]

let log report =
  List.iter
    (fun (reporter : t Reporter.t) -> reporter#log_agnostic report)
    reporters

let wrap_up () = List.iter (fun reporter -> reporter#wrap_up) reporters
