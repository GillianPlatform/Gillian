module Mode = Mode
module Report = Report

let current_reporters = ref [ Reporter.file_reporter () ]

let wrap_up () =
  List.iter (fun reporter -> reporter.Reporter.wrap_up ()) !current_reporters

let logr lvl msgf =
  if Mode.should_log lvl then
    List.iter
      (fun reporter -> reporter.Reporter.report @@ Report.info "" msgf)
      !current_reporters

let normal msgf = logr Normal msgf

let verbose msgf = logr Verbose msgf

let verboser msgf = logr Verboser msgf

let tmi msgf = logr TMI msgf

let print_to_all (str : string) =
  normal (fun m -> m "%s" str);
  print_endline str

(* Failure *)
let fail msg =
  normal (fun m -> m "%a" Format.pp_print_string msg);
  raise (Failure msg)
