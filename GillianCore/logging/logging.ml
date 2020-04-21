module Mode = Mode
module Report = Report

let current_reporters = ref [ Reporter.file_reporter () ]

let wrap_up () =
  List.iter (fun reporter -> reporter.Reporter.wrap_up ()) !current_reporters

let log lvl msgf =
  if Mode.should_log lvl then
    let report = Report.info "" msgf () in
    List.iter (fun reporter -> reporter.Reporter.log report) !current_reporters

let normal msgf = log Normal msgf

let verbose msgf = log Verbose msgf

let tmi msgf = log TMI msgf

let print_to_all (str : string) =
  normal (fun m -> m "%s" str);
  print_endline str

(* Failure *)
let fail msg =
  normal (fun m -> m "%a" Format.pp_print_string msg);
  raise (Failure msg)
