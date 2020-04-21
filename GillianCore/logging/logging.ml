module Mode = Mode

let wrap_up = Reporter.wrap_up

let log lvl msgf =
  if Mode.should_log lvl then
    let report = ReportBuilder.info "" (Debug msgf) () in
    Reporter.log report

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
