module LoggingConstants = LoggingConstants
module Mode = Mode
module Report = Report
module Reporter = Reporter
module Loggable = Loggable
module LogQueryer = LogQueryer
module ReportId = ReportId

let () =
  Printexc.register_printer (function
    | Failure s ->
        Some (Format.asprintf "!!!!!!!!!!\nFAILURE:\n%s\n!!!!!!!!!!\n\n" s)
    | _ -> None)

let file_reporter : Reporter.t = (module FileReporter)
let database_reporter : Reporter.t = (module DatabaseReporter)
let reporters = ref []

let initialize (reporters_to_initialize : (module Reporter.S) list) =
  reporters := reporters_to_initialize;
  List.iter (fun reporter -> Reporter.initialize reporter) !reporters

let wrap_up () =
  List.iter (fun reporter -> Reporter.wrap_up reporter) !reporters

let log_on_all_reporters (report : Report.t) =
  List.iter (fun reporter -> Reporter.log reporter report) !reporters

let log lvl ?title ?severity msgf =
  if Mode.should_log lvl then
    let report =
      ReportBuilder.make ?title
        ~content:
          (Loggable.make PackedPP.pp PackedPP.of_yojson PackedPP.to_yojson
             (PP msgf))
        ~type_:LoggingConstants.ContentType.debug ?severity ()
    in
    log_on_all_reporters report

let log_specific lvl ?title ?severity loggable type_ =
  if Mode.should_log lvl then
    let report =
      ReportBuilder.make ?title ~content:loggable ~type_ ?severity ()
    in
    let () = log_on_all_reporters report in
    Some report.id
  else None

let normal ?title ?severity msgf = log Normal ?title ?severity msgf
let verbose ?title ?severity msgf = log Verbose ?title ?severity msgf
let tmi ?title ?severity msgf = log TMI ?title ?severity msgf

let normal_specific ?title ?severity loggable type_ =
  log_specific Normal ?title ?severity loggable type_

let verbose_specific ?title ?severity loggable type_ =
  log_specific Verbose ?title ?severity loggable type_

let tmi_specific ?title ?severity loggable type_ =
  log_specific TMI ?title ?severity loggable type_

let print_to_all (str : string) =
  normal (fun m -> m "%s" str);
  print_endline str

(* Failure *)
let fail msg =
  normal ~severity:Error (fun m -> m "%a" Format.pp_print_string msg);
  raise (Failure msg)

let start_phase level ?title ?severity () =
  let phase_report = ReportBuilder.start_phase level ?title ?severity () in
  match phase_report with
  | Some phase_report ->
      let () = log_on_all_reporters phase_report in
      Some phase_report.id
  | None -> None

let normal_phase = start_phase Normal
let verbose_phase = start_phase Verbose
let tmi_phase = start_phase TMI
let end_phase = ReportBuilder.end_phase

let with_phase level ?title ?severity f =
  let phase = start_phase level ?title ?severity () in
  let result =
    try Ok (f ())
    with e ->
      Printf.printf "Original Backtrace:\n%s" (Printexc.get_backtrace ());
      Error e
  in
  ReportBuilder.end_phase phase;
  match result with
  | Ok ok -> ok
  | Error e -> raise e

let with_normal_phase ?title ?severity f = with_phase Normal ?title ?severity f

let with_verbose_phase ?title ?severity f =
  with_phase Verbose ?title ?severity f

let with_tmi_phase ?title ?severity f = with_phase TMI ?title ?severity f
