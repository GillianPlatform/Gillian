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

let will_log_on_any_reporter (type_ : string) =
  List.exists (fun reporter -> Reporter.will_log reporter type_) !reporters

let log lvl ?title ?severity msgf =
  let type_ = LoggingConstants.ContentType.debug in
  if Mode.should_log lvl && will_log_on_any_reporter type_ then
    let report =
      ReportBuilder.make ?title
        ~content:
          (Loggable.make PackedPP.pp PackedPP.of_yojson PackedPP.to_yojson
             (PP msgf))
        ~type_ ?severity ()
    in
    log_on_all_reporters report

let log_specific lvl ?title ?severity loggable type_ =
  if Mode.should_log lvl && will_log_on_any_reporter type_ then
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

let set_previous = ReportBuilder.set_previous
let get_parent = ReportBuilder.get_parent
let set_parent = ReportBuilder.set_parent
let release_parent = ReportBuilder.release_parent

let with_parent_id id f =
  match id with
  | None -> f ()
  | Some id -> (
      set_parent id;
      let result =
        try Ok (f ())
        with e ->
          Printf.printf "Original Backtrace:\n%s" (Printexc.get_backtrace ());
          Error e
      in
      release_parent (Some id);
      match result with
      | Ok ok -> ok
      | Error e -> raise e)

let with_parent ?title ?(lvl = Mode.Normal) ?severity loggable type_ f =
  let id =
    Option.bind loggable (fun loggable ->
        log_specific lvl ?title ?severity loggable type_)
  in
  with_parent_id id f

let start_phase level ?title ?severity () =
  if will_log_on_any_reporter LoggingConstants.ContentType.phase then
    let phase_report = ReportBuilder.start_phase level ?title ?severity () in
    match phase_report with
    | Some phase_report ->
        let () = log_on_all_reporters phase_report in
        Some phase_report.id
    | None -> None
  else None

let normal_phase = start_phase Normal
let verbose_phase = start_phase Verbose
let tmi_phase = start_phase TMI

let end_phase id =
  if will_log_on_any_reporter LoggingConstants.ContentType.phase then
    ReportBuilder.end_phase id

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

let dummy_pp fmt _ =
  Fmt.pf fmt "!!! YOU SHOULDN'T BE SEEING THIS PRETTY PRINT !!!"
