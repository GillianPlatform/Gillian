module Mode = Mode
module Report = Report
module Reporter = Reporter
module FileReporter = FileReporter
module DatabaseReporter = DatabaseReporter
module Loggable = Loggable

let () =
  Printexc.register_printer (function
    | Failure s ->
        Some (Format.asprintf "!!!!!!!!!!\nFAILURE:\n%s\n!!!!!!!!!!\n\n" s)
    | _         -> None)

let wrap_up = Default.wrap_up

let log lvl ?title ?severity msgf =
  if Mode.should_log lvl then
    let s =
      let str = ref "" in
      (fun fmt -> Format.kasprintf (fun s -> str := s) fmt) |> msgf;
      !str
    in
    let report =
      ReportBuilder.make ?title ~content:(Loggable.make_string s)
        ~type_:Report.Debug ?severity ()
    in
    Default.log report

let log_specific lvl ?title ?severity loggable type_ =
  if Mode.should_log lvl then
    let report =
      ReportBuilder.make ?title ~content:loggable ~type_ ?severity ()
    in
    Default.log report

let normal ?title ?severity msgf = log Normal ?title ?severity msgf

let verbose ?title ?severity msgf = log Verbose ?title ?severity msgf

let tmi ?title ?severity msgf = log TMI ?title ?severity msgf

let print_to_all (str : string) =
  normal (fun m -> m "%s" str);
  print_endline str

(* Failure *)
let fail msg =
  normal ~severity:Error (fun m -> m "%a" Format.pp_print_string msg);
  raise (Failure msg)

let normal_phase = ReportBuilder.start_phase Normal

let verbose_phase = ReportBuilder.start_phase Verbose

let tmi_phase = ReportBuilder.start_phase TMI

let end_phase = ReportBuilder.end_phase

let with_phase level ?title ?severity f =
  let phase = ReportBuilder.start_phase level ?title ?severity () in
  let result =
    try Ok (f ())
    with e ->
      Printf.printf "Original Backtrace:\n%s" (Printexc.get_backtrace ());
      Error e
  in
  ReportBuilder.end_phase phase;
  match result with
  | Ok ok   -> ok
  | Error e -> raise e

let with_normal_phase ?title ?severity f = with_phase Normal ?title ?severity f

let with_verbose_phase ?title ?severity f =
  with_phase Verbose ?title ?severity f

let with_tmi_phase ?title ?severity f = with_phase TMI ?title ?severity f
