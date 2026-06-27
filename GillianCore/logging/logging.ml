module Logging_constants = Logging_constants
module Mode = Mode
module Report = Report
module Reporter = Reporter
module Loggable = Loggable
module Log_queryer = Log_queryer
module Report_id = Report_id
module Report_state = Report_builder.Report_state

let () =
  Printexc.register_printer (function
    | Failure s ->
        Some (Format.asprintf "!!!!!!!!!!\nFAILURE:\n%s\n!!!!!!!!!!\n\n" s)
    | _ -> None)

let file_reporter : Reporter.t = (module File_reporter)
let database_reporter : Reporter.t = (module Database_reporter)
let reporters = ref []

let wrap_up () =
  List.iter (fun reporter -> Reporter.wrap_up reporter) !reporters

let initialize (reporters_to_initialize : (module Reporter.S) list) =
  reporters := reporters_to_initialize;
  List.iter (fun reporter -> Reporter.initialize reporter) !reporters;
  at_exit wrap_up

let log_on_all_reporters (report : Report.t) =
  List.iter (fun reporter -> Reporter.log reporter report) !reporters

let will_log_on_any_reporter (type_ : string) =
  List.exists (fun reporter -> Reporter.will_log reporter type_) !reporters

let log lvl ?title ?severity msgf =
  let type_ = Logging_constants.Content_type.debug in
  if Mode.should_log lvl && will_log_on_any_reporter type_ then
    let report =
      Report_builder.make ?title
        ~content:
          (Loggable.make Packed_pp.pp Packed_pp.of_yojson Packed_pp.to_yojson
             (PP msgf))
        ~type_ ?severity ()
    in
    log_on_all_reporters report

let log_specific lvl ?title ?severity loggable type_ =
  if Mode.should_log lvl && will_log_on_any_reporter type_ then
    let report =
      Report_builder.make ?title ~content:loggable ~type_ ?severity ()
    in
    let () = log_on_all_reporters report in
    Some report.id
  else None

let normal ?title ?severity msgf = log Normal ?title ?severity msgf
let verbose ?title ?severity msgf = log Verbose ?title ?severity msgf
let tmi ?title ?severity msgf = log TMI ?title ?severity msgf

module Specific = struct
  let normal ?title ?severity loggable type_ =
    log_specific Normal ?title ?severity loggable type_

  let verbose ?title ?severity loggable type_ =
    log_specific Verbose ?title ?severity loggable type_

  let tmi ?title ?severity loggable type_ =
    log_specific TMI ?title ?severity loggable type_
end

let print_to_all (str : string) =
  normal (fun m -> m "%s" str);
  Fmt.pr "%s\n" str

(* Failure *)
let fail msg =
  normal ~severity:Error (fun m -> m "%a" Format.pp_print_string msg);
  raise (Failure msg)

let set_previous = Report_builder.set_previous

module Parent = struct
  let get = Report_builder.get_parent
  let set = Report_builder.set_parent
  let release = Report_builder.release_parent

  let with_id id f =
    match id with
    | None -> f ()
    | Some id ->
        let () = set id in
        Fun.protect ~finally:(fun () -> release (Some id)) f

  let with_specific ?title ?(lvl = Mode.Normal) ?severity loggable type_ f =
    let id =
      Option.bind loggable (fun loggable ->
          log_specific lvl ?title ?severity loggable type_)
    in
    with_id id (fun () -> f id)
end

let start_phase level ?title ?severity () =
  if will_log_on_any_reporter Logging_constants.Content_type.phase then
    let phase_report = Report_builder.start_phase level ?title ?severity () in
    match phase_report with
    | Some phase_report ->
        let () = log_on_all_reporters phase_report in
        Some phase_report.id
    | None -> None
  else None

module Phase = struct
  let normal = start_phase Normal
  let verbose = start_phase Verbose
  let tmi = start_phase TMI

  let stop id =
    if will_log_on_any_reporter Logging_constants.Content_type.phase then
      Report_builder.end_phase id

  let with_phase level ?title ?severity f =
    let phase = start_phase level ?title ?severity () in
    let result =
      try Ok (f ())
      with e ->
        print_to_all
          (Fmt.str "Original Backtrace:@\n%s" (Printexc.get_backtrace ()));
        Error e
    in
    Report_builder.end_phase phase;
    match result with
    | Ok ok -> ok
    | Error e -> raise e

  let with_normal ?title ?severity f = with_phase Normal ?title ?severity f
  let with_verbose ?title ?severity f = with_phase Verbose ?title ?severity f
  let with_tmi ?title ?severity f = with_phase TMI ?title ?severity f
end

let dummy_pp fmt _ =
  Fmt.pf fmt "!!! YOU SHOULDN'T BE SEEING THIS PRETTY PRINT !!!"

module Statistics = struct
  (*************************
   * Timing and Statistics *
   *************************)

  open Utils.Containers

  let active () = !Utils.Config.stats || Mode.enabled ()
  let exec_cmds = ref 0

  (* Performance statistics *)
  let statistics = Hashtbl.create 511

  (* Update the value of the fname statistic in the table, or add it if not present *)
  let update_statistics (fname : string) (time : float) =
    if not (active ()) then ()
    else if Hashtbl.mem statistics fname then
      let stat = Hashtbl.find statistics fname in
      Hashtbl.replace statistics fname (time :: stat)
    else Hashtbl.add statistics fname [ time ]

  let print_statistics () =
    print_to_all "\n STATISTICS \n ========== \n";

    let keys : SS.t =
      SS.of_list (Hashtbl.fold (fun k _ ac -> k :: ac) statistics [])
    in

    print_to_all (Printf.sprintf "Executed commands: %d" !exec_cmds);

    (* Process each item in statistics table *)
    SS.iter
      (fun f ->
        let lt = Hashtbl.find statistics f in
        (* Calculate average, min, max *)
        let min = ref infinity in
        let max = ref 0. in
        let tot = ref 0. in
        let avg = ref 0. in
        let std = ref 0. in
        let len = float_of_int (List.length lt) in
        tot :=
          List.fold_left
            (fun ac t ->
              if t < !min then min := t;
              if t > !max then max := t;
              ac +. t)
            0. lt;
        avg := !tot /. len;
        std :=
          (List.fold_left (fun ac t -> ac +. ((!avg -. t) ** 2.)) 0. lt /. len)
          ** 0.5;
        print_to_all (Printf.sprintf "\t%s" f);
        print_to_all
          (Printf.sprintf
             "Tot: %f\tCll: %d\nMin: %f\tMax: %f\nAvg: %f\tStd: %f\n" !tot
             (int_of_float len) !min !max !avg !std))
      keys
end
