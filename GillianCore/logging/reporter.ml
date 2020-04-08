type t = {
  report : 'a. 'a Report.report_builder -> unit;
  wrap_up : unit -> unit;
}

let file_reporter () =
  let out_channel = open_out "out.log" in
  let formatter = Format.formatter_of_out_channel out_channel in
  let report : 'a. 'a Report.report_builder -> unit =
   fun report_builder ->
    let logf msgf =
      msgf @@ fun fmt -> Format.fprintf formatter @@ fmt ^^ "@,@?"
    in
    let to_report = report_builder () in
    logf to_report.content
  in
  let wrap_up () = close_out out_channel in
  { report; wrap_up }
