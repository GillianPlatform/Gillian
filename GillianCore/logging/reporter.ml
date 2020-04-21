type t = { log : 'a. 'a Report.t -> unit; wrap_up : unit -> unit }

let file_reporter () =
  let out_channel = open_out "out.log" in
  let formatter = Format.formatter_of_out_channel out_channel in
  let log : 'a. 'a Report.t -> unit =
   fun report ->
    report.content @@ fun fmt -> Format.fprintf formatter @@ fmt ^^ "@,@?"
  in
  let wrap_up () = close_out out_channel in
  { log; wrap_up }

let reporters = ref [ file_reporter () ]

let log report = List.iter (fun reporter -> reporter.log report) !reporters

let wrap_up () = List.iter (fun reporter -> reporter.wrap_up ()) !reporters
