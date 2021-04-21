let file_name = "./db.log"
let reset () = if Sys.file_exists file_name then Sys.remove file_name else ()

let log_specific loggable report =
  let out = open_out_gen [ Open_append; Open_creat ] 0o666 file_name in
  let yojson = Report.to_yojson (Loggable.to_yojson loggable) report in
  let yojson = Yojson.Safe.to_string yojson in
  Printf.fprintf out "%s\n" yojson;
  close_out out
