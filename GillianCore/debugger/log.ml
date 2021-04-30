let file_name = "./debugger-temp.log"

let info line =
  let out = open_out_gen [ Open_append; Open_creat ] 0o666 file_name in
  Printf.fprintf out "%s\n" line;
  close_out out

let reset () = if Sys.file_exists file_name then Sys.remove file_name else ()
