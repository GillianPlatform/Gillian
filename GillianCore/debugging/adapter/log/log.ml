let file_name = "./gillian-debugger.log"

let info line =
  let out = open_out_gen [ Open_append; Open_creat ] 0o666 file_name in
  Printf.fprintf out "%s\n" line;
  close_out out

let reset () = if Sys.file_exists file_name then Sys.remove file_name else ()

module Log_event = struct
  let type_ = "log"

  module Payload = struct
    type t = { msg : string; json : string option }
    [@@deriving yojson { strict = false }]
  end
end

let to_rpc rpc ?json msg =
  info
    (match json with
    | Some _ -> msg ^ " (+)"
    | None -> msg);
  Debug_rpc.send_event rpc (module Log_event) { msg; json } |> ignore
