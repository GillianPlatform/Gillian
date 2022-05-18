
module Log_event = struct
  let type_ = "log"

  module Payload = struct
    type t = {
      msg : string;
      json : string option;
    }
    [@@deriving make, yojson {strict = false}]
  end
end

let log_to_rpc msg ?json rpc =
  Debug_rpc.send_event rpc
    (module Log_event)
    { msg; json }
    (* Log_event.Payload.(make ~msg ?json ()) *)
