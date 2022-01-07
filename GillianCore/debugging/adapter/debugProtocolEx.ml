(** Extension of the Debug Protocol *)

include Debug_protocol

(** Extension of the Launch command to include custom arguments *)
module Launch_command = struct
  let type_ = Launch_command.type_

  module Arguments = struct
    type t = {
      program : string;
      stop_on_entry : bool; [@default false] [@key "stopOnEntry"]
      procedure_name : string option; [@default None] [@key "procedureName"]
    }
    [@@deriving yojson { strict = false }]
  end

  module Result = Launch_command.Result
end
