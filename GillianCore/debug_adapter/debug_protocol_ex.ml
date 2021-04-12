include Debug_protocol

module Launch_command = struct
  let type_ = Launch_command.type_

  module Arguments = struct
    type t =
      { program : string
      ; stop_on_entry : bool [@default false] [@key "stopOnEntry"]
      }
    [@@deriving yojson { strict = false }]
  end

  module Result = Launch_command.Result
end
