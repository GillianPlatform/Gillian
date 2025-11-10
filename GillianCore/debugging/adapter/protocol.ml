include Sedap_types
module DL = Debugger_log

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

type 'dbg adapter_config = {
  rpc : Debug_rpc.t;
  dbg : 'dbg;
  launch_args : Launch_command.Arguments.t;
  send_stopped_events : stop_reason -> unit Lwt.t;
}
