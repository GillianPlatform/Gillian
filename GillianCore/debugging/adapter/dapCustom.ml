open DebugProtocolEx

module Events = struct
  module Debug_state_update_event (Debugger : Debugger.S) = struct
    let type_ = "debugStateUpdate"

    module Payload = struct
      type t = Debugger.Inspect.debug_state [@@deriving yojson]
    end
  end
end

module Commands = struct
  module Debugger_state_command (Debugger : Debugger.S) = struct
    let type_ = "debuggerState"

    module Arguments = struct
      type t = Empty_dict.t [@@deriving yojson]
    end

    module Result = struct
      type t = Debugger.Inspect.debug_state [@@deriving yojson]
    end
  end

  module Jump_command = struct
    let type_ = "jump"

    module Arguments = struct
      type t = { id : Logging.ReportId.t } [@@deriving yojson]
    end

    module Result = struct
      type t = { success : bool; err : string option [@default None] }
      [@@deriving make, yojson]
    end
  end

  module Step_specific_command (Debugger : Debugger.S) = struct
    let type_ = "stepSpecific"

    module Arguments = struct
      type t = {
        prev_id : Logging.ReportId.t; [@key "prevId"]
        branch_case : Debugger.PackagedBranchCase.t option; [@key "branchCase"]
      }
      [@@deriving yojson]
    end

    module Result = struct
      type t = { success : bool; err : string option [@default None] }
      [@@deriving make, yojson]
    end
  end
end
