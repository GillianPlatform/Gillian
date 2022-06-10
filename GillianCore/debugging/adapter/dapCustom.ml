open DebugProtocolEx
open Debugger.DebuggerTypes
module L = Logging
module DL = Debugger_log

module Events (Debugger : Debugger.S) = struct
  module Debug_state_update_event = struct
    let type_ = "debugStateUpdate"

    module Payload = struct
      type t = Debugger.Inspect.debug_state [@@deriving yojson]
    end
  end

  let send_stopped_events dbg rpc stop_reason =
    (match stop_reason with
    | Step | ReachedStart | ReachedEnd ->
        DL.log (fun m ->
            m
              ~json:[ ("reason", stop_reason_to_yojson stop_reason) ]
              "Stopped: Step/ReachedStart/ReachedEnd");
        (* Send step stopped event after reaching the end to allow for stepping
           backwards *)
        Debug_rpc.send_event rpc
          (module Stopped_event)
          Stopped_event.Payload.(
            make ~reason:Stopped_event.Payload.Reason.Step ~thread_id:(Some 0)
              ())
    | Breakpoint ->
        DL.log (fun m -> m "Stopped: Breakpoint");
        Debug_rpc.send_event rpc
          (module Stopped_event)
          Stopped_event.Payload.(
            make ~reason:Stopped_event.Payload.Reason.Breakpoint
              ~thread_id:(Some 0) ())
    | ExecutionError ->
        DL.log (fun m -> m "Stopped: ExecutionError");
        Debug_rpc.send_event rpc
          (module Stopped_event)
          Stopped_event.Payload.(
            make ~reason:Stopped_event.Payload.Reason.Exception
              ~thread_id:(Some 0) ()));%lwt
    Debug_rpc.send_event rpc
      (module Debug_state_update_event)
      (Debugger.Inspect.get_debug_state dbg)
end

module Commands (Debugger : Debugger.S) = struct
  module Debugger_state_command = struct
    let type_ = "debuggerState"

    module Arguments = struct
      type t = Empty_dict.t [@@deriving yojson]
    end

    module Result = struct
      type t = Debugger.Inspect.debug_state [@@deriving yojson]
    end
  end

  module Unification_command = struct
    let type_ = "unification"

    module Arguments = struct
      type t = { id : L.ReportId.t } [@@deriving yojson]
    end

    module Result = struct
      type t = {
        unify_id : L.ReportId.t; [@key "unifyId"]
        unify_map : Debugger.UnifyMap.t; [@key "unifyMap"]
      }
      [@@deriving yojson, make]
    end
  end

  module Jump_command = struct
    let type_ = "jump"

    module Arguments = struct
      type t = { id : L.ReportId.t } [@@deriving yojson]
    end

    module Result = struct
      type t = { success : bool; err : string option [@default None] }
      [@@deriving make, yojson]
    end
  end

  module Step_specific_command = struct
    let type_ = "stepSpecific"

    module Arguments = struct
      type t = {
        prev_id : L.ReportId.t; [@key "prevId"]
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
