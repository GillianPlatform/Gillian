type branch_case_pkg = [%import: CommonTypes.branch_case_pkg]
[@@deriving yojson]

type unify_kind = [%import: CommonTypes.unify_kind]
[@@deriving yojson]

module UnifyMap = struct
  type unify_result = [%import: CommonTypes.UnifyMap.unify_result]
  [@@deriving yojson]

  type 'id assertion_data = [%import: 'id CommonTypes.UnifyMap.assertion_data]
  [@@deriving yojson]

  type 'id unify_seg = [%import: 'id CommonTypes.UnifyMap.unify_seg]
  [@@deriving yojson]

  type 'id map = [%import: 'id CommonTypes.UnifyMap.map]
  [@@deriving yojson]

  type 'id t = [%import: 'id CommonTypes.UnifyMap.t]
  [@@deriving yojson]
  
end

module ExecMap = struct
  type 'id unifys = [%import: 'id CommonTypes.ExecMap.unifys]
  [@@deriving yojson]

  type 'id cmd_data = [%import: 'id CommonTypes.ExecMap.cmd_data]
  [@@deriving yojson]

  type ('id, 'case) t = [%import: ('id, 'case) CommonTypes.ExecMap.t]
  [@@deriving yojson]
end

type 'id debug_state = [%import: 'id CommonTypes.debug_state]
[@@deriving yojson]
