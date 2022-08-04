type branch_case_pkg = {
  kind : string;
  display : string * string;
  json : string;
}
[@@genType]

type unify_kind =
  | Postcondition
  | Fold
  | FunctionCall
  | Invariant
  | LogicCommand
[@@genType]

module UnifyMap = struct
  type unify_result = Success | Failure
  [@@genType]

  type 'id assertion_data = {
    id : 'id;
    fold : ('id * unify_result) option;
    assertion : string;
    substitutions : (string * string) list;
  }
  [@@genType]

  type 'id unify_seg =
      | Assertion of 'id assertion_data * 'id unify_seg
      | UnifyResult of 'id * unify_result
  [@@genType]

  type 'id map = Direct of 'id unify_seg | Fold of 'id unify_seg list
  [@@genType]

  type 'id t = unify_kind * 'id map
  [@@genType]
end

module ExecMap = struct
  type 'id unifys = ('id * unify_kind * UnifyMap.unify_result) list
  [@@genType]

  type 'id cmd_data = {
    id : 'id;
    origin_id : int option;
    display : string;
    unifys : 'id unifys;
    errors : string list;
  }
  [@@genType]

  type ('id, 'case) t =
    | Nothing
    | Cmd of 'id cmd_data * ('id, 'case) t
    | BranchCmd of 'id cmd_data * ('case * ('id, 'case) t) list
    | FinalCmd of 'id cmd_data
  [@@genType]
end

type 'id debug_state = {
  exec_map : ('id, branch_case_pkg) ExecMap.t;
  lifted_exec_map : ('id, branch_case_pkg) ExecMap.t option;
  current_cmd_id : 'id;
  unifys : 'id ExecMap.unifys;
  proc_name : string;
}
[@@genType]
