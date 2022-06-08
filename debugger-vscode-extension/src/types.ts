export type BranchCase = {
  kind: string;
  display: [string, string];
  json: any;
};

export type CmdData = {
  id: number;
  display: string;
  hasUnify: boolean;
};

export type ExecMap =
  | ['Nothing']
  | ['Cmd', CmdData, ExecMap]
  | ['BranchCmd', CmdData, [BranchCase, ExecMap][]]
  | ['FinalCmd', CmdData];

export type AssertionData = {
  foldId: number;
};

export type UnifySeg =
  | ['Assertion', AssertionData, UnifySeg]
  | ['UnifyResult', boolean];

export type UnifyMap = ['Direct', UnifySeg] | ['Fold', UnifySeg[]];

export type DebugState = {
  execMap: ExecMap;
  currentCmdId: number;
  procName: string;
};

type StateUpdateMsg = {
  type: 'state_update';
  state: DebugState;
};

type UnifyUpdateMsg = {
  type: 'unify_update';
  unifyId: number;
  unifyMap: UnifyMap;
};

export type MessageToWebview = StateUpdateMsg | UnifyUpdateMsg;

type RequestStateUpdateMsg = {
  type: 'request_state_update';
};

type RequestJump = {
  type: 'request_jump';
  cmdId: number;
};

type RequestExecSpecific = {
  type: 'request_exec_specific';
  prevId: number;
  branchCase: BranchCase | null;
};

type RequestUnification = {
  type: 'request_unification';
  parentId: number;
};

export type MessageFromWebview =
  | RequestStateUpdateMsg
  | RequestJump
  | RequestExecSpecific
  | RequestUnification;
