export type BranchCase = {
  kind: string;
  display: [string, string];
  json: any;
};

type BranchPath = BranchCase[];

export type CmdData = {
  id: number;
  display: string;
};

export type ExecMap =
  | ['Nothing']
  | ['Cmd', CmdData & { next: ExecMap }]
  | ['BranchCmd', CmdData & { nexts: [BranchCase, ExecMap][] }]
  | ['FinalCmd', CmdData];

export type DebugState = {
  execMap: ExecMap;
  currentCmdId: number;
  procName: string;
};

type StateUpdateMsg = {
  type: 'state_update';
  state: DebugState;
};

export type MessageToWebview = StateUpdateMsg;

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

export type MessageFromWebview =
  | RequestStateUpdateMsg
  | RequestJump
  | RequestExecSpecific;
