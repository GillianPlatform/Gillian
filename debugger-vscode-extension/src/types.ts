// #region ExecMap

export type BranchCase = {
  readonly kind: string;
  readonly display: [string, string];
  readonly json: any;
};

export type CmdData = {
  readonly id: number;
  readonly originId: number | null;
  readonly display: string;
  readonly unifys: readonly (readonly [number, UnifyKind, UnifyResult])[];
  readonly errors: readonly string[];
};

export type ExecMap =
  | readonly ['Nothing']
  | readonly ['Cmd', CmdData, ExecMap]
  | readonly ['BranchCmd', CmdData, [BranchCase, ExecMap][]]
  | readonly ['FinalCmd', CmdData];

// #endregion

// #region UnifyMap

export type UnifyResult = readonly ['Success' | 'Failure'];

export type AssertionData = {
  readonly id: number;
  readonly fold: readonly [number, UnifyResult] | null;
  readonly assertion: string;
  readonly substitutions: readonly [string, string][];
};

export type UnifySeg =
  | readonly ['Assertion', AssertionData, UnifySeg]
  | readonly ['UnifyResult', number, UnifyResult];

export type UnifyKind = [
  'Postcondition' | 'Fold' | 'FunctionCall' | 'Invariant' | 'LogicCommand'
];

export type UnifyMap = readonly [
  UnifyKind,
  readonly ['Direct', UnifySeg] | readonly ['Fold', UnifySeg[]]
];

// #endregion

export type DebugState = {
  readonly execMap: ExecMap;
  readonly liftedExecMap: ExecMap | null;
  readonly currentCmdId: number;
  readonly unifys: readonly (readonly [number, UnifyKind, UnifyResult])[];
  readonly procName: string;
};

export type UnifyStep =
  | readonly ['Assertion', AssertionData]
  | readonly ['Result', number, UnifyResult];

export type Unification = {
  readonly map: unknown; // TODO: fix when Immer supports recursive types
  readonly selected?: UnifyStep;
};

export type UnifyState = {
  readonly path: number[];
  readonly unifications: Record<number, Unification | undefined>;
};

export type State = {
  readonly debugState?: DebugState;
  readonly unifyState: UnifyState;
};

// #region MessageToWebview

type StateUpdateMsg = {
  readonly type: 'state_update';
  readonly state: DebugState;
};

type UnifyUpdateMsg = {
  readonly type: 'unify_update';
  readonly unifyId: number;
  readonly unifyMap: UnifyMap;
};

export type MessageToWebview = StateUpdateMsg | UnifyUpdateMsg;

// #endregion

// #region MessageFromWebiew

type RequestStateUpdateMsg = {
  readonly type: 'request_state_update';
};

type RequestJump = {
  readonly type: 'request_jump';
  readonly cmdId: number;
};

type RequestExecSpecific = {
  readonly type: 'request_exec_specific';
  readonly prevId: number;
  readonly branchCase: BranchCase | null;
};

type RequestUnification = {
  readonly type: 'request_unification';
  readonly id: number;
};

export type MessageFromWebview =
  | RequestStateUpdateMsg
  | RequestJump
  | RequestExecSpecific
  | RequestUnification;

// #endregion
