// #region ExecMap

export type BranchCase = {
  readonly kind: string;
  readonly display: [string, string];
  readonly json: any;
};

type Submap =
  | readonly ['NoSubmap']
  | readonly ['Submap', ExecMap]
  | readonly ['Proc', string];

export type CmdData = {
  readonly id: number;
  readonly display: string;
  readonly unifys: readonly (readonly [number, UnifyKind, UnifyResult])[];
  readonly errors: readonly string[];
  readonly submap: Submap;
};

export type ExecMap =
  | readonly ['Nothing']
  | readonly ['Cmd', { data: CmdData, next: ExecMap }]
  | readonly ['BranchCmd', { data: CmdData, nexts: [BranchCase, ExecMap][] }]
  | readonly ['FinalCmd', { data: CmdData }];

// #endregion

// #region UnifyMap

export type UnifyResult = readonly ['Success' | 'Failure'];

export type Substitution = {
  readonly assertId: number;
  readonly subst: readonly [string, string];
};

export type AssertionData = {
  readonly id: number;
  readonly fold: readonly [number, UnifyResult] | null;
  readonly assertion: string;
  readonly substitutions: readonly Substitution[];
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

export type DebugProcState = {
  readonly execMap: ExecMap;
  readonly liftedExecMap: ExecMap | null;
  readonly currentCmdId: number;
  readonly unifys: readonly (readonly [number, UnifyKind, UnifyResult])[];
  readonly procName: string;
};

export type DebugState = {
  readonly mainProc : string;
  readonly currentProc : string;
  readonly procs: Record<string, DebugProcState>;
}

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
