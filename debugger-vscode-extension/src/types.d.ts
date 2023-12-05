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

type Unification = {
  readonly id: number;
  readonly kind: UnifyKind;
  readonly result: UnifyResult;
};

export type CmdData = {
  readonly ids: readonly number[];
  readonly display: string;
  readonly unifys: readonly Unification[];
  readonly errors: readonly string[];
  readonly submap: Submap;
};

export type ExecMap =
  | readonly ['Nothing']
  | readonly ['Cmd', { data: CmdData; next: ExecMap }]
  | readonly [
      'BranchCmd',
      { data: CmdData; nexts: [BranchCase, [null, ExecMap]][] }
    ]
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

export type UnifyMapInner =
  | readonly ['Direct', UnifySeg]
  | readonly ['Fold', UnifySeg[]];

export type UnifyMap = readonly [UnifyKind, UnifyMapInner];

// #endregion

export type DebugProcState = {
  readonly execMap: ExecMap;
  readonly liftedExecMap: ExecMap | null;
  readonly currentCmdId: number;
  readonly unifys: readonly Unification[];
  readonly procName: string;
};

export type DebuggerState = {
  readonly mainProc: string;
  readonly currentProc: string;
  readonly procs: Record<string, DebugProcState>;
};

export type UnifyStep =
  | readonly ['Assertion', AssertionData]
  | readonly ['Result', number, UnifyResult];

export type UnificationState = {
  readonly id: number;
  readonly map: unknown; // TODO: fix when Immer supports recursive types
  readonly selected?: UnifyStep;
};

export type UnifyState = {
  readonly path: number[];
  readonly unifications: Record<number, UnificationState | undefined>;
  readonly expandedNodes: Set<number>;
};

export type State = {
  readonly debuggerState?: DebuggerState;
  readonly unifyState: UnifyState;
  readonly expandedExecNodes: Set<string>;
};

// #region MessageToWebview

type StateUpdateMsg = {
  readonly type: 'state_update';
  readonly state: DebuggerState;
};

type UnifyUpdateMsg = {
  readonly type: 'unify_update';
  readonly unifyId: number;
  readonly unifyMap: UnifyMap;
};

type ResetViewMsg = {
  readonly type: 'reset_view';
};

type ClearStateMsg = {
  readonly type: 'clear_state';
}

export type MessageToWebview = StateUpdateMsg | UnifyUpdateMsg | ResetViewMsg | ClearStateMsg;

// #endregion

// #region MessageFromWebview

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

type RequestStartProc = {
  readonly type: 'request_start_proc';
  readonly procName: string;
};

export type MessageFromWebview =
  | RequestStateUpdateMsg
  | RequestJump
  | RequestExecSpecific
  | RequestUnification
  | RequestStartProc;

// #endregion

export type ExecMode = 'debugverify' | 'debugwpst';
