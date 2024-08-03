// #region ExecMap

export type BranchCase = NonNullable<unknown>;

type Submap =
  | readonly ['NoSubmap']
  | readonly ['Submap', number]
  | readonly ['Proc', string];

type Matching = {
  readonly id: number;
  readonly kind: MatchKind;
  readonly result: MatchResult;
};

export type CmdData = {
  readonly id: readonly number;
  readonly all_ids: readonly number[];
  readonly display: string;
  readonly matches: readonly Matching[];
  readonly errors: readonly string[];
  readonly submap: Submap;
};

export type ExecMapNext =
  | ['Single', [number | null, string]]
  | ['Branch', readonly [BranchCase, [number | null, string]][]];

export type ExecMapNode = {
  readonly data: CmdData;
  readonly next: ExecMapNext | null;
};

export type ExecMapEntry =
  | readonly ['Node', ExecMapNode]
  | readonly ['Alias', number];

export type ExecMap = {
  readonly root: number | null;
  readonly entries: [number, ExecMapEntry][];
};

export type SafeExecMap = {
  readonly root: number | null;
  readonly entries: Record<number, ExecMapEntry>;
};

// #endregion

// #region MatchMap

export type MatchResult = readonly ['Success' | 'Failure'];

export type Substitution = {
  readonly assertId: number;
  readonly subst: readonly [string, string];
};

export type AssertionData = {
  readonly id: number;
  readonly fold: readonly [number, MatchResult] | null;
  readonly assertion: string;
  readonly substitutions: readonly Substitution[];
};

export type MatchSeg =
  | readonly ['Assertion', AssertionData, MatchSeg]
  | readonly ['MatchResult', number, MatchResult];

export type MatchKind = [
  'Postcondition' | 'Fold' | 'FunctionCall' | 'Invariant' | 'LogicCommand'
];

export type MatchMapInner =
  | readonly ['Direct', MatchSeg]
  | readonly ['Fold', MatchSeg[]];

export type MatchMap = readonly [MatchKind, MatchMapInner];

// #endregion

export type DebugProcState = {
  readonly execMap: ExecMap;
  readonly liftedExecMap: ExecMap | null;
  readonly currentCmdId: number;
  readonly matches: readonly Matching[];
  readonly procName: string;
};

export type DebuggerState = {
  readonly mainProc: string;
  readonly currentProc: string;
  readonly procs: Record<string, DebugProcState>;
};

export type MatchStep =
  | readonly ['Assertion', AssertionData]
  | readonly ['Result', number, MatchResult];

export type MatchingState = {
  readonly id: number;
  readonly map: unknown; // TODO: fix when Immer supports recursive types
  readonly selected?: MatchStep;
};

export type MatchState = {
  readonly path: number[];
  readonly matches: Record<number, MatchingState | undefined>;
  readonly expandedNodes: Set<number>;
};

export type State = {
  readonly debuggerState?: DebuggerState;
  readonly matchState: MatchState;
  readonly expandedExecNodes: Set<string>;
};

// #region MessageToWebview

type StateUpdateMsg = {
  readonly type: 'state_update';
  readonly state: DebuggerState;
};

type MatchUpdateMsg = {
  readonly type: 'match_update';
  readonly matchId: number;
  readonly matchMap: MatchMap;
};

type ResetViewMsg = {
  readonly type: 'reset_view';
};

type ClearStateMsg = {
  readonly type: 'clear_state';
};

export type MessageToWebview =
  | StateUpdateMsg
  | MatchUpdateMsg
  | ResetViewMsg
  | ClearStateMsg;

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

type RequestMatching = {
  readonly type: 'request_matching';
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
  | RequestMatching
  | RequestStartProc;

// #endregion

export type ExecMode = 'debugverify' | 'debugwpst';
