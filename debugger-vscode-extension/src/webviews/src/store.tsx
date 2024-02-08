import produce, { Draft, enableMapSet } from 'immer';
import create, { State as ZState, StateCreator } from 'zustand';
import { DebuggerState, MatchMap, State, MatchStep } from '../../types';
import { requestMatching } from './VSCodeAPI';

enableMapSet();
const immer =
  <T extends ZState>(
    config: StateCreator<T, (fn: (draft: Draft<T>) => void) => void>
  ): StateCreator<T> =>
  (set, get, api) =>
    config(fn => set(produce<T>(fn)), get, api);

export type Store = State & {
  updateDebuggerState: (debuggerState: DebuggerState) => void;
  loadMatching: (matchId: number, map: MatchMap) => void;
  requestMatching: (matchId: number) => void;
  selectBaseMatching: (matchId: number) => boolean;
  clearMatching: () => void;
  pushMatching: (matchId: number) => boolean;
  popMatchings: (n: number) => void;
  selectMatchStep: (step: MatchStep) => void;
  toggleExecNodeExpanded: (id: string) => void;
  toggleMatchNodeExpanded: (id: number) => void;
  clear: () => void;
};

const useStore = create<Store>(
  immer((set, get) => {
    const isMatchInStore = (matchId: number) =>
      get().matchState.matches[matchId] !== undefined;

    return {
      matchState: {
        path: [],
        matches: {},
        expandedNodes: new Set(),
      },
      expandedExecNodes: new Set(),
      updateDebuggerState: debuggerState => {
        set(() => ({ debuggerState }));
      },
      loadMatching: (matchId, map) => {
        set(({ matchState }) => {
          matchState.matches[matchId] = { id: matchId, map };
        });
      },
      requestMatching: matchId => {
        requestMatching(matchId);
        set(({ matchState }) => {
          matchState.matches[matchId] = undefined;
        });
      },
      selectBaseMatching: matchId => {
        set(({ matchState }) => {
          matchState.path = [matchId];
          matchState.expandedNodes.clear();
        });
        return isMatchInStore(matchId);
      },
      clearMatching: () => {
        set(({ matchState }) => {
          matchState.path = [];
          matchState.expandedNodes.clear();
        });
      },
      pushMatching: (matchId: number) => {
        set(({ matchState }) => {
          matchState.path.unshift(matchId);
        });
        return isMatchInStore(matchId);
      },
      popMatchings: (n: number) => {
        set(({ matchState }) => {
          while (n--) {
            matchState.path.shift();
          }
        });
      },
      selectMatchStep: step => {
        set(({ matchState: { path, matches } }) => {
          const matchId = path[0];
          const matching = matches[matchId];
          if (matching !== undefined) {
            matching.selected = step as any;
          }
        });
      },
      toggleExecNodeExpanded: (id: string) => {
        set(({ expandedExecNodes }) => {
          if (expandedExecNodes.has(id)) expandedExecNodes.delete(id);
          else expandedExecNodes.add(id);
        });
      },
      toggleMatchNodeExpanded: (id: number) => {
        set(({ matchState: { expandedNodes } }) => {
          if (expandedNodes.has(id)) expandedNodes.delete(id);
          else expandedNodes.add(id);
        });
      },
      clear: () => {
        set(state => {
          state.debuggerState = undefined;
          state.matchState.path = [];
          state.matchState.matches = {};
          state.matchState.expandedNodes.clear();
          state.expandedExecNodes.clear();
        });
      },
    };
  })
);

export const mutateStore = () =>
  useStore(
    ({
      updateDebuggerState,
      loadMatching,
      requestMatching,
      selectBaseMatching,
      clearMatching,
      pushMatching,
      popMatchings,
      selectMatchStep,
      toggleExecNodeExpanded,
      toggleMatchNodeExpanded,
    }) => ({
      updateDebuggerState,
      loadMatching,
      requestMatching,
      selectBaseMatching,
      clearMatching,
      pushMatching,
      popMatchings,
      selectMatchStep,
      toggleExecNodeExpanded,
      toggleMatchNodeExpanded,
    })
  );

export default useStore;
