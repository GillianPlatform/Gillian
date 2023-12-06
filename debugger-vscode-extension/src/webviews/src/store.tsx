import produce, { Draft, enableMapSet } from 'immer';
import create, { State as ZState, StateCreator } from 'zustand';
import { DebuggerState, UnifyMap, State, UnifyStep } from '../../types';
import { requestUnification } from './VSCodeAPI';

enableMapSet();
const immer =
  <T extends ZState>(
    config: StateCreator<T, (fn: (draft: Draft<T>) => void) => void>
  ): StateCreator<T> =>
  (set, get, api) =>
    config(fn => set(produce<T>(fn)), get, api);

export type Store = State & {
  updateDebuggerState: (debuggerState: DebuggerState) => void;
  loadUnification: (unifyId: number, map: UnifyMap) => void;
  requestUnification: (unifyId: number) => void;
  selectBaseUnification: (unifyId: number) => boolean;
  clearUnification: () => void;
  pushUnification: (unifyId: number) => boolean;
  popUnifications: (n: number) => void;
  selectUnifyStep: (step: UnifyStep) => void;
  toggleExecNodeExpanded: (id: string) => void;
  toggleUnifyNodeExpanded: (id: number) => void;
  clear: () => void;
};

const useStore = create<Store>(
  immer((set, get) => {
    const isUnifyInStore = (unifyId: number) =>
      get().unifyState.unifications[unifyId] !== undefined;

    return {
      unifyState: {
        path: [],
        unifications: {},
        expandedNodes: new Set(),
      },
      expandedExecNodes: new Set(),
      updateDebuggerState: debuggerState => {
        set(() => ({ debuggerState }));
      },
      loadUnification: (unifyId, map) => {
        set(({ unifyState }) => {
          unifyState.unifications[unifyId] = { id: unifyId, map };
        });
      },
      requestUnification: unifyId => {
        requestUnification(unifyId);
        set(({ unifyState }) => {
          unifyState.unifications[unifyId] = undefined;
        });
      },
      selectBaseUnification: unifyId => {
        set(({ unifyState }) => {
          unifyState.path = [unifyId];
          unifyState.expandedNodes.clear();
        });
        return isUnifyInStore(unifyId);
      },
      clearUnification: () => {
        set(({ unifyState }) => {
          unifyState.path = [];
          unifyState.expandedNodes.clear();
        });
      },
      pushUnification: (unifyId: number) => {
        set(({ unifyState }) => {
          unifyState.path.unshift(unifyId);
        });
        return isUnifyInStore(unifyId);
      },
      popUnifications: (n: number) => {
        set(({ unifyState }) => {
          while (n--) {
            unifyState.path.shift();
          }
        });
      },
      selectUnifyStep: step => {
        set(({ unifyState: { path, unifications } }) => {
          const unifyId = path[0];
          const unification = unifications[unifyId];
          if (unification !== undefined) {
            unification.selected = step as any;
          }
        });
      },
      toggleExecNodeExpanded: (id: string) => {
        set(({ expandedExecNodes }) => {
          if (expandedExecNodes.has(id)) expandedExecNodes.delete(id);
          else expandedExecNodes.add(id);
        });
      },
      toggleUnifyNodeExpanded: (id: number) => {
        set(({ unifyState: { expandedNodes } }) => {
          if (expandedNodes.has(id)) expandedNodes.delete(id);
          else expandedNodes.add(id);
        });
      },
      clear: () => {
        set((state) => {
          state.debuggerState = undefined;
          state.unifyState.path = [];
          state.unifyState.unifications = {};
          state.unifyState.expandedNodes.clear();
          state.expandedExecNodes.clear();
        })
      },
    };
  })
);

export const mutateStore = () =>
  useStore(
    ({
      updateDebuggerState,
      loadUnification,
      requestUnification,
      selectBaseUnification,
      clearUnification,
      pushUnification,
      popUnifications,
      selectUnifyStep,
      toggleExecNodeExpanded,
      toggleUnifyNodeExpanded,
    }) => ({
      updateDebuggerState,
      loadUnification,
      requestUnification,
      selectBaseUnification,
      clearUnification,
      pushUnification,
      popUnifications,
      selectUnifyStep,
      toggleExecNodeExpanded,
      toggleUnifyNodeExpanded,
    })
  );

export default useStore;
