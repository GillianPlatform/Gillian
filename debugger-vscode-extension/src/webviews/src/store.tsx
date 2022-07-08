import produce, { Draft, enableMapSet } from 'immer';
import create, { State as ZState, StateCreator } from 'zustand';
import { DebugState, UnifyMap, State, UnifyStep, UnifyKind } from '../../types';

enableMapSet();
const immer =
  <T extends ZState>(
    config: StateCreator<T, (fn: (draft: Draft<T>) => void) => void>
  ): StateCreator<T> =>
  (set, get, api) =>
    config(fn => set(produce<T>(fn)), get, api);

export type Store = State & {
  updateDebugState: (debugState: DebugState) => void;
  loadUnification: (unifyId: number, map: UnifyMap) => void;
  selectBaseUnification: (unifyId: number) => boolean;
  clearUnification: () => void;
  pushUnification: (unifyId: number) => boolean;
  popUnifications: (n: number) => void;
  selectUnifyStep: (step: UnifyStep) => void;
};

const useStore = create<Store>(
  immer((set, get) => {
    const isUnifyInStore = (unifyId: number) =>
      get().unifyState.unifications[unifyId] !== undefined;

    return {
      unifyState: {
        path: [],
        unifications: {},
      },
      updateDebugState: debugState => {
        set(() => ({ debugState }));
      },
      loadUnification: (unifyId, map) => {
        set(({ unifyState }) => {
          unifyState.unifications[unifyId] = { map };
        });
      },
      selectBaseUnification: unifyId => {
        set(({ unifyState }) => {
          unifyState.path = [unifyId];
        });
        return isUnifyInStore(unifyId);
      },
      clearUnification: () => {
        set(({ unifyState }) => {
          unifyState.path = [];
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
        console.log('selecting step!');
        set(({ unifyState: { path, unifications } }) => {
          const unifyId = path[0];
          const unification = unifications[unifyId];
          if (unification !== undefined) {
            unification.selected = step as any;
          }
        });
      },
    };
  })
);

export const mutateStore = () =>
  useStore(
    ({
      updateDebugState,
      loadUnification,
      selectBaseUnification,
      clearUnification,
      pushUnification,
      popUnifications,
      selectUnifyStep,
    }) => ({
      updateDebugState,
      loadUnification,
      selectBaseUnification,
      clearUnification,
      pushUnification,
      popUnifications,
      selectUnifyStep,
    })
  );

export default useStore;
