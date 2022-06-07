import produce, { Draft } from 'immer';
import create, { State, StateCreator } from 'zustand';
import { DebugState } from '../../types';

const immer =
  <T extends State>(
    config: StateCreator<T, (fn: (draft: Draft<T>) => void) => void>
  ): StateCreator<T> =>
  (set, get, api) =>
    config(fn => set(produce<T>(fn)), get, api);

type DebugStore = {
  state?: DebugState;
  update: (fn: (draft: Draft<DebugStore>) => void) => void;
};

const useDebugStore = create<DebugStore>(
  immer(set => ({
    state: undefined,
    update: fn => {
      set(fn);
    },
  }))
);

export default useDebugStore;
