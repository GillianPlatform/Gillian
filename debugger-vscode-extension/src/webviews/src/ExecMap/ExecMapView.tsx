import React from 'react';
import { BranchCase, DebugState, ExecMap } from '../../../types';
import TreeMapView, {
  TransformResult,
  TransformFunc,
} from '../TreeMapView/TreeMapView';
import { execSpecific, jumpToId, startProc } from '../VSCodeAPI';
import ExecMapNode, { ExecMapNodeData } from './ExecMapNode';
import {
  NODE_WIDTH,
  NODE_HEIGHT,
  DEFAULT_NODE_SIZE,
} from '../TreeMapView/TreeMapView';

export type Props = {
  state: DebugState;
};

type M = ExecMap;
type D = ExecMapNodeData;
type A = {
  procName: string;
  branchCase?: BranchCase | null;
  hasParent?: boolean;
};

const ExecMapView = ({ state }: Props) => {
  const { mainProc: mainProcName, currentProc: currentProcName, procs } = state;
  const mainProc = procs[mainProcName];
  const { execMap, liftedExecMap } = mainProc;
  const usedExecMap = liftedExecMap ?? execMap;

  const initElem: TransformResult<M, D, A> = {
    id: 'root',
    data: {
      type: 'Root',
      procName: mainProcName,
    },
    nexts: [[{ procName: mainProcName }, usedExecMap]],
    width: NODE_WIDTH,
    height: NODE_HEIGHT,
  };

  const cleanNexts = (
    nexts: [BranchCase | null, [null, M]][],
    aux: A
  ): [A, M][] => {
    return nexts.map(next => {
      return [{ ...aux, branchCase: next[0] }, next[1][1]];
    });
  };

  let emptyCount = 0;
  const transform: TransformFunc<M, D, A> = (map, parent, aux) => {
    const { procName, branchCase = null, hasParent = true } = aux;
    const procState = procs[procName];
    const edgeLabel = branchCase ? <>{branchCase.display[1]}</> : undefined;

    if (map[0] == 'Nothing') {
      if (parent === undefined) {
        throw '\'Nothing\' node has no parent!';
      }
      return {
        id: `empty${emptyCount++}`,
        data: {
          type: 'Empty',
          hasParent: true,
          exec: () => {
            execSpecific(procName, +parent, branchCase as BranchCase);
          },
        },
        nexts: [],
        edgeLabel,
        width: NODE_HEIGHT,
        height: NODE_HEIGHT,
      };
    }

    const [, { data: cmdData }] = map;
    const nexts = (() => {
      if (map[0] === 'Cmd') {
        return [[{ procName }, map[1].next] as [A, ExecMap]];
      } else if (map[0] === 'BranchCmd') {
        return cleanNexts(map[1].nexts, aux);
      } else {
        return [];
      }
    })();

    const submap = (() => {
      if (cmdData.submap[0] === 'NoSubmap') {
        return undefined;
      } else if (cmdData.submap[0] === 'Submap') {
        return transform(cmdData.submap[1], undefined, {
          procName,
          hasParent: false,
        });
      } else if (cmdData.submap[0] === 'Proc') {
        const submapProcName = cmdData.submap[1];
        const proc = procs[submapProcName];
        if (proc === undefined) {
          const result: TransformResult<M, D, A> = {
            id: `empty-${submapProcName}`,
            data: {
              type: 'Empty',
              hasParent: false,
              exec: () => {
                startProc(submapProcName);
              },
            },
            nexts: [],
            edgeLabel: undefined,
            width: NODE_HEIGHT,
            height: NODE_HEIGHT,
          };
          return result;
        } else {
          return transform(proc.liftedExecMap || proc.execMap, undefined, {
            procName: submapProcName,
            hasParent: false,
          });
        }
      }
    })();

    const isCurrentCmd =
      procName === currentProcName &&
      cmdData.ids.includes(procState.currentCmdId);

    return {
      id: `${cmdData.ids[0]}`,
      data: {
        type: 'Cmd',
        cmdData,
        isCurrentCmd,
        isFinal: map[0] === 'FinalCmd',
        hasParent,
        jump: () => {
          jumpToId(procName, cmdData.ids[0]);
        },
      },
      nexts,
      edgeLabel,
      ...DEFAULT_NODE_SIZE,
      submap,
    };
  };

  return (
    <TreeMapView {...{ initElem, transform, nodeComponent: ExecMapNode }} />
  );
};

export default ExecMapView;
