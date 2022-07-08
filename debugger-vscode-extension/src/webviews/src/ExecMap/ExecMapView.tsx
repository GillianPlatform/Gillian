import React from 'react';
import { BranchCase, DebugState, ExecMap } from '../../../types';
import TreeMapView, {
  TransformResult,
  TransformFunc,
} from '../TreeMapView/TreeMapView';
import { execSpecific, jumpToId } from '../VSCodeAPI';
import ExecMapNode, { ExecMapNodeData } from './ExecMapNode';

export type Props = {
  state: DebugState;
};

type M = ExecMap;
type D = ExecMapNodeData;
type A = BranchCase | null;

const ExecMapView = ({ state }: Props) => {
  const { execMap, currentCmdId, procName, liftedExecMap } = state;
  const usedExecMap = liftedExecMap ?? execMap;

  const initElem: TransformResult<M, D, A> = {
    id: 'root',
    data: {
      type: 'Root',
      procName,
    },
    nexts: [[null, usedExecMap]],
  };

  let emptyCount = 0;
  const transform: TransformFunc<M, D, A> = (map, parent, branchCase) => {
    const edgeLabel = branchCase ? <>{branchCase.display[1]}</> : undefined;

    if (map[0] == 'Nothing') {
      return {
        id: `empty${emptyCount++}`,
        data: {
          type: 'Empty',
          exec: () => {
            execSpecific(+parent, branchCase as BranchCase);
          },
        },
        nexts: [],
        edgeLabel,
      };
    }

    const [, cmdData] = map;
    const nexts = (() => {
      if (map[0] === 'Cmd') {
        return [[null, map[2]] as [A, ExecMap]];
      } else if (map[0] === 'BranchCmd') {
        return map[2];
      } else {
        return [];
      }
    })();
    return {
      id: `${cmdData.id}`,
      data: {
        type: 'Cmd',
        cmdData,
        isCurrentCmd: cmdData.id === currentCmdId,
        isFinal: map[0] === 'FinalCmd',
        jump: () => {
          jumpToId(cmdData.id);
        },
      },
      nexts,
      edgeLabel,
    };
  };

  return (
    <TreeMapView {...{ initElem, transform, nodeComponent: ExecMapNode }} />
  );
};

export default ExecMapView;
