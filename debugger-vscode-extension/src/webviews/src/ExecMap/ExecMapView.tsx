import React from 'react';
import ReactFlow, {
  Background,
  Edge,
  Controls,
  Node,
} from 'react-flow-renderer';
import { BranchCase, CmdData, DebugState, ExecMap } from '../../../types';
import ExecMapNode, { ExecMapNodeData } from './ExecMapNode';

import './ExecMapView.css';

export type Props = {
  state: DebugState;
};

type IntermediateElem = {
  id: string;
  depth: number;
  nthInDepth: number;
  data: ExecMapNodeData;
};

export const NODE_WIDTH = 150;
export const NODE_HEIGHT = 50;
const NODE_GAP_X = 50;
const NODE_GAP_Y = 50;

const ExecMapView = ({ state }: Props) => {
  const { execMap, currentCmdId, procName } = state;
  const nodeTypes = { cmdNode: ExecMapNode };

  let emptyCount = 0;
  const dataFromMap = (map: ExecMap): [string, CmdData | null, boolean] => {
    if (map[0] == 'Nothing') {
      return [`empty${emptyCount++}`, null, false];
    }
    const { id, display } = map[1];
    const hasNext = map[0] != 'FinalCmd';
    return [`${id}`, { id, display }, hasNext];
  };

  const depthCounts: number[] = [1];
  const intermediateElems: IntermediateElem[] = [
    {
      id: 'root',
      depth: 0,
      nthInDepth: 0,
      data: { type: 'Root', procName },
    },
  ];
  const edges: Edge[] = [];
  const mapToElems = (
    map: ExecMap,
    depth: number,
    parent: string,
    branchCase: BranchCase | null
  ) => {
    const [id, cmdData, hasNext] = dataFromMap(map);
    if (!depthCounts[depth]) {
      depthCounts[depth] = 0;
    }

    const nthInDepth = depthCounts[depth]++;
    const data: ExecMapNodeData = (() => {
      if (cmdData === null) {
        return {
          type: 'Empty',
          prevId: +parent,
          branchCase: branchCase,
        };
      }
      return {
        type: 'Cmd',
        cmdData,
        isFinal: !hasNext,
        isCurrentCmd: cmdData.id === currentCmdId,
      };
    })();
    intermediateElems.push({ id, data, depth, nthInDepth });

    edges.push({
      id: `edge-${parent}-${id}`,
      source: parent,
      target: id,
      ...(branchCase !== null ? { label: branchCase.display[1] } : {}),
    });

    if (map[0] == 'Cmd') {
      mapToElems(map[1].next, depth + 1, id, null);
    } else if (map[0] == 'BranchCmd') {
      for (const [branchCase, next] of map[1].nexts) {
        mapToElems(next, depth + 1, id, branchCase);
      }
    }
  };
  mapToElems(execMap, 1, 'root', null);

  const maxWidth = Math.max(...depthCounts);
  const nodes = intermediateElems.map(
    ({ id, data, depth, nthInDepth }): Node<ExecMapNodeData> => {
      const x =
        (nthInDepth + (maxWidth - (depthCounts[depth] || 0)) / 2 + 0.5) *
          (NODE_WIDTH + NODE_GAP_X) -
        NODE_WIDTH / 2;
      const y = depth * (NODE_HEIGHT + NODE_GAP_Y) + NODE_GAP_Y;
      return {
        id,
        position: { x, y },
        type: 'cmdNode',
        data,
        style: {
          width: `${NODE_WIDTH}px`,
          height: `${NODE_HEIGHT}px`,
          display: 'flex',
          justifyContent: 'center',
        },
        draggable: false,
        connectable: false,
      };
    }
  );

  const ret = (
    <div className="exec-map-view">
      <ReactFlow
        nodes={nodes}
        edges={edges}
        nodesDraggable={false}
        nodesConnectable={false}
        nodeTypes={nodeTypes}
      >
        <Controls showInteractive={false} />
        <Background color="#aaa" gap={16} />
      </ReactFlow>
    </div>
  );

  console.log({ nodes, edges, execMap, ret });

  return ret;
};

export default ExecMapView;
