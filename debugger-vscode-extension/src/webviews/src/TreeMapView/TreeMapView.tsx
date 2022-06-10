import React from 'react';
import ReactFlow, {
  Background,
  Edge,
  Controls,
  Node,
  NodeProps,
} from 'react-flow-renderer';

import './TreeMapView.css';

export type TransformResult<M, D, A> = {
  id: string | number;
  data: D;
  nexts: [A, M][];
  edgeLabel?: React.ReactNode;
};

export type TransformFunc<M, D, A> = (
  map: M,
  parent: string,
  aux: A
) => TransformResult<M, D, A>;

export type Props<M, D, A> = {
  initElem: TransformResult<M, D, A>;
  transform: TransformFunc<M, D, A>;
  nodeComponent: React.ComponentType<NodeProps<D>>;
};

type IntermediateElem<D> = {
  id: string | number;
  depth: number;
  nthInDepth: number;
  data: D;
};

export const NODE_WIDTH = 150;
export const NODE_HEIGHT = 50;
const NODE_GAP_X = 50;
const NODE_GAP_Y = 50;

const TreeMapView = <M, D, A>({
  initElem,
  transform,
  nodeComponent,
}: Props<M, D, A>) => {
  const nodeTypes = { customNode: nodeComponent };
  const depthCounts: number[] = [1];
  const intermediateElems: IntermediateElem<D>[] = [
    {
      id: initElem.id,
      data: initElem.data,
      depth: 0,
      nthInDepth: 0,
    },
  ];

  const edges: Edge[] = [];
  const buildElems = (map: M, aux: A, parent: string, depth: number) => {
    const { id, data, nexts, edgeLabel } = transform(map, parent, aux);

    if (!depthCounts[depth]) {
      depthCounts[depth] = 0;
    }

    const nthInDepth = depthCounts[depth]++;
    intermediateElems.push({ id, data, depth, nthInDepth });

    edges.push({
      id: `edge-${parent}-${id}`,
      source: parent,
      target: `${id}`,
      ...(edgeLabel ? { label: edgeLabel } : {}),
    });

    for (const [aux, nextMap] of nexts) {
      buildElems(nextMap, aux, `${id}`, depth + 1);
    }
  };

  for (const [aux, nextMap] of initElem.nexts) {
    buildElems(nextMap, aux, `${initElem.id}`, 1);
  }

  const maxWidth = Math.max(...depthCounts);
  const nodes = intermediateElems.map(
    ({ id, data, depth, nthInDepth }): Node<D> => {
      const x =
        (nthInDepth + (maxWidth - (depthCounts[depth] || 0)) / 2 + 0.5) *
          (NODE_WIDTH + NODE_GAP_X) -
        NODE_WIDTH / 2;
      const y = depth * (NODE_HEIGHT + NODE_GAP_Y) + NODE_GAP_Y;
      return {
        id: `${id}`,
        position: { x, y },
        type: 'customNode',
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
    <div className="tree-map-view">
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

  console.log({ nodes, edges, initElem, ret });

  return ret;
};

export default TreeMapView;
