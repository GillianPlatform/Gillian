import React, { useRef } from 'react';
import ReactFlow, {
  Background,
  Edge,
  Controls,
  Node,
  NodeProps,
} from 'react-flow-renderer';

import './TreeMapView.css';

export type TransformResult<M, D, A> = {
  id: string;
  data: D;
  nexts: [A, M][];
  edgeLabel?: React.ReactNode;
  width: number;
  height: number;
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
  id: string;
  data: D;
  width: number;
  height: number;
  minX: number;
  maxX: number;
  nexts: IntermediateElem<D>[];
  childOffset: number;
};

export const NODE_WIDTH = 150;
export const NODE_HEIGHT = 50;
export const DEFAULT_NODE_SIZE = {
  width: NODE_WIDTH,
  height: NODE_HEIGHT,
};
const NODE_GAP_X = 50;
const NODE_GAP_Y = 50;

export type FlowRef = React.MutableRefObject<HTMLDivElement> | undefined;
export const FlowRefContext = React.createContext(undefined as FlowRef);

const TreeMapView = <M, D, A>({
  initElem,
  transform,
  nodeComponent,
}: Props<M, D, A>) => {
  const flowRef: FlowRef = useRef(undefined as unknown as HTMLDivElement);
  const nodeTypes = { customNode: nodeComponent };

  const edges: Edge[] = [];

  const buildElem = (tResult: TransformResult<M, D, A>, minX = 0, parent?: string) : IntermediateElem<D> => {
    const { id, data, nexts, edgeLabel, width, height } = tResult;
    let maxX = minX;
    const nextElems = nexts.map(([aux, nextMap]) => {
      const transformed = transform(nextMap, id, aux);
      const next = buildElem(transformed, maxX, id);
      maxX = next.maxX + NODE_GAP_X;
      return next;
    });
    const childOffset = Math.max(minX + width - (maxX - NODE_GAP_X), 0) / 2;
    maxX = Math.max(minX + width, maxX - NODE_GAP_X);

    if (parent !== undefined) {
      edges.push({
        id: `${parent}-${id}`,
        source: parent,
        target: id,
        ...(edgeLabel ? { label: edgeLabel } : {}),
      });
    }

    const elem = {
      id,
      data,
      width,
      height,
      minX,
      maxX,
      nexts: nextElems,
      childOffset,
    };
    return elem;
  };

  const elem = buildElem(initElem);
  const nodes: Node<D>[] = [];

  const buildNodes = ({ id, data, width, height, minX, maxX, nexts, childOffset }: IntermediateElem<D>, xOffset = 0, y = 0) => {
    const x = (minX + maxX) / 2 - width / 2 + xOffset;
    nodes.push({
      id: `${id}`,
      position: { x, y },
      type: 'customNode',
      data,
      style: {
        width: `${width}px`,
        height: `${height}px`,
        display: 'flex',
        justifyContent: 'center',
      },
      draggable: false,
      connectable: false,
      selectable: true,
    });

    nexts.forEach((next) => {
      buildNodes(next, xOffset + childOffset, y + height + NODE_GAP_Y);
    });
  };
  buildNodes(elem);

  const ret = (
    <div className="tree-map-view">
      <FlowRefContext.Provider value={flowRef}>
        <ReactFlow
          ref={flowRef}
          nodes={nodes}
          edges={edges}
          nodesDraggable={false}
          nodesConnectable={false}
          nodeTypes={nodeTypes}
        >
          <Controls showInteractive={false} />
          <Background color="#aaa" gap={16} />
        </ReactFlow>
      </FlowRefContext.Provider>
    </div>
  );

  console.log({ nodes, edges, initElem, ret });

  return ret;
};

export default TreeMapView;
