import React, { useEffect, useRef } from 'react';
import ReactFlow, {
  Background,
  Edge,
  Controls,
  Node,
  NodeProps,
  useReactFlow,
  ReactFlowProvider,
} from 'react-flow-renderer';

import * as events from '../events';
import './TreeMapView.css';

export type TransformResult<M, D, A> = {
  id: string;
  data: D;
  nexts: [A, M][];
  edgeLabel?: React.ReactNode;
  width: number;
  height: number;
  submap?: TransformResult<M, D, A>;
};

export type TransformFunc<M, D, A> = (
  map: M,
  parent: string | undefined,
  aux: A
) => TransformResult<M, D, A>;

export type Dims = {
  width: number;
  height: number;
};

export type Props<M, D, A> = {
  initElem: TransformResult<M, D, A>;
  transform: TransformFunc<M, D, A>;
  nodeComponent: React.ComponentType<NodeProps<D & Dims>>;
};

type IntermediateElem<D> = {
  id: string;
  data: D;
  width: number;
  height: number;
  minX: number;
  maxX: number;
  nexts: IntermediateElem<D>[];
  childXOffset: number;
  submap?: IntermediateElem<D>;
  submapXOffset: number;
  y: number;
  maxY: number;
};

export const NODE_WIDTH = 150;
export const NODE_HEIGHT = 50;
export const DEFAULT_NODE_SIZE = {
  width: NODE_WIDTH,
  height: NODE_HEIGHT,
};
const NODE_GAP_X = 50;
const NODE_GAP_Y = 50;

const NODE_PAD = 25;

export type FlowRef = React.MutableRefObject<HTMLDivElement> | undefined;
export const FlowRefContext = React.createContext(undefined as FlowRef);

const TreeMapView = <M, D, A>({
  initElem,
  transform,
  nodeComponent,
}: Props<M, D, A>) => {
  const reactFlowInstance = useReactFlow();
  const flowRef: FlowRef = useRef(undefined as unknown as HTMLDivElement);

  useEffect(() => {
    events.subscribe('resetView', () => {
      reactFlowInstance.setViewport({ x: 0, y: 0, zoom: 1 });
    });

    return () => {
      events.unsubscribe('resetView');
    };
  }, [reactFlowInstance]);
  const nodeTypes = { customNode: nodeComponent };

  const edges: Edge[] = [];

  const buildElem = (
    {
      id,
      data,
      nexts,
      edgeLabel,
      width: initWidth,
      height: initHeight,
      submap: rawSubmap,
    }: TransformResult<M, D, A>,
    minX = 0,
    y = 0,
    parent?: string
  ): IntermediateElem<D> => {
    let submap: IntermediateElem<D> | undefined = undefined;
    let width = initWidth;
    let height = initHeight;
    let submapXOffset = 0;
    if (rawSubmap !== undefined) {
      submap = buildElem(rawSubmap, 0, y + height + NODE_PAD);
      const submapWidth = submap.maxX - submap.minX + NODE_PAD * 2;
      submapXOffset = Math.max(width - submapWidth, 0) / 2;
      width = Math.max(submapWidth, width);
      height = submap.maxY - y + NODE_PAD;
    }

    let maxX = minX;
    let maxY = y + height;
    const nextElems = nexts.map(([aux, nextMap]) => {
      const transformed = transform(nextMap, id, aux);
      const next = buildElem(transformed, maxX, y + height + NODE_GAP_Y, id);
      maxX = Math.max(next.maxX + NODE_GAP_X, maxX);
      maxY = Math.max(next.maxY, maxY);
      return next;
    });
    const childXOffset = Math.max(minX + width - (maxX - NODE_GAP_X), 0) / 2;
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
      childXOffset,
      y,
      maxY,
      submap,
      submapXOffset,
    };
    return elem;
  };

  const elem = buildElem(initElem);
  const nodes: Node<D & Dims>[] = [];

  const buildNodes = (
    {
      id,
      data,
      width,
      height,
      minX,
      maxX,
      y,
      nexts,
      childXOffset,
      submap,
      submapXOffset,
    }: IntermediateElem<D>,
    xOffset = 0
  ) => {
    const x = (minX + maxX) / 2 - width / 2 + xOffset;
    nodes.push({
      id: `${id}`,
      position: { x, y },
      type: 'customNode',
      data: {
        width,
        height,
        ...data,
      },
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

    if (submap !== undefined) {
      buildNodes(submap, x + submapXOffset + NODE_PAD);
    }

    nexts.forEach(next => {
      buildNodes(next, xOffset + childXOffset);
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
          <Background color="#aaa" gap={25} />
        </ReactFlow>
      </FlowRefContext.Provider>
    </div>
  );

  return ret;
};

const TreeMapViewWrap = <M, D, A>(props: Props<M, D, A>) => (
  <ReactFlowProvider>
    <TreeMapView {...props} />
  </ReactFlowProvider>
);

export default TreeMapViewWrap;
