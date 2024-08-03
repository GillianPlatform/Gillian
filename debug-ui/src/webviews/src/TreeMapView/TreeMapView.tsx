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

export const NODE_WIDTH = 150;
export const NODE_HEIGHT = 50;
export const DEFAULT_NODE_SIZE = {
  width: NODE_WIDTH,
  height: NODE_HEIGHT,
};
const NODE_GAP_X = 50;
const NODE_GAP_Y = 50;

const NODE_PAD = 25;

export type PreNode<Id, Aux, D> = {
  id: string;
  submap: [Id, Aux] | undefined;
  parent: string | undefined;
  nexts: [Id, Aux][];
  edgeLabel: string | undefined;
  width: number;
  height: number;
  data: D;
};

type Props<Id, Aux, D> = {
  getNode: (id: Id, aux: Aux) => PreNode<Id, Aux, D>;
  rootId: Id;
  rootAux: Aux;
  nodeComponent: React.ComponentType<NodeProps<D & NodeData>>;
};

type StackElem<Id, Aux, D> = {
  node: PreNode<Id, Aux, D>;
  x: number;
  y: number;
};

export type NodeData = {
  width: number;
  height: number;
  maxX: number;
  maxChildX: number;
  maxChildY: number;
  submap: string | undefined;
  nexts: string[];
};

export type FlowRef = React.MutableRefObject<HTMLDivElement> | undefined;
export const FlowRefContext = React.createContext(undefined as FlowRef);

const TreeMapView = <Id, Aux, D>({
  getNode: getNodeRaw,
  rootId,
  rootAux,
  nodeComponent,
}: Props<Id, Aux, D>) => {
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

  const preNodeCache = new Map<Id, PreNode<Id, Aux, D>>();
  const getNode = (id: Id, aux: Aux) => {
    if (preNodeCache.has(id)) return preNodeCache.get(id)!;
    const node = getNodeRaw(id, aux);
    preNodeCache.set(id, node);
    return node;
  };
  const root = getNode(rootId, rootAux);

  // First pass: initial elem placement, edges
  const nodes: Record<string, Node<D & NodeData>> = {};
  const edges: Edge[] = [];
  const init = { node: root, x: 50, y: 50 };
  const preNodeStack: StackElem<Id, Aux, D>[] = [init];
  for (let i = init; i != undefined; i = preNodeStack[0]) {
    // Function block here so we can 'continue' the outer loop by returning
    (() => {
      const { node, x, y } = i;
      let width = node.width;
      let height = node.height;

      // Handle submaps
      let submapId: string | undefined = undefined;
      if (node.submap !== undefined) {
        const submap = getNode(...node.submap);
        const submapNode = nodes[submap.id];
        if (submapNode) {
          submapId = submapNode.id;
          // Adjust size for submap
          const subWidth = submapNode.data.maxX + NODE_PAD - x;
          const subHeight = submapNode.data.maxChildY + NODE_PAD - y;
          width = Math.max(width, subWidth);
          height = Math.max(height, subHeight);
        } else {
          // Do submap first
          preNodeStack.unshift({
            node: submap,
            x: x + NODE_PAD,
            y: y + NODE_HEIGHT + NODE_PAD,
          });
          return;
        }
      }

      // Handle nexts
      let maxChildX = x - NODE_GAP_X;
      let maxChildY = y + height;
      const nexts: string[] = [];
      for (const [nextId, nextAux] of node.nexts) {
        const next = getNode(nextId, nextAux);
        const nextNode = nodes[next.id];
        if (nextNode) {
          nexts.push(nextNode.id);
          // Shift offset for next node
          maxChildX = nextNode.data.maxX;
          maxChildY = Math.max(maxChildY, nextNode.data.maxChildY);
        } else {
          // Do child first
          preNodeStack.unshift({
            node: next,
            x: maxChildX + NODE_GAP_X,
            y: y + height + NODE_GAP_Y,
          });
          return;
        }
      }
      const maxX = Math.max(maxChildX, x + width);

      nodes[node.id] = {
        id: `${node.id}`,
        position: { x, y },
        type: 'customNode',
        data: {
          width,
          height,
          maxX,
          maxChildX,
          maxChildY,
          nexts,
          submap: submapId,
          ...node.data,
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
      };

      // Add edge
      if (node.parent !== undefined) {
        edges.push({
          id: `${node.parent}-${node.id}`,
          source: node.parent,
          target: node.id,
          ...(node.edgeLabel ? { label: node.edgeLabel } : {}),
        });
      }

      preNodeStack.shift();
    })();
  }

  // Second pass: center nodes
  const centerStack: [string, number][] = [];
  for (
    let i: [string, number] | undefined = [root.id, 0];
    i !== undefined;
    i = centerStack.shift()
  ) {
    const [id, parentOffset] = i;
    const node = nodes[id]!;
    const thisOffset =
      parentOffset + (node.data.maxX - node.position.x - node.data.width) / 2;
    const x = thisOffset + node.position.x;
    node.position = { ...node.position, x };
    if (node.data.submap) {
      const submapNode = nodes[node.data.submap]!;
      const submapOffset =
        thisOffset +
        Math.max(
          (node.position.x +
            node.data.width -
            NODE_PAD -
            submapNode.data.maxX) /
            2,
          0
        );
      centerStack.unshift([node.data.submap, submapOffset]);
    }
    const nextOffset =
      parentOffset + Math.max((node.data.maxX - node.data.maxChildX) / 2, 0);
    for (const next of node.data.nexts) centerStack.unshift([next, nextOffset]);
  }

  const nodeTypes = { customNode: nodeComponent };

  return (
    <div className="tree-map-view">
      <FlowRefContext.Provider value={flowRef}>
        <ReactFlow
          ref={flowRef}
          nodes={Object.values(nodes)}
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
};

const TreeMapViewWrap = <Id, Aux, D>(props: Props<Id, Aux, D>) => (
  <ReactFlowProvider>
    <TreeMapView {...props} />
  </ReactFlowProvider>
);

export default TreeMapViewWrap;
