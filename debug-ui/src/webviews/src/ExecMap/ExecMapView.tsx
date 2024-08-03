import React from 'react';
import {
  BranchCase,
  DebuggerState,
  DebugProcState,
  ExecMap,
  SafeExecMap,
} from '../../../types';
import TreeMapView, { PreNode as PreNode_ } from '../TreeMapView/TreeMapView';
import { execSpecific, jumpToId, startProc } from '../VSCodeAPI';
import ExecMapNode, { ExecMapNodeData } from './ExecMapNode';
import { NODE_HEIGHT, DEFAULT_NODE_SIZE } from '../TreeMapView/TreeMapView';

export type Props = {
  state: DebuggerState;
  expandedNodes: Set<string>;
  toggleNodeExpanded: (id: string) => void;
};

type Id =
  | ['Root']
  | ['Node', string, number]
  | ['Empty']
  | ['ProcEmpty', string];
type Aux = {
  parentNode: string | undefined;
  parentId: number | undefined;
  branchCase: BranchCase | undefined;
  edgeLabel: string;
};
type D = ExecMapNodeData;
type PreNode = PreNode_<Id, Aux, D>;

const emptyAux = {
  parentNode: undefined,
  parentId: undefined,
  branchCase: undefined,
  edgeLabel: '',
};

const ExecMapView = ({ state, expandedNodes, toggleNodeExpanded }: Props) => {
  const { mainProc: mainProcName, currentProc: currentProcName, procs } = state;

  const procMapCache: Record<string, [DebugProcState, SafeExecMap]> = {};
  const getProcAndMap = (
    procName: string
  ): [DebugProcState, SafeExecMap] | undefined => {
    if (!(procName in procMapCache)) {
      const proc = procs[procName];
      if (proc === undefined) return undefined;
      const rawMap = proc.liftedExecMap ?? proc.execMap;
      const map: SafeExecMap = {
        root: rawMap.root,
        entries: Object.fromEntries(rawMap.entries),
      };
      procMapCache[procName] = [proc, map];
    }
    return procMapCache[procName];
  };

  const getRoot = (): PreNode => {
    const procAndMap = getProcAndMap(mainProcName);
    if (procAndMap === undefined)
      throw `Couldn't find main proc ${mainProcName}`;
    const rootId = procAndMap[1].root!;
    const next: Id = ['Node', mainProcName, rootId];
    return {
      id: 'root',
      submap: undefined,
      parent: undefined,
      nexts: [[next, { ...emptyAux, parentNode: 'root' }]],
      edgeLabel: undefined,
      ...DEFAULT_NODE_SIZE,
      data: {
        type: 'Root',
        procName: mainProcName,
        isActive: currentProcName === mainProcName,
      },
    };
  };

  let emptyCount = 0;
  const getEmpty = (aux: Aux): PreNode => {
    const { parentNode, parentId, branchCase, edgeLabel } = aux;
    if (parentId === undefined) throw 'Empty with no parent ID!';
    return {
      id: `empty${emptyCount++}`,
      submap: undefined,
      parent: parentNode,
      nexts: [],
      edgeLabel,
      width: NODE_HEIGHT,
      height: NODE_HEIGHT,
      data: {
        type: 'Empty',
        hasParent: true,
        isActive: false,
        exec: () => {
          execSpecific(parentId, branchCase || null);
        },
      },
    };
  };

  const getProcEmpty = (procName: string): PreNode => {
    return {
      id: `empty-${procName}`,
      submap: undefined,
      parent: undefined,
      nexts: [],
      edgeLabel: undefined,
      width: NODE_HEIGHT,
      height: NODE_HEIGHT,
      data: {
        type: 'Empty',
        hasParent: false,
        isActive: false,
        exec: () => {
          startProc(procName);
        },
      },
    };
  };

  const getNode = (nodeId: Id, aux: Aux): PreNode => {
    if (nodeId[0] === 'Root') return getRoot();
    if (nodeId[0] === 'Empty') return getEmpty(aux);
    if (nodeId[0] === 'ProcEmpty') return getProcEmpty(nodeId[1]);

    const procName = nodeId[1];
    const procAndMap = getProcAndMap(procName);
    if (procAndMap === undefined) throw `Couldn't find proc ${procName}`;
    const [proc, map] = procAndMap;
    const entry = map.entries[nodeId[2]];
    if (entry === undefined) {
      console.error({ proc, nodeId, aux });
      throw `Couldn't find entry ${nodeId[2]} in ${procName}`;
    }
    if (entry[0] === 'Alias') return getNode(['Node', procName, entry[1]], aux);
    const node = entry[1];
    const cmdData = node.data;
    const id = cmdData.id;
    const expanded = expandedNodes.has(`${id}`);

    const submap = ((): [Id, Aux] | undefined => {
      if (!expanded) return undefined;
      if (node.data.submap[0] === 'NoSubmap') return undefined;
      if (node.data.submap[0] === 'Submap') {
        return [['Node', procName, node.data.submap[1]], emptyAux];
      }
      if (node.data.submap[0] === 'Proc') {
        const procName = node.data.submap[1];
        const procAndMap = getProcAndMap(procName);
        if (procAndMap === undefined)
          return [['ProcEmpty', procName], emptyAux];
        const rootId = procAndMap[1].root!;
        return [['Node', procName, rootId], emptyAux];
      }
    })();

    const nexts = ((): [Id, Aux][] => {
      if (node.next === null) return [];
      if (node.next[0] === 'Single') {
        const [nextId, edgeLabel] = node.next[1];
        const aux = {
          parentNode: `${id}`,
          parentId: id,
          branchCase: undefined,
          edgeLabel,
        };
        if (nextId === null) {
          return [[['Empty'], aux]];
        }
        return [[['Node', procName, nextId], aux]];
      }
      // node.next[0] === 'Branch'
      return node.next[1].map(([branchCase, [nextId, edgeLabel]]) => {
        const aux = {
          parentNode: `${id}`,
          parentId: id,
          branchCase,
          edgeLabel,
        };
        if (nextId === null) {
          return [['Empty'], aux];
        }
        return [['Node', procName, nextId], aux];
      });
    })();

    const isCurrentCmd =
      procName === currentProcName &&
      cmdData.all_ids.includes(proc.currentCmdId);

    const toggleExpanded =
      node.data.submap[0] === 'NoSubmap'
        ? undefined
        : () => {
            toggleNodeExpanded(`${id}`);
          };

    const data: ExecMapNodeData = {
      type: 'Cmd',
      cmdData,
      isFinal: nexts.length === 0,
      isCurrentCmd,
      hasParent: aux.parentNode !== undefined,
      jump: () => {
        jumpToId(id);
      },
      expanded,
      toggleExpanded,
      isActive: procName === currentProcName,
    };

    return {
      id: `${id}`,
      submap,
      parent: aux.parentNode,
      nexts,
      edgeLabel: aux.edgeLabel,
      ...DEFAULT_NODE_SIZE,
      data,
    };
  };

  return (
    <TreeMapView
      {...{
        getNode,
        rootId: ['Root'],
        rootAux: emptyAux,
        nodeComponent: ExecMapNode,
      }}
    />
  );
};

export default ExecMapView;
