import React from 'react';
import {
  AssertionData,
  MatchingState,
  MatchMap,
  MatchResult,
} from '../../../types';
import TreeMapView, {
  PreNode as PreNode_,
  DEFAULT_NODE_SIZE,
} from '../TreeMapView/TreeMapView';
import MatchMapNode, { MatchMapNodeData } from './MatchMapNode';

import 'allotment/dist/style.css';
import { getMatchName } from '../util';
import useStore from '../store';

type Props = {
  matchId: number;
  selectStep: (step: number) => void;
  matches: Record<number, MatchingState | undefined>;
  expandedNodes: Set<number>;
  requestMatching: (id: number) => void;
  toggleNodeExpanded: (id: number) => void;
};

type Id = [number, 'Node', number] | [number, 'Root'];
type Aux = string | undefined;
type D = MatchMapNodeData;
type PreNode = PreNode_<Id, Aux, D>;

const MatchMapView = ({
  matchId: baseMatchId,
  selectStep,
  expandedNodes,
  matches,
  requestMatching,
  toggleNodeExpanded,
}: Props) => {
  const rootTitles = useStore(state => getMatchName(state));

  const match = matches[baseMatchId]!;
  const selectedId = match.selected !== undefined ? match.selected : -1;

  const getRoot = (matchId: number): PreNode => {
    const match = matches[matchId];
    if (match === undefined) throw `Couldn't find match ${matchId}!`;
    const id = `root-${matchId}`;
    const [title, subtitle] =
      // eslint-disable-next-line react/jsx-key
      matchId === baseMatchId ? rootTitles : [<i>Fold</i>, undefined];
    const nexts = match.map.roots.map(
      rootId => [[matchId, 'Node', rootId], id] as [Id, Aux]
    );
    return {
      id,
      submap: undefined,
      parent: undefined,
      nexts,
      edgeLabel: undefined,
      ...DEFAULT_NODE_SIZE,
      data: {
        type: 'Root',
        title,
        subtitle,
      },
    };
  };

  const getAssertion = (
    matchId: number,
    data: AssertionData,
    nextIds: readonly number[],
    parent: Aux
  ): PreNode => {
    const { id, fold } = data;
    const nexts = nextIds.map(
      nextId => [[matchId, 'Node', nextId], `${id}`] as [Id, Aux]
    );

    const expanded = expandedNodes.has(id);

    const [submap, hasSubmap] = ((): [[Id, Aux] | undefined, boolean] => {
      // No fold
      if (fold === null) return [undefined, false];

      const foldId = fold[0];

      // Fold matching hasn't been requested yet
      if (!(foldId in matches)) {
        requestMatching(foldId);
        return [undefined, true];
      }
      if (!expanded) return [undefined, true];

      return [[[foldId, 'Root'], undefined], true];
    })();

    const toggleExpanded = hasSubmap
      ? () => {
          toggleNodeExpanded(id);
        }
      : undefined;

    return {
      id: `${data.id}`,
      submap,
      parent,
      nexts,
      edgeLabel: undefined,
      ...DEFAULT_NODE_SIZE,
      data: {
        type: 'Assertion',
        assertionData: data,
        isSelected: id === selectedId,
        setSelected: () => {
          selectStep(id);
        },
        expanded,
        toggleExpanded,
      },
    };
  };

  const getResult = (
    id: number,
    result: MatchResult,
    parent: Aux
  ): PreNode => ({
    id: `result-${id}`,
    submap: undefined,
    parent,
    nexts: [],
    edgeLabel: undefined,
    ...DEFAULT_NODE_SIZE,
    data: {
      type: 'Result',
      result,
      setSelected: () => {
        selectStep(id);
      },
    },
  });

  const getNode = (nodeId: Id, parent: Aux): PreNode => {
    const matchId = nodeId[0];
    if (nodeId[1] === 'Root') return getRoot(matchId);
    const node = matches[matchId]!.map.nodes[nodeId[2]];
    if (!node) throw `Couldn't find node ${nodeId}!`;
    if (node[0] === 'MatchResult') return getResult(nodeId[2], node[2], parent);
    return getAssertion(matchId, node[1], node[2], parent);
  };

  return (
    <TreeMapView
      {...{
        rootId: [baseMatchId, 'Root'],
        rootAux: undefined,
        getNode,
        nodeComponent: MatchMapNode,
      }}
    />
  );
};

export default MatchMapView;
