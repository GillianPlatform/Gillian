import React from 'react';
import {
  AssertionData,
  MatchingState,
  MatchMap,
  MatchMapInner,
  MatchResult,
  MatchSeg,
  MatchStep,
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
  selectStep: (step: MatchStep) => void;
  matches: Record<number, MatchingState | undefined>;
  expandedNodes: Set<number>;
  requestMatching: (id: number) => void;
  toggleNodeExpanded: (id: number) => void;
};

type Id = ['Assertion', number] | ['Result', number] | ['Root', number];
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

  const assertions: Record<number, [AssertionData, MatchSeg]> = {};
  const results: Record<number, MatchResult> = {};

  const selectedId = (() => {
    const match = matches[baseMatchId]!;
    if (!match.selected) {
      return -1;
    }

    if (match.selected[0] === 'Assertion') {
      return match.selected[1].id;
    } else if (match.selected[0] === 'Result') {
      return match.selected[1];
    }
  })();

  const prepNext = (seg: MatchSeg): Id => {
    if (seg[0] === 'Assertion') {
      const id = seg[1].id;
      assertions[id] = [seg[1], seg[2]];
      return ['Assertion', id];
    }
    // seg[0] === 'Result'
    const id = seg[1];
    results[id] = seg[2];
    return ['Result', id];
  };

  const getRoot = (matchId: number): PreNode => {
    const match = matches[matchId];
    if (match === undefined) throw `Couldn't find match ${matchId}!`;
    const id = `root-${matchId}`;
    const map = match.map as MatchMap;
    const [title, subtitle] =
      // eslint-disable-next-line react/jsx-key
      matchId === baseMatchId ? rootTitles : [<i>Fold</i>, undefined];
    const nexts = ((): [Id, Aux][] => {
      if (map[1][0] === 'Direct') {
        return [[prepNext(map[1][1]), id]];
      }
      // map[1][0] === 'Fold'
      return map[1][1].map(seg => [prepNext(seg), id]);
    })();
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

  const getResult = (id: number, parent: Aux): PreNode => {
    const result = results[id];
    return {
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
          selectStep(['Result', id, result]);
        },
      },
    };
  };

  const getAssertion = (asrtId: number, parent: Aux): PreNode => {
    const assertion = assertions[asrtId];
    if (assertion === undefined) throw `Couldn't find assertion ${asrtId}!`;
    const [assertionData, seg] = assertion;
    const id = `${asrtId}`;
    const [nexts, result]: [[Id, Aux][], boolean | undefined] = (() => {
      if (seg[0] !== 'MatchResult') return [[[prepNext(seg), id]], undefined];
      if (seg[2][0] === 'Success') return [[], true];
      return [[], false];
    })();

    const expanded = expandedNodes.has(asrtId);

    const [submap, hasSubmap] = ((): [[Id, Aux] | undefined, boolean] => {
      const fold = assertionData.fold;
      // No fold
      if (fold === null) return [undefined, false];

      const foldId = fold[0];

      // Fold matching hasn't been requested yet
      if (!(foldId in matches)) {
        requestMatching(foldId);
        return [undefined, true];
      }
      if (!expanded) return [undefined, true];

      return [[['Root', foldId], undefined], true];
    })();

    const toggleExpanded = hasSubmap
      ? () => {
          toggleNodeExpanded(asrtId);
        }
      : undefined;

    return {
      id,
      submap,
      parent,
      nexts,
      edgeLabel: undefined,
      ...DEFAULT_NODE_SIZE,
      data: {
        type: 'Assertion',
        assertionData,
        isSelected: asrtId === selectedId,
        setSelected: () => {
          selectStep(['Assertion', assertionData]);
        },
        expanded,
        toggleExpanded,
        result,
      },
    };
  };

  const getNode = (nodeId: Id, aux: Aux): PreNode => {
    if (nodeId[0] === 'Root') return getRoot(nodeId[1]);
    if (nodeId[0] === 'Result') return getResult(nodeId[1], aux);
    // nodeId[0] === 'Assertion')
    return getAssertion(nodeId[1], aux);
  };

  return (
    <TreeMapView
      {...{
        rootId: ['Root', baseMatchId],
        rootAux: undefined,
        getNode,
        nodeComponent: MatchMapNode,
      }}
    />
  );
};

export default MatchMapView;
