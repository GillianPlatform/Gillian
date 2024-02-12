import React from 'react';
import {
  MatchingState,
  MatchMap,
  MatchMapInner,
  MatchSeg,
  MatchStep,
} from '../../../types';
import TreeMapView, {
  TransformFunc,
  TransformResult,
  DEFAULT_NODE_SIZE,
} from '../TreeMapView/TreeMapView';
import MatchMapNode, { MatchMapNodeData } from './MatchMapNode';

import 'allotment/dist/style.css';
import { getMatchName } from '../util';
import useStore from '../store';

type Props = {
  matching: MatchingState;
  selectStep: (step: MatchStep) => void;
  matches: Record<number, MatchingState | undefined>;
  expandedNodes: Set<number>;
  requestMatching: (id: number) => void;
  toggleNodeExpanded: (id: number) => void;
};

type M = MatchSeg;
type D = MatchMapNodeData;
type A = null;

const MatchMapView = ({
  matching,
  selectStep,
  expandedNodes,
  matches,
  requestMatching,
  toggleNodeExpanded,
}: Props) => {
  const matchMap = (matching.map as MatchMap)[1];
  const selectedId = (() => {
    if (!matching.selected) {
      return -1;
    }

    if (matching.selected[0] === 'Assertion') {
      return matching.selected[1].id;
    } else if (matching.selected[0] === 'Result') {
      return matching.selected[1];
    }
  })();

  const [title, subtitle] = useStore(state => getMatchName(state));
  const buildInitElem = (
    id: number,
    title: React.ReactNode,
    subtitle: React.ReactNode = <></>,
    map: MatchMapInner
  ): TransformResult<M, D, A> => ({
    id: `root-${id}`,
    data: {
      type: 'Root',
      title,
      subtitle,
    },
    nexts: (() => {
      if (map[0] === 'Direct') {
        return [[null, map[1]]];
      } else {
        return map[1].map(seg => [null, seg]);
      }
    })(),
    ...DEFAULT_NODE_SIZE,
  });

  const initElem = buildInitElem(matching.id, title, subtitle, matchMap);

  const transform: TransformFunc<M, D, A> = map => {
    if (map[0] === 'MatchResult') {
      const [, id, result] = map;
      return {
        id: `${id}`,
        data: {
          type: 'Result',
          result,
          setSelected: () => {
            selectStep(['Result', id, result]);
          },
        },
        nexts: [],
        ...DEFAULT_NODE_SIZE,
      };
    }

    const [, assertionData, next] = map;
    const isSelected = assertionData.id === selectedId;
    const { id, fold } = assertionData;

    const expanded = expandedNodes.has(id);
    const [submap, hasSubmap] = (() => {
      // No fold
      if (fold === null) return [undefined, false];

      const foldId = fold[0];

      // Fold matching hasn't been requested yet
      if (!(foldId in matches)) {
        requestMatching(foldId);
        return [undefined, true];
      }

      const matching = matches[foldId];

      // Fold matching is loading or hidden
      if (matching === undefined || !expanded) return [undefined, true];

      const map = matching.map as MatchMap;

      const initElem = buildInitElem(foldId, <i>Fold</i>, undefined, map[1]);
      return [initElem, true];
    })();

    const toggleExpanded = hasSubmap
      ? () => {
          toggleNodeExpanded(id);
        }
      : undefined;

    const [nexts, result]: [[null, MatchSeg][], boolean | undefined] = (() => {
      if (next[0] !== 'MatchResult') return [[[null, next]], undefined];
      if (next[2][0] === 'Success') return [[], true];
      return [[], false];
    })();
    return {
      id: `${id}`,
      data: {
        type: 'Assertion',
        assertionData,
        isSelected,
        setSelected: () => {
          selectStep(['Assertion', assertionData]);
        },
        expanded,
        toggleExpanded,
        result,
      },
      nexts,
      submap,
      ...DEFAULT_NODE_SIZE,
    };
  };

  return (
    <TreeMapView
      {...{ initElem, transform, nodeComponent: MatchMapNode, expandedNodes }}
    />
  );
};

export default MatchMapView;
