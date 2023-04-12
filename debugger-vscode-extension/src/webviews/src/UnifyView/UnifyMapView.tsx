import React from 'react';
import {
  UnificationState,
  UnifyMap,
  UnifyMapInner,
  UnifySeg,
  UnifyStep,
} from '../../../types';
import TreeMapView, {
  TransformFunc,
  TransformResult,
  DEFAULT_NODE_SIZE,
} from '../TreeMapView/TreeMapView';
import UnifyMapNode, { UnifyMapNodeData } from './UnifyMapNode';

import 'allotment/dist/style.css';
import { getUnifyName } from '../util';
import useStore from '../store';

type Props = {
  unification: UnificationState;
  selectStep: (step: UnifyStep) => void;
  unifications: Record<number, UnificationState | undefined>;
  expandedNodes: Set<number>;
  requestUnification: (id: number) => void;
  toggleNodeExpanded: (id: number) => void;
};

type M = UnifySeg;
type D = UnifyMapNodeData;
type A = null;

const UnifyMapView = ({
  unification,
  selectStep,
  expandedNodes,
  unifications,
  requestUnification,
  toggleNodeExpanded,
}: Props) => {
  const unifyMap = (unification.map as UnifyMap)[1];
  const selectedId = (() => {
    if (!unification.selected) {
      return -1;
    }

    if (unification.selected[0] === 'Assertion') {
      return unification.selected[1].id;
    } else if (unification.selected[0] === 'Result') {
      return unification.selected[1];
    }
  })();

  const [title, subtitle] = useStore(state => getUnifyName(state));
  const buildInitElem = (
    id: number,
    title: React.ReactNode,
    subtitle: React.ReactNode = <></>,
    map: UnifyMapInner
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

  const initElem = buildInitElem(unification.id, title, subtitle, unifyMap);

  const transform: TransformFunc<M, D, A> = map => {
    if (map[0] === 'UnifyResult') {
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

      // Fold unification hasn't been requested yet
      if (!(foldId in unifications)) {
        requestUnification(foldId);
        return [undefined, true];
      }

      const unification = unifications[foldId];

      // Fold unification is loading or hidden
      if (unification === undefined || !expanded) return [undefined, true];

      const map = unification.map as UnifyMap;

      const initElem = buildInitElem(foldId, <i>Fold</i>, undefined, map[1]);
      return [initElem, true];
    })();

    const toggleExpanded = hasSubmap
      ? () => {
          toggleNodeExpanded(id);
        }
      : undefined;

    const [nexts, result]: [[null, UnifySeg][], boolean | undefined] = (() => {
      if (next[0] !== 'UnifyResult') return [[[null, next]], undefined];
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
      {...{ initElem, transform, nodeComponent: UnifyMapNode, expandedNodes }}
    />
  );
};

export default UnifyMapView;
