import React from 'react';
import { Unification, UnifyMap, UnifySeg, UnifyStep } from '../../../types';
import TreeMapView, {
  TransformFunc,
  TransformResult,
  DEFAULT_NODE_SIZE
} from '../TreeMapView/TreeMapView';
import UnifyMapNode, { UnifyMapNodeData } from './UnifyMapNode';

import 'allotment/dist/style.css';
import { getUnifyName } from '../util';
import useStore from '../store';

type Props = {
  unification: Unification;
  selectStep: (step: UnifyStep) => void;
};

type M = UnifySeg;
type D = UnifyMapNodeData;
type A = null;

const UnifyMapView = ({ unification, selectStep }: Props) => {
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
  const initElem: TransformResult<M, D, A> = {
    id: 'root',
    data: {
      type: 'Root',
      title,
      subtitle,
    },
    nexts: (() => {
      if (unifyMap[0] === 'Direct') {
        return [[null, unifyMap[1]]];
      } else {
        return unifyMap[1].map(seg => [null, seg]);
      }
    })(),
    ...DEFAULT_NODE_SIZE
  };

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
        ...DEFAULT_NODE_SIZE
      };
    }

    const [, assertionData, next] = map;
    const { id } = assertionData;
    return {
      id: `${id}`,
      data: {
        type: 'Assertion',
        assertionData,
        isSelected: id === selectedId,
        setSelected: () => {
          selectStep(['Assertion', assertionData]);
        },
      },
      nexts: [[null, next]],
      ...DEFAULT_NODE_SIZE
    };
  };

  return (
    <TreeMapView {...{ initElem, transform, nodeComponent: UnifyMapNode }} />
  );
};

export default UnifyMapView;
