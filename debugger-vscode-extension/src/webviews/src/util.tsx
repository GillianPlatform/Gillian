import React, { ReactNode } from 'react';
import { Unification, UnifyKind, UnifyMap } from '../../types';
import { Store } from './store';

export const Code: React.FC = ({ children }) => (
  <span className="code">{children}</span>
);

export const showUnifyKind = ([kind]: UnifyKind) => {
  switch (kind) {
    case 'Postcondition':
      return 'post-condition';
    case 'FunctionCall':
      return 'function call';
    case 'LogicCommand':
      return 'logic command';
    default:
      return kind.toLowerCase();
  }
};

export const getBaseUnification = ({ unifyState }: Store) => {
  const { path, unifications } = unifyState;
  const baseUnification = unifications[path[path.length - 1]];
  return baseUnification;
};

export const showUnificationKind = (unification: Unification | undefined) => {
  if (!unification) return undefined;
  return showUnifyKind((unification.map as UnifyMap)[0]);
};

export const showBaseUnifyKind = (store: Store) =>
  showUnificationKind(getBaseUnification(store));

export const getUnifyName = (store: Store): [ReactNode, ReactNode] => {
  const { path, unifications } = store.unifyState;
  if (path.length < 2) {
    const kind = showBaseUnifyKind(store);
    const procName = store.debugState?.procName || 'unknown proc';
    return [
      <>
        Unify <Code>{procName}</Code>
      </>,
      <>{kind}</>,
    ];
  }
  const prevStep = unifications[path[1]!]!.selected!;
  if (prevStep[0] !== 'Assertion') throw 'getUnifyName error';
  const assertion = prevStep[1].assertion;
  return [
    <>Fold</>,
    <>
      <Code>{assertion}</Code>
    </>,
  ];
};
