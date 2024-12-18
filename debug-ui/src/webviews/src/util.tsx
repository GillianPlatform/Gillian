import React, { ReactNode } from 'react';
import { MatchingState, MatchKind, MatchMapSafe } from '../../types';
import { Store } from './store';

export const Code: React.FC = ({ children }) => (
  <span className="code">{children}</span>
);

export const showMatchKind = ([kind]: MatchKind) => {
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

export const getBaseMatching = ({ matchState }: Store) => {
  const { path, matches } = matchState;
  const baseMatching = matches[path[path.length - 1]];
  return baseMatching;
};

export const showMatchingKind = (matching: MatchingState | undefined) => {
  if (!matching) return undefined;
  return showMatchKind((matching.map as MatchMapSafe).kind);
};

export const showBaseMatchKind = (store: Store) =>
  showMatchingKind(getBaseMatching(store));

export const getMatchName = (store: Store): [ReactNode, ReactNode] => {
  const { path, matches } = store.matchState;
  if (path.length < 2) {
    // Top-level match
    const kind = showBaseMatchKind(store);
    const procName = store.debuggerState?.mainProc || 'unknown proc';
    return [
      <>
        <Code>{procName}</Code>
      </>,
      <>Match {kind}</>,
    ];
  }
  // Fold
  const prevMatch = matches[path[1]!]!;
  const prevStep = prevMatch.map.nodes[prevMatch.selected!];
  if (prevStep[0] !== 'Assertion') throw 'getMatchName error';
  const assertion = prevStep[1].assertion;
  return [
    <>Fold</>,
    <>
      <Code>{assertion}</Code>
    </>,
  ];
};
