import {
  VSCodeButton,
  VSCodeDataGrid,
  VSCodeDataGridCell,
  VSCodeDataGridRow,
  VSCodeDivider,
  VSCodeLink,
} from '@vscode/webview-ui-toolkit/react';
import React, { useEffect, useMemo } from 'react';
import { MatchMap, MatchSeg, MatchStep } from '../../../types';
import useStore, { mutateStore } from '../store';
import { Code, getMatchName, showBaseMatchKind } from '../util';
import { requestMatching } from '../VSCodeAPI';

import './MatchData.css';

type Props = {
  selectStep: (step: MatchStep) => void;
};

const extractTargets = (seg: MatchSeg): { [key: number]: MatchStep } => {
  if (seg[0] === 'MatchResult') {
    const [, id, result] = seg;
    return { [id]: ['Result', id, result] };
  }

  const [, asrt, next] = seg;
  return { [asrt.id]: ['Assertion', asrt], ...extractTargets(next) };
};

const MatchData = ({ selectStep }: Props) => {
  const mainProcName = useStore(store => store.debuggerState?.mainProc || '');
  const [{ path, matches }, baseMatchKind] = useStore(store => [
    store.matchState,
    showBaseMatchKind(store),
  ]);
  const { pushMatching, popMatchings } = mutateStore();
  const [title, subtitle] = useStore(state => getMatchName(state));

  useEffect(() => {
    console.log('Showing match data', path, matches);
  }, [path, matches]);
  const matching = matches[path[0]];
  const selectedStep = matching?.selected;

  const matchJumpTargets = useMemo(() => {
    if (matching?.map === undefined) return {};
    const matchMap = matching?.map as MatchMap;
    if (matchMap[1][0] === 'Direct') {
      return extractTargets(matchMap[1][1]);
    } else {
      return matchMap[1][1].reduce(
        (acc, seg) => ({ ...acc, ...extractTargets(seg) }),
        {}
      );
    }
  }, [matching]);

  const matchNames = [
    <>
      <span>
        {title} ({subtitle})
      </span>
    </>,
  ];
  for (let i = path.length - 1; i > 0; i--) {
    const matchId = path[i];
    const matching = matches[matchId];
    if (
      !matching ||
      !matching.selected ||
      matching.selected[0] !== 'Assertion'
    ) {
      console.error('MatchData: malformed state', {
        path,
        matchId,
        matching,
      });
      continue;
    }
    const { assertion } = matching.selected[1];

    matchNames.push(<Code>{assertion}</Code>);
  }
  const matchLinks = matchNames.map((name, i) => {
    let link =
      i > path.length - 2 ? (
        name
      ) : (
        <VSCodeLink
          title="Step out to this matching"
          onClick={() => {
            popMatchings(path.length - i - 1);
          }}
        >
          {name}
        </VSCodeLink>
      );
    if (i > 0) {
      link = (
        <>
          <div className="codicon codicon-reply" />
          {link}
        </>
      );
    }
    return (
      <div key={`${i}`} className="match-link">
        {link}
      </div>
    );
  });

  let stepInFoldButton = <></>;
  let asrt = <></>;
  let subst = <></>;
  if (selectedStep !== undefined && selectedStep[0] === 'Assertion') {
    const { assertion, fold, substitutions } = selectedStep[1];

    asrt = (
      <p>
        <Code>{assertion}</Code>
      </p>
    );

    if (fold !== null) {
      const foldId = fold[0];
      const stepInFold = () => {
        const isInStore = pushMatching(foldId);
        if (!isInStore) {
          requestMatching(foldId);
        }
      };
      stepInFoldButton = (
        <VSCodeButton onClick={stepInFold}>Step into fold</VSCodeButton>
      );
    }

    if (substitutions.length === 0) {
      subst = <i>No substitutions</i>;
    } else {
      subst = (
        <>
          <h3>Substitutions</h3>
          <VSCodeDataGrid className="subst-grid">
            <VSCodeDataGridRow rowType="header">
              <VSCodeDataGridCell cellType="columnheader" gridColumn="1">
                Expr
              </VSCodeDataGridCell>
              <VSCodeDataGridCell cellType="columnheader" gridColumn="2">
                Value
              </VSCodeDataGridCell>
              <VSCodeDataGridCell cellType="columnheader" gridColumn="3" />
            </VSCodeDataGridRow>
            {substitutions.map(({ assertId, subst: [expr, val] }) => {
              const target = matchJumpTargets[assertId];
              const jumpButtonIcon =
                target === undefined ? 'question' : 'target';
              const jumpButton = (
                <VSCodeButton
                  appearance="icon"
                  aria-label="Jump to assertion of origin"
                  title="Jump to assertion of origin"
                  disabled={
                    target === undefined || selectedStep[1].id === assertId
                  }
                  onClick={() => {
                    selectStep(target);
                  }}
                >
                  <span className={`codicon codicon-${jumpButtonIcon}`} />
                </VSCodeButton>
              );
              return (
                <VSCodeDataGridRow key={expr}>
                  <VSCodeDataGridCell gridColumn="1" className="subst-expr">
                    <Code>
                      <b>{expr}</b>
                    </Code>
                  </VSCodeDataGridCell>
                  <VSCodeDataGridCell gridColumn="2" className="subst-val">
                    <Code>{val}</Code>
                  </VSCodeDataGridCell>
                  <VSCodeDataGridCell gridColumn="3" className="subst-jump">
                    {jumpButton}
                  </VSCodeDataGridCell>
                </VSCodeDataGridRow>
              );
            })}
          </VSCodeDataGrid>
        </>
      );
    }
  }

  return (
    <div className="match-data-wrap">
      <div className="match-data">
        <p>{matchLinks}</p>
        {stepInFoldButton}

        <VSCodeDivider />

        {asrt}
        {subst}
      </div>
    </div>
  );
};

export default MatchData;
