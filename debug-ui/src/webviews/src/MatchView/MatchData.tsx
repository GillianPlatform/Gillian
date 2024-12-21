import {
  VSCodeButton,
  VSCodeDataGrid,
  VSCodeDataGridCell,
  VSCodeDataGridRow,
  VSCodeDivider,
  VSCodeLink,
} from '@vscode/webview-ui-toolkit/react';
import React, { useEffect, useMemo } from 'react';
import { MatchMap, MatchMapNode } from '../../../types';
import useStore, { mutateStore } from '../store';
import { Code, getMatchName, showBaseMatchKind } from '../util';
import { requestMatching } from '../VSCodeAPI';

import './MatchData.css';

type Props = {
  selectStep: (step: number) => void;
};

const MatchData = ({ selectStep }: Props) => {
  const [{ path, matches }] = useStore(store => [
    store.matchState,
    showBaseMatchKind(store),
  ]);
  const { pushMatching, popMatchings } = mutateStore();
  const [title, subtitle] = useStore(state => getMatchName(state));

  useEffect(() => {
    console.log('Showing match data', path, matches);
  }, [path, matches]);
  const matching = matches[path[0]];
  const selectedStep = matching?.selected
    ? matching.map.nodes[matching.selected]
    : undefined;

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
      matching &&
      matching.selected &&
      matching.map.nodes[matching.selected][0] === 'Assertion'
    ) {
      const assertion = matching.map.nodes[matching.selected][1];
      matchNames.push(<Code>{assertion}</Code>);
    } else {
      console.error('MatchData: malformed state', {
        path,
        matchId,
        matching,
      });
    }
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
              const jumpButtonIcon = 'target';
              const jumpButton = (
                <VSCodeButton
                  appearance="icon"
                  aria-label="Jump to assertion of origin"
                  title="Jump to assertion of origin"
                  disabled={selectedStep[1].id === assertId}
                  onClick={() => {
                    selectStep(assertId);
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
