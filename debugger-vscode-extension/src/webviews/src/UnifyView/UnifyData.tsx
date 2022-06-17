import {
  VSCodeButton,
  VSCodeDataGrid,
  VSCodeDataGridCell,
  VSCodeDataGridRow,
  VSCodeDivider,
  VSCodeLink,
} from '@vscode/webview-ui-toolkit/react';
import React, { useEffect } from 'react';
import useStore, { mutateStore } from '../store';
import { Code, showBaseUnifyKind } from '../util';
import VSCodeAPI from '../VSCodeAPI';

import './UnifyData.css';

const UnifyData = () => {
  const procName = useStore(store => store.debugState?.procName || '');
  const [{ path, unifications }, baseUnifyKind] = useStore(store => [
    store.unifyState,
    showBaseUnifyKind(store),
  ]);
  const { pushUnification, popUnifications } = mutateStore();

  useEffect(() => {
    console.log('Showing unify data', path, unifications);
  }, [path, unifications]);
  const selectedStep = unifications[path[0]]?.selected;

  const unifyNames = [
    <>
      <span>
        Unify <Code>{procName}</Code> ({baseUnifyKind})
      </span>
    </>,
  ];
  for (let i = path.length - 1; i > 0; i--) {
    const unifyId = path[i];
    const unification = unifications[unifyId];
    if (
      !unification ||
      !unification.selected ||
      unification.selected[0] !== 'Assertion'
    ) {
      console.error('UnifyData: malformed state', {
        path,
        unifyId,
        unification,
      });
      continue;
    }
    const { assertion } = unification.selected[1];

    unifyNames.push(<Code>{assertion}</Code>);
  }
  const unifyLinks = unifyNames.map((name, i) => {
    let link =
      i > path.length - 2 ? (
        name
      ) : (
        <VSCodeLink
          title="Step out to this unification"
          onClick={() => {
            popUnifications(path.length - i - 1);
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
      <div key={`${i}`} className="unify-link">
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
        const isInStore = pushUnification(foldId);
        if (!isInStore) {
          VSCodeAPI.postMessage({
            type: 'request_unification',
            id: foldId,
          });
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
            </VSCodeDataGridRow>
            {substitutions.map(([expr, val]) => (
              <VSCodeDataGridRow key={expr}>
                <VSCodeDataGridCell gridColumn="1" className="subst-expr">
                  <Code>
                    <b>{expr}</b>
                  </Code>
                </VSCodeDataGridCell>
                <VSCodeDataGridCell gridColumn="2" className="subst-val">
                  <Code>{val}</Code>
                </VSCodeDataGridCell>
              </VSCodeDataGridRow>
            ))}
          </VSCodeDataGrid>
        </>
      );
    }
  }

  return (
    <div className="unify-data-wrap">
      <div className="unify-data">
        <p>{unifyLinks}</p>
        {stepInFoldButton}

        <VSCodeDivider />

        {asrt}
        {subst}
      </div>
    </div>
  );
};

export default UnifyData;
