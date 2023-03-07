import {
  VSCodeButton,
  VSCodeDataGrid,
  VSCodeDataGridCell,
  VSCodeDataGridRow,
  VSCodeDivider,
  VSCodeLink,
} from '@vscode/webview-ui-toolkit/react';
import React, { useEffect, useMemo } from 'react';
import { UnifyMap, UnifySeg, UnifyStep } from '../../../types';
import useStore, { mutateStore } from '../store';
import { Code, showBaseUnifyKind } from '../util';
import { requestUnification } from '../VSCodeAPI';

import './UnifyData.css';

type Props = {
  selectStep: (step: UnifyStep) => void;
};

const extractTargets = (seg: UnifySeg): { [key: number]: UnifyStep } => {
  if (seg[0] === 'UnifyResult') {
    const [, id, result] = seg;
    return { [id]: ['Result', id, result] };
  }

  const [, asrt, next] = seg;
  return { [asrt.id]: ['Assertion', asrt], ...extractTargets(next) };
};

const UnifyData = ({ selectStep }: Props) => {
  const mainProcName = useStore(store => store.debuggerState?.mainProc || '');
  const [{ path, unifications }, baseUnifyKind] = useStore(store => [
    store.unifyState,
    showBaseUnifyKind(store),
  ]);
  const { pushUnification, popUnifications } = mutateStore();

  useEffect(() => {
    console.log('Showing unify data', path, unifications);
  }, [path, unifications]);
  const unification = unifications[path[0]];
  const selectedStep = unification?.selected;

  const unifyJumpTargets = useMemo(() => {
    if (unification?.map === undefined) return {};
    const unifyMap = unification?.map as UnifyMap;
    if (unifyMap[1][0] === 'Direct') {
      return extractTargets(unifyMap[1][1]);
    } else {
      return unifyMap[1][1].reduce(
        (acc, seg) => ({ ...acc, ...extractTargets(seg) }),
        {}
      );
    }
  }, [unification]);

  const unifyNames = [
    <>
      <span>
        Unify <Code>{mainProcName}</Code> ({baseUnifyKind})
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
          requestUnification(foldId);
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
              const target = unifyJumpTargets[assertId];
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
