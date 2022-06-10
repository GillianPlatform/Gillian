import { VSCodeButton, VSCodeDataGrid, VSCodeDataGridCell, VSCodeDataGridRow, VSCodeDivider } from '@vscode/webview-ui-toolkit/react';
import React from 'react';
import useStore, { mutateStore } from '../store';
import VSCodeAPI from '../VSCodeAPI';

import './UnifyData.css';

const UnifyData = () => {
  const { path, unifications } = useStore((store) => store.unifyState);
  const { pushUnification } = mutateStore();

  const selectedStep = unifications[path[0]]?.selected;

  let stepInFoldButton = <></>;
  let subst = <></>;
  if (selectedStep !== undefined && selectedStep[0] === 'Assertion') {
    const { fold, substitutions } = selectedStep[1];

    if (fold !== null) {
      const foldId = fold[0];
      const stepInFold = () => {
        const isInStore = pushUnification(foldId);
        if (!isInStore) {
          VSCodeAPI.postMessage({
            type: 'request_unification',
            id: foldId
          });
        }
      };
      stepInFoldButton = (
        <VSCodeButton onClick={stepInFold}>
          Step into fold
        </VSCodeButton>
      );
    }

    if (substitutions.length === 0) {
      subst = (
        <i>No substitutions</i>
      );
    } else {
      subst = (
        <>
          <h3>Substitutions</h3>
          <VSCodeDataGrid className='subst-grid'>
            <VSCodeDataGridRow rowType="header">
              <VSCodeDataGridCell cellType="columnheader" gridColumn="1">
                Expr
              </VSCodeDataGridCell>
              <VSCodeDataGridCell cellType="columnheader" gridColumn="2">
                Value
              </VSCodeDataGridCell>
            </VSCodeDataGridRow>
            {
              substitutions.map(([expr, val]) => (
                <VSCodeDataGridRow key={expr}>
                  <VSCodeDataGridCell gridColumn="1" className='subst-expr'>
                    <b><pre>{expr}</pre></b>
                  </VSCodeDataGridCell>
                  <VSCodeDataGridCell gridColumn="2" className='subst-val'>
                    <pre>{val}</pre>
                  </VSCodeDataGridCell>
                </VSCodeDataGridRow>
              ))
            }
          </VSCodeDataGrid>
        </>
      );
    }
  }

  return (
    <div className='unify-data-wrap'>
      <div className='unify-data'>
        (unify path here)
        <br/>
        {stepInFoldButton}
        
        <VSCodeDivider />

        {subst}
      </div>
    </div>
  );
};

export default UnifyData;
