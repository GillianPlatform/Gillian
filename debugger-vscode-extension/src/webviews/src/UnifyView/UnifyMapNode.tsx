import React, { ReactNode } from 'react';
import { NodeProps } from 'react-flow-renderer';
import { VSCodeBadge, VSCodeButton } from '@vscode/webview-ui-toolkit/react';
import { AssertionData, UnifyResult } from '../../../types';
import NodeWrap from '../TreeMapView/NodeWrap';

import './UnifyMapNode.css';

export type UnifyMapNodeData =
  | {
      type: 'Assertion';
      assertionData: AssertionData;
      isSelected: boolean;
      setSelected: () => void;
    }
  | {
      type: 'Result';
      result: UnifyResult;
      setSelected: () => void;
    }
  | {
      type: 'Root';
      title: ReactNode;
      subtitle: ReactNode;
    };

const UnifyMapNode = ({ data }: NodeProps<UnifyMapNodeData>) => {
  if (data.type === 'Root') {
    const { title, subtitle } = data;
    return (
      <NodeWrap root noTargetHandle>
        <span className="node-title">{title}</span>
        <span className="node-subtitle">{subtitle}</span>
      </NodeWrap>
    );
  }

  if (data.type === 'Result') {
    const [result] = data.result;
    return (
      <NodeWrap
        classes={['unify-map-result']}
        noSourceHandle
        error={result !== 'Success'}
      >
        <i>{data.result}</i>
      </NodeWrap>
    );
  }

  const { isSelected, setSelected, assertionData } = data;

  const { fold } = assertionData;
  const foldBadge = (() => {
    if (fold) {
      const [, [result]] = fold;
      return (
        <>
          <VSCodeBadge>
            <div
              className={`codicon codicon-${
                result === 'Success' ? 'pass' : 'error'
              }`}
            />
            &nbsp; Fold
          </VSCodeBadge>
          &nbsp;
        </>
      );
    } else {
      return <></>;
    }
  })();

  return (
    <NodeWrap selected={isSelected}>
      <pre>{assertionData.assertion}</pre>
      <div className="node-button-row">
        {foldBadge}
        <VSCodeButton
          appearance="icon"
          aria-label="Jump here"
          title="Jump here"
          disabled={isSelected}
          onClick={isSelected ? undefined : setSelected}
        >
          <span className="codicon codicon-target" />
        </VSCodeButton>
      </div>
    </NodeWrap>
  );
};

export default UnifyMapNode;
