import React, { FC } from 'react';
import { Handle, NodeProps, Position } from 'react-flow-renderer';
import { VSCodeBadge, VSCodeButton } from '@vscode/webview-ui-toolkit/react';
import { AssertionData, UnifyResult } from '../../../types';
import VSCodeAPI from '../VSCodeAPI';
import { NODE_WIDTH, NODE_HEIGHT } from '../TreeMapView/TreeMapView';
import NodeWrap from '../TreeMapView/NodeWrap';

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
    };

const UnifyMapNode = ({ data }: NodeProps<UnifyMapNodeData>) => {
  if (data.type === 'Root') {
    return (
      <NodeWrap root noTargetHandle>
        <pre>
          <b>Unify</b>
        </pre>
      </NodeWrap>
    );
  }

  if (data.type === 'Result') {
    return (
      <NodeWrap noSourceHandle>
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
