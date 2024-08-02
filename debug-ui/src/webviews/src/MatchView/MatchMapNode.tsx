import React, { ReactNode } from 'react';
import { NodeProps } from 'react-flow-renderer';
import { VSCodeBadge, VSCodeButton } from '@vscode/webview-ui-toolkit/react';
import { AssertionData, MatchResult } from '../../../types';
import NodeWrap from '../TreeMapView/NodeWrap';
import type { NodeData } from '../TreeMapView/TreeMapView';

import './MatchMapNode.css';

export type MatchMapNodeData =
  | {
      type: 'Assertion';
      assertionData: AssertionData;
      isSelected: boolean;
      setSelected: () => void;
      expanded: boolean;
      toggleExpanded?: () => void;
      result?: boolean;
    }
  | {
      type: 'Result';
      result: MatchResult;
      setSelected: () => void;
    }
  | {
      type: 'Root';
      title: ReactNode;
      subtitle: ReactNode;
    }
  | {
      type: 'Missing';
    };

const MatchMapNode = ({ data }: NodeProps<MatchMapNodeData & NodeData>) => {
  const { type, width, height } = data;
  if (type === 'Root') {
    const { title, subtitle } = data;
    return (
      <NodeWrap root noTargetHandle>
        <span className="node-title">{title}</span>
        <span className="node-subtitle">{subtitle}</span>
      </NodeWrap>
    );
  }

  if (type === 'Result') {
    const [result] = data.result;
    return (
      <NodeWrap
        classes={['match-map-result']}
        noSourceHandle
        error={result !== 'Success'}
        width={width}
        height={height}
      >
        <i>{data.result}</i>
      </NodeWrap>
    );
  }

  if (type === 'Missing') {
    return (
      <NodeWrap noSourceHandle noTargetHandle width={width} height={height}>
        <i>Loading...</i>
      </NodeWrap>
    );
  }

  const {
    isSelected,
    setSelected,
    assertionData,
    toggleExpanded,
    expanded,
    result,
  } = data;

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

  const expandButton = (() => {
    if (toggleExpanded === undefined) return <></>;

    const icon = expanded ? 'chevron-down' : 'chevron-right';

    return (
      <VSCodeButton
        appearance="icon"
        aria-label="Expand / Collapse"
        title="Expand / Collapse"
        onClick={toggleExpanded}
      >
        <span className={`codicon codicon-${icon}`} />
      </VSCodeButton>
    );
  })();

  return (
    <NodeWrap
      selected={isSelected}
      noSourceHandle={result !== undefined}
      error={result === false}
      width={width}
      height={height}
    >
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
        {expandButton}
      </div>
    </NodeWrap>
  );
};

export default MatchMapNode;
