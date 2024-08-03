import React from 'react';
import {
  VSCodeBadge,
  VSCodeButton,
  VSCodeDivider,
} from '@vscode/webview-ui-toolkit/react';
import { CmdData } from '../../../types';
import type { NodeData } from '../TreeMapView/TreeMapView';
import NodeWrap from '../TreeMapView/NodeWrap';
import { NodeProps } from 'react-flow-renderer';
import { Code } from '../util';

import './ExecMap.css';

export type ExecMapNodeData = { isActive: boolean } & (
  | {
      type: 'Cmd';
      cmdData: CmdData;
      isFinal: boolean;
      isCurrentCmd: boolean;
      hasParent: boolean;
      jump: () => void;
      expanded: boolean;
      toggleExpanded?: () => void;
    }
  | {
      type: 'Empty';
      hasParent: boolean;
      exec: () => void;
    }
  | {
      type: 'Root';
      procName: string;
    }
);

const ExecMapNode = ({ data }: NodeProps<ExecMapNodeData & NodeData>) => {
  const { type, isActive, width, height } = data;
  if (type === 'Empty') {
    return (
      <NodeWrap
        classes={['node-empty']}
        noSourceHandle
        noTargetHandle={!data.hasParent}
        width={width}
        height={height}
        active={isActive}
      >
        <VSCodeButton
          appearance="icon"
          aria-label="Execute here"
          title="Execute here"
          onClick={data.exec}
        >
          <span className="codicon codicon-play" />
        </VSCodeButton>
      </NodeWrap>
    );
  }

  if (type === 'Root') {
    return (
      <NodeWrap
        root
        noTargetHandle
        width={width}
        height={height}
        active={isActive}
      >
        <span className="node-title">
          <Code>{data.procName}</Code>
        </span>
      </NodeWrap>
    );
  }

  const {
    cmdData,
    isFinal,
    isCurrentCmd,
    hasParent,
    expanded,
    toggleExpanded,
    jump,
  } = data;

  const matchBadge = (() => {
    if (cmdData.matches.length === 0) return <></>;

    const [result] = cmdData.matches[0].result;
    const colorStyle = result === 'Success' ? {} : { color: 'red' };
    return (
      <>
        <VSCodeBadge>
          <div
            style={colorStyle}
            className={`codicon codicon-${
              result === 'Success' ? 'pass' : 'error'
            }`}
          />
          &nbsp;
          <div style={colorStyle}>Match</div>
        </VSCodeBadge>
        &nbsp;
      </>
    );
  })();

  const errorTooltip = (() => {
    if (cmdData.errors.length === 0) return <></>;

    return (
      <>
        <VSCodeDivider />
        <div className="tooltip-error">
          <ul>
            {cmdData.errors.map((error, i) => (
              <li key={i}>{error}</li>
            ))}
          </ul>
        </div>
      </>
    );
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

  const tooltip = (
    <div className="tooltip">
      <Code>{cmdData.display}</Code>
      {errorTooltip}
    </div>
  );

  return (
    <NodeWrap
      selected={isCurrentCmd}
      error={cmdData.errors.length > 0}
      noSourceHandle={isFinal}
      noTargetHandle={!hasParent}
      tooltip={tooltip}
      width={width}
      height={height}
      active={isActive}
    >
      <pre>{cmdData.display}</pre>
      <div className="node-button-row">
        {matchBadge}
        <VSCodeButton
          appearance="icon"
          aria-label="Jump here"
          title="Jump here"
          disabled={isCurrentCmd}
          onClick={isCurrentCmd ? undefined : jump}
        >
          <span className="codicon codicon-target" />
        </VSCodeButton>
        {expandButton}
      </div>
    </NodeWrap>
  );
};

export default ExecMapNode;
