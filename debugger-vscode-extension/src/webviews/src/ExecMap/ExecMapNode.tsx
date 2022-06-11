import React from 'react';
import { VSCodeBadge, VSCodeButton } from '@vscode/webview-ui-toolkit/react';
import { CmdData } from '../../../types';
import { NODE_HEIGHT } from '../TreeMapView/TreeMapView';
import NodeWrap from '../TreeMapView/NodeWrap';
import { NodeProps } from 'react-flow-renderer';
import { Code } from '../util';

export type ExecMapNodeData =
  | {
      type: 'Cmd';
      cmdData: CmdData;
      isFinal: boolean;
      isCurrentCmd: boolean;
      jump: () => void;
    }
  | {
      type: 'Empty';
      exec: () => void;
    }
  | {
      type: 'Root';
      procName: string;
    };

const ExecMapNode = ({ data }: NodeProps<ExecMapNodeData>) => {
  if (data.type === 'Empty') {
    return (
      <NodeWrap classes={['node-empty']} noSourceHandle width={NODE_HEIGHT}>
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

  if (data.type === 'Root') {
    return (
      <NodeWrap root noTargetHandle>
        <span className="node-title">
          <Code>{data.procName}</Code>
        </span>
      </NodeWrap>
    );
  }

  const { cmdData, isFinal, isCurrentCmd } = data;

  const unifyBadge = (() => {
    if (cmdData.unifyResult) {
      const [result] = cmdData.unifyResult;
      return (
        <>
          <VSCodeBadge>
            <div
              className={`codicon codicon-${
                result === 'Success' ? 'pass' : 'error'
              }`}
            />
            &nbsp; Unify
          </VSCodeBadge>
          &nbsp;
        </>
      );
    } else {
      return <></>;
    }
  })();

  return (
    <NodeWrap selected={isCurrentCmd} noSourceHandle={isFinal}>
      <pre>{cmdData.display}</pre>
      <div className="node-button-row">
        {unifyBadge}
        <VSCodeButton
          appearance="icon"
          aria-label="Jump here"
          title="Jump here"
          disabled={isCurrentCmd}
          onClick={isCurrentCmd ? undefined : data.jump}
        >
          <span className="codicon codicon-target" />
        </VSCodeButton>
      </div>
    </NodeWrap>
  );
};

export default ExecMapNode;
