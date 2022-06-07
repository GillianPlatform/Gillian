import React, { FC } from 'react';
import { Handle, NodeProps, Position } from 'react-flow-renderer';
import { VSCodeButton } from '@vscode/webview-ui-toolkit/react';
import { BranchCase, CmdData } from '../../../types';
import VSCodeAPI from '../VSCodeAPI';
import { NODE_WIDTH, NODE_HEIGHT } from './ExecMapView';

import './ExecMapView.css';

export type ExecMapNodeData =
  | {
      type: 'Cmd';
      cmdData: CmdData;
      isFinal: boolean;
      isCurrentCmd: boolean;
    }
  | {
      type: 'Empty';
      prevId: number;
      branchCase: BranchCase | null;
    }
  | {
      type: 'Root';
      procName: string;
    };

type NodeWrapProps = {
  noTargetHandle?: boolean;
  noSourceHandle?: boolean;
  width?: number;
  height?: number;
  classes: string[];
};

const HANDLE_STYLE = { background: '#555' };

const NodeWrap: FC<NodeWrapProps> = ({
  children,
  noTargetHandle = false,
  noSourceHandle = false,
  width = NODE_WIDTH,
  height = NODE_HEIGHT,
  classes,
}) => (
  <div
    className={`node-wrap${
      classes.length === 0 ? '' : ' ' + classes.join(' ')
    }`}
    style={{
      width: `${width}px`,
      height: `${height}px`,
      position: 'absolute',
    }}
  >
    {noTargetHandle ? undefined : (
      <Handle
        type="target"
        position={Position.Top}
        style={HANDLE_STYLE}
        isConnectable={false}
      />
    )}
    <div
      className="node-content"
      style={{ width: `${width}px`, height: `${height}px` }}
    >
      {children}
    </div>
    {noSourceHandle ? undefined : (
      <Handle
        type="source"
        position={Position.Bottom}
        style={HANDLE_STYLE}
        isConnectable={false}
      />
    )}
    <div className="node-background" />
  </div>
);

const ExecMapNode = ({ data }: NodeProps<ExecMapNodeData>) => {
  if (data.type === 'Empty') {
    const exec = () => {
      const { prevId, branchCase } = data;
      VSCodeAPI.postMessage({
        type: 'request_exec_specific',
        prevId,
        branchCase,
      });
    };

    return (
      <NodeWrap classes={['node-empty']} noSourceHandle width={NODE_HEIGHT}>
        <VSCodeButton
          appearance="icon"
          aria-label="Execute here"
          title="Execute here"
          onClick={exec}
        >
          <span className="codicon codicon-play" />
        </VSCodeButton>
      </NodeWrap>
    );
  }

  if (data.type === 'Root') {
    return (
      <NodeWrap classes={['node-root']} noTargetHandle>
        <pre>
          <b>{data.procName}</b>
        </pre>
      </NodeWrap>
    );
  }

  const { cmdData, isFinal, isCurrentCmd } = data;

  const jump = () => {
    VSCodeAPI.postMessage({ type: 'request_jump', cmdId: cmdData.id });
  };

  return (
    <NodeWrap
      classes={isCurrentCmd ? ['node-selected'] : []}
      noSourceHandle={isFinal}
    >
      <pre>{cmdData.display}</pre>
      <VSCodeButton
        appearance="icon"
        aria-label="Jump here"
        title="Jump here"
        disabled={isCurrentCmd}
        onClick={isCurrentCmd ? undefined : jump}
      >
        <span className="codicon codicon-target" />
      </VSCodeButton>
    </NodeWrap>
  );
};

export default ExecMapNode;
