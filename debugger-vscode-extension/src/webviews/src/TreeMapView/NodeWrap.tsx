import React, { ReactNode, FC, useContext } from 'react';
import { Handle, NodeProps, Position } from 'react-flow-renderer';
import Tippy from '@tippyjs/react';
import 'tippy.js/dist/tippy.css';
import { NODE_WIDTH, NODE_HEIGHT, FlowRefContext } from './TreeMapView';

import './NodeWrap.css';

type NodeWrapProps = {
  noTargetHandle?: boolean;
  noSourceHandle?: boolean;
  width?: number;
  height?: number;
  root?: boolean;
  selected?: boolean;
  error?: boolean;
  classes?: string[];
  tooltip?: ReactNode;
};

const HANDLE_STYLE = { background: '#555' };

const NodeWrap: FC<NodeWrapProps> = ({
  children,
  noTargetHandle = false,
  noSourceHandle = false,
  width = NODE_WIDTH,
  height = NODE_HEIGHT,
  root = false,
  selected = false,
  error = false,
  classes = [],
  tooltip,
}) => {
  const flowRef = useContext(FlowRefContext);

  classes.unshift('node-wrap');
  if (selected) {
    classes.unshift('node-selected');
  }
  if (root) {
    classes.unshift('node-root');
  }
  if (error) {
    classes.unshift('node-error');
  }

  const getFlowViewport = () => {
    const flow = flowRef!.current;
    const elems = flow.getElementsByClassName('react-flow__viewport');
    const elem = elems[0];
    return elem;
  };

  const nodeWrap = (
    <div
      className={classes.join(' ')}
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

  if (tooltip) {
    return (
      <Tippy
        content={tooltip}
        maxWidth={`${NODE_WIDTH * 2}px`}
        interactive
        className="tooltip"
        // Append to viewport, since node width constrains tooltip width
        appendTo={getFlowViewport}
        placement="bottom"
      >
        {nodeWrap}
      </Tippy>
    );
  }
  return nodeWrap;
};

export default NodeWrap;
