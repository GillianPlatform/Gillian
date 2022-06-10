import React from 'react';
import { Handle, NodeProps, Position } from 'react-flow-renderer';
import { NODE_WIDTH, NODE_HEIGHT } from './TreeMapView';

import './NodeWrap.css';

type NodeWrapProps = {
  noTargetHandle?: boolean;
  noSourceHandle?: boolean;
  width?: number;
  height?: number;
  root?: boolean;
  selected?: boolean;
  classes?: string[];
};

const HANDLE_STYLE = { background: '#555' };

const NodeWrap: React.FC<NodeWrapProps> = ({
  children,
  noTargetHandle = false,
  noSourceHandle = false,
  width = NODE_WIDTH,
  height = NODE_HEIGHT,
  root = false,
  selected = false,
  classes = [],
}) => {
  classes.unshift('node-wrap');
  if (selected) {
    classes.unshift('node-selected');
  }
  if (root) {
    classes.unshift('node-root');
  }

  return (
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
};

export default NodeWrap;
