import React from 'react';
import { DebugState } from '../../types';
import ExecMapView from './ExecMap/ExecMapView';

type Props = {
  state: DebugState;
};

const DebuggerPanel = ({ state }: Props) => {
  return (
    <div style={{ position: 'absolute', width: '100vw', height: '100vh', top: '0', left: '0' }}>
      <ExecMapView {...{ state }} />
    </div>
  );
};

export default DebuggerPanel;
