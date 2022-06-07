import React from 'react';
import {
  VSCodeButton,
  VSCodeProgressRing,
} from '@vscode/webview-ui-toolkit/react';
import VSCodeAPI from './VSCodeAPI';

const WaitingForState = () => (
  <div
    style={{
      width: '100%',
      height: '100%',
      display: 'flex',
      justifyContent: 'center',
      alignItems: 'center',
      flexDirection: 'column',
    }}
  >
    <VSCodeProgressRing />
    <h2>Waiting for debugger...</h2>
    <VSCodeButton
      onClick={() => {
        VSCodeAPI.postMessage({ type: 'request_state_update' });
      }}
    >
      Manual Refresh
    </VSCodeButton>
  </div>
);

export default WaitingForState;
