import React from 'react';
import ReactDOM from 'react-dom';
import App from './App';
import useDebugStore from './store';
import './vscode.css';
import VSCodeAPI, { requestUnification } from './VSCodeAPI';
import * as events from './events';

VSCodeAPI.onMessage(e => {
  const message = e.data;
  const store = useDebugStore.getState();

  if (message.type === 'state_update') {
    const { state } = message;
    store.updateDebuggerState(state);
    const currentProcState = state.procs[state.currentProc];
    const { unifys } = currentProcState;
    if (unifys.length > 0) {
      const unifyId = unifys[0].id;
      const isInStore = store.selectBaseUnification(unifyId);
      if (!isInStore) {
        requestUnification(unifyId);
      }
    } else {
      store.clearUnification();
    }
  } else if (message.type === 'unify_update') {
    store.loadUnification(message.unifyId, message.unifyMap);
  } else if (message.type === 'reset_view') {
    events.publish('resetView');
  } else if (message.type === 'clear_state') {
    store.clear();
  }
});

ReactDOM.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
  document.getElementById('root')
);

VSCodeAPI.postMessage({ type: 'request_state_update' });
