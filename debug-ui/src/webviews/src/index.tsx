import React from 'react';
import ReactDOM from 'react-dom';
import App from './App';
import useDebugStore from './store';
import './vscode.css';
import VSCodeAPI, { requestMatching } from './VSCodeAPI';
import * as events from './events';

VSCodeAPI.onMessage(e => {
  const message = e.data;
  const store = useDebugStore.getState();

  if (message.type === 'state_update') {
    const { state } = message;
    store.updateDebuggerState(state);
    const currentProcState = state.procs[state.currentProc];
    const { matches } = currentProcState;
    if (matches.length > 0) {
      const matchId = matches[0].id;
      const isInStore = store.selectBaseMatching(matchId);
      if (!isInStore) {
        requestMatching(matchId);
      }
    } else {
      store.clearMatching();
    }
  } else if (message.type === 'match_update') {
    store.loadMatching(message.matchId, message.matchMap);
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
