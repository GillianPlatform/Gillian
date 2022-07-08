import React from 'react';
import ReactDOM from 'react-dom';
import App from './App';
import useDebugStore from './store';
import './vscode.css';
import VSCodeAPI from './VSCodeAPI';

VSCodeAPI.onMessage(e => {
  const message = e.data;
  const store = useDebugStore.getState();

  if (message.type === 'state_update') {
    store.updateDebugState(message.state);
    const { unifys } = message.state;
    if (unifys.length > 0) {
      const [unifyId, ,] = unifys[0];
      const isInStore = store.selectBaseUnification(unifyId);
      if (!isInStore) {
        VSCodeAPI.postMessage({
          type: 'request_unification',
          id: unifyId,
        });
      }
    } else {
      store.clearUnification();
    }
  }

  if (message.type === 'unify_update') {
    store.loadUnification(message.unifyId, message.unifyMap);
  }
});

ReactDOM.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
  document.getElementById('root')
);

VSCodeAPI.postMessage({ type: 'request_state_update' });
