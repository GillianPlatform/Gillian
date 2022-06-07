import React from 'react';
import ReactDOM from 'react-dom';
import App from './App';
import useDebugStore from './store';
import './vscode.css';
import VSCodeAPI from './VSCodeAPI';

VSCodeAPI.onMessage(e => {
  const message = e.data;
  if (message.type === 'state_update') {
    useDebugStore.setState({
      state: message.state,
    });
  }
});

ReactDOM.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
  document.getElementById('root')
);

VSCodeAPI.postMessage({ type: 'request_state_update' });
