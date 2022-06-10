import React from 'react';
import useStore from './store';
import Loading from './util/Loading';
import DebuggerPanel from './DebuggerPanel/DebuggerPanel';
import VSCodeAPI from './VSCodeAPI';

import './style.css';

const App = () => {
  const { debugState } = useStore();

  const refresh = () => {
    VSCodeAPI.postMessage({ type: 'request_state_update' });
  };

  const content =
    debugState === undefined ? <Loading {...{ refresh }} /> : <DebuggerPanel />;

  return (
    <div style={{ margin: '10px', boxSizing: 'border-box' }}>{content}</div>
  );
};

export default App;
