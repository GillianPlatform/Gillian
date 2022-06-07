import React from 'react';
import useDebugStore from './store';
import WaitingForState from './WaitingForState';
import DebuggerPanel from './DebuggerPanel';

const App = () => {
  const { state } = useDebugStore();

  const content =
    state === undefined ? (
      <WaitingForState />
    ) : (
      <DebuggerPanel {...{ state }} />
    );

  return <div style={{ margin: '10px' }}>{content}</div>;
};

export default App;
