import './App.css'
import { TraceView } from '@sedap/react';
import { useMapState } from './mapState';

function isEmpty(obj: Record<string, unknown>) {
  for (const prop in obj) {
    if (Object.prototype.hasOwnProperty.call(obj, prop)) {
      return false;
    }
  }

  return true
}

function App() {
  const { roots, nodes } = useMapState();

  const root = Object.values(roots)[0];

  let body = (
    <b><i>Empty map!</i></b>
  );
  if (!isEmpty(roots) && !isEmpty(nodes)) {
    body = (
      <TraceView {...{root, nodes}} />
    );
  }

  return (
    <div style={{height: '100vh', width: '100vw'}}>
      {body}
    </div>
  )
}

export default App
