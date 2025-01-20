import "./App.css";
import { TraceView, TraceViewProps } from "@sedap/react";
import { useSEDAPMap } from "./sedapMap";
import IconButton from "./IconButton";
import Badge from "./Badge";

function isEmpty(obj: Record<string, unknown>) {
  for (const prop in obj) {
    if (Object.prototype.hasOwnProperty.call(obj, prop)) {
      return false;
    }
  }

  return true;
}

function App() {
  const { roots, nodes, selectedNodes, onNodeSelected, onNextStepSelected } = useSEDAPMap();

  const root = Object.values(roots)[0];

  let body = (
    <b>
      <i>Empty map!</i>
    </b>
  );
  if (!isEmpty(roots) && !isEmpty(nodes)) {
    const props: TraceViewProps = {
      root,
      nodes,
      selectedNodes,
      onNodeSelected,
      onNextStepSelected,
      componentOverrides: {
        button: IconButton,
        badge: Badge,
      },
    };
    body = (
      <>
        <TraceView {...props} />
      </>
    );
  }

  return <div style={{ height: "100vh", width: "100vw" }}>{body}</div>;
}

export default App;
