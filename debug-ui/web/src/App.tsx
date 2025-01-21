import "./App.css";
import { TraceView, TraceViewProps } from "@sedap/react";
import { useSEDAPMap } from "./sedapMap";
import IconButton from "./IconButton";
import Badge from "./Badge";
import styled from "styled-components";
import Sidebar, { Subst } from "./Sidebar";

function isEmpty(obj: Record<string, unknown>) {
  for (const prop in obj) {
    if (Object.prototype.hasOwnProperty.call(obj, prop)) {
      return false;
    }
  }

  return true;
}

const Wrap = styled.div`
  width: 100vw;
  height: 100vh;
  display: flex;
  flex-direction: row;
  @media (max-width: 500px) {
    flex-direction: column;
  }
`;
const MapWrap = styled.div`
  flex-grow: 3;
`;

type Ext = {
  substs: Record<string, Subst[] | undefined>;
};

function App() {
  const { roots, nodes, selectedNodes, onNodeSelected, onNextStepSelected, ext } = useSEDAPMap();

  if (isEmpty(roots) || isEmpty(nodes)) {
    return (
      <b>
        <i>Empty map!</i>
      </b>
    );
  }

  const [proc_name, root] = Object.entries(roots)[0];
  const substs = (ext as Ext).substs[proc_name];
  const traceViewProps: TraceViewProps = {
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

  return (
    <Wrap>
      <MapWrap>
        <TraceView {...traceViewProps} />
      </MapWrap>
      <Sidebar {...{ substs, selectedNodes, onNodeSelected }} />
    </Wrap>
  );
}

export default App;
