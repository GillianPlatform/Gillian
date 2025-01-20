import { MapNode, MapUpdateEventBody } from "@sedap/types";
import { debuggerCommand, DebuggerEventMessage, Message, useVSCode } from "./vscode";
import { useCallback, useEffect, useState } from "react";
import { NodePrev } from "@sedap/react";

export type Nodes = {
  [k: string]: MapNode;
};

export type Roots = {
  [k: string]: string;
};

export type MapState = {
  roots: Roots;
  nodes: Nodes;
  selectedNodes: readonly string[];
  onNodeSelected: (id: string) => void;
  onNextStepSelected: (prev: NodePrev) => void;
};

export function useSEDAPMap(): MapState {
  const [initCommandSent, setInitCommandSent] = useState(false);
  const [initialised, setInitialised] = useState(false);
  const [nodes, setNodes] = useState({} as Nodes);
  const [roots, setRoots] = useState({} as Roots);
  const [selectedNodes, setSelectedNodes] = useState([] as readonly string[]);

  const handleMapUpdate = useCallback(
    (body: MapUpdateEventBody) => {
      if (!initialised && !body.reset) {
        return;
      }
      setRoots(body.roots || {});
      setSelectedNodes(body.currentSteps || []);
      const newNodes = body.reset ? {} : { ...nodes };
      Object.entries(body.nodes || {}).forEach(([id, node]) => {
        if (node === null) {
          delete newNodes[id];
        } else {
          newNodes[id] = node;
        }
      });
      setNodes(newNodes);
      console.log("State updated", body);
    },
    [initialised, nodes],
  );

  const onMessageReceived = (message_: unknown) => {
    const message = message_ as Message;
    if (message && message.type === "debuggerEvent") {
      const body = message.body as DebuggerEventMessage;
      if (body.event === "mapUpdate") {
        handleMapUpdate(body.body as MapUpdateEventBody);
      }
    }
  };
  useVSCode(onMessageReceived);

  useEffect(() => {
    if (!initCommandSent) {
      debuggerCommand("getFullMap", {}).then((result) => {
        handleMapUpdate(result as MapUpdateEventBody);
        setInitialised(true);
      });
      setInitCommandSent(true);
    }
  }, [initCommandSent, handleMapUpdate]);

  const onNodeSelected = (stepId: string) => {
    debuggerCommand("jump", { stepId });
  };

  const onNextStepSelected = (prev: NodePrev) => {
    debuggerCommand("stepSpecific", { stepId: prev.id, branchCase: prev.case });
  };

  return { nodes, roots, selectedNodes, onNodeSelected, onNextStepSelected };
}
