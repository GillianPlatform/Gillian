import { MapNode, MapUpdateEventBody } from "@sedap/types";
import { debuggerCommand, DebuggerEventMessage, Message, useVSCode } from "./vscode";
import { useCallback, useEffect, useState } from "react";

export type Nodes = {
  [k: string]: MapNode;
}

export type Roots = {
  [k: string]: string;
}

export type MapState = {
  roots: Roots;
  nodes: Nodes;
}

export function useMapState(): MapState {
  const [initCommandSent, setInitCommandSent] = useState(false);
  const [initialised, setInitialised] = useState(false);
  const [nodes, setNodes] = useState({} as Nodes);
  const [roots, setRoots] = useState({} as Roots);

  const handleMapUpdate = useCallback((body: MapUpdateEventBody) => {
    if (!initialised && !body.reset) {
      return;
    }
    setRoots(body.roots || {});
    const newNodes = body.reset ? {} : { ...nodes };
    Object.entries(body.nodes || {}).forEach(([id, node]) => {
      if (node === null) {
        delete newNodes[id];
      } else {
        newNodes[id] = node;
      }
    })
    setNodes(newNodes);
    console.log("State updated", { nodes: newNodes, oldNodes: nodes, roots: body.roots });
  }, [initialised, nodes]);

  const onMessageReceived = (message_: unknown) => {
    const message = message_ as Message;
    if (message && message.type === "debuggerEvent") {
      const body = message.body as DebuggerEventMessage;
      if (body.event === "mapUpdate") {
        handleMapUpdate(body.body as MapUpdateEventBody);
      }
    }
  }
  useVSCode(onMessageReceived);

  useEffect(() => {
    if (!initCommandSent) {
      debuggerCommand("getFullMap", {}).then(result => {
        handleMapUpdate(result as MapUpdateEventBody);
        setInitialised(true);
      });
      setInitCommandSent(true);
    }
  }, [ initCommandSent, handleMapUpdate ]);

  return { nodes, roots };
}
