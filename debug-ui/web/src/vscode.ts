import { useEffect } from "react";
import { WebviewApi } from "vscode-webview";

type MessageHandler = (message: unknown) => void

export type Message = {
  type?: string;
  body: unknown;
} | undefined

export type DebuggerEventMessage = {
  event: string;
  body: unknown;
}

export type DebuggerCommandResult = {
  commandId: string;
  result: unknown;
}

export const vscodeApi = acquireVsCodeApi()

function onMessage(f: MessageHandler): () => void {
  console.log("listening");
  function callback(event: MessageEvent<unknown>) {
    f(event.data);
  }
  window.addEventListener("message", callback);
  return () => {
    window.removeEventListener("message", callback);
  };
}

export function useVSCode(messageHandler: MessageHandler): WebviewApi<unknown>['postMessage'] {
  useEffect(() => {
    onMessage(messageHandler)
  });

  return vscodeApi.postMessage;
}

let commandCount = 0;
export function debuggerCommand(command: string, args: unknown): Promise<unknown> {
  return new Promise(resolve => {
    const commandId = `${commandCount++}`;

    const cancelListener = onMessage((message_) => {
      const message = message_ as Message;
      if (message && message.type === "debuggerCommandResult") {
        const body = message.body as DebuggerCommandResult;
        if (body.commandId === commandId) {
          cancelListener();
          resolve(body.result);
        }
      }
    });

    vscodeApi.postMessage({ type: "debuggerCommand", body: { command, commandId, args } });
  });
}
