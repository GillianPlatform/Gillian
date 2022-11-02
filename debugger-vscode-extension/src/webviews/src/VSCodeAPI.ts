import { BranchCase, MessageFromWebview, MessageToWebview } from '../../types';

declare const acquireVsCodeApi: () => VSCodeApi;

interface VSCodeApi {
  getState: () => any
  setState: (newState: any) => any
  postMessage: (message: any) => void
}

class VSCodeWrapper {
  private readonly vscodeApi: VSCodeApi = acquireVsCodeApi()

  /**
   * Send message to the extension framework.
   * @param message
   */
  public postMessage(message: MessageFromWebview): void {
    this.vscodeApi.postMessage(message);
  }

  /**
   * Add listener for messages from extension framework.
   * @param callback called when the extension sends a message
   * @returns function to clean up the message eventListener.
   */
  public onMessage(handler: (message: MessageEvent<MessageToWebview>) => void): () => void {
    const callback = (message : MessageEvent<any>) => { 
      handler(message);
    };
    window.addEventListener('message', callback);
    return () => window.removeEventListener('message', callback);
  }
}

export const execSpecific = (procName: string, prevId: number, branchCase: BranchCase) => {
  VSCodeAPI.postMessage({
    type: 'request_exec_specific',
    procName,
    prevId,
    branchCase,
  });
};

export const jumpToId = (procName: string, id: number) => {
  VSCodeAPI.postMessage({
    type: 'request_jump',
    procName,
    cmdId: id
  });
};

export const startProc = (procName: string) => {
  VSCodeAPI.postMessage({ type: 'request_start_proc', procName });
};

// Singleton to prevent multiple fetches of VsCodeAPI.
const VSCodeAPI: VSCodeWrapper = new VSCodeWrapper();
export default VSCodeAPI;
