import * as vscode from 'vscode';
import { DebugSession, Disposable, ViewColumn, WebviewOptions, WebviewPanel, WebviewPanelOptions } from 'vscode';

type SEDAPSessionPartialProps = {
  panelName?: string,
  panelIcon?: vscode.Uri,
  getWebviewHtml: (panel: WebviewPanel) => string,
  webviewOptions?: WebviewPanelOptions & WebviewOptions,
  webviewShowOptions?: ViewColumn | { viewColumn: ViewColumn; preserveFocus?: boolean }
  showPanel?: boolean,
};

type SEDAPSessionProps = SEDAPSessionPartialProps & {
  session: DebugSession,
};

const defaultOptions: WebviewPanelOptions & WebviewOptions = {
  retainContextWhenHidden: true,
  enableScripts: true,
};

export default class SEDAPSession implements Disposable {
  // #region Fields
  private panelName: string;
  private panelIcon: vscode.Uri | undefined;
  private session: DebugSession;
  private debugType: string;
  private panel: WebviewPanel | undefined;
  private getWebviewHtml: (panel: WebviewPanel) => string;
  private webviewOptions: WebviewPanelOptions & WebviewOptions;
  private webviewShowOptions: ViewColumn | { viewColumn: ViewColumn; preserveFocus?: boolean };
  private disposables: Disposable[] = [];
  private disposed = false;
  private disposeListeners: ((session: SEDAPSession) => void)[] = [];
  // #endregion

  private async cusomDebuggerCommand(command: string, args: any): Promise<any> {
    return await this.session.customRequest(command, args);
  }

  private async sendWebviewMessage(type: string, body: any): Promise<boolean> {
    if (!this.panel) {
      return false;
    }
    return this.panel.webview.postMessage({ type, body });
  }

  private handleCustomEvent(event: string, body: any) {
    this.panel?.webview?.postMessage({ type: "debuggerEvent", body: { event, body } });
  }

  private handleWebviewMessage(type: string, body: any) {
    const panel = this.panel!;
    switch (type) {
      case "setPanelTitle":
        if (typeof body === 'string') {
          panel.title = body;
          break;
        }
      case "debuggerCommand":
        if (body) {
          const { command, commandId, args } = body;
          if (typeof body.command === "string") {
            this.cusomDebuggerCommand(command, args).then((result) => {
              this.sendWebviewMessage("debuggerCommandResult", { commandId: commandId, result }).then((success) => { console.log("sent", success); });
            });
            break;
          }
        }
      default:
        console.warn('Unknown or invalid webview message:', type, body);
    }
  }

  // #region Webview handling

  public getWebviewPanel(): vscode.WebviewPanel | undefined {
    return this.panel;
  }

  private createWebview() {
    let panel = vscode.window.createWebviewPanel(this.debugType, this.panelName, this.webviewShowOptions, this.webviewOptions);

    panel.webview.html = this.getWebviewHtml(panel);
    panel.iconPath = this.panelIcon;

    panel.webview.onDidReceiveMessage(msg => {
      this.handleWebviewMessage(msg.type, msg.body);
    });
  
    this.panel = panel;
    panel.onDidDispose(() => {
      this.panel = undefined;
    });
  }

  public showWebviewPanel(force: boolean = false) {
    if (!force && vscode.debug.activeDebugSession?.id !== this.session.id) {
      return;
    }
    if (this.panel) {
      this.panel.reveal();
    } else {
      this.createWebview();
    }
  }

  // #endregion

  public getSessionId() {
    return this.session.id;
  }

  public onDispose(listener: (session: SEDAPSession) => void): Disposable {
    this.disposeListeners.push(listener);
    return {
      dispose: () => {
        this.disposeListeners = this.disposeListeners.filter(x => x !== listener);
      }
    };
  }

  public dispose() {
    if (!this.disposed) {
      this.disposables.forEach(x => x.dispose());
      this.panel?.dispose();
      this.disposeListeners.forEach(f => f(this));
      this.disposed = true;
    }
  }

  public constructor({
    panelName,
    panelIcon,
    session,
    getWebviewHtml,
    webviewOptions = {},
    webviewShowOptions = ViewColumn.Two,
    showPanel = false
  }: SEDAPSessionProps) {
    this.panelName = panelName || session.name;
    this.panelIcon = panelIcon;
    this.session = session;
    this.debugType = session.type;
    this.webviewOptions = { ...defaultOptions, ...webviewOptions };
    this.webviewShowOptions = webviewShowOptions;
    this.getWebviewHtml = getWebviewHtml;

    vscode.debug.onDidReceiveDebugSessionCustomEvent(({ session, event, body }) => {
      if (session.id === this.session.id) {
        this.handleCustomEvent(event, body);
      }
    }, this, this.disposables);

    vscode.debug.onDidTerminateDebugSession(session => {
      if (session.id === this.session.id) {
        this.dispose();
      }
    });

    if (showPanel) {
      this.showWebviewPanel();
    }
  }

  public static autoAttachIf(
    p: (session: DebugSession) => SEDAPSessionPartialProps | null,
    onSessionCreated: (session: SEDAPSession) => void = () => {},
  ): Disposable {
    return vscode.debug.onDidStartDebugSession(session => {
      const props = p(session);
      if (props) {
        const sedapSession = new SEDAPSession({ ...props, session });
        onSessionCreated(sedapSession);
      }
    });
  }

  public static autoAttachOnType(
    type: string,
    props: SEDAPSessionPartialProps,
    onSessionCreated: (session: SEDAPSession) => void = () => {},
  ): Disposable {
    return SEDAPSession.autoAttachIf(
      session => session.type === type ? props : null,
      onSessionCreated
    );
  }
}
