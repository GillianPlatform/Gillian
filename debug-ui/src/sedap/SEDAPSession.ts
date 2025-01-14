import * as vscode from 'vscode';
import { DebugSession, Disposable, ViewColumn, WebviewOptions, WebviewPanel, WebviewPanelOptions } from 'vscode';

type SEDAPSessionPartialProps = {
  panelName?: string,
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
  private sessionId: string;
  private debugType: string;
  private panel: WebviewPanel | undefined;
  private getWebviewHtml: (panel: WebviewPanel) => string;
  private webviewOptions: WebviewPanelOptions & WebviewOptions;
  private webviewShowOptions: ViewColumn | { viewColumn: ViewColumn; preserveFocus?: boolean };
  private disposables: Disposable[] = [];
  private disposed = false;
  private disposeListeners: ((session: SEDAPSession) => void)[] = [];
  // #endregion

  private handleCustomEvent(event: string, body: any) {
  }

  private handleWebviewMessage(type: string, body: any) {
  }

  // #region Webview handling

  public getWebviewPanel(): vscode.WebviewPanel | undefined {
    return this.panel;
  }

  private createWebview() {
    let panel = vscode.window.createWebviewPanel(this.debugType, this.panelName, this.webviewShowOptions, this.webviewOptions);

    panel.webview.html = this.getWebviewHtml(panel);

    panel.webview.onDidReceiveMessage(msg => {
      this.handleWebviewMessage(msg.type, msg.body);
    });
  
    panel.onDidDispose(() => {
      this.panel = undefined;
    });
    
    this.panel = panel;
  }

  public showWebviewPanel(force: boolean = false) {
    if (!force && vscode.debug.activeDebugSession?.id !== this.sessionId) {
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
    return this.sessionId;
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
    session,
    getWebviewHtml,
    webviewOptions = {},
    webviewShowOptions = ViewColumn.Two,
    showPanel = false
  }: SEDAPSessionProps) {
    this.panelName = panelName || session.name;
    this.sessionId = session.id;
    this.debugType = session.type;
    this.webviewOptions = { ...defaultOptions, ...webviewOptions };
    this.webviewShowOptions = webviewShowOptions;
    this.getWebviewHtml = getWebviewHtml;

    vscode.debug.onDidReceiveDebugSessionCustomEvent(({ session, event, body }) => {
      if (session.id === this.sessionId) {
        this.handleCustomEvent(event, body);
      }
    }, this, this.disposables);

    vscode.debug.onDidTerminateDebugSession(session => {
      if (session.id === this.sessionId) {
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
