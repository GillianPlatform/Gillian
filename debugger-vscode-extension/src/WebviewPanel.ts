import * as vscode from 'vscode'

import { getNonce } from './lib'

export class WebviewPanel {
  public static currentPanel: WebviewPanel | undefined
  private readonly _panel: vscode.WebviewPanel
  private _disposables: vscode.Disposable[] = []

  private constructor(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
    this._panel = panel
    this._panel.onDidDispose(this.dispose, null, this._disposables)
    this._panel.webview.html = this.getWebviewContent(
      this._panel.webview,
      extensionUri
    )
  }

  public static render(extensionUri: vscode.Uri) {
    if (WebviewPanel.currentPanel) {
      WebviewPanel.currentPanel._panel.reveal(vscode.ViewColumn.One)
    } else {
      const panel = vscode.window.createWebviewPanel(
        'hello-world',
        'Hello World',
        vscode.ViewColumn.One,
        {
          enableScripts: true,
        }
      )

      WebviewPanel.currentPanel = new WebviewPanel(panel, extensionUri)
    }
  }

  public dispose() {
    WebviewPanel.currentPanel = undefined

    this._panel.dispose()

    while (this._disposables.length) {
      const disposable = this._disposables.pop()
      if (disposable) {
        disposable.dispose()
      }
    }
  }

  /**
   * Get the static html used for the editor webviews.
   */
  private getWebviewContent(
    webview: vscode.Webview,
    extensionUri: vscode.Uri
  ): string {
    // Local path to script and css for the webview
    const scriptUri = webview.asWebviewUri(
      vscode.Uri.joinPath(extensionUri, 'out/webviews/index.js')
    )

    const styleVSCodeUri = webview.asWebviewUri(
      vscode.Uri.joinPath(extensionUri, 'out/webviews/index.css')
    )
    const codiconsUri = webview.asWebviewUri(
      vscode.Uri.joinPath(extensionUri, 'out/webviews/public/codicon.css')
    )

    // Use a nonce to whitelist which scripts can be run
    const nonce = getNonce()
    console.log(nonce)

    return /* html */ `
      <!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="UTF-8">

        <!--
        Use a content security policy to only allow loading images from https or from our extension directory,
        and only allow scripts that have a specific nonce.
        -->
        <meta http-equiv="Content-Security-Policy" content="default-src 'none'; img-src ${webview.cspSource} 'self' data:; style-src ${webview.cspSource}; script-src 'nonce-${nonce}'; font-src ${webview.cspSource};">

        <meta name="viewport" content="width=device-width, initial-scale=1.0">


        <link href="${styleVSCodeUri}" rel="stylesheet" />
        <link href="${codiconsUri}" rel="stylesheet" />
        <script nonce="${nonce}">
          window.acquireVsCodeApi = acquireVsCodeApi;
        </script>

        <title>Gillian Debugger</title>
      </head>
      <body>
        <div id="root"></div>
        <script nonce="${nonce}" src="${scriptUri}"></script>
      </body>
      </html>`
  }
}
