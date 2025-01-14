import { ExtensionContext, Uri, WebviewPanel } from "vscode";

const scriptSrcs = [
  "web/dist/index.js",
];
const styleSrcs = [
  "web/dist/index.css",
];

export const getWebviewHtml = (context: ExtensionContext) => ({ webview }: WebviewPanel): string => {
  const scripts = scriptSrcs.map((src) => {
    const script = webview.asWebviewUri(Uri.joinPath(context.extensionUri, src));
    return `<script src="${script}"></script>`;
  }).join('\n');
  const styles = styleSrcs.map((src) => {
    const style = webview.asWebviewUri(Uri.joinPath(context.extensionUri, src));
    return `<link rel="stylesheet" href="${style}">`;
  }).join('\n');
  
  return `
    <!doctype html>
    <html lang="en">
      <head>
        <meta charset="UTF-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        ${styles}
      </head>
      <body>
        <div id="root"></div>
        ${scripts}
      </body>
    </html>
  `;
};
