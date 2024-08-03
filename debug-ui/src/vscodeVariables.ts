// Adapted from https://github.com/DominicVonk/vscode-variables (MIT License)

import * as vscode from 'vscode';
import { env } from 'process';
import { sep } from 'path';

export default function variables(s: string): string {
  const workspaces = vscode.workspace.workspaceFolders || [];
  const workspace = workspaces[0];
  s = s.replace(/\${workspaceFolder}/g, workspace?.uri.fsPath);
  s = s.replace(/\${workspaceFolderBasename}/g, workspace?.name);
  s = s.replace(/\${fileWorkspaceFolder}/g, workspace?.uri.fsPath);
  s = s.replace(/\${pathSeparator}/g, sep);
  s = s.replace(/\${env:(.*?)}/g, variable => {
    const matches = variable.match(/\${env:(.*?)}/);
    if (matches) return env[matches[1]] || variable;
    return variable; // leave untouched
  });
  s = s.replace(/\${config:(.*?)}/g, function (variable) {
    const matches = variable.match(/\${config:(.*?)}/);
    if (matches)
      return vscode.workspace.getConfiguration().get(matches[1], variable);
    return variable; // leave untouched
  });

  return s;
}
