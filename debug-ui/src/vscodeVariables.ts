// Adapted from https://github.com/DominicVonk/vscode-variables (MIT License)

import * as vscode from "vscode";
import { env } from "process";
import { sep } from "path";

function selectWorkspace(): vscode.WorkspaceFolder {
  const workspaces = vscode.workspace.workspaceFolders || [];
  if (workspaces.length === 0) {
    const msg = "No workspace folder found!";
    vscode.window.showErrorMessage(msg);
    throw new Error(msg);
  }
  if (workspaces.length === 1) {
    return workspaces[0];
  }
  // TODO: Multi-root workspaces
  const msg = "Multi-root workspaces are not yet supported!";
  vscode.window.showErrorMessage(msg);
  throw new Error(msg);
}

export default function variables(
  s: string,
  workspaceFolder?: vscode.WorkspaceFolder | undefined,
): string {
  const workspace = workspaceFolder || selectWorkspace();
  s = s.replace(/\${workspaceFolder}/g, workspace?.uri.fsPath);
  s = s.replace(/\${workspaceFolderBasename}/g, workspace?.name);
  s = s.replace(/\${fileWorkspaceFolder}/g, workspace?.uri.fsPath);
  s = s.replace(/\${pathSeparator}/g, sep);
  s = s.replace(/\${env:(.*?)}/g, (variable) => {
    const matches = variable.match(/\${env:(.*?)}/);
    if (matches) {
      return env[matches[1]] || variable;
    }
    return variable; // leave untouched
  });
  s = s.replace(/\${config:(.*?)}/g, function (variable) {
    const matches = variable.match(/\${config:(.*?)}/);
    if (matches) {
      return vscode.workspace.getConfiguration().get(matches[1], variable);
    }
    return variable; // leave untouched
  });

  return s;
}
