import * as vscode from "vscode";
import { activateCodeLens } from "./codeLens";
import { activateDebug } from "./debug";
import { DebugAdapterExecutableFactory } from "./debugAdapter";

export function activate(context: vscode.ExtensionContext) {
  activateDebug(context, new DebugAdapterExecutableFactory());
  activateCodeLens(context);
}

export function deactivate() {
  // nothing to do
}
