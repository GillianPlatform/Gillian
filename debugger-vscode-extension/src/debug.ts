'use strict';

import * as vscode from 'vscode';
import {
  WorkspaceFolder,
  DebugConfiguration,
  ProviderResult,
  CancellationToken,
} from 'vscode';

import { startDebugging } from './commands';
import { BranchCase, DebuggerState, MatchMap } from './types';
import { WebviewPanel } from './WebviewPanel';

type LogEvent = {
  msg: string;
  json: any;
};

type Disposable = {
  dispose(): any;
};

function handleCustomDebugEvent({
  body,
  event,
}: vscode.DebugSessionCustomEvent) {
  if (event === 'log') {
    const { msg, json } = body as LogEvent;
    if (Object.keys(json).length === 0) {
      console.log(`<D> ${msg}`);
    } else {
      console.log(`<D> ${msg}`, json);
    }
  } else if (event === 'debugStateUpdate') {
    WebviewPanel.currentPanel?.updateState(body as DebuggerState);
  } else {
    console.error(`Unhandled custom event '${event}'`);
  }
}

export function activateDebug(
  context: vscode.ExtensionContext,
  factory: vscode.DebugAdapterDescriptorFactory
) {
  context.subscriptions.push(
    vscode.commands.registerCommand(
      'extension.gillian-debug.runEditorContents',
      (resource: vscode.Uri) => {
        let targetResource = resource;
        if (!targetResource && vscode.window.activeTextEditor) {
          targetResource = vscode.window.activeTextEditor.document.uri;
        }
        if (targetResource) {
          startDebugging({
            type: 'gillian',
            name: 'Run File',
            request: 'launch',
            program: targetResource.fsPath,
            execMode: 'debugverify',
          });
        }
      }
    ),
    vscode.commands.registerCommand(
      'extension.gillian-debug.debugEditorContents',
      (resource: vscode.Uri) => {
        let targetResource = resource;
        if (!targetResource && vscode.window.activeTextEditor) {
          targetResource = vscode.window.activeTextEditor.document.uri;
        }
        if (targetResource) {
          startDebugging({
            type: 'gillian',
            name: 'Debug File',
            request: 'launch',
            program: targetResource.fsPath,
            execMode: 'debugverify',
          });
        }
      }
    ),
    vscode.commands.registerCommand(
      'extension.gillian-debug.showDebuggerWebview',
      () => {
        WebviewPanel.render(context.extensionUri);
      }
    ),
    vscode.debug.onDidStartDebugSession(() => {
      WebviewPanel.render(context.extensionUri);
    }),
    vscode.debug.onDidReceiveDebugSessionCustomEvent(handleCustomDebugEvent)
  );

  context.subscriptions.push(
    vscode.commands.registerCommand(
      'extension.gillian-debug.getProgramName',
      config => {
        return vscode.window.showInputBox({
          placeHolder: 'Please enter the name of a file',
        });
      }
    )
  );

  // register a configuration provider for 'gillian' debug type
  const provider = new ConfigurationProvider();
  context.subscriptions.push(
    vscode.debug.registerDebugConfigurationProvider('gillian', provider)
  );

  context.subscriptions.push(
    vscode.debug.registerDebugAdapterDescriptorFactory('gillian', factory)
  );
  if ('dispose' in factory) {
    context.subscriptions.push(factory as unknown as Disposable);
  }
}

class ConfigurationProvider implements vscode.DebugConfigurationProvider {
  /**
   * Massage a debug configuration just before a debug session is being launched,
   * e.g. add all missing attributes to the debug configuration.
   */
  resolveDebugConfiguration(
    folder: WorkspaceFolder | undefined,
    config: DebugConfiguration,
    token?: CancellationToken
  ): ProviderResult<DebugConfiguration> {
    // if launch.json is missing or empty
    if (!config.type && !config.request && !config.name) {
      const editor = vscode.window.activeTextEditor;
      if (editor) {
        config.type = 'gillian';
        config.name = 'Launch';
        config.request = 'launch';
        config.program = '${file}';
        config.stopOnEntry = true;
      }
    }

    if (!config.program) {
      return vscode.window
        .showInformationMessage('Cannot find a program to debug')
        .then(_ => {
          return undefined; // abort launch
        });
    }

    return config;
  }
}

export async function getDebuggerState() {
  const session = vscode.debug.activeDebugSession;
  if (session !== undefined) {
    const state: DebuggerState = await session.customRequest('debuggerState');
    return state;
  }
}

export async function getMatch(
  id: number
): Promise<[number, MatchMap] | undefined> {
  const session = vscode.debug.activeDebugSession;
  if (session !== undefined) {
    const result = await session.customRequest('matching', { id });
    const { matchId, matchMap } = result;
    return [matchId, matchMap];
  }
}

export async function jumpToCmd(id: number) {
  const session = vscode.debug.activeDebugSession;
  if (session !== undefined) {
    console.log('Requesting jump', {
      id,
    });
    const result = await session.customRequest('jump', {
      id,
    });
    if (!result.success) {
      vscode.window.showErrorMessage(result.err || 'jumpToCmd: unknown error');
    }
  }
}

export async function execSpecificCmd(
  prevId: number,
  branchCase: BranchCase | null
) {
  const session = vscode.debug.activeDebugSession;
  if (session !== undefined) {
    console.log('Requesting step specific', {
      prevId,
      branchCase,
    });
    const result = await session.customRequest('stepSpecific', {
      prevId,
      branchCase,
    });
    if (!result.success) {
      vscode.window.showErrorMessage(result.err || 'help');
    }
  }
}

export async function startProc(procName: string) {
  const session = vscode.debug.activeDebugSession;
  if (session !== undefined) {
    const result = await session.customRequest('startProc', { procName });
    if (!result.success) {
      vscode.window.showErrorMessage(result.err || 'startProc: unknown error');
    }
  }
}
