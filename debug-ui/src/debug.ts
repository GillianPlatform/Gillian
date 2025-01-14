'use strict';

import * as vscode from 'vscode';
import {
  CancellationToken,
  DebugConfiguration,
  Disposable,
  ProviderResult,
  WorkspaceFolder,
} from 'vscode';
import { DEBUG_TYPE } from './consts';
import SEDAPSession from './sedap/SEDAPSession';
import { getWebviewHtml } from './webviewHtml';

type LogEvent = {
  msg: string;
  json: any;
};

const sessions: Record<string, SEDAPSession> = {};

function handleCustomDebugEvent({
  session,
  body,
  event,
}: vscode.DebugSessionCustomEvent) {
  if (session.type === DEBUG_TYPE && event === 'log') {
    const { msg, json } = body as LogEvent;
    if (Object.keys(json).length === 0) {
      console.log(`<D> ${msg}`);
    } else {
      console.log(`<D> ${msg}`, json);
    }
  }
}

export function activateDebug(
  context: vscode.ExtensionContext,
  factory: vscode.DebugAdapterDescriptorFactory
) {
  context.subscriptions.push(
    // vscode.commands.registerCommand(
    //   'extension.gillian-debug.runEditorContents',
    //   (resource: vscode.Uri) => {
    //     let targetResource = resource;
    //     if (!targetResource && vscode.window.activeTextEditor) {
    //       targetResource = vscode.window.activeTextEditor.document.uri;
    //     }
    //     if (targetResource) {
    //       startDebugging({
    //         type: DEBUG_TYPE,
    //         name: 'Run File',
    //         request: 'launch',
    //         program: targetResource.fsPath,
    //         execMode: 'debugverify',
    //       });
    //     }
    //   }
    // ),
    // vscode.commands.registerCommand(
    //   'extension.gillian-debug.debugEditorContents',
    //   (resource: vscode.Uri) => {
    //     let targetResource = resource;
    //     if (!targetResource && vscode.window.activeTextEditor) {
    //       targetResource = vscode.window.activeTextEditor.document.uri;
    //     }
    //     if (targetResource) {
    //       startDebugging({
    //         type: DEBUG_TYPE,
    //         name: 'Debug File',
    //         request: 'launch',
    //         program: targetResource.fsPath,
    //         execMode: 'debugverify',
    //       });
    //     }
    //   }
    // ),
    vscode.debug.onDidStartDebugSession(session => {
      if (session.type !== DEBUG_TYPE) {
        return;
      }
      const sedapSession = new SEDAPSession({
        panelName: 'Gillian Debugging',
        session,
        getWebviewHtml: getWebviewHtml(context),
      });
      sessions[session.id] = sedapSession;
      sedapSession.showWebviewPanel();
      sedapSession.onDispose(() => {
        delete sessions[session.id];
      });
    }),
    vscode.commands.registerCommand(
      'extension.gillian-debug.showDebuggerWebview',
      () => {
        Object.values(sessions).forEach(s => s.showWebviewPanel());
      }
    ),
    vscode.debug.onDidReceiveDebugSessionCustomEvent(handleCustomDebugEvent)
  );

  context.subscriptions.push(
    vscode.commands.registerCommand(
      'extension.gillian-debug.getProgramName',
      () => {
        return vscode.window.showInputBox({
          placeHolder: 'Please enter the name of a file',
        });
      }
    )
  );

  // register a configuration provider for our debug type
  const provider = new ConfigurationProvider();
  context.subscriptions.push(
    vscode.debug.registerDebugConfigurationProvider(DEBUG_TYPE, provider),
    vscode.debug.registerDebugAdapterDescriptorFactory(DEBUG_TYPE, factory),
    ...('dispose' in factory ? [factory as Disposable] : []),
  );
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
        config.type = DEBUG_TYPE;
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
