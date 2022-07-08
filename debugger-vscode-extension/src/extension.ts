/*---------------------------------------------------------
 * Copyright (C) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------*/

'use strict';

import * as vscode from 'vscode';
import { ProviderResult } from 'vscode';
import { activateCodeLens } from './activateCodeLens';
import { activateDebug } from './debug';

export function activate(context: vscode.ExtensionContext) {
  activateDebug(context, new DebugAdapterExecutableFactory());
  activateCodeLens(context);
}

export function deactivate() {
  // nothing to do
}

class DebugAdapterExecutableFactory
  implements vscode.DebugAdapterDescriptorFactory
{
  // The following use of a DebugAdapter factory shows how to control what debug adapter executable is used.
  // Since the code implements the default behavior, it is absolutely not neccessary and we show it here only for educational purpose.

  createDebugAdapterDescriptor(
    _session: vscode.DebugSession,
    executable: vscode.DebugAdapterExecutable | undefined
  ): ProviderResult<vscode.DebugAdapterDescriptor> {
    const fileExtension = _session.configuration.program.split('.').pop();
    let gillianExecutableCommand: string;
    // Match of the file extension first
    switch (fileExtension) {
      case 'js':
        gillianExecutableCommand = 'gillian-js';
        break;
      case 'wisl':
        gillianExecutableCommand = 'wisl';
        break;
      case 'gil':
        // Check the target language if it is a GIL file
        switch (_session.configuration.targetLanguage) {
          case 'js':
            gillianExecutableCommand = 'gillian-js';
            break;
          case 'c':
            gillianExecutableCommand = 'gillian-c';
            break;
          case 'wisl':
          default:
            // Default to WISL
            gillianExecutableCommand = 'wisl';
            break;
        }
        break;
      default:
        // Default to WISL
        gillianExecutableCommand = 'wisl';
        break;
    }

    const gillianSourceRepository: string =
      vscode.workspace.getConfiguration('gillianDebugger')
        .gillianSourceRepository === null
        ? __dirname + '/../..'
        : vscode.workspace.getConfiguration('gillianDebugger')
            .gillianSourceRepository;

    const command = 'esy';
    const args = ['x', gillianExecutableCommand, 'debugverify', '-r', 'db'];
    const options = {
      cwd: gillianSourceRepository,
    };
    executable = new vscode.DebugAdapterExecutable(command, args, options);

    // make VS Code launch the DA executable
    return executable;
  }
}
