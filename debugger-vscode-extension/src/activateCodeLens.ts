import {
  CodeLens,
  CodeLensProvider,
  commands,
  ExtensionContext,
  languages,
  Range,
  TextDocument,
  workspace,
} from 'vscode';
import { startDebugging } from './commands';
import { ExecMode } from './types';

export function activateCodeLens(context: ExtensionContext) {
  const commandDisposable = commands.registerCommand(
    'extension.gillian-debug.debugProcedure',
    startDebugging
  );

  const supportedLanguages = ['javascript', 'gil', 'wisl', 'c'];

  for (const language of supportedLanguages) {
    const docSelector = {
      language: language,
      scheme: 'file',
    };

    const codeLensProviderDisposable = languages.registerCodeLensProvider(
      docSelector,
      new DebugCodeLensProvider()
    );

    context.subscriptions.push(commandDisposable);
    context.subscriptions.push(codeLensProviderDisposable);
  }
}

function getLensKinds(): [ExecMode, string][] {
  const config = workspace.getConfiguration('gillianDebugger');
  const lensKinds: [ExecMode, string][] = [];
  if (config.showVerifyLens) lensKinds.push(['debugverify', 'Verify ']);
  if (config.showSymbolicDebugLens)
    lensKinds.push(['debugwpst', 'Symbolic-debug ']);
  return lensKinds;
}

class DebugCodeLensProvider implements CodeLensProvider {
  private makeLensesFromPattern(
    pattern: RegExp,
    document: TextDocument
  ): CodeLens[] {
    const text = document.getText();
    const procNamePattern = /(.+?)\(/g;

    let reProcedure: RegExp;
    switch (document.languageId) {
      case 'gil':
        reProcedure = /proc /g;
        break;
      case 'javascript':
      case 'wisl':
      default:
        reProcedure = /function /g;
        break;
    }

    const lensKinds = getLensKinds();
    const lenses: CodeLens[] = [];
    while (pattern.exec(text) !== null) {
      procNamePattern.lastIndex = pattern.lastIndex;
      const match = procNamePattern.exec(text);
      const procedureName = match === null ? null : match[1].trim();
      if (procedureName) {
        for (const [execMode, commandPrefix] of lensKinds) {
          const codeLens = this.makeCodeLens(
            procNamePattern.lastIndex,
            procedureName,
            document,
            execMode,
            commandPrefix
          );
          if (codeLens !== undefined) {
            lenses.push(codeLens);
          }
        }
      }
    }

    return lenses;
  }

  private makeCLens(document: TextDocument): CodeLens[] {
    const text = document.getText();
    const pattern = /int\s+main\s*\(\)/g;
    const lensKinds = getLensKinds();

    const lenses: CodeLens[] = [];
    let match = pattern.exec(text);
    while (match !== null) {
      const ix = match.index + 'int main'.length;
      for (const [execMode, commandPrefix] of lensKinds) {
        const codeLens = this.makeCodeLens(
          ix,
          'main',
          document,
          execMode,
          commandPrefix
        );
        if (codeLens !== undefined) {
          lenses.push(codeLens);
        }
      }
      match = pattern.exec(text);
    }

    return lenses;
  }

  async provideCodeLenses(document: TextDocument): Promise<CodeLens[]> {
    let pattern: RegExp;
    switch (document.languageId) {
      case 'gil':
        pattern = /proc /g;
        break;
      case 'c':
        return this.makeCLens(document);
      case 'javascript':
      case 'wisl':
      default:
        pattern = /function /g;
        break;
    }

    return this.makeLensesFromPattern(pattern, document);
  }

  private makeCodeLens(
    index: number,
    procedureName: string,
    document: TextDocument,
    execMode: ExecMode,
    commandPrefix: string
  ) {
    const startIdx = index - procedureName.length;
    const start = document.positionAt(startIdx);
    const end = document.positionAt(index);
    const range = new Range(start, end);
    const debugConfig = this.createDebugConfig(
      procedureName,
      document.uri.fsPath,
      execMode
    );
    return new CodeLens(range, {
      command: 'extension.gillian-debug.debugProcedure',
      title: commandPrefix + procedureName,
      tooltip: commandPrefix + procedureName,
      arguments: [debugConfig],
    });
  }

  private createDebugConfig(
    procedureName: string,
    program: string,
    execMode: ExecMode
  ) {
    return {
      type: 'gillian',
      name: 'Debug File',
      request: 'launch',
      program: program,
      procedureName: procedureName,
      stopOnEntry: true,
      execMode,
    };
  }
}
