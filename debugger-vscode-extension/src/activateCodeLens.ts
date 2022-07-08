import {
  CodeLens,
  CodeLensProvider,
  commands,
  ExtensionContext,
  languages,
  Range,
  TextDocument,
} from 'vscode';
import { startDebugging } from './commands';

export function activateCodeLens(context: ExtensionContext) {
  const commandDisposable = commands.registerCommand(
    'extension.code-lens.debugProcedure',
    startDebugging
  );

  const supportedLanguages = ['javascript', 'gil', 'wisl'];

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

class DebugCodeLensProvider implements CodeLensProvider {
  async provideCodeLenses(document: TextDocument): Promise<CodeLens[]> {
    const text = document.getText();

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

    const reProcedureName = /(.+?)\(/g;

    const lenses: CodeLens[] = [];
    while (reProcedure.exec(text) !== null) {
      reProcedureName.lastIndex = reProcedure.lastIndex;
      const match = reProcedureName.exec(text);
      const procedureName = match === null ? null : match[1].trim();
      if (procedureName) {
        const codeLens = this.makeCodeLens(
          reProcedureName.lastIndex,
          procedureName,
          document
        );
        if (codeLens !== undefined) {
          lenses.push(codeLens);
        }
      }
    }

    return lenses;
  }

  private makeCodeLens(
    index: number,
    procedureName: string,
    document: TextDocument
  ) {
    const startIdx = index - procedureName.length;
    const start = document.positionAt(startIdx);
    const end = document.positionAt(index);
    const range = new Range(start, end);
    const debugConfig = this.createDebugConfig(
      procedureName,
      document.uri.fsPath
    );
    return new CodeLens(range, {
      command: 'extension.code-lens.debugProcedure',
      title: 'Debug ' + procedureName,
      tooltip: 'Debug ' + procedureName,
      arguments: [debugConfig],
    });
  }

  private createDebugConfig(procedureName: string, program: string) {
    return {
      type: 'gillian',
      name: 'Debug File',
      request: 'launch',
      program: program,
      procedureName: procedureName,
      stopOnEntry: true,
    };
  }
}
