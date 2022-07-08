import { debug, DebugConfiguration, window } from 'vscode';

export async function startDebugging(
  config: DebugConfiguration,
  noDebug = false
) {
  const validInputs = new Set(['wisl', 'js', 'c']);
  const validateInput = (input: string) => {
    if (!validInputs.has(input)) {
      return 'The target language must be one of: wisl, js, c';
    }

    return null;
  };

  const fileExtension = config.program.split('.').pop();
  if (fileExtension === 'gil') {
    const targetLanguage = await window.showInputBox({
      prompt: 'Target language',
      value: 'wisl',
      valueSelection: [0, 5],
      placeHolder: 'e.g. wisl, js, c',
      validateInput: validateInput,
    });
    config = {
      targetLanguage: targetLanguage,
      ...config,
    };
    if (!targetLanguage || !validInputs.has(targetLanguage)) {
      // Do not start debugger if invalid target language specified for GIL file
      return;
    }
  }

  debug.startDebugging(undefined, config, { noDebug: noDebug });
}
