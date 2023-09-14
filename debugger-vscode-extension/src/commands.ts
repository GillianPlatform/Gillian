import { debug, DebugConfiguration, DebugSession, window } from 'vscode';

let debugSession: DebugSession | null = null;
let isDebugStarting = false;

debug.onDidStartDebugSession(session => {
  debugSession = session;
});

debug.onDidTerminateDebugSession(() => {
  debugSession = null;
});

async function checkForExistingDebugSession() {
  if (!debug.activeDebugSession) debugSession = null;
  if (debugSession === null) return true;
  const response = await window.showInformationMessage(
    'Only one Gillian debugger can run at a time.\nTerminate old session and continue?',
    'OK',
    'Cancel'
  );
  if (response === 'OK') {
    await debug.stopDebugging(debugSession);
    return true;
  }
  return false;
}

export async function startDebugging(
  config: DebugConfiguration,
  noDebug = false
) {
  if (isDebugStarting) return;
  isDebugStarting = true;
  try {
    const canContinue = await checkForExistingDebugSession();
    if (!canContinue) return;
    const validInputs = new Set(['wisl', 'js', 'c', 'kani']);
    const validateInput = (input: string) => {
      if (!validInputs.has(input)) {
        return 'The target language must be one of: wisl, js, c, kani';
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

    await debug.startDebugging(undefined, config, { noDebug: noDebug });
  } finally {
    isDebugStarting = false;
  }
}
