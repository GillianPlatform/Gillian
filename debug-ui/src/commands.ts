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
    const validLangs = ['wisl', 'js', 'c', 'c2'];
    const langOptions: Record<string, string> = {
      'WISL': 'wisl',
      'JS': 'js',
      'C (CompCert)': 'c',
      'C (CBMC)': 'c2',
    };

    const fileExtension = config.program.split('.').pop();
    if (fileExtension === 'gil') {
      const selected = await window.showQuickPick(validLangs, {
        title: 'Target language',
      });
      const targetLanguage = selected ? langOptions[selected] : undefined;
      config = {
        targetLanguage: targetLanguage,
        ...config,
      };
      if (!targetLanguage || !validLangs.includes(targetLanguage)) {
        // Do not start debugger if invalid target language specified for GIL file
        return;
      }
    }

    await debug.startDebugging(undefined, config, { noDebug: noDebug });
  } finally {
    isDebugStarting = false;
  }
}
