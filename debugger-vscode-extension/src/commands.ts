import { debug, DebugConfiguration, window } from "vscode";

export async function startDebugging(config: DebugConfiguration, noDebug: boolean = false) {
  const validInputs = new Set(["wisl", "js"]);
  const validateInput = (input : string) => {
    if (!validInputs.has(input)) {
      return "The target language must be one of: wisl, js";
    }

    return null;
  };

  const fileExtension = config.program.split('.').pop();
  if (fileExtension === "gil") {
    let targetLanguage = await window.showInputBox({
      prompt: "Target language",
      value: "wisl",
      valueSelection: [0, 5],
      placeHolder: "e.g. wisl, js",
      validateInput: validateInput
    });
    config = {
      "targetLanguage": targetLanguage,
      ...config
    };
    if (!targetLanguage || !validInputs.has(targetLanguage)) {
      // Do not start debugger if invalid target language specified for GIL file
      return;
    }
  }

  debug.startDebugging(undefined, config, { noDebug: noDebug });
}