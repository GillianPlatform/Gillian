# gillian-debug-adapter

This software archive contains both Gillian (in the `Gillian` directory) and the Gillian Debugger extension (in the `gillian-debugger-extension` directory). To run the Gillian Debugger, VSCode must be installed.

## Prerequisites
1. Clone the `Gillian` repo (https://github.com/GillianPlatform/Gillian)
2. Install esy from https://esy.sh/docs/en/getting-started.html
3. Install the Gillian extension in VSCode from https://marketplace.visualstudio.com/items?itemName=gillian.code-gillian&utm_source=VSCode.pro&utm_campaign=AhmadAwais (this is for syntax highlighting and making VSCode aware that .gil and .wisl files exist)

## Building the Gillian Debugger extension
1. Change into the `gillian-debugger-extension` using `cd gillian-debugger-extension`
2. Run `npm install`
3. Run `npm run watch`

## Running the Gillian Debugger
1. Open the `Run and Debug` side panel in VSCode
2. Click the `Extension` button
![extension-button-image](./extension-button.png)
3. A new window should open with some examples to run