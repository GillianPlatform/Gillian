import React from 'react';
import {
  VSCodeButton,
  VSCodeProgressRing,
} from '@vscode/webview-ui-toolkit/react';
import Hero from './Hero';

type Props = {
  refresh: () => void;
  msg?: string;
};

const Loading = ({ refresh, msg = 'Waiting for debugger...' }: Props) => (
  <Hero>
    <VSCodeProgressRing />
    <h2>{msg}</h2>
    <VSCodeButton onClick={refresh}>Manual Refresh</VSCodeButton>
  </Hero>
);

export default Loading;
