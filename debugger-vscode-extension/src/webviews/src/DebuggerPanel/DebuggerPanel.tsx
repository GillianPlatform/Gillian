import {
  VSCodePanels,
  VSCodePanelTab,
  VSCodePanelView,
} from '@vscode/webview-ui-toolkit/react';
import React, { useEffect, useState } from 'react';
import { DebuggerState } from '../../../types';
import ExecMapView from '../ExecMap/ExecMapView';
import useStore, { mutateStore } from '../store';
import UnifyView from '../UnifyView/UnifyView';
import * as events from '../events';

import './DebuggerPanel.css';

const DebuggerPanel = () => {
  const [debuggerState, expandedExecNodes] = useStore(
    ({ debuggerState, expandedExecNodes }) => [debuggerState, expandedExecNodes]
  );
  const { toggleExecNodeExpanded } = mutateStore();
  const hasUnify = useStore(
    ({ unifyState: { path } }) => path && path.length > 0
  );
  const [activeTab, setActiveTab] = useState('debug-exec-tab');

  useEffect(() => {
    events.subscribe('resetView', () => {
      setActiveTab('debug-exec-tab');
    });

    return () => {
      events.unsubscribe('resetView');
    };
  }, []);

  useEffect(() => {
    if (!hasUnify) {
      setActiveTab('debug-exec-tab');
    }
  }, [hasUnify]);

  return (
    <div className="debugger-panel">
      <VSCodePanels activeid={activeTab}>
        <VSCodePanelTab
          id="debug-exec-tab"
          onClick={() => {
            setActiveTab('debug-exec-tab');
          }}
        >
          EXEC MAP
        </VSCodePanelTab>
        <VSCodePanelTab
          id="debug-unify-tab"
          onClick={() => {
            setActiveTab('debug-unify-tab');
          }}
        >
          UNIFICATION
        </VSCodePanelTab>

        <VSCodePanelView id="debug-exec-panel">
          <ExecMapView
            {...{
              state: debuggerState as DebuggerState,
              expandedNodes: expandedExecNodes,
              toggleNodeExpanded: toggleExecNodeExpanded,
            }}
          />
        </VSCodePanelView>
        <VSCodePanelView id="debug-unify-panel">
          <UnifyView />
        </VSCodePanelView>
      </VSCodePanels>
    </div>
  );
};

export default DebuggerPanel;
