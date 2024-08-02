import { Allotment } from 'allotment';
import React from 'react';
import useStore, { mutateStore } from '../store';
import Hero from '../util/Hero';
import Loading from '../util/Loading';
import { requestMatching } from '../VSCodeAPI';
import MatchData from './MatchData';
import MatchMapView from './MatchMapView';

const MatchView = () => {
  const { path, matches, expandedNodes } = useStore(
    ({ matchState }) => matchState
  );
  const {
    selectMatchStep: selectStep,
    requestMatching,
    toggleMatchNodeExpanded: toggleNodeExpanded,
  } = mutateStore();

  const hasMatch = path && path.length > 0;

  if (!hasMatch) {
    return (
      <Hero>
        <h1>No matching</h1>
        <p>Please select a command that has matching</p>
      </Hero>
    );
  }

  const matchMapView = (() => {
    const matchId = path[0];
    if (!(matchId in matches)) {
      const load = () => {
        requestMatching(matchId);
      };

      return <Loading refresh={load} />;
    }
    return (
      <MatchMapView
        {...{
          matchId,
          selectStep,
          expandedNodes,
          matches,
          requestMatching,
          toggleNodeExpanded,
        }}
      />
    );
  })();

  return (
    <Allotment>
      <Allotment.Pane>{matchMapView}</Allotment.Pane>
      <Allotment.Pane>
        <MatchData {...{ selectStep }} />
      </Allotment.Pane>
    </Allotment>
  );
};

export default MatchView;
