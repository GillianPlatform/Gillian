import { Allotment } from 'allotment';
import React from 'react';
import useStore from '../store';
import Hero from '../util/Hero';
import Loading from '../util/Loading';
import VSCodeAPI from '../VSCodeAPI';
import UnifyData from './UnifyData';
import UnifyMapView from './UnifyMapView';

const UnifyView = () => {
  const { path, unifications } = useStore(({ unifyState }) => unifyState);
  const selectStep = useStore(({ selectUnifyStep }) => selectUnifyStep);

  const hasUnify = path && path.length > 0;

  if (!hasUnify) {
    return (
      <Hero>
        <h1>No unification</h1>
        <p>Please select a command that has unification</p>
      </Hero>
    );
  }

  const unifyMapView = (() => {
    const unifyId = path[0];
    const unification = unifications[unifyId];
    if (unification === undefined) {
      const load = () => {
        VSCodeAPI.postMessage({
          type: 'request_unification',
          id: unifyId,
        });
      };

      return <Loading refresh={load} />;
    }
    return <UnifyMapView {...{ unification, selectStep }} />;
  })();

  return (
    <Allotment>
      <Allotment.Pane>{unifyMapView}</Allotment.Pane>
      <Allotment.Pane>
        <UnifyData />
      </Allotment.Pane>
    </Allotment>
  );
};

export default UnifyView;
