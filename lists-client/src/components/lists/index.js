import React, { useContext } from 'react';
import StateContext from  '../../state/stateContext';
import { Redirect } from 'react-router-dom';
import { get } from 'lodash';
import ListsPanel from './listsPanel';
import ItemsPanel from './itemsPanel';

const Lists = (params) => {
  const listId = get(params, ['match', 'params', 'id']);
  const [state] = useContext(StateContext);

  if (!get(state, 'user') && get(state, 'userState') !== 'INIT') {
    return (<Redirect to={{
      pathname: "/login"
    }}/>)
  }

  return (<div>
    <h1>Lists</h1>
    <div className="d-flex">
      <ListsPanel />
      {listId && <ItemsPanel id={listId} />}
    </div>
  </div>);

};

export default Lists;