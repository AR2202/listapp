import React, { useContext } from 'react';
import StateContext from  '../../state/stateContext';
import { Redirect } from 'react-router-dom';
import { get } from 'lodash';

import ItemsPanel from './itemsPanel';

const Items = (params) => {
  const itemId = get(params, ['match', 'params', 'id']);
  const [state] = useContext(StateContext);


  return (<div>
    <h1>Items</h1>
    <div className="d-flex">
      <ItemsPanel />
      {itemId && <ItemsPanel id={itemId} />}
    </div>
  </div>);

};

export default Items;
