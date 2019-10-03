import React, { useContext, useEffect } from 'react';
import StateContext from '../../state/stateContext';
import { get } from 'lodash';
// import { getItemsAction }
import { useGetItemsAction } from '../../state/items';
import { v4 } from 'uuid';





const ItemsPanel = () => {
  const [state] = useContext(StateContext);

  const { getItemsAction } = useGetItemsAction();
  const user = get(state, 'user');
  useEffect(() => {
     getItemsAction()
  } , [user]); // eslint-disable-line react-hooks/exhaustive-deps

  return (<ul className="p-2 list-group">
      {get(state, 'items', []).map((item) => {
        return (<li key={v4()} className="list-group-item">
            <button className="btn btn-info">{item.name}</button>
          </li>)
      })}
      
    </ul>);
}



export default ItemsPanel;



