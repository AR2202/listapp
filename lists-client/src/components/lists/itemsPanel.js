import React, { useContext, useEffect } from 'react';
import StateContext from '../../state/stateContext';
import { get } from 'lodash';
// import { getItemsAction }
import { useGetItemsAction } from '../../state/items';
import { v4 } from 'uuid';
import { ItemsPanelNew } from './itemsPanelNew';
import { Link } from 'react-router-dom';
import { useGetListItemsAction } from '../../state/listItems';

const ItemsPanel = ({
  id
}) => {
  const [state] = useContext(StateContext);

  const { getListItemsAction } = useGetListItemsAction();

  const user = get(state, 'user');
  useEffect(() => {
    if (!user || !id) return;
    getListItemsAction({ id });
  }, [ user, id ]); // eslint-disable-line react-hooks/exhaustive-deps

  useEffect(() => {
    // getItemsAction()
  } , [user, id])// eslint-disable-line react-hooks/exhaustive-deps

  console.log('items', get(state, 'items'));

  return (<ul className="p-2 flex-grow-1 list-group">
      {get(state, 'items', []).map((items) => {
        return (<li key={v4()} className="list-group-item">
            {/* <button className="btn btn-info">{items.name}</button> */}
            <span>{items.name}</span>
          </li>)
      })}
      
      <ItemsPanelNew listId={id} />
    </ul>);
}



export default ItemsPanel;



