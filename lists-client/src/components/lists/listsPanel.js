import React, { useContext, useEffect } from 'react';
import { get } from 'lodash';
import StateContext from  '../../state/stateContext';
import { useGetListAction } from '../../state/list';
import ListsPanelNew from './listsPanelNew';
import { v4 } from 'uuid';
import { Link } from 'react-router-dom'

const ListPanel = () => {
  const [state] = useContext(StateContext);

  const { getListsAction } = useGetListAction();
  const user = get(state, 'user');
  useEffect(() => {
    if (!user) return;
    getListsAction();
  }, [ user ]); // eslint-disable-line react-hooks/exhaustive-deps

  return (<ul className="p-2 list-group">
      {get(state, 'lists', []).map((list) => {
        return (<li key={v4()} className="list-group-item">
            <Link className="btn btn-info w-100" to={`/list/${list.id}`}>{list.name}</Link>
          </li>)
      })}
      <ListsPanelNew 
        getListsAction={getListsAction}/>
    </ul>);
}

export default ListPanel;
