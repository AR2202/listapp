import { useContext } from 'react';
import StateContext from './stateContext';
import { isArray } from 'lodash';

export const useGetListItemsAction = () => {
  const [, dispatch] = useContext(StateContext);

  return {
    getListItemsAction: async ({ id }) => {
      const response = await fetch(`/list/${id}`, {
        method: 'get',
        header: {
          'Content-Type': 'application/json'
        }
      });

      let responseJson = await response.json();

      if(!isArray(responseJson)) {
        // prorably responseJson.success: false
        responseJson = [];
      }

      dispatch({
        type: 'listItems',
        listItems: responseJson
      })
    }
  }
}