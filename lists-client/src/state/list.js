import { useContext } from 'react';
import StateContext from './stateContext';
import { isArray } from 'lodash';

export const useGetListAction = () => {
  const [, dispatch] = useContext(StateContext);

  return {
    getListsAction: async () => {
      const response = await fetch('/lists', {
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
        type: 'lists',
        lists: responseJson
      })
    }
  }
}