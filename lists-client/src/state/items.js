import { useContext } from 'react';
import StateContext from './stateContext';

export const useGetItemsAction = () => {
  const [, dispatch] = useContext(StateContext);

  return {
    getItemsAction: async () => {
      const response = await fetch('/items', {
        method: 'get',
        header: {
          'Content-Type': 'application/json'
        }
      });

      dispatch({
        type: 'items',
        items: await response.json()
      })
    }
  }
}
