import { useContext } from 'react';
import StateContext from './stateContext';

export const useGetUserAction = () => {
  const [, dispatch] = useContext(StateContext);
  return {
    getUserAction: async () => {

      const response = await fetch('/user', {
        method: 'get',
        headers: {
          'Content-Type': 'application/json'
        }
      });

      dispatch({
        type: 'user',
        user: await response.json()
      });
    }
  }
};
