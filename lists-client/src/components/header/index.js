import React, { useContext, useEffect } from 'react';
import { get } from 'lodash';
import { useGetUserAction } from '../../state/user';
import StateContext from  '../../state/stateContext';
import { Link } from 'react-router-dom';

const useLogout = ({ getUserAction }) => {
  return {
    logout: async () => {
      const response = await fetch('/logout', {
        method: 'post',
        headers: {
          'Content-Type': 'application/json'
        }
      });
    
      const result = await response.json();
      if (get(result, 'success') === true) {
        getUserAction();
      }
    }
  }
}

const Welcome = () => {
  const { getUserAction } = useGetUserAction();
  useEffect(() => {
    getUserAction();
  }, []); // eslint-disable-line react-hooks/exhaustive-deps

  const [state] = useContext(StateContext);

  const { logout } = useLogout({ getUserAction });

  // const user = await state.user;

  // return (get(state, 'user') && <h1>Welcome {get(state, ['user', 'username'])}</h1>)
  return <nav className="navbar bg-light">
    <span className="navbar-brand">Lists</span>
    <span className="navbar-text">
      {!get(state, 'user') && <Link to="/register">Register&nbsp;</Link>}
      {!get(state, 'user') && <Link to="/login">Login</Link>}
      {get(state, 'user') && <span className="align-middle">{get(state, ['user', 'username'])}</span>}
      {get(state, 'user') && <button
        onClick={logout}
        className="btn btn-link">
          Logout
        </button>}
    </span>
  </nav>
}

export default Welcome;