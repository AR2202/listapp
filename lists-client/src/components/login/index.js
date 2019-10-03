import React, { useState, useContext } from 'react';
import { get } from 'lodash';
import { useGetUserAction } from '../../state/user';
import { v4 } from 'uuid';
import { Redirect } from 'react-router-dom';
import StateContext from  '../../state/stateContext';

const useLogin = ({
  usernameEmailRef,
  passwordRef,
  setRequestState,
  getUserAction
}) => {
  return {
    handleSubmit: async (event) => {
      if (event) event.preventDefault()

      const data = {
        usernameEmail: usernameEmailRef.current.value,
        password: passwordRef.current.value,
      };

      setRequestState('...');

      const response = await fetch('/login', {
        method: 'post',
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json'
        }
      });

      const result = await response.json();

      if(get(result, 'success') === true) {
        setRequestState('Success');
        // TODO: fire success user
        await getUserAction();

      }
    }
  }
}

const LoginComponent = () => {
  const [requestState, setRequestState] = useState();

  const { getUserAction } = useGetUserAction();

  const usernameEmailRef = React.createRef();
  const passwordRef = React.createRef();

  const { handleSubmit } = useLogin({
    usernameEmailRef,
    passwordRef,
    setRequestState,
    getUserAction
  });

  const usernameUuid = `username-${v4()}`;
  const passwordUuid = `password-${v4()}`;

  const [state] = useContext(StateContext);
  if (get(state, 'user') && get(state, 'userState') !== 'INIT') {
    return (<Redirect to={{
      pathname: "/lists"
    }}/>)
  }

  return (<form onSubmit={handleSubmit}>
    <h3>Login</h3>
    {requestState && <div className="alert alert-success" role="alert">
        {requestState}
      </div>}
    <div className="form-group">
        <label htmlFor={usernameUuid}>Username or Email</label>
        <input
          id={usernameUuid}
          className="form-control"
          placeholder="Username"
          autoComplete="username"
          ref={usernameEmailRef} />
      </div>

      <div className="form-group">
        <label htmlFor={passwordUuid}>Password</label>
        <input
          id={passwordUuid}
          className="form-control"
          type="password"
          placeholder="Password"
          ref={passwordRef}
          autoComplete="current-password" />
      </div>

      <input
        className="form-control"
        type="submit" />
    </form>);
}

export default LoginComponent;
