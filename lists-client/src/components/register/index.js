import React, { useState } from 'react';
import { get } from 'lodash';
import { v4 } from 'uuid'

const useRegister = ({
  usernameRef,
  emailRef,
  passwordRef,
  setRequestState
}) => {
return {
    handleSubmit: async (event) => {
      if (event) event.preventDefault();

      const data = {
        userUsername: usernameRef.current.value, 
        userEmail: emailRef.current.value,
        userPassword: passwordRef.current.value
      };

      const response = await fetch('/register', {
        method: 'post',
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json'
        }
      });

      setRequestState('...');

      const result = await response.json();

      if(get(result, 'success') === true) {
        setRequestState('Success');
      }
    }
  }
}

const RegisterComponent = () => {
  // Maybe move to global state
  const [requestState, setRequestState] = useState();

  const usernameRef = React.createRef();
  const emailRef = React.createRef();
  const passwordRef = React.createRef();
  

  const { handleSubmit } = useRegister({
    usernameRef,
    emailRef,
    passwordRef,
    setRequestState
  });

  const usernameUuid = `username-${v4()}`;
  const passwordUuid = `password-${v4()}`;
  const emailUuid = `email-${v4()}`;

  return (
    <form onSubmit={handleSubmit}>
      <h3>Register</h3>
      {requestState && <div className="alert alert-success" role="alert">
        {requestState}
      </div>}
      <div className="form-group">
        <label htmlFor={usernameUuid}>Username</label>
        <input
          id={usernameUuid}
          className="form-control"
          placeholder="Username"
          autoComplete="username"
          ref={usernameRef} />
      </div>
      <div className="form-group">
        <label htmlFor={emailUuid}>Email</label>
        <input
          id={emailUuid}
          className="form-control"
          placeholder="Email"
          ref={emailRef}
          autoComplete="email" />
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
    </form>
  );
}

export default RegisterComponent;
