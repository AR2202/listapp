import React, { useReducer } from 'react';

const StateContext = React.createContext({
  user: "Nothing"
});

const theInitialState = {
  user: null,
  userState: 'INIT'
};

const theReducer = (state, action) => {
  if(action.type === 'user') {
    return {
      ...state,
      user: action.user,
      userState: 'FETCHED'
    };
  } else if (action.type === 'lists') {
    return {
      ...state,
      lists: action.lists
    }
  } else if (action.type === 'items') {
    return {
      ...state,
      items: action.items
    }
  } else if (action.type === 'listItems') {
    return {
      ...state,
      listItems: action.listItems
    }
  }
    
  return state;
}

export const StateProvider = ({
  reducer = theReducer,
  initialState = theInitialState,
  children
}) => (<StateContext.Provider value={useReducer(reducer, initialState)}>
  {children}
</StateContext.Provider>)

export default StateContext;
