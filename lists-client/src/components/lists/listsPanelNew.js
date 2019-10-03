import React from 'react';
import { v4 } from 'uuid';
import { get } from 'lodash';

const useList = ({
  nameRef,
  getListsAction
}) => {
  return {
    handleSubmit: async (event) => {
      if (event) event.preventDefault();

      const data = {
        listName: nameRef.current.value
      };

      const response = await fetch('/list', {
        method: 'post',
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json'
        }
      });

      const result = await response.json();

      if (
        get(result, 'success') === true
      ) {
        // refresh the list
        getListsAction();
      }
    }
  }
};

const ListsPanelNew = ({
  getListsAction
}) => {
  const nameRef = React.createRef();
  const { handleSubmit } = useList({
    nameRef,
    getListsAction
  });

  return <li key={v4()} className="list-group-item">
    <form className="form-group" onSubmit={handleSubmit}>
      <div className="form-group">
        <input
          className="form-control"
          type="text"
          placeholder="name"
          ref={nameRef}/>
      </div>
      <div className="form-group">
        <input
          type="submit"
          className="btn btn-primary float-right"
          value="create" />
      </div>
    </form>
  </li>
}

export default ListsPanelNew;
