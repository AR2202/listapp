import React from 'react';
import { v4 } from 'uuid';
import { get } from 'lodash';

const useListItem = ({
  nameRef,
  listId
}) => {
  return {
    handleSubmit: async (event) => {
      if (event) event.preventDefault();

      const data = {
        listItemName: nameRef.current.value
      };

      const response = await fetch(`/list/${listId}/item`, {
        method: 'post',
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json'
        }
      });

      const result = await response.json();
    }
  }
}

export const ItemsPanelNew = ({
  listId
}) => {
  const nameRef = React.createRef();

  const { handleSubmit } = useListItem({
    nameRef,
    listId
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
};