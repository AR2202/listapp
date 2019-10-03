{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, TypeFamilies #-}

module Commands.Index (
  getUser,
  createList',
  getList',
  getLists',
  addItem',
  getItems',
  getItem',
  addItemToList,
  getListItems,
  getListMixinByChild,
  getListMixinByParent,
  getItemFromListItem,
  createListMixin,
  deleteItemFromList,
  deleteListItems
) where

import Database2 (
  User(..),
    List(..),
    Item(..),
    ListItem(..),
    ListMixin(..),
    EntityField(UserUsername, ListName, ListUserId, ItemUserId, ListItemListId, ListMixinParent, ListMixinChild,ItemId,ItemName, ListItemItemId,ListItemSubheading, ListItemListitemtype),
    getDbPool
  )
import Types
import Database.Persist.Postgresql as Pgsql
import Control.Exception (throw, SomeException(..))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Database.Persist.Class as PClass
import Database.Persist(deleteWhere)

getUser :: String -> IO (Entity User)
getUser username = do
  pool <- getDbPool
  maybeUser <- flip Pgsql.runSqlPersistMPool pool $ do
    selectFirst ([UserUsername ==. (T.pack username)]) [LimitTo 1]
  case maybeUser of
    Nothing -> error "No user"
    Just user -> return user


createList' :: Pgsql.ConnectionPool ->Entity User -> String -> IO ()
createList' pool eUser name = do
  
  flip Pgsql.runSqlPersistMPool pool $ do
    _ <- insert List { listName = (T.pack name), listUserId = (entityKey eUser), listOrder = 0 }
    return ()

getList' :: Pgsql.ConnectionPool  -> Entity User -> String -> IO (Entity List)
getList' pool eUser name = do
  maybeList <- flip Pgsql.runSqlPersistMPool pool $ do
    selectFirst ([ListName ==. T.pack name, ListUserId ==. (entityKey eUser)]) [LimitTo 1]
  case maybeList of
    Nothing -> error "No list"
    Just list -> return list

getLists' :: Pgsql.ConnectionPool -> Entity User -> IO ([Entity List])
getLists' pool eUser = do
  
  allLists <- flip Pgsql.runSqlPersistMPool pool $ do
    selectList ([ListUserId ==. (entityKey eUser)]) []
  return allLists
  
addItem' :: Pgsql.ConnectionPool ->  Entity User -> String -> IO (Key Item)
addItem' pool eUser name = do
  item <- flip Pgsql.runSqlPersistMPool pool $ do
    insert Item { itemName = (T.pack name), itemUserId = (entityKey eUser) }
  return item

getItems' :: Pgsql.ConnectionPool -> Entity User -> IO ([Entity Item])
getItems' pool eUser = do
  
  items <- flip Pgsql.runSqlPersistMPool pool $ do
    selectList ([ItemUserId ==. (entityKey eUser)]) []
  return items

getItem' :: Pgsql.ConnectionPool ->  Entity User -> String -> IO (Maybe (Entity Item))
getItem' pool eUser name = do
  
  maybeitem <- flip Pgsql.runSqlPersistMPool pool $ do
    selectFirst ([ItemUserId ==. (entityKey eUser), ItemName ==. (T.pack name)]) [LimitTo 1]
  return maybeitem
  

getItemFromListItem :: Entity ListItem -> IO (Entity Item)
getItemFromListItem eListItem = do
  pool <- getDbPool
  let id = getid eListItem
        where  getid (Entity k v) = listItemItemId v
  item <- flip Pgsql.runSqlPersistMPool pool $ do
    selectFirst ([ItemId ==. id]) [LimitTo 1]
  case item of
    Nothing -> error "no item"
    Just i -> return i

deleteItemFromList :: Entity Item -> Entity List -> IO ()
deleteItemFromList eItem eList = do
  pool <- getDbPool
  flip Pgsql.runSqlPersistMPool pool $ do
    deleteWhere [ListItemItemId ==. (entityKey eItem), ListItemListId ==. (entityKey eList)]
  return ()

addItemToList :: Entity Item -> Entity List -> Maybe (Entity ListItem) -> IO ()
addItemToList eItem eList maybeESubheading= do
  pool <- getDbPool
  flip Pgsql.runSqlPersistMPool pool $ do
    insert ListItem {
      listItemItemId = (entityKey eItem),
      listItemListId = (entityKey eList),
      listItemSubheading= (fmap entityKey maybeESubheading),
      listItemListitemtype=Standarditem,
      listItemOrder = 0
    }
  return ()

addSubheadingToList :: Entity Item -> Entity List -> IO ()
addSubheadingToList eItem eList = do
  pool <- getDbPool
  flip Pgsql.runSqlPersistMPool pool $ do
    insert ListItem {
      listItemItemId = (entityKey eItem),
      listItemListId = (entityKey eList),
      listItemSubheading=Nothing,
      listItemListitemtype=Subheading,
      listItemOrder = 0
    }
  return ()

addSubheadingToListItem :: Entity ListItem -> Entity ListItem -> IO()
addSubheadingToListItem eListItem eSubheading = do
  pool <- getDbPool
  flip Pgsql.runSqlPersistMPool pool $ do
    update (entityKey eListItem) [ListItemSubheading=.Just(entityKey eSubheading)]
    return()

getListItems :: Entity List -> IO ([Entity ListItem])
getListItems  eList = do
  pool <- getDbPool
  listItems <- flip Pgsql.runSqlPersistMPool pool $ do
    selectList ([ListItemListId ==. (entityKey eList)]) []
  return listItems

getListMixinByChild :: Entity List -> IO ([Entity ListMixin])
getListMixinByChild eList = do
  pool <- getDbPool
  listMixin <- flip Pgsql.runSqlPersistMPool pool $ do
    selectList ([ListMixinChild ==. (entityKey eList)]) []
  return listMixin

getListMixinByParent :: Entity List -> IO ([Entity ListMixin])
getListMixinByParent eList = do
  pool <- getDbPool
  listMixin <- flip Pgsql.runSqlPersistMPool pool $ do
    selectList ([ListMixinParent ==. (entityKey eList)]) []
  return listMixin
  
createListMixin :: Entity List -> Entity List -> IO ()  
createListMixin eListC eListP = do
  pool <- getDbPool
  flip Pgsql.runSqlPersistMPool pool $ do
        _ <- PClass.insertUniqueEntity ListMixin { listMixinParent = (entityKey eListP), listMixinChild = (entityKey eListC), listMixinOrder = 0}
        return ()
  
deleteListItems :: Entity List -> IO ()  
deleteListItems eList = do
  pool <- getDbPool
  flip Pgsql.runSqlPersistMPool pool $ do
    deleteWhere ([ListItemListId ==. (entityKey eList)]) 
