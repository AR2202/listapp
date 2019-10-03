{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies      #-}

module List (
  getLists,
  getItems,
  createList,
  getList,
  addItem,
  getItem,
  sessionToUser,
  authenticate,
  addListItem,
  getListDetails
) where

import GHC.Generics
import Web.Scotty as S
import Network.HTTP.Types (status404)
import Database2 as DB
import Commands.Index as I
import qualified Data.Vault.Lazy as V (Key, lookup)
import qualified Database.Persist.Postgresql as Pgsql
import qualified Network.Wai.Session as Session
import Database.Persist
import Network.Wai (vault)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text
import Database.Esqueleto as E
import Database.Esqueleto.Internal.Sql
import Debug.Trace as D
import Data.Pool
import Types as T

getItems :: Pgsql.ConnectionPool -> Entity User -> ActionM ()
getItems pool user = do
  items <- liftIO $ I.getItems' pool user
  S.json $ items

getLists :: Pgsql.ConnectionPool -> Entity User -> ActionM ()
getLists pool user = do
  lists <- liftIO $ I.getLists' pool user
  S.json $ lists

data CreateList = CreateList {
  listName :: Text
} deriving (Generic, Show)
instance ToJSON CreateList where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON CreateList

data CreateItem = CreateItem {
  itemName :: Text
} deriving (Generic, Show)
instance ToJSON CreateItem where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON CreateItem


createList :: Pgsql.ConnectionPool -> Entity User -> ActionM ()
createList pool user = do
  createListData::CreateList <- jsonData
  _<-liftIO$I.createList' pool user (unpack $ (List.listName createListData))
  text $ "{\"success\": true}"
  return ()
      
getList ::   Pgsql.ConnectionPool -> Entity User -> ActionM ()
getList pool user = do
  createListData::CreateList <- jsonData
  list<-liftIO$I.getList' pool user (unpack $ (List.listName createListData))
  S.json $ list

addItem :: Pgsql.ConnectionPool -> Entity User -> ActionM ()
addItem pool user = do
  createItemData :: CreateItem <- jsonData
  _<-liftIO $ I.addItem' pool user (unpack $ (List.itemName createItemData))
  return ()

getItem ::   Pgsql.ConnectionPool -> Entity User ->  ActionM ()
getItem pool user = do
  createItemData :: CreateItem <- jsonData
  maybeitem<-liftIO$I.getItem' pool user (unpack $ (List.itemName createItemData))
  case maybeitem of
    Nothing -> do
      S.status status404
      text $ "{\"success\": false}"
    Just item -> do
      S.json $ item

sessionToUser :: Pgsql.ConnectionPool ->
  V.Key (Session.Session IO String (Maybe (Entity User))) ->
  ActionM (Maybe (Entity User))
sessionToUser pool sessionKey = do
  theRequest <- request
  let theVault = vault theRequest
  let Just (sessionLookup, sessionInsert) = V.lookup sessionKey theVault
  entityUser <- liftIO $ sessionLookup "user"
  case entityUser of
    Nothing -> do
      S.status status404
      text $ "{\"success\": false}"
      return Nothing
    Just Nothing -> do
      S.status status404
      text $ "{\"success\": false}"
      return Nothing
    Just (Just entityUser) -> do
      return (Just (entityUser))
      
authenticate :: Pgsql.ConnectionPool ->
  V.Key (Session.Session IO String (Maybe (Entity User))) ->
  (Pgsql.ConnectionPool -> Entity User -> ActionM ()) ->
  ActionM ()
authenticate pool sessionKey func = do
  theRequest <- request
  let theVault = vault theRequest
  let Just (sessionLookup, sessionInsert) = V.lookup sessionKey theVault
  entityUser <- liftIO $ sessionLookup "user"
  case entityUser of
    Nothing -> do
      S.status status404
      text $ "{\"success\": false}"
      return ()
    Just Nothing -> do
      S.status status404
      text $ "{\"success\": false}"
      return ()
    Just (Just entityUser) -> do
      func pool entityUser

data CreateListItem = CreateListItem {
  listItemName :: Text  
} deriving (Generic, Show)
instance ToJSON CreateListItem where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON CreateListItem

addListItem :: Pgsql.ConnectionPool -> Entity User -> ActionM ()
addListItem pool (Entity userId _) = do
  createListItem :: CreateListItem <- jsonData
  listIdParam <- param "id"
  let listId = Pgsql.toSqlKey listIdParam
  liftIO $ flip Pgsql.runSqlPersistMPool pool $ do
    itemId <- insert $ DB.Item { DB.itemName = (listItemName createListItem), itemUserId = userId }

    let query = E.insertSelect $ E.from $ \list -> do
        where_ $ ((list E.^. ListUserId) E.==. val userId) E.&&. ((list E.^. ListId) E.==. (val listId))
        return $ ListItem <# (list E.^. ListId) <&>
            val itemId <&>
            val Nothing <&>
            val T.Standarditem <&>
            val 0

    -- backend <- liftIO $ Data.Pool.takeResource pool
    -- let (a :: SqlBackend, b) = backend
    -- let (tlb, _) = Database.Esqueleto.Internal.Sql.toRawSql UPDATE (a, initialIdentState) subQuery

    query
  text $ "{\"success\": true}"

getListDetails :: Pgsql.ConnectionPool -> Entity User -> ActionM ()
getListDetails pool (Entity userId _) = do
  listIdParam <- param "id"
  let listId = Pgsql.toSqlKey listIdParam
  result <- liftIO $ flip Pgsql.runSqlPersistMPool pool $ do
    -- 
    let query = E.select $ E.from $ \(listItem `InnerJoin` list `InnerJoin` item) -> do
        on ((item E.^. ItemId) E.==. (listItem E.^. ListItemItemId))
        on ((list E.^. ListId) E.==. (listItem E.^. ListItemListId))
        where_ $ listItem E.^. ListItemId E.==. val listId E.&&. (list E.^. ListUserId E.==. val userId)
        -- orderBy
        return (listItem, item)
    query
  -- text $ "{\"success\": ture}"
  S.json $ result