{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Lib
(
  startServer
) where

import Web.Scotty
import Data.Monoid (mconcat)
import Debug.Trace
import Data.Configurator (load, Worth(Required), require)
import qualified Database.Persist.Postgresql as Pgsql
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks, lift)
import Database2
import User
import Network.Wai
import Network.Wai.Session.Map (mapStore_)
import Network.Wai.Session
import Web.Cookie (def, setCookiePath)
import Data.Vault.Lazy as Vault (newKey, insert, lookup)
import List
--import qualified Commands.Index as Index

startServer :: IO ()
startServer = do
  pool <- Database2.getDbPool

  flip Pgsql.runSqlPersistMPool pool $ do
    Pgsql.runMigration Database2.migrateDatabase
    liftIO $ print ("Done Migration" :: [Char])

  sessionKey <- liftIO newKey
  store:: SessionStore IO String (Maybe (Pgsql.Entity User)) <- liftIO mapStore_
  let theMiddleware = (withSession store "lists" (def { setCookiePath = (Just "/") }) sessionKey)

  scotty 3101 $ do
    middleware theMiddleware

    get "/hello" $ do html $ mconcat ["<h1>Hello World</h1>"]
    post "/register" $ User.register pool
    post "/login" $ User.login pool sessionKey
    get "/user" $ User.getUser pool sessionKey
    post "/logout" $ User.logout sessionKey
    get "/items" $ do
      -- maybeEUser<-List.sessionToUser pool sessionKey
      -- List.getItems pool maybeEUser
      authenticate pool sessionKey List.getItems
    get "/lists" $ do
      -- maybeEUser<-List.sessionToUser pool sessionKey
      -- List.getLists pool maybeEUser
      authenticate pool sessionKey List.getLists
    post "/list" $ do
      --maybeEUser<-List.sessionToUser pool sessionKey
      --List.createList pool maybeEUser
      authenticate pool sessionKey List.createList
    post "/lists" $ do
     -- maybeEUser<-List.sessionToUser pool sessionKey
     -- name <- jsonData
     -- List.getList pool maybeEUser name
      authenticate pool sessionKey List.getList
    post "/item" $ do
      authenticate pool sessionKey List.addItem
    post "/list/:id/item" $ do
      authenticate pool sessionKey List.addListItem
    get "/list/:id" $ do
      authenticate pool sessionKey List.getListDetails


