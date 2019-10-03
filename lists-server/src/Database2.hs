{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Database2 (
  migrateDatabase,
  User(..),
  List(..),
  Item(..),
  ListItem(..),
  ListMixin(..),
  P.EntityField(..),
  
  getDbPool
) where

import GHC.Generics
import Data.Text
import Database.Persist.TH
import Data.Aeson
import Database.Persist as P
import Utils (getEnvironment)
import Types (Itemtype(..))

-- getDbPool
import Database.Persist.Sql (PersistFieldSql)
import qualified Database.Persist.Postgresql as Pgsql
import Data.Configurator (load, Worth(Required), require)
import System.Environment (lookupEnv)
import Control.Monad.Logger (runStderrLoggingT)




getDbPool :: IO (Pgsql.ConnectionPool)
getDbPool = do
  args:: Maybe String <- lookupEnv "env"
  let env:: String = case args of
                          Nothing -> "dev"
                          Just env -> env
  cfg <- load [Required ("./cfg/" ++ env ++ ".cfg")]
  connStr <- require cfg "connStr"
  runStderrLoggingT $ Pgsql.createPostgresqlPool connStr 10


share [mkPersist sqlSettings, mkMigrate "migrateDatabase"] [persistLowerCase|
User
  email Text
  username Text
  password Text
  sessionId Text Maybe
  UniqueUsername username
  UniqueEmail email
  deriving Generic Show
List json
  name Text
  userId UserId
  order Int
  UniqueNameUser name userId
  deriving Generic Show
ListMixin
  parent ListId
  child ListId
  order Int
  deriving Generic Show
Item json
  name Text
  userId UserId
  deriving Generic Show
ListItem json
  listId ListId
  itemId ItemId
  subheading ListItemId Maybe
  listitemtype Itemtype
  order Int
  deriving Generic Show
ListItemInheritance
  parent ListItemId
  child ListItemId
  deriving Generic Show
|]

instance ToJSON User where
  toEncoding (user :: User) =
    let thePairs = "email" .= userEmail user <>
                   "username" .= userUsername user <>
                   "sessionId" .= userSessionId user in
    pairs(thePairs)

instance FromJSON User
