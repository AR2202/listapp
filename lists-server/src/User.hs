{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module User (
  register,
  login,
  getUser,
  logout
) where

import GHC.Generics
import Web.Scotty as S
import qualified Web.Scotty.Trans as Trans
import Database2
import Debug.Trace
import qualified Database.Persist.Postgresql as Pgsql
import Database.Persist
import Database2 as DB
import User.Validate as UV
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.Text
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types (status400, status500, status401)
import Crypto.PasswordStore
import Control.Exception (try)
import Database.PostgreSQL.Simple (SqlError(..))
import Data.Aeson
import Network.Wai (vault)
import qualified Data.Vault.Lazy as V (Key, newKey, insert, lookup)
import qualified Network.Wai.Session as Session

register :: Pgsql.ConnectionPool -> ActionM ()
register pool = do
  user :: Database2.User <- jsonData

  isValid <- liftIO $ UV.isUserValid pool user
  case isValid of
    UV.Validate {UV.valid = valid@False} -> do
      S.status status400
      S.json isValid
    validate -> register' pool user

register' :: Pgsql.ConnectionPool -> User -> ActionM ()
register' pool user = do
  let rawPassword :: Text = userPassword user
  let rawPasswordBs = T.encodeUtf8 rawPassword
  newPasswordBs <- liftIO $ (makePassword rawPasswordBs 17)
  traceShowM newPasswordBs
  let newPassword = T.decodeUtf8 newPasswordBs
  let user' = user { userPassword = newPassword }
  -- write to db
  result :: Either SqlError () <- liftIO $ try $ flip Pgsql.runSqlPersistMPool pool $ do
    _ <- insert user'
    pure ()
  -- eo write to db
  case result of
    Right _ -> text $ "{\"success\": true}"
    Left (SqlError{sqlState = "23505"}) -> do
      S.status status400
      S.json UV.Validate {messages = [("email", "email already exsits"), ("username", "username already exsits")], valid = False}
    Left _ -> do
      S.status status500
      text $ "{\"success\": false}"

-- ------------------------

data Login = Login {
  usernameEmail :: Text,
  password :: Text
} deriving (Generic, Show)

instance ToJSON Login where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Login

login :: Pgsql.ConnectionPool -> V.Key (Session.Session IO String (Maybe (Entity User))) -> ActionM ()
login pool sessionKey = do
  login :: Login <- jsonData
  traceShowM login
  maybeUser :: Maybe (Entity User) <- liftIO $ flip Pgsql.runSqlPersistMPool pool $ do
    selectFirst ([Database2.UserUsername ==. usernameEmail login] ||. [Database2.UserEmail ==. usernameEmail login]) [LimitTo 1]

  case maybeUser of
    Nothing -> do
      S.status status401
      text $ "{\"success\": false}"
    Just userEntity -> do
      let (Entity userKey user) = userEntity
      let rawPassword :: Text = password login
      let rawPasswordBs = T.encodeUtf8 rawPassword
      newPasswordBs <- liftIO $ (makePassword rawPasswordBs 17)
      let newPassword = T.decodeUtf8 newPasswordBs
      if verifyPassword rawPasswordBs (T.encodeUtf8 $ userPassword user) then do
        theRequest <- request
        let theVault = vault theRequest
        let Just (sessionLookup, sessionInsert) = V.lookup sessionKey theVault
        liftIO $ sessionInsert "user" (Just userEntity)

        text $ "{\"success\": true}"
      else do
        S.status status401
        text $ "{\"success\": false}"

getUser :: Pgsql.ConnectionPool -> V.Key (Session.Session IO String (Maybe (Entity User))) -> ActionM ()
getUser pool sessionKey = do
  theRequest <- request
  let theVault = vault theRequest
  let Just (sessionLookup, sessionInsert) = V.lookup sessionKey theVault
  entityUser <- liftIO $ sessionLookup "user"
  case entityUser of
    Nothing -> S.json $ (Nothing :: (Maybe User))
    Just Nothing -> S.json $ (Nothing :: (Maybe User))
    Just (Just (Entity userKey user)) -> do
      S.json $ Just user

logout :: V.Key (Session.Session IO String (Maybe (Entity User))) -> ActionM ()
logout sessionKey = do
  theRequest <- request
  let theVault = vault theRequest
  let Just (sessionLookup, sessionInsert) = V.lookup sessionKey theVault
  liftIO $ sessionInsert "user" Nothing
  text $ "{\"success\": true}"