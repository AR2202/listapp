{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module User.Validate (
  isUserValid,
  Validate(..)
) where

import Data.Maybe
import Data.Text as T
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)
import Data.String (IsString)
import Database.Persist
import Database.Persist.Postgresql as Pgsql
import Text.Email.Validate
import Database2
import Data.ByteString.Char8 as BS8
import Data.Aeson
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Debug.Trace


data Validate = Validate {
    messages :: [(String, String)],
    valid :: Bool
  } deriving (Generic, Show)

instance ToJSON Validate where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Validate

isUserValid :: Pgsql.ConnectionPool -> User -> IO (Validate)
isUserValid pool user = do
  let emptyValidate = Validate {messages = [], valid = True}
  let valid1 = isEmailValid (userEmail user) emptyValidate
  let valid2 = isLongEnough "username" (userUsername user) valid1
  let valid3 = isLongEnough "password" (userPassword user) valid2
  -- valid4 <- isEmailUnqiue (email user) pool valid3
  -- valid5 <- isUsernameUnqiue (username user) pool valid4
  return valid3

isEmailValid email validate = 
  case isValid $ BS8.pack $ T.unpack email of 
    True -> validate
    False -> validate {valid = False, messages = (("email", "Invalid email") : (messages validate))}


isEmailUnqiue :: 
  T.Text -> 
  Pgsql.ConnectionPool -> 
  Validate -> 
  IO (Validate)
isEmailUnqiue email pool validate = do
  maybeUser :: Maybe (Entity User) <- liftIO $ flip runSqlPersistMPool pool $ do
    selectFirst ([Database2.UserUsername ==. email]
      ) [LimitTo 1]
  case maybeUser of
    Just x -> return validate {valid = False, messages = (("email", "Email has allready been taken") : (messages validate))}
    Nothing -> return validate

isLongEnough :: String -> T.Text -> Validate -> Validate
isLongEnough key in1 validate = 
  if T.length in1 > 4 then
      validate
    else 
      validate {valid = False, messages = ((key, T.unpack $ T.concat [(T.toTitle $ T.pack key)," needs to be > 4 characters"]) : (messages validate))}  

isUsernameUnqiue :: 
  T.Text -> 
  Pgsql.ConnectionPool -> 
  Validate -> 
  IO (Validate)
isUsernameUnqiue username pool validate = do
  maybeUser :: Maybe (Entity User) <- liftIO $ flip runSqlPersistMPool pool $ do
    selectFirst ([UserUsername ==. username]
      ) [LimitTo 1]
  case maybeUser of
    Just x -> return validate {valid = False, messages = (("username", "Username has allready been taken") : (messages validate))}
    Nothing -> return validate
