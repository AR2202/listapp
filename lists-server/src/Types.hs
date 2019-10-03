{-# LANGUAGE DeriveGeneric #-}

module Types (
  Itype(..),
  Switch(..),
  Itemtype(..)
) where
import GHC.Generics
import Database.Persist as P
import Data.Text as T
import Database.Persist.Sql (PersistFieldSql(..))
import Data.Aeson
type Itype = Bool


data Switch = On | Off
  deriving (Show, Eq)

instance P.PersistField Switch where
  toPersistValue s = case s of
    On -> PersistBool True
    Off -> PersistBool False
  fromPersistValue (PersistBool b) = if b then Right On else Right Off
  fromPersistValue x = Left $ T.pack $ "File.hs: When trying to deserialize a Switch: expected PersistBool, received: " <> (show x)

instance PersistFieldSql Switch where
  sqlType _ = SqlBool



data Itemtype = Standarditem | Subheading
  deriving (Show, Eq, Generic)

instance ToJSON Itemtype where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Itemtype

instance P.PersistField Itemtype where
  toPersistValue s = case s of
    Standarditem -> PersistBool True
    Subheading -> PersistBool False
  fromPersistValue (PersistBool b) = if b then Right Standarditem else Right Subheading
  fromPersistValue x = Left $ T.pack $ "Types.hs: When trying to deserialize an Itemtype : expected PersistBool, received: " <> (show x)

instance PersistFieldSql Itemtype where
  sqlType _ = SqlBool
