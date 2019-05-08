{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.SqliteTypes where

import Prelude

import Data.Aeson
import Data.Aeson.Types
    ( Parser )
import Data.Bifunctor
    ( bimap, first )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), ToText (..), fromTextMaybe, getTextDecodingError )
import Database.Persist.Sqlite
    ( PersistField (..), PersistFieldSql (..) )
import GHC.Generics
    ( Generic )
import Web.HttpApiData
import Web.PathPieces


import qualified Data.Text as T

import Cardano.Wallet.Primitive.Types
    ( Direction (..), Hash (..), SlotId (..), WalletId (..) )


instance PersistField Direction where
    toPersistValue = toPersistValue . directionToBool
    fromPersistValue pv = do
        let err = T.pack $ "not a valid value: " <> show pv
        bimap (const err) directionFromBool (fromPersistValue pv)

instance PersistFieldSql Direction where
    sqlType _ = sqlType (Proxy @Bool)

directionToBool :: Direction -> Bool
directionToBool Incoming = True
directionToBool Outgoing = False

directionFromBool :: Bool -> Direction
directionFromBool True = Incoming
directionFromBool False = Outgoing

-- Wraps Hash "Tx" because the persistent dsl doesn't like (Hash "Tx")
newtype TxId = TxId { getTxId :: Hash "Tx" } deriving (Show, Generic)

instance PersistField WalletId where
    toPersistValue =
        toPersistValue . toText
    fromPersistValue pv = do
        a <- fromText <$> fromPersistValue pv
        case a of
             Left _     ->
                 Left . T.pack $ "not a valid value: " <> show pv
             Right txid ->
                 pure txid

instance PersistField TxId where
    toPersistValue =
        toPersistValue . toText . getTxId
    fromPersistValue pv = do
        a <- fromText <$> fromPersistValue pv
        case a of
             Left _     ->
                 Left . T.pack $ "not a valid value: " <> show pv
             Right txid ->
                 pure (TxId txid)

instance PersistFieldSql WalletId where
    sqlType _ = sqlType (Proxy @Text)

instance PersistFieldSql TxId where
    sqlType _ = sqlType (Proxy @Text)

instance PersistFieldSql SlotId where
    sqlType _ = sqlType (Proxy @Text)

instance PersistField SlotId where
    toPersistValue = undefined
    fromPersistValue = undefined

instance Read WalletId where
    readsPrec _ = undefined -- fixme: bomb

instance ToHttpApiData WalletId where
    toUrlPiece = toText

instance FromHttpApiData WalletId where
    parseUrlPiece = first (T.pack . getTextDecodingError) . fromText

instance ToJSON WalletId where
    toJSON = String . toText

aesonFromText :: FromText a => String -> Value -> Parser a
aesonFromText what = withText what $ either (fail . show) pure . fromText

instance FromJSON WalletId where
    parseJSON = aesonFromText "WalletId"

instance PathPiece WalletId where
    fromPathPiece = fromTextMaybe
    toPathPiece = toText
