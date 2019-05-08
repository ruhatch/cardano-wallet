{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Data.Quantity
    ( Quantity (..), mkPercentage )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), ToText (..), fromTextMaybe, getTextDecodingError )
import Data.Word
    ( Word64, Word8 )
import Database.Persist.Sqlite
    ( PersistField (..), PersistFieldSql (..) )
import GHC.Generics
    ( Generic )
import Web.HttpApiData
import Web.PathPieces

import qualified Data.Text as T

import Cardano.Wallet.Primitive.Types
    ( Direction (..)
    , Hash (..)
    , SlotId (..)
    , WalletId (..)
    , WalletState (..)
    , flatSlot
    , fromFlatSlot
    )

----------------------------------------------------------------------------
-- Helper functions

parseUrlPieceFromText :: FromText a => Text -> Either Text a
parseUrlPieceFromText = first (T.pack . getTextDecodingError) . fromText

aesonFromText :: FromText a => String -> Value -> Parser a
aesonFromText what = withText what $ either (fail . show) pure . fromText

----------------------------------------------------------------------------
-- Direction

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

----------------------------------------------------------------------------
-- WalletId

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

instance PersistFieldSql WalletId where
    sqlType _ = sqlType (Proxy @Text)

instance Read WalletId where
    readsPrec = undefined -- fixme: bomb

instance ToHttpApiData WalletId where
    toUrlPiece = toText

instance FromHttpApiData WalletId where
    parseUrlPiece = parseUrlPieceFromText

instance ToJSON WalletId where
    toJSON = String . toText

instance FromJSON WalletId where
    parseJSON = aesonFromText "WalletId"

instance PathPiece WalletId where
    fromPathPiece = fromTextMaybe
    toPathPiece = toText


----------------------------------------------------------------------------
-- TxId

-- Wraps Hash "Tx" because the persistent dsl doesn't like (Hash "Tx")
newtype TxId = TxId { getTxId :: Hash "Tx" } deriving (Show, Eq, Ord, Generic)

instance PersistField TxId where
    toPersistValue = toPersistValue . toText . getTxId
    fromPersistValue pv = do
        a <- fromText <$> fromPersistValue pv
        case a of
             Left _     ->
                 Left . T.pack $ "not a valid value: " <> show pv
             Right txid ->
                 pure (TxId txid)

instance PersistFieldSql TxId where
    sqlType _ = sqlType (Proxy @Text)

instance Read TxId where
    readsPrec = undefined -- fixme: bomb

instance ToJSON TxId where
    toJSON = String . toText . getTxId

instance FromJSON TxId where
    parseJSON = fmap TxId . aesonFromText "WalletId"

instance ToHttpApiData TxId where
    toUrlPiece = toText . getTxId

instance FromHttpApiData TxId where
    parseUrlPiece = fmap TxId . parseUrlPieceFromText

instance PathPiece TxId where
    toPathPiece = toText . getTxId
    fromPathPiece = fmap TxId . fromTextMaybe

----------------------------------------------------------------------------
-- SlotId

instance PersistFieldSql SlotId where
    sqlType _ = sqlType (Proxy @Word64)

instance PersistField SlotId where
    toPersistValue = toPersistValue . flatSlot
    fromPersistValue = fmap fromFlatSlot . fromPersistValue

{-
instance Read SlotId where
    readsPrec _ input | sep == '.' = [(SlotId (read epoch) (read slot), rest)]
                      | otherwise = []
      where
        (epoch, sep:rest1) = span isDigit input
        (slot, rest) = span isDigit rest1
-}

instance ToJSON SlotId where
    toJSON = genericToJSON defaultOptions

instance FromJSON SlotId where
    parseJSON = genericParseJSON defaultOptions

----------------------------------------------------------------------------
-- WalletState

walletStateNum :: WalletState -> Word8
walletStateNum Ready = 100
walletStateNum (Restoring (Quantity pc)) = n
    where Just n = decode (encode pc) -- fixme: naff

walletStateFromNum :: Word8 -> WalletState
walletStateFromNum n | n < 100 = Restoring (Quantity pc)
                     | otherwise = Ready
    where Right pc = mkPercentage n

instance PersistField WalletState where
    toPersistValue = toPersistValue . walletStateNum
    fromPersistValue = fmap walletStateFromNum . fromPersistValue

instance PersistFieldSql WalletState where
    sqlType _ = sqlType (Proxy @Text)

instance Read WalletState where
    readsPrec = undefined -- fixme: bomb

instance ToJSON WalletState where
    toJSON = genericToJSON defaultOptions

instance FromJSON WalletState where
    parseJSON = genericParseJSON defaultOptions
