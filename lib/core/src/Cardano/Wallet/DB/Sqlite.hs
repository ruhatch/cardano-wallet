{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.Sqlite where

import Prelude

import Data.Text
    ( Text )
import qualified Data.Text as T

import Data.Proxy
    ( Proxy (..) )
import Data.Word
    ( Word64 )
import qualified Database.Esqueleto as E
import Database.Persist.Sqlite as Persist
import Database.Persist.TH
import qualified Database.Sqlite as Sqlite

import Cardano.Wallet.Primitive.Types
    ( Direction, Hash (..), SlotId, WalletId (..) )
import Data.Quantity
    ( Quantity )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import GHC.Generics
    ( Generic (..) )

newtype TxId = TxId { getTxId :: Hash "Tx" } deriving (Show, Generic)
-- type TxId = Text

share [ mkPersist sqlSettings { mpsPrefixFields = False } , mkMigrate "migrateAll" ] [persistLowerCase|
Wallet
  walletId              Text                   sql=id
  walletName            Text                   sql=name

  Primary walletId
  deriving Show Generic

TxMeta
  txMetaId              Text                   sql=id
  txMetaWalletId        Text                   sql=wallet_id
  txMetaDirection       Text                   sql=direction
  txMetaSlotId          Word64                 sql=slot_id
  txAmount              Word64                 sql=amount

  Primary txMetaId txMetaWalletId

  deriving Show Generic

TxInput
  txInputTxId           TxId                   sql=tx_id
  txInputSourceTxId     TxId                   sql=source_id
  txInputSourceIndex    Word32                 sql=source_index
  txInputAddress        Text                   sql=address
  txInputAmount         Word64                 sql=amount

  deriving Show Generic

TxOutput
  txOutputTxId          TxId                   sql=tx_id
  txOutputIndex         Word32                 sql=index
  txOutputAddress       Text                   sql=address
  txOutputAmount        Word64                 sql=amount

  deriving Show Generic
|]

{-
instance (ToText a, FromText a) => PersistField a where
    toPersistValue =
        toPersistValue . toText
    fromPersistValue pv = do
        res <- fromText <$> fromPersistValue pv
        case res of
             Left _ -> Left $ "not a valid value: " <> show pv
             Right a -> pure a
-}

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
