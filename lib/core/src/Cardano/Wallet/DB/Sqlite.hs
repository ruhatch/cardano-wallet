{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.Sqlite where

import Prelude

import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import qualified Data.Text as T
import Data.Word
    ( Word32, Word64 )
import GHC.Generics
    ( Generic (..) )

import qualified Database.Esqueleto as E
import Database.Persist.Sqlite as Persist
import Database.Persist.TH
import qualified Database.Sqlite as Sqlite

import Cardano.Wallet.Primitive.Types
    ( Direction, Hash (..), SlotId )
import qualified Cardano.Wallet.Primitive.Types as W

import Data.Quantity
    ( Quantity )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )

-- Wraps Hash "Tx" because the persistent dsl doesn't like (Hash "Tx")
newtype TxId = TxId { getTxId :: Hash "Tx" } deriving (Show, Generic)

share [ mkPersist sqlSettings { mpsPrefixFields = False } , mkMigrate "migrateAll" ] [persistLowerCase|
Wallet
  walletId              W.WalletId             sql=id
  walletName            Text                   sql=name

  Primary walletId
  deriving Show Generic

TxMeta
  txMetaId              TxId                   sql=tx_id
  txMetaWalletId        W.WalletId             sql=wallet_id
  txMetaDirection       Direction              sql=direction
  txMetaSlotId          SlotId                 sql=slot_id
  txMetaAmount          Word64                 sql=amount

  Primary txMetaId txMetaWalletId

  deriving Show Generic

TxInput
  txInputTxId           TxId                   sql=tx_id
  txInputSourceTxId     TxId                   sql=source_id
  txInputSourceIndex    Word32                 sql=source_index
  txInputAddress        Text                   sql=address
  txInputAmount         Word64                 sql=amount

  Primary txInputTxId txInputSourceTxId txInputSourceIndex

  deriving Show Generic

TxOutput
  txOutputTxId          TxId                   sql=tx_id
  txOutputIndex         Word32                 sql=index
  txOutputAddress       Text                   sql=address
  txOutputAmount        Word64                 sql=amount

  Primary txOutputTxId txOutputIndex

  deriving Show Generic
|]

instance PersistField W.WalletId where
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

instance PersistFieldSql W.WalletId where
    sqlType _ = sqlType (Proxy @Text)

instance PersistFieldSql TxId where
    sqlType _ = sqlType (Proxy @Text)
