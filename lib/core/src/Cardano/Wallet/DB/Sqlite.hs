{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Sqlite where

import Prelude

import Cardano.Wallet.DB.SqliteTypes
    ( TxId )
import Cardano.Wallet.Primitive.Types
    ( Direction, SlotId )
import Data.Text
    ( Text )
import Data.Word
    ( Word32, Word64 )
import Database.Persist.TH
import GHC.Generics
    ( Generic (..) )

import qualified Cardano.Wallet.Primitive.Types as W

share
    [ mkPersist sqlSettings { mpsPrefixFields = False }
    , mkMigrate "migrateAll"
    ]
    [persistLowerCase|

Wallet
  walId                W.WalletId              sql=wallet_id
  walName              Text                    sql=name

  Primary walId
  deriving Show Generic

WalletPrivateKey sql=private_key
  walPrivateKeyWalId   W.WalletId              sql=wallet_id
  walPrivateKey        Text                    sql=private_key

  Primary walPrivateKeyWalId
  Foreign Wallet fk_wallet_private_key walPrivateKeyWalId

  deriving Show Generic

TxMeta
  txId                  TxId                   sql=tx_id
  txMetaWalletId        W.WalletId             sql=wallet_id
  -- txStatus             W.TxStatus             sql=status
  txMetaDirection       Direction              sql=direction
  txMetaSlotId          SlotId                 sql=slot_id
  txMetaAmount          Word64                 sql=amount

  Primary txId txMetaWalletId
  Foreign Wallet fk_wallet_tx_meta txMetaWalletId
  deriving Show Generic

TxInput
  txInputTxId           TxId                   sql=tx_id
  txInputSourceTxId     TxId                   sql=source_id
  txInputSourceIndex    Word32                 sql=source_index
  txInputAddress        Text                   sql=address
  txInputAmount         Word64                 sql=amount

  Primary txInputTxId txInputSourceTxId txInputSourceIndex
  -- constraint: tx_id must exist in TxMeta
  deriving Show Generic

TxOutput
  txOutputTxId          TxId                   sql=tx_id
  txOutputIndex         Word32                 sql=index
  txOutputAddress       Text                   sql=address
  txOutputAmount        Word64                 sql=amount

  Primary txOutputTxId txOutputIndex
  -- constraint: tx_id must exist in TxMeta
  deriving Show Generic

Utxo
  utxoWalletId          W.WalletId             sql=wallet_id
  utxoTxOutputTxId      TxId                   sql=tx_id
  utxoTxOutputIndex     Word32                 sql=index

  Primary utxoWalletId utxoTxOutputTxId utxoTxOutputIndex
  Foreign Wallet fk_wallet_utxo utxoWalletId
  Foreign TxOutput fk_tx_output_utxo utxoTxOutputTxId utxoTxOutputIndex

  deriving Show Generic

Checkpoint
  checkpointWalId       W.WalletId             sql=wallet_id
  checkpointWalSlot     SlotId                 sql=slot_id

  Primary checkpointWalId checkpointWalSlot
  Foreign Wallet fk_wallet_checkpoint checkpointWalId

  deriving Show Generic

PendingTx
  pendingTxId2         TxId                   sql=tx_id
  pendingTxWalletId    W.WalletId             sql=wallet_id
  pendingTxSlotId      SlotId                 sql=slot_id

  Primary pendingTxId2
  Foreign Checkpoint fk_checkpoint_pending_tx pendingTxWalletId pendingTxSlotId
  -- constraint: Inputs and outputs come from TxInput and TxOutput

  deriving Show Generic

|]
