{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Sqlite where

import Prelude

import Cardano.Wallet.DB.SqliteTypes
    ()
import Cardano.Wallet.Primitive.Types
    ( Direction )
import Data.Text
    ( Text )
import Data.Word
    ( Word16, Word32, Word64 )
import Database.Persist.TH
import GHC.Generics
    ( Generic (..) )

share [ mkPersist sqlSettings { mpsPrefixFields = False } , mkMigrate "migrateAll" ] [persistLowerCase|
Slot
  epochNumber           Word64                 sql=epoch
  slotNumber            Word16                 sql=slot

  Primary epochNumber slotNumber
  deriving Show Generic

Wallet
  walId                Text                    sql=wallet_id
  walName              Text                    sql=name

  Primary walId walName
  deriving Show Generic

TxMeta
  theTxMetaId           Text                   sql=tx_id
  txMetaWalletId        WalletId               sql=wallet_id
  txMetaDirection       Direction              sql=direction
  txMetaSlotId          SlotId                 sql=slot_id
  txMetaAmount          Word64                 sql=amount

  Primary theTxMetaId txMetaWalletId
  deriving Show Generic

TxInput
  txInputTxId           Text                   sql=tx_id
  txInputSourceTxId     Text                   sql=source_id
  txInputSourceIndex    Word32                 sql=source_index
  txInputAddress        Text                   sql=address
  txInputAmount         Word64                 sql=amount

  Primary txInputTxId txInputSourceTxId txInputSourceIndex
  deriving Show Generic

TxOutput
  txOutputTxId          Text                   sql=tx_id
  txOutputIndex         Word32                 sql=index
  txOutputAddress       Text                   sql=address
  txOutputAmount        Word64                 sql=amount

  Primary txOutputTxId txOutputIndex
  deriving Show Generic

Utxo
  txinId                TxInputId              sql=txin_id
  txoutId               TxOutputId             sql=txout_id

  Primary txinId txoutId
  deriving Show Generic
|]
