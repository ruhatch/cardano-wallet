{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.SqliteTypes where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Direction (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Database.Persist.Sqlite
    ( PersistField (..), PersistFieldSql (..) )

import qualified Data.Text as T

instance PersistField Direction where
    toPersistValue =
        toPersistValue . toText
    fromPersistValue pv = do
        a <- fromText <$> fromPersistValue pv
        case a of
             Left _     ->
                 Left . T.pack $ "not a valid value: " <> show pv
             Right direction ->
                 pure direction

instance PersistFieldSql Direction where
    sqlType _ = sqlType (Proxy @Text)

instance FromText Direction where
    fromText txt = case txt of
        "Outgoing" -> Right Outgoing
        "Incoing" -> Right Incoming
        _ ->
            Left . TextDecodingError $ "not a valid value: " <> show txt

instance ToText Direction where
    toText Outgoing = "Outgoing"
    toText Incoming = "Incoming"
