{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.SqliteTypes where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Direction (..) )
import Data.Bifunctor
    ( first )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Database.Persist.Sqlite
    ( PersistField (..), PersistFieldSql (..) )

import qualified Data.Text as T

instance PersistField Direction where
    toPersistValue =
        toPersistValue . toText
    fromPersistValue pv = do
        let err = T.pack $ "not a valid value: " <> show pv
        first (const err) (fromPersistValue pv)

instance PersistFieldSql Direction where
    sqlType _ = sqlType (Proxy @Text)
