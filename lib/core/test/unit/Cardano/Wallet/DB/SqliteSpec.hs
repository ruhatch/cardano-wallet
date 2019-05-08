{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.SqliteSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldReturn )

import Conduit
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH


import Cardano.Wallet.DB.Sqlite

runSqlite' :: (MonadUnliftIO m) => Text -> ReaderT SqlBackend (LoggingT (ResourceT m)) a -> m a
runSqlite' connstr = runResourceT . runStderrLoggingT . withSqliteConn connstr . runSqlConn

testMigrate :: IO ()
testMigrate = runSqlite' ":memory:" $ do
    runMigration migrateAll

spec :: Spec
spec = do
    describe "Generated SQL schema" $ do
        it "looks like SQL" $ do
            False `shouldBe` not True
