module Main where

import Prelude

import Cardano.Launcher
    ( Command (..), StdStream (..), launch )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, cancel, link )
import Control.Monad
    ( void )
import Data.Time
    ( addUTCTime, defaultTimeLocale, formatTime, getCurrentTime )
import Network.HTTP.Client
    ( defaultManagerSettings, newManager )
import System.Environment
    ( setEnv )
import System.IO
    ( IOMode (..), hClose, openFile )
import Test.Hspec
    ( after, afterAll, beforeAll, describe, hspec )
import Test.Integration.Framework.DSL
    ( Context (..), tearDown )

import qualified Cardano.Wallet.Network.HttpBridgeSpec as HttpBridge
import qualified Cardano.WalletSpec as Wallet
import qualified Test.Integration.Scenario.Wallets as Wallets

main :: IO ()
main = do
    hspec $ do
        describe "Cardano.WalletSpec" Wallet.spec
        describe "Cardano.Wallet.Network.HttpBridge" HttpBridge.spec
        beforeAll startCluster $ afterAll killCluster $ after tearDown $ do
            describe "Wallets API endpoint tests" Wallets.spec
  where
    startUpDelay :: Int
    startUpDelay = 4 * 1000 * 1000 -- 4 seconds in milliseconds

    -- Run a local cluster of cardano-sl nodes, a cardano-http-bridge on top and
    -- a cardano wallet server connected to the bridge.
    startCluster :: IO Context
    startCluster = do
        let stateDir = "./test/data/cardano-node-simple"
        handle <-
            openFile "/tmp/cardano-wallet-launcher" WriteMode
        systemStart <-
            formatTime defaultTimeLocale "%s" . addUTCTime 10 <$> getCurrentTime
        cluster <- async $ void $ launch
            [ cardanoNodeSimple stateDir systemStart ("core0", "127.0.0.1:3000")
            , cardanoNodeSimple stateDir systemStart ("core1", "127.0.0.1:3001")
            , cardanoNodeSimple stateDir systemStart ("core2", "127.0.0.1:3002")
            , cardanoNodeSimple stateDir systemStart ("relay", "127.0.0.1:3100")
            , cardanoWalletLauncher "1337" "8080" "local" handle
            ]
        link cluster
        let baseURL = "http://localhost:1337/"
        manager <- newManager defaultManagerSettings
        threadDelay (2 * startUpDelay)
        return $ Context cluster (baseURL, manager) handle

    killCluster :: Context -> IO ()
    killCluster (Context cluster _ handle) = do
        cancel cluster
        hClose handle

    cardanoNodeSimple stateDir systemStart (nodeId, nodeAddr) = Command
        "cardano-node-simple"
        [ "--system-start", systemStart
        , "--node-id", nodeId
        , "--keyfile", stateDir <> "/keys/" <> nodeId <> ".sk"
        , "--configuration-file", stateDir <> "/configuration.yaml"
        , "--configuration-key", "default"
        , "--topology", stateDir <> "/topology.json"
        , "--db-path", "/tmp/cardano-node-simple/db/" <> nodeId
        , "--listen", nodeAddr
        , "--log-config", stateDir <> "/logs/" <> nodeId <> "/config.json"
        , "--rebuild-db"
        ] (pure ())
        NoStream

    cardanoWalletLauncher serverPort bridgePort network handle = Command
        "cardano-wallet-launcher"
        [ "--wallet-server-port", serverPort
        , "--http-bridge-port", bridgePort
        ] (setEnv "NETWORK" network *> threadDelay startUpDelay)
        (UseHandle handle)