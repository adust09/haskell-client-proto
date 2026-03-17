module Main (main) where

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM
import qualified Data.ByteString.Lazy as BL
import System.Posix.Signals (installHandler, Handler(..), sigINT, sigTERM)

import LeanConsensus (version)
import Config (NodeConfig (..))
import Genesis (GenesisConfig (..), parseGenesisConfig, initializeGenesis)
import Node (startNode, stopNode, runSlotTicker)
import Consensus.SlotTimer (slotTicker)
import Options (parseOptions, toNodeConfig)
import Storage (withStorage)

main :: IO ()
main = do
  opts <- parseOptions
  let config = toNodeConfig opts
  putStrLn $ "lean-consensus v" <> version
  putStrLn $ "Data dir: " <> ncDataDir config
  putStrLn $ "Log level: " <> show (ncLogLevel config)

  -- Load genesis
  genesisBytes <- BL.readFile (ncGenesisFile config)
  genesis <- case parseGenesisConfig genesisBytes of
    Left err -> fail $ "Failed to parse genesis: " <> err
    Right gc -> pure gc

  let (genesisState, genesisStore) = initializeGenesis genesis
  putStrLn $ "Genesis loaded: " <> show (length (gcValidators genesis)) <> " validators"

  -- Install signal handlers for graceful shutdown
  shutdownVar <- newTVarIO False
  let shutdown = atomically $ writeTVar shutdownVar True
  _ <- installHandler sigINT  (Catch shutdown) Nothing
  _ <- installHandler sigTERM (Catch shutdown) Nothing

  -- Start storage and actors
  withStorage (ncDataDir config) genesisState genesisStore $ \storageHandle -> do
    actors <- startNode config storageHandle genesis
    putStrLn "Node started. Waiting for shutdown signal (Ctrl-C)..."

    -- Start slot ticker feeding the blockchain actor
    tickerThread <- async $ slotTicker (gcGenesisTime genesis) $ \slot -> do
      runSlotTicker actors slot

    -- Wait for shutdown
    atomically $ do
      done <- readTVar shutdownVar
      if done then pure () else retry

    -- Graceful shutdown
    putStrLn "Shutting down..."
    cancel tickerThread
    stopNode actors
    putStrLn "Shutdown complete."
