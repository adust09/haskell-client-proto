module Main (main) where

import Control.Concurrent.STM
import System.Posix.Signals (installHandler, Handler(..), sigINT, sigTERM)

import LeanConsensus (version)
import Config (NodeConfig (..))
import Options (parseOptions, toNodeConfig)

main :: IO ()
main = do
  opts <- parseOptions
  let config = toNodeConfig opts
  putStrLn $ "lean-consensus v" <> version
  putStrLn $ "Data dir: " <> ncDataDir config
  putStrLn $ "Genesis: " <> ncGenesisFile config
  putStrLn $ "Listening on port " <> show (ncListenPort config)
  putStrLn $ "Bootnodes: " <> show (ncBootnodes config)
  putStrLn $ "Log level: " <> show (ncLogLevel config)

  -- Install signal handlers for graceful shutdown
  shutdownVar <- newTVarIO False
  let shutdown = atomically $ writeTVar shutdownVar True
  _ <- installHandler sigINT  (Catch shutdown) Nothing
  _ <- installHandler sigTERM (Catch shutdown) Nothing

  -- TODO: Initialize genesis state, storage, and start actors (Step 4).
  putStrLn "Waiting for shutdown signal (Ctrl-C)..."
  atomically $ do
    done <- readTVar shutdownVar
    if done then pure () else retry

  putStrLn "Shutting down."
