module Main (main) where

import Control.Concurrent.STM
import System.Posix.Signals (installHandler, Handler(..), sigINT, sigTERM)

import LeanConsensus (version)
import Options (Options (..), parseOptions)

main :: IO ()
main = do
  opts <- parseOptions
  putStrLn $ "lean-consensus v" <> version
  putStrLn $ "Listening on port " <> show (optListenPort opts)
  putStrLn $ "Bootnodes: " <> show (optBootnodes opts)
  putStrLn $ "Aggregator mode: " <> show (optIsAggregator opts)

  -- Install signal handlers for graceful shutdown
  shutdownVar <- newTVarIO False
  let shutdown = atomically $ writeTVar shutdownVar True
  _ <- installHandler sigINT  (Catch shutdown) Nothing
  _ <- installHandler sigTERM (Catch shutdown) Nothing

  -- TODO: Initialize genesis state, P2P handle, and start actors.
  -- This is the main loop skeleton — full wiring requires P2P backend
  -- (FFI or IPC) which is Step 6.
  putStrLn "Waiting for shutdown signal (Ctrl-C)..."
  atomically $ do
    done <- readTVar shutdownVar
    if done then pure () else retry

  putStrLn "Shutting down."
