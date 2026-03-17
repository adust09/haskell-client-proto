-- | Node-level actor orchestration for the lean-consensus client.
module Node
  ( NodeActors (..)
  , BlockchainMsg (..)
  , P2PMsg (..)
  , ValidatorMsg (..)
  , startNode
  , stopNode
  , waitAllActors
  ) where

import Control.Concurrent.STM
import Control.Exception (SomeException)

import Actor (Actor, spawnActor, stopActor, waitActor)
import Config (NodeConfig (..))
import Consensus.Types
  ( SignedBeaconBlock
  , SignedAttestation
  , SignedAggregatedAttestation
  )
import Consensus.Constants (Slot)
import Genesis (GenesisConfig)
import Storage (StorageHandle)

-- | Messages for the blockchain (fork-choice) actor.
data BlockchainMsg
  = BcSlotTick !Slot
  | BcNewBlock !SignedBeaconBlock
  | BcNewAttestation !SignedAttestation
  | BcNewAggregation !SignedAggregatedAttestation
  | BcShutdown

-- | Messages for the P2P networking actor.
data P2PMsg
  = P2PPublishBlock !SignedBeaconBlock
  | P2PPublishAttestation !SignedAttestation
  | P2PPublishAggregation !SignedAggregatedAttestation
  | P2PShutdown

-- | Messages for the validator duty actor.
data ValidatorMsg
  = ValSlotTick !Slot
  | ValShutdown

-- | Collection of all running node actors.
data NodeActors = NodeActors
  { naBlockchain :: !(Actor BlockchainMsg)
  , naP2P        :: !(Actor P2PMsg)
  , naValidator  :: !(Maybe (Actor ValidatorMsg))
  }

-- | Start all node actors. Validator actor only starts if ncValidatorKeyDir is set.
startNode
  :: NodeConfig
  -> StorageHandle
  -> GenesisConfig
  -> IO NodeActors
startNode config _storageHandle _genesis = do
  bcActor <- spawnActor "blockchain" blockchainLoop
  p2pActor <- spawnActor "p2p" p2pLoop
  valActor <- case ncValidatorKeyDir config of
    Nothing  -> pure Nothing
    Just _   -> Just <$> spawnActor "validator" validatorLoop
  pure $ NodeActors bcActor p2pActor valActor

-- | Stop all node actors gracefully.
stopNode :: NodeActors -> IO ()
stopNode actors = do
  maybe (pure ()) stopActor (naValidator actors)
  stopActor (naP2P actors)
  stopActor (naBlockchain actors)

-- | Wait for all actors to complete, returning the first error if any.
waitAllActors :: NodeActors -> IO (Maybe SomeException)
waitAllActors actors = do
  bcResult <- waitActor (naBlockchain actors)
  p2pResult <- waitActor (naP2P actors)
  valResult <- case naValidator actors of
    Nothing -> pure (Right ())
    Just v  -> waitActor v
  pure $ firstLeft [bcResult, p2pResult, valResult]
  where
    firstLeft [] = Nothing
    firstLeft (Left e : _) = Just e
    firstLeft (Right _ : rest) = firstLeft rest

-- | Blockchain actor loop: processes messages from its queue.
blockchainLoop :: TQueue BlockchainMsg -> IO ()
blockchainLoop queue = go
  where
    go = do
      msg <- atomically $ readTQueue queue
      case msg of
        BcShutdown -> pure ()
        BcSlotTick _slot -> go
        BcNewBlock _block -> go
        BcNewAttestation _att -> go
        BcNewAggregation _agg -> go

-- | P2P actor loop: processes outgoing publish messages.
p2pLoop :: TQueue P2PMsg -> IO ()
p2pLoop queue = go
  where
    go = do
      msg <- atomically $ readTQueue queue
      case msg of
        P2PShutdown -> pure ()
        P2PPublishBlock _block -> go
        P2PPublishAttestation _att -> go
        P2PPublishAggregation _agg -> go

-- | Validator actor loop: responds to slot ticks with duty checks.
validatorLoop :: TQueue ValidatorMsg -> IO ()
validatorLoop queue = go
  where
    go = do
      msg <- atomically $ readTQueue queue
      case msg of
        ValShutdown -> pure ()
        ValSlotTick _slot -> go
