-- | Node-level actor orchestration for the lean-consensus client.
module Node
  ( NodeActors (..)
  , BlockchainMsg (..)
  , P2PMsg (..)
  , ValidatorMsg (..)
  , startNode
  , stopNode
  , waitAllActors
  , runSlotTicker
  ) where

import Control.Concurrent.STM
import Control.Exception (SomeException)

import Actor (Actor (..), spawnActor, send, waitActor)
import Config (NodeConfig (..))
import qualified Data.Map.Strict as Map
import Consensus.Types
  ( SignedBeaconBlock
  , SignedAttestation
  , SignedAggregatedAttestation (..)
  , Store (..)
  , BeaconState (..)
  , AttestationData (..)
  , LatestMessage (..)
  , Checkpoint (..)
  )
import Consensus.Constants (Root, Slot, ValidatorIndex)
import Consensus.ForkChoice (onBlock, onAttestation)
import Consensus.StateTransition (processSlots, expandAggregationBits)
import Genesis (GenesisConfig)
import Storage
  ( StorageHandle
  , readCurrentState
  , writeCurrentState
  , readForkChoiceStore
  , writeForkChoiceStore
  , putBlock
  )
import SSZ.Common (mkBytesN)
import SSZ.Merkleization (SszHashTreeRoot (..))

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
startNode config storageHandle _genesis = do
  bcActor <- spawnActor "blockchain" (blockchainLoop storageHandle)
  p2pActor <- spawnActor "p2p" p2pLoop
  valActor <- case ncValidatorKeyDir config of
    Nothing  -> pure Nothing
    Just _   -> Just <$> spawnActor "validator" validatorLoop
  pure $ NodeActors bcActor p2pActor valActor

-- | Stop all node actors gracefully by sending shutdown messages.
stopNode :: NodeActors -> IO ()
stopNode actors = do
  maybe (pure ()) (\v -> atomically $ send v ValShutdown) (naValidator actors)
  atomically $ send (naP2P actors) P2PShutdown
  atomically $ send (naBlockchain actors) BcShutdown
  -- Wait for graceful completion
  maybe (pure ()) (void . waitActor) (naValidator actors)
  void $ waitActor (naP2P actors)
  void $ waitActor (naBlockchain actors)
  where
    void f = f >> pure ()

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

-- | Feed slot ticks to the blockchain and (optional) validator actors.
runSlotTicker :: NodeActors -> Slot -> IO ()
runSlotTicker actors slot = do
  atomically $ send (naBlockchain actors) (BcSlotTick slot)
  case naValidator actors of
    Nothing -> pure ()
    Just v  -> atomically $ send v (ValSlotTick slot)

-- | Blockchain actor loop: processes slot ticks, blocks, and attestations.
blockchainLoop :: StorageHandle -> TQueue BlockchainMsg -> IO ()
blockchainLoop storage queue = go
  where
    go = do
      msg <- atomically $ readTQueue queue
      case msg of
        BcShutdown -> pure ()

        BcSlotTick targetSlot -> do
          state <- atomically $ readCurrentState storage
          case processSlots state targetSlot of
            Left _err    -> pure ()
            Right state' -> atomically $ writeCurrentState storage state'
          go

        BcNewBlock signedBlock -> do
          store <- atomically $ readForkChoiceStore storage
          case onBlock store signedBlock of
            Left _err    -> pure ()
            Right store' -> do
              let root = toRoot signedBlock
              putBlock storage root signedBlock
              atomically $ writeForkChoiceStore storage store'
          go

        BcNewAttestation att -> do
          store <- atomically $ readForkChoiceStore storage
          case onAttestation store att of
            Left _err    -> pure ()
            Right store' -> atomically $ writeForkChoiceStore storage store'
          go

        BcNewAggregation agg -> do
          -- Expand aggregation bits and apply each voter's attestation to fork choice
          store <- atomically $ readForkChoiceStore storage
          let ad = saaData agg
              subnetId = saaSubnetId agg
              voterIndices = expandAggregationBits
                (bsValidators (lookupJustifiedState store)) subnetId (saaAggregationBits agg)
              headRoot = adHeadRoot ad
              attSlot = adSlot ad
              store' = foldl (\s vi -> updateLatestMsg s vi attSlot headRoot) store voterIndices
          atomically $ writeForkChoiceStore storage store'
          go

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

-- | Compute the SSZ hash tree root as a Bytes32 root.
toRoot :: SszHashTreeRoot a => a -> Root
toRoot a = case mkBytesN @32 (hashTreeRoot a) of
  Right r -> r
  Left _  -> error "toRoot: hashTreeRoot did not produce 32 bytes"

-- | Look up the beacon state at the justified checkpoint root.
lookupJustifiedState :: Store -> BeaconState
lookupJustifiedState store =
  let justRoot = cpRoot (stJustifiedCheckpoint store)
  in  case Map.lookup justRoot (stBlockStates store) of
        Just bs -> bs
        Nothing -> error "lookupJustifiedState: justified state missing"

-- | Update a validator's latest message if the new slot is newer.
updateLatestMsg :: Store -> ValidatorIndex -> Slot -> Root -> Store
updateLatestMsg store vi slot root =
  let msg = LatestMessage slot root
      shouldUpdate = case Map.lookup vi (stLatestMessages store) of
        Nothing  -> True
        Just old -> slot > lmSlot old
  in  if shouldUpdate
        then store { stLatestMessages = Map.insert vi msg (stLatestMessages store) }
        else store
