-- | Aggregator role: collect attestations per subnet, aggregate them with
-- leanMultisig, and publish the result on the aggregation topic.
module Network.Aggregator
  ( -- * Types
    AggregatorEnv (..)
  , AttestationPool
  , newAttestationPool
    -- * Pool operations
  , addAttestation
  , drainAttestations
    -- * Aggregator
  , startAggregator
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Consensus.Constants (SubnetId, Root, Domain)
import Consensus.SlotTimer (SlotPhase (..), waitUntilPhase)
import Consensus.Types
    ( AttestationData, SignedAttestation (..)
    , Store (..), Checkpoint (..)
    , BeaconState (..), Validator (..)
    , XmssPubkey
    )
import Crypto.LeanMultisig (ProverContext)
import Crypto.Operations (aggregateAttestations)
import Network.P2P.Types (P2PHandle (..), Topic (..))
import Network.P2P.Wire (decodeWire, encodeWire)
import SSZ.List (unSszList)

import Data.Time.Clock (UTCTime)

-- ---------------------------------------------------------------------------
-- Attestation pool
-- ---------------------------------------------------------------------------

-- | Per-AttestationData collection of signed attestations.
type AttestationPool = TVar (Map AttestationData [SignedAttestation])

-- | Create a new empty attestation pool.
newAttestationPool :: IO AttestationPool
newAttestationPool = newTVarIO Map.empty

-- | Add a verified attestation to the pool, grouped by AttestationData.
addAttestation :: AttestationPool -> SignedAttestation -> STM ()
addAttestation pool sa = do
  m <- readTVar pool
  let ad = saData sa
      existing = Map.findWithDefault [] ad m
      vi = saValidatorIndex sa
      isDup = any (\s -> saValidatorIndex s == vi) existing
  if isDup
    then pure ()
    else writeTVar pool (Map.insert ad (sa : existing) m)

-- | Drain and return all attestations, clearing the pool.
drainAttestations :: AttestationPool -> STM (Map AttestationData [SignedAttestation])
drainAttestations pool = do
  m <- readTVar pool
  writeTVar pool Map.empty
  pure m

-- ---------------------------------------------------------------------------
-- Aggregator environment and main loop
-- ---------------------------------------------------------------------------

-- | Environment for the aggregator actor.
data AggregatorEnv = AggregatorEnv
  { aeP2PHandle    :: !P2PHandle
  , aeStore        :: !(TVar Store)
  , aeProver       :: !ProverContext
  , aePool         :: !AttestationPool
  , aeGenesisTime  :: !UTCTime
  , aeSubnets      :: ![SubnetId]
  , aeDomain       :: !Domain
  }

-- | Aggregation timeout in microseconds (800ms).
aggregationTimeoutUs :: Int
aggregationTimeoutUs = 800_000

-- | Start the aggregator: subscribe to attestation subnets, collect,
-- aggregate at ConfirmationPhase, publish result.
startAggregator :: AggregatorEnv -> IO ()
startAggregator env = do
  -- Subscribe to assigned attestation subnets
  mapM_ (\sid ->
    p2hSubscribe (aeP2PHandle env) (TopicAttestation sid) $ \raw ->
      case decodeWire raw of
        Left _ -> pure ()
        Right sa -> atomically $ addAttestation (aePool env) sa
    ) (aeSubnets env)

  -- Main loop: wait for ConfirmationPhase, aggregate, publish
  aggregatorLoop env

aggregatorLoop :: AggregatorEnv -> IO ()
aggregatorLoop env = do
  waitUntilPhase (aeGenesisTime env) ConfirmationPhase

  -- Drain the pool
  groups <- atomically $ drainAttestations (aePool env)

  -- Get current validator pubkeys from store
  store <- readTVarIO (aeStore env)
  let justRoot = cpRoot (stJustifiedCheckpoint store)
      mState = Map.lookup justRoot (stBlockStates store)

  case mState of
    Nothing -> pure ()
    Just bs -> do
      let validators = unSszList (bsValidators bs)
          pubkeys = [ vPubkey v | v <- validators ]
          domain = aeDomain env

      -- Aggregate each group with timeout
      mapM_ (\(ad, atts) ->
        aggregateAndPublish env pubkeys domain ad atts
        ) (Map.toList groups)

  aggregatorLoop env

-- | Aggregate a group of attestations and publish the result.
-- Times out after 800ms and skips if the prover is too slow.
aggregateAndPublish :: AggregatorEnv -> [XmssPubkey] -> Root
                    -> AttestationData -> [SignedAttestation] -> IO ()
aggregateAndPublish env pubkeys domain _ad atts = do
  let subnetId = case atts of
        (sa:_) -> saValidatorIndex sa `mod` 4
        []     -> 0
  result <- race
    (threadDelay aggregationTimeoutUs)
    (aggregateAttestations (aeProver env) atts pubkeys domain subnetId)
  case result of
    Left () -> pure ()  -- timeout, skip
    Right (Left _err) -> pure ()  -- aggregation failed, skip
    Right (Right saa) ->
      p2hPublish (aeP2PHandle env) TopicAggregation (encodeWire saa)
