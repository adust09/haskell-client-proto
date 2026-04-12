-- | Gossip message validation and dispatch.
-- Validates incoming blocks, attestations, and aggregations before forwarding
-- to the fork choice / state transition layer.
module Network.MessageHandler
  ( -- * Types
    GossipMessage (..)
  , ValidationResult (..)
    -- * Seen cache
  , SeenCache
  , newSeenCache
  , markSeen
    -- * Validation
  , validateBlock
  , validateAttestation
  , validateAggregation
    -- * Handler
  , MessageHandlerEnv (..)
  , startMessageHandler
  ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Consensus.Constants (Slot, SubnetId)
import Consensus.ForkChoice (onBlock, onAttestation)
import Consensus.Types
import Crypto.Hashing (sha256)
import Crypto.LeanMultisig (VerifierContext)
import Crypto.Operations (verifyAggregatedAttestation)
import Network.P2P.Types (P2PHandle (..), Topic (..))
import Network.P2P.Wire (decodeWire)
import SSZ.List (unSszList)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Decoded gossip message variants.
data GossipMessage
  = GossipBlock !SignedBlock
  | GossipAttestation !SignedAttestation !SubnetId
  | GossipAggregation !SignedAggregatedAttestation
  deriving stock (Show)

-- | Validation outcome for a gossip message.
data ValidationResult = Accept | Reject | Ignore
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- Seen cache (bounded dedup)
-- ---------------------------------------------------------------------------

-- | Bounded dedup cache keyed by sha256 of SSZ-encoded message.
data SeenCache = SeenCache
  { scMap   :: !(Map ByteString ())
  , scQueue :: !(Seq ByteString)
  , scMax   :: !Int
  }

-- | Create a new empty cache with the given max size.
newSeenCache :: Int -> SeenCache
newSeenCache maxSize = SeenCache Map.empty Seq.empty maxSize

-- | Check and mark a message as seen. Returns 'True' if already seen.
markSeen :: ByteString -> SeenCache -> (Bool, SeenCache)
markSeen raw cache =
  let key = sha256 raw
  in  if Map.member key (scMap cache)
        then (True, cache)
        else
          let cache1 = if Seq.length (scQueue cache) >= scMax cache
                then evictOldest cache
                else cache
          in  ( False
              , cache1
                  { scMap   = Map.insert key () (scMap cache1)
                  , scQueue = scQueue cache1 Seq.|> key
                  }
              )

evictOldest :: SeenCache -> SeenCache
evictOldest cache =
  case Seq.viewl (scQueue cache) of
    Seq.EmptyL -> cache
    oldest Seq.:< rest ->
      cache { scMap = Map.delete oldest (scMap cache), scQueue = rest }

-- ---------------------------------------------------------------------------
-- Validation
-- ---------------------------------------------------------------------------

-- | Validate a gossiped block against the current store.
validateBlock :: Store -> SignedBlock -> Slot -> ValidationResult
validateBlock store sb currentSlot =
  let block = sbMessage sb
      blockSlot = bbSlot block
      parentRoot = bbParentRoot block
  in  if blockSlot > currentSlot + 1
        then Reject
        else if not (Map.member parentRoot (stBlocks store))
          then Ignore
          else case onBlock store sb of
            Left _  -> Reject
            Right _ -> Accept

-- | Validate a gossiped individual attestation.
validateAttestation :: Store -> SignedAttestation -> SubnetId -> Slot -> ValidationResult
validateAttestation store sa _expectedSubnet currentSlot =
  let ad = saData sa
      attSlot = adSlot ad
  in  if attSlot > currentSlot
        then Reject
        else if attSlot + 4 < currentSlot
          then Ignore
          else case onAttestation store sa of
            Left _  -> Reject
            Right _ -> Accept

-- | Validate a gossiped aggregated attestation (requires IO for multisig verify).
validateAggregation :: VerifierContext -> Store -> SignedAggregatedAttestation -> Slot
                    -> IO ValidationResult
validateAggregation verifier store saa currentSlot = do
  let ad = saaData saa
      attSlot = adSlot ad
  if attSlot > currentSlot
    then pure Reject
    else if attSlot + 4 < currentSlot
      then pure Ignore
      else do
        let justRoot = cpRoot (stJustifiedCheckpoint store)
        case Map.lookup justRoot (stBlockStates store) of
          Nothing -> pure Ignore
          Just bs -> do
            let validators = unSszList (bsValidators bs)
                pubkeys = [ vAttestationPubkey v | v <- validators ]
                domain = cpRoot (stFinalizedCheckpoint store)
            result <- verifyAggregatedAttestation verifier saa pubkeys domain
            pure $ case result of
              Left _       -> Reject
              Right True   -> Accept
              Right False  -> Reject

-- ---------------------------------------------------------------------------
-- Handler
-- ---------------------------------------------------------------------------

-- | Environment for the message handler actor.
data MessageHandlerEnv = MessageHandlerEnv
  { mhStore      :: !(TVar Store)
  , mhSeenCache  :: !(TVar SeenCache)
  , mhP2PHandle  :: !P2PHandle
  , mhVerifier   :: !VerifierContext
  , mhCurrentSlot :: !(TVar Slot)
  }

-- | Subscribe to all gossip topics and dispatch validated messages.
startMessageHandler :: MessageHandlerEnv -> IO ()
startMessageHandler env = do
  -- Subscribe to block topic
  p2hSubscribe (mhP2PHandle env) TopicBeaconBlock $ \raw ->
    handleBlockMessage env raw

  -- Subscribe to aggregation topic
  p2hSubscribe (mhP2PHandle env) TopicAggregation $ \raw ->
    handleAggregationMessage env raw

  -- Subscribe to attestation subnets 0..3
  mapM_ (\sid ->
    p2hSubscribe (mhP2PHandle env) (TopicAttestation sid) $ \raw ->
      handleAttestationMessage env sid raw
    ) [0..3]

handleBlockMessage :: MessageHandlerEnv -> ByteString -> IO ()
handleBlockMessage env raw = do
  isDup <- atomically $ do
    cache <- readTVar (mhSeenCache env)
    let (seen, cache') = markSeen raw cache
    writeTVar (mhSeenCache env) cache'
    pure seen
  if isDup
    then pure ()
    else case decodeWire raw of
      Left _ -> pure ()
      Right sb -> do
        store <- readTVarIO (mhStore env)
        slot <- readTVarIO (mhCurrentSlot env)
        case validateBlock store sb slot of
          Accept -> atomically $ do
            s <- readTVar (mhStore env)
            case onBlock s sb of
              Right s' -> writeTVar (mhStore env) s'
              Left _   -> pure ()
          _ -> pure ()

handleAttestationMessage :: MessageHandlerEnv -> SubnetId -> ByteString -> IO ()
handleAttestationMessage env subnetId raw = do
  isDup <- atomically $ do
    cache <- readTVar (mhSeenCache env)
    let (seen, cache') = markSeen raw cache
    writeTVar (mhSeenCache env) cache'
    pure seen
  if isDup
    then pure ()
    else case decodeWire raw of
      Left _ -> pure ()
      Right sa -> do
        store <- readTVarIO (mhStore env)
        slot <- readTVarIO (mhCurrentSlot env)
        case validateAttestation store sa subnetId slot of
          Accept -> atomically $ do
            s <- readTVar (mhStore env)
            case onAttestation s sa of
              Right s' -> writeTVar (mhStore env) s'
              Left _   -> pure ()
          _ -> pure ()

handleAggregationMessage :: MessageHandlerEnv -> ByteString -> IO ()
handleAggregationMessage env raw = do
  isDup <- atomically $ do
    cache <- readTVar (mhSeenCache env)
    let (seen, cache') = markSeen raw cache
    writeTVar (mhSeenCache env) cache'
    pure seen
  if isDup
    then pure ()
    else case decodeWire raw of
      Left _ -> pure ()
      Right saa -> do
        store <- readTVarIO (mhStore env)
        slot <- readTVarIO (mhCurrentSlot env)
        result <- validateAggregation (mhVerifier env) store saa slot
        case result of
          Accept -> pure ()
          _      -> pure ()
