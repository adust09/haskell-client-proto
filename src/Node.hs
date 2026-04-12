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
import Consensus.Types (XmssPubkey)
import Consensus.Constants (Root, Slot, ValidatorIndex)
import Consensus.ForkChoice (onBlock, onAttestation)
import Consensus.StateTransition (processSlots)
import Crypto.KeyManager (loadManagedKey, managedPublicKey)
import Crypto.SigningRoot (computeDomain)
import Genesis (GenesisConfig (..), GenesisValidator (..))
import NodeTypes
import SSZ.Common (mkBytesN, zeroN)
import SSZ.Merkleization (SszHashTreeRoot (..))
import Storage
  ( StorageHandle
  , readCurrentState
  , writeCurrentState
  , readForkChoiceStore
  , writeForkChoiceStore
  , putBlock
  )
import Validator (ValidatorEnv (..), validatorLoop)

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
startNode config storageHandle genesis = do
  bcActor <- spawnActor "blockchain" (blockchainLoop storageHandle)
  p2pActor <- spawnActor "p2p" p2pLoop
  valActor <- case ncValidatorKeyDir config of
    Nothing  -> pure Nothing
    Just keyDir -> do
      let keyPath = keyDir <> "/validator.key"
      mkResult <- loadManagedKey keyPath
      case mkResult of
        Left _err -> pure Nothing
        Right managedKey -> do
          pubKey <- managedPublicKey managedKey
          let valIdx = findValidatorIndex genesis pubKey
              domain = computeDomain (zeroN @4) (gcForkVersion genesis) (zeroN @32)
              env = ValidatorEnv
                { veStorage        = storageHandle
                , veManagedKey     = managedKey
                , veValidatorIndex = valIdx
                , veDomain         = domain
                , veKeyPersistPath = keyPath
                , veBcActor        = bcActor
                , veP2PActor       = p2pActor
                }
          Just <$> spawnActor "validator" (validatorLoop env)
  pure $ NodeActors bcActor p2pActor valActor

-- | Stop all node actors gracefully by sending shutdown messages.
stopNode :: NodeActors -> IO ()
stopNode actors = do
  maybe (pure ()) (\v -> atomically $ send v ValShutdown) (naValidator actors)
  atomically $ send (naP2P actors) P2PShutdown
  atomically $ send (naBlockchain actors) BcShutdown
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

        BcNewAttestation _att -> do
          -- In leanSpec, individual attestations are not processed by the store.
          -- Attestation data is extracted from blocks during onBlock.
          go

        BcNewAggregation _agg -> do
          -- In leanSpec, aggregations are handled via BlockSignatures
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

-- | Compute the SSZ hash tree root as a Bytes32 root.
toRoot :: SszHashTreeRoot a => a -> Root
toRoot a = case mkBytesN @32 (hashTreeRoot a) of
  Right r -> r
  Left _  -> error "toRoot: hashTreeRoot did not produce 32 bytes"

-- | Find a validator's index in the genesis config by matching public keys.
findValidatorIndex :: GenesisConfig -> XmssPubkey -> ValidatorIndex
findValidatorIndex gc pubKey =
  case [ fromIntegral i :: ValidatorIndex
       | (i, gv) <- zip [(0 :: Int)..] (gcValidators gc)
       , gvAttestationPubkey gv == pubKey || gvProposalPubkey gv == pubKey
       ] of
    (idx : _) -> idx
    []        -> 0
