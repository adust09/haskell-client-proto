-- | Validator actor: proposes blocks and creates attestations based on slot duties.
module Validator
  ( ValidatorEnv (..)
  , validatorLoop
  ) where

import Control.Concurrent.STM

import Actor (Actor, send)
import Consensus.Constants (Domain, Root, Slot, ValidatorIndex, MAX_ATTESTATIONS)
import Consensus.ForkChoice (getHead)
import Consensus.StateTransition (getProposerIndex, processSlots)
import Consensus.Types
    ( AttestationData (..)
    , BeaconBlock (..)
    , BeaconBlockBody (..)
    , BeaconState (..)
    , Checkpoint (..)
    , SignedAggregatedAttestation (..)
    , Store
    )
import Crypto.KeyManager (ManagedKey)
import Crypto.Operations (signBlock, signAttestation)
import NodeTypes (BlockchainMsg (..), P2PMsg (..), ValidatorMsg (..))
import SSZ.Common (mkBytesN, zeroN)
import SSZ.List (mkSszList, unSszList)
import SSZ.Merkleization (SszHashTreeRoot (..))
import Storage (StorageHandle, readCurrentState, readForkChoiceStore)

-- | Environment for the validator actor.
data ValidatorEnv = ValidatorEnv
  { veStorage        :: !StorageHandle
  , veManagedKey     :: !ManagedKey
  , veValidatorIndex :: !ValidatorIndex
  , veDomain         :: !Domain
  , veKeyPersistPath :: !FilePath
  , veBcActor        :: !(Actor BlockchainMsg)
  , veP2PActor       :: !(Actor P2PMsg)
  }

-- | Validator actor loop: responds to slot ticks with proposal and attestation duties.
validatorLoop :: ValidatorEnv -> TQueue ValidatorMsg -> IO ()
validatorLoop env queue = go
  where
    go = do
      msg <- atomically $ readTQueue queue
      case msg of
        ValShutdown -> pure ()
        ValSlotTick slot -> do
          handleSlotDuties env slot
          go

-- | Handle all validator duties for a given slot.
handleSlotDuties :: ValidatorEnv -> Slot -> IO ()
handleSlotDuties env slot = do
  state <- atomically $ readCurrentState (veStorage env)
  store <- atomically $ readForkChoiceStore (veStorage env)

  -- Advance state to current slot for accurate duty checks
  case processSlots state slot of
    Left _err -> pure ()
    Right currentState -> do
      -- Check proposal duty
      let expectedProposer = getProposerIndex currentState
      if expectedProposer == veValidatorIndex env
        then proposeBlock env currentState store slot
        else pure ()

      -- All validators attest every slot
      createAttestation env currentState store slot

-- | Propose a block for the current slot.
proposeBlock :: ValidatorEnv -> BeaconState -> Store -> Slot -> IO ()
proposeBlock env state _store slot = do
  let parentRoot = toRoot (bsLatestBlockHeader state)
      pendingAtts = unSszList (bsCurrentAttestations state)

  -- Filter attestations: only include those from recent slots
  let recentAtts = take 128 $ filter
        (\saa -> adSlot (saaData saa) + 3 >= slot && adSlot (saaData saa) < slot)
        pendingAtts

  body <- case mkSszList @MAX_ATTESTATIONS recentAtts of
    Left _  -> pure $ BeaconBlockBody { bbbAttestations = forceRight $ mkSszList @MAX_ATTESTATIONS [] }
    Right attList -> pure $ BeaconBlockBody { bbbAttestations = attList }

  let block = BeaconBlock
        { bbSlot          = slot
        , bbProposerIndex = veValidatorIndex env
        , bbParentRoot    = parentRoot
        , bbStateRoot     = zeroN @32
        , bbBody          = body
        }

  result <- signBlock (veManagedKey env) (veKeyPersistPath env) block (veDomain env)
  case result of
    Left _err -> pure ()
    Right signedBlock -> atomically $ do
      send (veBcActor env) (BcNewBlock signedBlock)
      send (veP2PActor env) (P2PPublishBlock signedBlock)

-- | Create and sign an attestation for the current slot.
createAttestation :: ValidatorEnv -> BeaconState -> Store -> Slot -> IO ()
createAttestation env state store slot = do
  let headRoot = getHead store
      source = bsJustifiedCheckpoint state
      target = Checkpoint slot headRoot

      attData = AttestationData
        { adSlot             = slot
        , adHeadRoot         = headRoot
        , adSourceCheckpoint = source
        , adTargetCheckpoint = target
        }

  result <- signAttestation
    (veManagedKey env) (veKeyPersistPath env) attData
    (veValidatorIndex env) (veDomain env)

  case result of
    Left _err -> pure ()
    Right signedAtt -> atomically $ do
      send (veBcActor env) (BcNewAttestation signedAtt)
      send (veP2PActor env) (P2PPublishAttestation signedAtt)

-- | Compute the SSZ hash tree root as a Bytes32 root.
toRoot :: SszHashTreeRoot a => a -> Root
toRoot a = case mkBytesN @32 (hashTreeRoot a) of
  Right r -> r
  Left _  -> error "toRoot: hashTreeRoot did not produce 32 bytes"

forceRight :: Either e a -> a
forceRight (Right a) = a
forceRight (Left _)  = error "forceRight: unexpected Left"
