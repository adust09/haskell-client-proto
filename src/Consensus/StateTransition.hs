-- | State transition logic for 3SF-mini consensus (leanSpec aligned).
--
-- Implements leanSpec State.state_transition(), State.process_block(),
-- and State.process_attestations().
module Consensus.StateTransition
  ( -- * Errors
    StateTransitionError (..)
    -- * Validator helpers
  , isActiveValidator
  , getProposerIndex
    -- * Per-slot processing
  , processSlot
  , processSlots
    -- * Block processing
  , processBlockHeader
  , processAttestations
  , processAttestation
  , processJustificationFinalization
    -- * Full state transition
  , stateTransition
  ) where

import Data.List (foldl')
import Data.Word (Word64)

import Consensus.Constants
import Consensus.Types
import SSZ.Bitlist (Bitlist, bitlistLen, getBitlistBit, mkBitlist)
import SSZ.Common (mkBytesN, zeroN)
import SSZ.List (mkSszList, unSszList)
import SSZ.Merkleization (SszHashTreeRoot (..))

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

data StateTransitionError
  = SlotTooOld Slot Slot
  | InvalidSlot Slot Slot
  | InvalidProposer ValidatorIndex ValidatorIndex
  | InvalidParentRoot
  | DuplicateBlock
  | InvalidAttestationSlot Slot Slot
  | AttestationListError String
  | BlockProcessingError String
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- Validator helpers
-- ---------------------------------------------------------------------------

-- | All validators are always active in leanSpec (no lifecycle).
isActiveValidator :: Validator -> Slot -> Bool
isActiveValidator _ _ = True

-- | Proposer selection: slot % numValidators.
getProposerIndex :: BeaconState -> ValidatorIndex
getProposerIndex bs =
  let numVals = fromIntegral (length (unSszList (bsValidators bs))) :: Word64
  in  if numVals == 0
        then 0
        else bsSlot bs `mod` numVals

-- ---------------------------------------------------------------------------
-- Per-slot processing (leanSpec aligned)
-- ---------------------------------------------------------------------------

-- | Process a single slot: append block hash to historical_block_hashes,
-- increment slot. No circular buffer caching, no attestation pruning.
processSlot :: BeaconState -> BeaconState
processSlot bs =
  let blockHash = toRoot (bsLatestBlockHeader bs)
      currentHashes = unSszList (bsHistoricalBlockHashes bs)
      newHashes = forceRight $
        mkSszList @HISTORICAL_BLOCK_HASHES_LIMIT (currentHashes ++ [blockHash])
  in  bs { bsSlot = bsSlot bs + 1
         , bsHistoricalBlockHashes = newHashes
         }

-- | Advance state to the target slot.
processSlots :: BeaconState -> Slot -> Either StateTransitionError BeaconState
processSlots bs target
  | target == bsSlot bs = Right bs
  | target < bsSlot bs  = Left (SlotTooOld target (bsSlot bs))
  | otherwise            = processSlots (processSlot bs) target

-- ---------------------------------------------------------------------------
-- Block processing
-- ---------------------------------------------------------------------------

processBlockHeader :: BeaconState -> BeaconBlock -> Either StateTransitionError BeaconState
processBlockHeader bs block = do
  if bbSlot block /= bsSlot bs
    then Left (InvalidSlot (bbSlot block) (bsSlot bs))
    else Right ()

  let expectedProposer = getProposerIndex bs
  if bbProposerIndex block /= expectedProposer
    then Left (InvalidProposer (bbProposerIndex block) expectedProposer)
    else Right ()

  let parentRoot = toRoot (bsLatestBlockHeader bs)
  if bbParentRoot block /= parentRoot
    then Left InvalidParentRoot
    else Right ()

  let bodyRoot = toRoot (bbBody block)
      newHeader = BeaconBlockHeader
        { bbhSlot          = bbSlot block
        , bbhProposerIndex = bbProposerIndex block
        , bbhParentRoot    = bbParentRoot block
        , bbhStateRoot     = zeroN @32
        , bbhBodyRoot      = bodyRoot
        }
  Right bs { bsLatestBlockHeader = newHeader }

-- | Process a single attestation: record votes in justifications_roots
-- and justifications_validators for later supermajority checking.
processAttestation
  :: BeaconState
  -> AggregatedAttestation
  -> Either StateTransitionError BeaconState
processAttestation bs att = do
  let ad = aaData att
      attSlot = adSlot ad

  if attSlot >= bsSlot bs
    then Left (InvalidAttestationSlot attSlot (bsSlot bs))
    else Right ()

  let targetRoot = cpRoot (adTarget ad)
      numValidators = length (unSszList (bsValidators bs))
      bits = aaAggregationBits att
      numBits = bitlistLen bits

      voterIndices =
        [ fromIntegral i :: ValidatorIndex
        | i <- [0 .. numBits - 1]
        , getBitlistBit bits i
        ]

  -- Record each voter's attestation in justifications
  let bs' = foldl' (recordJustification targetRoot numValidators) bs voterIndices
  Right bs'

-- | Record a single validator's justification vote.
-- justifications_roots[slot] = target_root
-- justifications_validators[slot * num_validators + validator_index] = True
recordJustification :: Root -> Int -> BeaconState -> ValidatorIndex -> BeaconState
recordJustification targetRoot numValidators bs vi =
  let slot = bsSlot bs
      slotIdx = fromIntegral slot :: Int
      viInt = fromIntegral vi :: Int
      bitIdx = slotIdx * numValidators + viInt

      -- Update justifications_roots: ensure slot entry exists
      currentRoots = unSszList (bsJustificationsRoots bs)
      newRoots = if slotIdx < length currentRoots
                   then currentRoots
                   else currentRoots ++ replicate (slotIdx - length currentRoots + 1) (zeroN @32)
      updatedRoots = take slotIdx newRoots ++ [targetRoot] ++ drop (slotIdx + 1) newRoots

      -- Update justifications_validators: set bit at bitIdx
      currentBits = bsJustificationsValidators bs
      currentLen = bitlistLen currentBits
      newLen = max currentLen (bitIdx + 1)
      currentBitsList = [ getBitlistBit currentBits i | i <- [0 .. currentLen - 1] ]
      paddedBits = currentBitsList ++ replicate (newLen - currentLen) False
      updatedBits = take bitIdx paddedBits ++ [True] ++ drop (bitIdx + 1) paddedBits

  in  bs { bsJustificationsRoots = forceRight $
             mkSszList @JUSTIFICATIONS_ROOTS_LIMIT updatedRoots
         , bsJustificationsValidators = forceRight $
             mkBitlist @JUSTIFICATIONS_VALIDATORS_LIMIT updatedBits
         }

-- | Process all attestations in a block.
processAttestations
  :: BeaconState
  -> [AggregatedAttestation]
  -> Either StateTransitionError BeaconState
processAttestations = foldl' step . Right
  where
    step (Left err) _ = Left err
    step (Right bs) att = processAttestation bs att

-- | Justification/finalization: check for supermajority votes per slot.
-- For each slot with justification data, count validators who voted.
-- If >= 2/3 of total active balance voted, the slot is justified.
-- When a new justified slot is found, check if the previous justified
-- can be finalized.
processJustificationFinalization :: BeaconState -> BeaconState
processJustificationFinalization bs =
  let validators = unSszList (bsValidators bs)
      numValidators = length validators
      totalActive = fromIntegral numValidators :: Word64

      justRoots = unSszList (bsJustificationsRoots bs)
      justBits = bsJustificationsValidators bs
      justBitsLen = bitlistLen justBits

      -- Check each slot for supermajority (flat voting: 1 validator = 1 vote)
      slotVotes =
        [ (slotIdx, justRoots !! slotIdx, voteCount)
        | slotIdx <- [0 .. length justRoots - 1]
        , let voteCount = countSlotVotes numValidators justBits justBitsLen slotIdx
        , voteCount * 3 >= totalActive * 2
        ]

      -- Find the latest justified slot
      (newJustified, newJustifiedSlots) = case slotVotes of
        [] -> (bsLatestJustified bs, bsJustifiedSlots bs)
        xs ->
          let (bestSlotIdx, bestRoot, _) = foldl1
                (\a@(s1, _, _) b@(s2, _, _) -> if s1 >= s2 then a else b) xs
              bestSlot = fromIntegral bestSlotIdx :: Slot
              newCp = Checkpoint bestRoot bestSlot
              -- Mark the slot as justified in justified_slots
              currentJSlots = bsJustifiedSlots bs
              currentJLen = bitlistLen currentJSlots
              newLen = max currentJLen (bestSlotIdx + 1)
              jBits = [ getBitlistBit currentJSlots i | i <- [0 .. currentJLen - 1] ]
              paddedJBits = jBits ++ replicate (newLen - currentJLen) False
              updatedJBits = take bestSlotIdx paddedJBits ++ [True] ++ drop (bestSlotIdx + 1) paddedJBits
              newJSlots = forceRight $ mkBitlist @JUSTIFIED_SLOTS_LIMIT updatedJBits
          in  if bestSlot > cpSlot (bsLatestJustified bs)
                then (newCp, newJSlots)
                else (bsLatestJustified bs, bsJustifiedSlots bs)

      -- Finalization: when we have a newer justified checkpoint,
      -- the old justified becomes finalized
      newFinalized
        | newJustified /= bsLatestJustified bs
        , cpSlot (bsLatestJustified bs) > cpSlot (bsLatestFinalized bs)
        = bsLatestJustified bs
        | otherwise
        = bsLatestFinalized bs

  in  bs { bsLatestJustified = newJustified
         , bsLatestFinalized = newFinalized
         , bsJustifiedSlots  = newJustifiedSlots
         }

-- | Count the number of votes for a slot in justifications_validators.
-- Flat voting: each validator contributes 1 vote (no balance weighting).
countSlotVotes
  :: Int -> Bitlist JUSTIFICATIONS_VALIDATORS_LIMIT -> Int -> Int -> Word64
countSlotVotes numValidators justBits justBitsLen slotIdx =
  fromIntegral $ length
      [ ()
      | vIdx <- [0 .. numValidators - 1]
      , let bitIdx = slotIdx * numValidators + vIdx
      , bitIdx < justBitsLen
      , getBitlistBit justBits bitIdx
      ]

-- ---------------------------------------------------------------------------
-- Full state transition (leanSpec: State.state_transition)
-- ---------------------------------------------------------------------------

stateTransition
  :: BeaconState
  -> SignedBeaconBlock
  -> Bool
  -> Either StateTransitionError BeaconState
stateTransition bs signedBlock _validateSigs = do
  let block = sbbBlock signedBlock
  bs1 <- processSlots bs (bbSlot block)
  bs2 <- processBlockHeader bs1 block

  let atts = unSszList (bbbAttestations (bbBody block))
  bs3 <- processAttestations bs2 atts
  let bs4 = processJustificationFinalization bs3
  Right bs4

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

forceRight :: Either e a -> a
forceRight (Right a) = a
forceRight (Left _)  = error "forceRight: unexpected Left"

-- | Convert hashTreeRoot output (ByteString) to Root (Bytes32).
toRoot :: SszHashTreeRoot a => a -> Root
toRoot a = case mkBytesN @32 (hashTreeRoot a) of
  Right r -> r
  Left _  -> error "toRoot: hashTreeRoot did not produce 32 bytes"
