-- | State transition logic for 3SF-mini consensus.
module Consensus.StateTransition
  ( -- * Errors
    StateTransitionError (..)
    -- * Validator helpers
  , isActiveValidator
  , getProposerIndex
  , getAttestationSubnet
    -- * Per-slot processing
  , processSlot
  , processSlots
    -- * Block processing
  , processBlockHeader
  , processAttestations
  , processAttestation
  , processJustificationFinalization
  , expandAggregationBits
    -- * Slashing
  , checkSlashingConditions
  , slashValidator
    -- * Full state transition
  , stateTransition
  ) where

import Data.List (foldl', sort)
import Data.Word (Word64)

import Consensus.Constants
import Consensus.Types
import SSZ.Bitlist (Bitlist, bitlistLen, getBitlistBit)
import SSZ.Common (mkBytesN, unBytesN, zeroN)
import SSZ.List (SszList, mkSszList, unSszList)
import SSZ.Merkleization (SszHashTreeRoot (..))
import qualified Crypto.LeanSig as LeanSig
import Crypto.SigningRoot (computeSigningRoot)

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

data StateTransitionError
  = SlotTooOld Slot Slot
  | InvalidSlot Slot Slot
  | InvalidProposer ValidatorIndex ValidatorIndex
  | InvalidParentRoot
  | DuplicateBlock
  | InvalidSourceCheckpoint
  | InvalidAttestationSlot Slot Slot
  | AttestationListError String
  | BlockProcessingError String
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- Validator helpers
-- ---------------------------------------------------------------------------

isActiveValidator :: Validator -> Slot -> Bool
isActiveValidator v slot =
  vActivationSlot v <= slot && slot < vExitSlot v && not (vSlashed v)

-- | Proposer selection: slot % numActiveValidators.
-- Assumption: verify against leanSpec's actual selection.
getProposerIndex :: BeaconState -> ValidatorIndex
getProposerIndex bs =
  let validators = unSszList (bsValidators bs)
      activeIndices =
        [ fromIntegral i :: ValidatorIndex
        | (i, v) <- zip [(0 :: Int)..] validators
        , isActiveValidator v (bsSlot bs)
        ]
      numActive = fromIntegral (length activeIndices) :: Word64
  in  if numActive == 0
        then 0
        else activeIndices !! fromIntegral (bsSlot bs `mod` numActive)

-- | Deterministic subnet assignment: validatorIndex % totalSubnets.
getAttestationSubnet :: ValidatorIndex -> SubnetId
getAttestationSubnet vi = vi `mod` totalSubnets

-- ---------------------------------------------------------------------------
-- Per-slot processing
-- ---------------------------------------------------------------------------

-- | Process a single slot: cache block hash in history, increment slot.
processSlot :: BeaconState -> BeaconState
processSlot bs =
  let slot = bsSlot bs
      -- Append the latest block header hash to historical_block_hashes
      blockHash = toRoot (bsLatestBlockHeader bs)
      currentHashes = unSszList (bsHistoricalBlockHashes bs)
      newHashes = forceRight $
        mkSszList @HISTORICAL_BLOCK_HASHES_LIMIT (currentHashes ++ [blockHash])
  in  bs { bsSlot = slot + 1
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

-- | Expand aggregation bits to validator indices for a given subnet.
expandAggregationBits
  :: SszList VALIDATORS_LIMIT Validator
  -> SubnetId
  -> Bitlist n
  -> [ValidatorIndex]
expandAggregationBits validators subnetId bits =
  let allVals = unSszList validators
      numBits = bitlistLen bits
      -- Collect validator indices assigned to this subnet, sorted
      subnetVals = sort
        [ fromIntegral i :: ValidatorIndex
        | (i, _) <- zip [(0 :: Int)..] allVals
        , getAttestationSubnet (fromIntegral i) == subnetId
        ]
      -- Use local position within subnet, not global validator index
  in  [ vi
      | (localIdx, vi) <- zip [0..] subnetVals
      , localIdx < numBits
      , getBitlistBit bits localIdx
      ]

processAttestation
  :: BeaconState
  -> SignedAggregatedAttestation
  -> Either StateTransitionError BeaconState
processAttestation bs saa = do
  let ad = saaData saa
      attSlot = adSlot ad

  if attSlot >= bsSlot bs
    then Left (InvalidAttestationSlot attSlot (bsSlot bs))
    else Right ()

  -- Source must match justified or finalized (devnet fallback)
  let sourceOk = adSourceCheckpoint ad == bsLatestJustified bs
              || adSourceCheckpoint ad == bsLatestFinalized bs
  if not sourceOk
    then Left InvalidSourceCheckpoint
    else Right ()

  -- Attestation accepted (state-level attestation tracking is done via
  -- justifiedSlots / justificationsRoots / justificationsValidators in
  -- the new leanSpec model; detailed logic TBD in consensus rewrite)
  Right bs

processAttestations
  :: BeaconState
  -> [SignedAggregatedAttestation]
  -> Either StateTransitionError BeaconState
processAttestations = foldl' step . Right
  where
    step (Left err) _ = Left err
    step (Right bs) saa = processAttestation bs saa

-- | Justification/finalization placeholder.
-- The full logic will be rewritten when the leanSpec consensus model is
-- implemented (justification tracking via justifiedSlots, justificationsRoots,
-- and justificationsValidators). For now, checkpoints are left unchanged.
processJustificationFinalization :: BeaconState -> BeaconState
processJustificationFinalization = id

-- ---------------------------------------------------------------------------
-- Slashing
-- ---------------------------------------------------------------------------

checkSlashingConditions :: [AttestationData] -> AttestationData -> Either SlashingEvidence ()
checkSlashingConditions history newVote =
  case findSlashable history newVote of
    Just evidence -> Left evidence
    Nothing       -> Right ()

findSlashable :: [AttestationData] -> AttestationData -> Maybe SlashingEvidence
findSlashable [] _ = Nothing
findSlashable (old : rest) newVote
  | adSlot old == adSlot newVote && old /= newVote =
      Just (DoubleVote old newVote)
  -- Surround vote: new vote's source is strictly earlier and target is strictly later
  | cpSlot (adSourceCheckpoint newVote) < cpSlot (adSourceCheckpoint old)
  , cpSlot (adTargetCheckpoint old) < cpSlot (adTargetCheckpoint newVote) =
      Just (SurroundVote newVote old)
  -- Surround vote: old vote's source is strictly earlier and target is strictly later
  | cpSlot (adSourceCheckpoint old) < cpSlot (adSourceCheckpoint newVote)
  , cpSlot (adTargetCheckpoint newVote) < cpSlot (adTargetCheckpoint old) =
      Just (SurroundVote old newVote)
  | otherwise = findSlashable rest newVote

slashValidator :: BeaconState -> ValidatorIndex -> BeaconState
slashValidator bs vi =
  let validators = unSszList (bsValidators bs)
      idx = fromIntegral vi
  in  if idx >= length validators
        then bs
        else
          let v = validators !! idx
              slashedV = v { vSlashed = True, vEffectiveBalance = 0 }
              newVals = take idx validators ++ [slashedV] ++ drop (idx + 1) validators
          in  case mkSszList @VALIDATORS_LIMIT newVals of
                Right sl -> bs { bsValidators = sl }
                Left _   -> bs

-- ---------------------------------------------------------------------------
-- Full state transition
-- ---------------------------------------------------------------------------

stateTransition
  :: BeaconState
  -> SignedBeaconBlock
  -> Bool
  -> Either StateTransitionError BeaconState
stateTransition bs signedBlock validateSigs = do
  let block = sbbBlock signedBlock
  bs1 <- processSlots bs (bbSlot block)
  bs2 <- processBlockHeader bs1 block

  -- Verify block proposer signature if requested
  if validateSigs
    then do
      let proposerIdx = fromIntegral (bbProposerIndex block)
          validators = unSszList (bsValidators bs2)
      case safeIndex validators proposerIdx of
        Nothing -> Left (BlockProcessingError "proposer index out of range")
        Just proposer -> do
          let domain = toRoot bs2  -- use state root as domain placeholder
              signingRoot = computeSigningRoot block domain
              message = unBytesN signingRoot
          case LeanSig.verify (vPubkey proposer) message (sbbSignature signedBlock) of
            Left _      -> Left (BlockProcessingError "block signature verification error")
            Right False -> Left (BlockProcessingError "invalid block signature")
            Right True  -> Right ()
    else Right ()

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

validatorBalance :: [Validator] -> ValidatorIndex -> Gwei
validatorBalance validators vi =
  case safeIndex validators (fromIntegral vi) of
    Just v  -> vEffectiveBalance v
    Nothing -> 0

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0 || i >= length xs = Nothing
  | otherwise                = Just (xs !! i)
