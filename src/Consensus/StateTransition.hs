-- | State transition logic for 3SF-mini consensus, aligned with leanSpec.
module Consensus.StateTransition
  ( -- * Errors
    StateTransitionError (..)
    -- * Validator helpers
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
import SSZ.Common (mkBytesN, unBytesN, zeroN)
import SSZ.List (mkSszList, unSszList)
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

-- | Proposer selection: slot % numValidators.
-- In leanSpec, all validators are always active.
getProposerIndex :: BeaconState -> ValidatorIndex
getProposerIndex bs =
  let validators = unSszList (bsValidators bs)
      numVals = fromIntegral (length validators) :: Word64
  in  if numVals == 0
        then 0
        else bsSlot bs `mod` numVals

-- ---------------------------------------------------------------------------
-- Per-slot processing
-- ---------------------------------------------------------------------------

-- | Process a single slot: append historical block hash, extend justified slots, increment.
processSlot :: BeaconState -> BeaconState
processSlot bs =
  let slot = bsSlot bs
      newSlot = slot + 1

      -- Append block root to historical block hashes
      blockRoot = toRoot (bsLatestBlockHeader bs)
      histHashes = unSszList (bsHistoricalBlockHashes bs)
      newHistHashes = forceRight $ mkSszList @HISTORICAL_ROOTS_LIMIT (histHashes ++ [blockRoot])

  in  bs { bsSlot = newSlot
         , bsHistoricalBlockHashes = newHistHashes
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

processAttestation
  :: BeaconState
  -> AggregatedAttestation
  -> Either StateTransitionError BeaconState
processAttestation bs aa = do
  let ad = aaData aa
      attSlot = adSlot ad

  if attSlot >= bsSlot bs
    then Left (InvalidAttestationSlot attSlot (bsSlot bs))
    else Right ()

  -- Source must match justified or finalized
  let sourceOk = adSource ad == bsLatestJustified bs
              || adSource ad == bsLatestFinalized bs
  if not sourceOk
    then Left InvalidSourceCheckpoint
    else Right ()

  -- In leanSpec, attestations are stored differently via justifications_roots/validators.
  -- For now, accept attestations that pass validation.
  Right bs

processAttestations
  :: BeaconState
  -> [AggregatedAttestation]
  -> Either StateTransitionError BeaconState
processAttestations = foldl' step . Right
  where
    step (Left err) _ = Left err
    step (Right bs) aa = processAttestation bs aa

-- | Justification/finalization: count unique votes per target, check 2/3 supermajority.
-- Simplified for leanSpec: all validators have equal weight.
processJustificationFinalization :: BeaconState -> BeaconState
processJustificationFinalization bs =
  -- In leanSpec, justification uses justifications_roots and justifications_validators.
  -- This is a simplified placeholder until full justification tracking is implemented.
  bs

-- ---------------------------------------------------------------------------
-- Full state transition
-- ---------------------------------------------------------------------------

stateTransition
  :: BeaconState
  -> SignedBlock
  -> Bool
  -> Either StateTransitionError BeaconState
stateTransition bs signedBlock validateSigs = do
  let block = sbMessage signedBlock
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
          let domain = toRoot bs2
              signingRoot = computeSigningRoot block domain
              message = unBytesN signingRoot
              proposerSig = bsigProposerSignature (sbSignature signedBlock)
          case LeanSig.verify (vProposalPubkey proposer) message proposerSig of
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

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0 || i >= length xs = Nothing
  | otherwise                = Just (xs !! i)
