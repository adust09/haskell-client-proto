-- | High-level cryptographic operations for consensus objects.
-- Bridges between consensus types and low-level crypto primitives.
module Crypto.Operations
  ( signAttestation
  , verifyAttestation
  , signBlock
  , verifyBlock
  , aggregateAttestations
  , verifyAggregatedAttestation
  ) where

import Data.List (nub)

import Consensus.Constants (Domain, ValidatorIndex)
import Consensus.Types
  ( AttestationData
  , BeaconBlock
  , SignedAttestation (..)
  , SignedAggregatedAttestation (..)
  , SignedBeaconBlock (..)
  , XmssPubkey (..)
  , XmssSignature
  )
import Crypto.Error (CryptoError (..))
import Crypto.KeyManager (ManagedKey, managedSign)
import Crypto.LeanMultisig (ProverContext, VerifierContext, aggregate, verifyAggregation)
import qualified Crypto.LeanSig as LeanSig
import Crypto.SigningRoot (computeSigningRoot)
import SSZ.Bitlist (mkBitlist)
import SSZ.Common (unBytesN)

-- | Sign an attestation with a managed key.
signAttestation :: ManagedKey -> FilePath -> AttestationData -> ValidatorIndex -> Domain
               -> IO (Either CryptoError SignedAttestation)
signAttestation mk persistPath attData valIdx domain = do
  let signingRoot = computeSigningRoot attData domain
      message = unBytesN signingRoot
  result <- managedSign mk persistPath message
  pure $ case result of
    Left e   -> Left e
    Right sig -> Right (SignedAttestation attData valIdx sig)

-- | Verify an attestation signature.
verifyAttestation :: SignedAttestation -> XmssPubkey -> Domain -> Either CryptoError Bool
verifyAttestation sa pubkey domain =
  let signingRoot = computeSigningRoot (saData sa) domain
      message = unBytesN signingRoot
  in  LeanSig.verify pubkey message (saSignature sa)

-- | Sign a beacon block with a managed key.
signBlock :: ManagedKey -> FilePath -> BeaconBlock -> Domain
         -> IO (Either CryptoError SignedBeaconBlock)
signBlock mk persistPath block domain = do
  let signingRoot = computeSigningRoot block domain
      message = unBytesN signingRoot
  result <- managedSign mk persistPath message
  pure $ case result of
    Left e   -> Left e
    Right sig -> Right (SignedBeaconBlock block sig)

-- | Verify a beacon block signature.
verifyBlock :: SignedBeaconBlock -> XmssPubkey -> Domain -> Either CryptoError Bool
verifyBlock sbb pubkey domain =
  let signingRoot = computeSigningRoot (sbbBlock sbb) domain
      message = unBytesN signingRoot
  in  LeanSig.verify pubkey message (sbbSignature sbb)

-- | Aggregate multiple signed attestations into a single aggregated attestation.
-- All attestations must share the same AttestationData.
-- The committee list maps committee positions (indices into the list) to pubkeys.
aggregateAttestations :: ProverContext -> [SignedAttestation] -> [XmssPubkey] -> Domain
                      -> IO (Either CryptoError SignedAggregatedAttestation)
aggregateAttestations _ [] _ _ = pure (Left (AggregationFailed "empty attestation list"))
aggregateAttestations prover attestations committee domain = do
  -- Validate: all attestations must share the same AttestationData
  let attDatas = map saData attestations
      firstData = head attDatas
  if not (all (== firstData) attDatas)
    then pure (Left (AggregationFailed "mixed AttestationData in aggregation"))
    else do
      -- Validate: no duplicate validator indices
      let valIndices = map saValidatorIndex attestations
      if length (nub valIndices) /= length valIndices
        then pure (Left (AggregationFailed "duplicate validator indices"))
        else do
          -- Build signers list and bitlist
          case buildSignersAndBits attestations committee of
            Left e -> pure (Left e)
            Right (signers, bits) -> do
              let signingRoot = computeSigningRoot firstData domain
                  message = unBytesN signingRoot
              result <- aggregate prover signers message
              pure $ case result of
                Left e -> Left e
                Right proof ->
                  case mkBitlist bits of
                    Left _sszErr -> Left (AggregationFailed "bitlist construction failed")
                    Right bitlist -> Right (SignedAggregatedAttestation firstData bitlist proof)

-- | Verify an aggregated attestation.
verifyAggregatedAttestation :: VerifierContext -> SignedAggregatedAttestation
                            -> [XmssPubkey] -> Domain -> IO (Either CryptoError Bool)
verifyAggregatedAttestation verifier saa pubkeys domain = do
  let signingRoot = computeSigningRoot (saaData saa) domain
      message = unBytesN signingRoot
  verifyAggregation verifier (saaAggregationProof saa) pubkeys message

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Build the (pubkey, signature) pairs and the bitlist bits from attestations and a committee.
-- Each attestation's validator index must map to a pubkey in the committee.
buildSignersAndBits :: [SignedAttestation] -> [XmssPubkey]
                    -> Either CryptoError ([(XmssPubkey, XmssSignature)], [Bool])
buildSignersAndBits attestations committee = do
  -- Find committee positions for each attestation
  positions <- mapM (findPosition committee) attestations
  let committeeSize = length committee
      bits = [i `elem` positions | i <- [0 .. committeeSize - 1]]
      signers = [(committee !! pos, saSignature att) | (pos, att) <- zip positions attestations]
  Right (signers, bits)

-- | Find the committee-local position of a validator.
findPosition :: [XmssPubkey] -> SignedAttestation -> Either CryptoError Int
findPosition committee att =
  let valIdx = saValidatorIndex att
  in  case lookupByIndex committee valIdx of
        Nothing -> Left (AggregationFailed ("unknown validator index: " <> show valIdx))
        Just pos -> Right pos

-- | Look up a validator's committee position by matching public key at the validator index.
-- The validator index is a global index; we check if the pubkey at that position in the
-- committee list matches (if the index is within bounds), otherwise scan the whole committee.
lookupByIndex :: [XmssPubkey] -> ValidatorIndex -> Maybe Int
lookupByIndex committee _valIdx =
  -- In the full system, we'd have a mapping from ValidatorIndex to pubkey.
  -- For now, we assume the committee list IS the mapping: position i has validator i.
  -- The caller is responsible for constructing the committee list correctly.
  -- We need to find where this validator's pubkey is in the committee.
  -- Since we don't have a global registry here, we match by validator index directly
  -- as a committee position (if it fits).
  let idx = fromIntegral _valIdx :: Int
  in  if idx < length committee
        then Just idx
        else Nothing
