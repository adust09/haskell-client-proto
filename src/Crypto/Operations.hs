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

import Data.List (nub, sort)

import Consensus.Constants (Domain, SubnetId, ValidatorIndex)
import Consensus.StateTransition (getAttestationSubnet)
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
aggregateAttestations :: ProverContext -> [SignedAttestation] -> [XmssPubkey] -> Domain -> SubnetId
                      -> IO (Either CryptoError SignedAggregatedAttestation)
aggregateAttestations _ [] _ _ _ = pure (Left (AggregationFailed "empty attestation list"))
aggregateAttestations prover attestations committee domain subnetId = do
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
          -- Build signers list and bitlist using subnet-local positions
          case buildSignersAndBits attestations committee subnetId of
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
                    Right bitlist -> Right (SignedAggregatedAttestation firstData subnetId bitlist proof)

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

-- | Build the (pubkey, signature) pairs and the bitlist bits from attestations
-- and a full committee list. Positions in the bitlist correspond to subnet-local
-- positions, matching how expandAggregationBits reads them.
buildSignersAndBits :: [SignedAttestation] -> [XmssPubkey] -> SubnetId
                    -> Either CryptoError ([(XmssPubkey, XmssSignature)], [Bool])
buildSignersAndBits attestations committee subnetId = do
  -- Build the sorted list of validator indices in this subnet
  let subnetVis = sort
        [ fromIntegral i :: ValidatorIndex
        | i <- [0 .. length committee - 1]
        , getAttestationSubnet (fromIntegral i) == subnetId
        ]
      subnetSize = length subnetVis
  -- Find subnet-local positions for each attestation
  positions <- mapM (findSubnetPosition subnetVis) attestations
  let bits = [i `elem` positions | i <- [0 .. subnetSize - 1]]
      signers = [ (committee !! fromIntegral (saValidatorIndex att), saSignature att)
                | att <- attestations ]
  Right (signers, bits)

-- | Find a validator's local position within a subnet.
findSubnetPosition :: [ValidatorIndex] -> SignedAttestation -> Either CryptoError Int
findSubnetPosition subnetVis att =
  let vi = saValidatorIndex att
  in  case lookup vi (zip subnetVis [0..]) of
        Nothing  -> Left (AggregationFailed ("unknown validator index in subnet: " <> show vi))
        Just pos -> Right pos
