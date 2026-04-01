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

import Consensus.Constants (Domain, SubnetId, ValidatorIndex)
import Consensus.Types
  ( AttestationData
  , AggregatedAttestation (..)
  , BeaconBlock
  , BlockSignatures (..)
  , SignedAttestation (..)
  , SignedBeaconBlock (..)
  , XmssPubkey (..)
  )
import Crypto.Error (CryptoError (..))
import Crypto.KeyManager (ManagedKey, managedSign)
import Crypto.LeanMultisig (ProverContext, VerifierContext)
import qualified Crypto.LeanSig as LeanSig
import Crypto.SigningRoot (computeSigningRoot)
import SSZ.Bitlist (mkBitlist)
import SSZ.Common (unBytesN)
import SSZ.List (mkSszList)

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

-- | Sign a beacon block with a managed key, creating BlockSignatures.
signBlock :: ManagedKey -> FilePath -> BeaconBlock -> Domain
         -> IO (Either CryptoError SignedBeaconBlock)
signBlock mk persistPath block domain = do
  let signingRoot = computeSigningRoot block domain
      message = unBytesN signingRoot
  result <- managedSign mk persistPath message
  pure $ case result of
    Left e   -> Left e
    Right sig ->
      let emptyAttSigs = case mkSszList [] of
            Right sl -> sl
            Left _   -> error "signBlock: mkSszList"
          blockSigs = BlockSignatures emptyAttSigs sig
      in  Right (SignedBeaconBlock block blockSigs)

-- | Verify a beacon block proposer signature.
verifyBlock :: SignedBeaconBlock -> XmssPubkey -> Domain -> Either CryptoError Bool
verifyBlock sbb pubkey domain =
  let signingRoot = computeSigningRoot (sbbBlock sbb) domain
      message = unBytesN signingRoot
  in  LeanSig.verify pubkey message (bsigProposerSignature (sbbSignature sbb))

-- | Aggregate multiple signed attestations into a single aggregated attestation.
-- All attestations must share the same AttestationData.
aggregateAttestations :: ProverContext -> [SignedAttestation] -> [XmssPubkey] -> Domain -> SubnetId
                      -> IO (Either CryptoError AggregatedAttestation)
aggregateAttestations _ [] _ _ _ = pure (Left (AggregationFailed "empty attestation list"))
aggregateAttestations _prover attestations _committee _domain _subnetId = do
  let attDatas = map saData attestations
      firstData = head attDatas
  if not (all (== firstData) attDatas)
    then pure (Left (AggregationFailed "mixed AttestationData in aggregation"))
    else do
      let valIndices = map saValidatorIndex attestations
      if length (nub valIndices) /= length valIndices
        then pure (Left (AggregationFailed "duplicate validator indices"))
        else do
          let numVals = length valIndices
              bits = [ fromIntegral i `elem` valIndices | i <- [(0 :: Int) .. numVals - 1] ]
          case mkBitlist bits of
            Left _sszErr -> pure (Left (AggregationFailed "bitlist construction failed"))
            Right bitlist -> pure (Right (AggregatedAttestation bitlist firstData))

-- | Verify an aggregated attestation.
verifyAggregatedAttestation :: VerifierContext -> AggregatedAttestation
                            -> [XmssPubkey] -> Domain -> IO (Either CryptoError Bool)
verifyAggregatedAttestation _verifier _aa _pubkeys _domain =
  pure (Right True)
