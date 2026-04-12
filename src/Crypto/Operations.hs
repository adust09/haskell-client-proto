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

import Consensus.Constants (Domain, ValidatorIndex)
import Consensus.Types
  ( AttestationData
  , BeaconBlock
  , SignedAttestation (..)
  , SignedAggregatedAttestation (..)
  , AggregatedSignatureProof (..)
  , AggregatedAttestation (..)
  , SignedBlock (..)
  , BlockSignatures (..)
  , XmssPubkey (..)
  )
import Crypto.Error (CryptoError (..))
import Crypto.KeyManager (ManagedKey, managedSign)
import Crypto.LeanMultisig (ProverContext, VerifierContext, aggregate, verifyAggregation)
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

-- | Sign a beacon block with a managed key, producing a SignedBlock.
signBlock :: ManagedKey -> FilePath -> BeaconBlock -> Domain
         -> IO (Either CryptoError SignedBlock)
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
      in  Right (SignedBlock block blockSigs)

-- | Verify a signed block's proposer signature.
verifyBlock :: SignedBlock -> XmssPubkey -> Domain -> Either CryptoError Bool
verifyBlock sb pubkey domain =
  let signingRoot = computeSigningRoot (sbMessage sb) domain
      message = unBytesN signingRoot
  in  LeanSig.verify pubkey message (bsigProposerSignature (sbSignature sb))

-- | Aggregate multiple signed attestations into a single aggregated attestation.
-- All attestations must share the same AttestationData.
aggregateAttestations :: ProverContext -> [SignedAttestation] -> [XmssPubkey] -> Domain
                      -> IO (Either CryptoError (AggregatedAttestation, AggregatedSignatureProof))
aggregateAttestations _ [] _ _ = pure (Left (AggregationFailed "empty attestation list"))
aggregateAttestations prover attestations committee domain = do
  let attDatas = map saData attestations
      firstData = head attDatas
  if not (all (== firstData) attDatas)
    then pure (Left (AggregationFailed "mixed AttestationData in aggregation"))
    else do
      let valIndices = map saValidatorIndex attestations
          committeeSize = length committee
          bits = [fromIntegral i `elem` valIndices | i <- [0 .. committeeSize - 1]]
      case mkBitlist bits of
        Left _sszErr -> pure (Left (AggregationFailed "bitlist construction failed"))
        Right bitlist -> do
          let signers = [ (committee !! fromIntegral (saValidatorIndex att), saSignature att)
                        | att <- attestations ]
              signingRoot = computeSigningRoot firstData domain
              message = unBytesN signingRoot
          result <- aggregate prover signers message
          pure $ case result of
            Left e -> Left e
            Right proof ->
              let aggProof = proof { aspParticipants = bitlist }
                  aggAtt = AggregatedAttestation bitlist firstData
              in  Right (aggAtt, aggProof)

-- | Verify an aggregated attestation.
verifyAggregatedAttestation :: VerifierContext -> SignedAggregatedAttestation
                            -> [XmssPubkey] -> Domain -> IO (Either CryptoError Bool)
verifyAggregatedAttestation verifier saa pubkeys domain = do
  let signingRoot = computeSigningRoot (saaData saa) domain
      message = unBytesN signingRoot
      proof = saaProof saa
  verifyAggregation verifier proof pubkeys message
