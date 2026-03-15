-- | LeanMultisig aggregation stub.
-- Uses signature-committed proofs (non-forgeable) as a stand-in for zkVM proofs.
-- Will be swapped for real FFI when C library becomes available.
module Crypto.LeanMultisig
  ( ProverContext (..)
  , VerifierContext (..)
  , setupProver
  , setupVerifier
  , teardownProver
  , teardownVerifier
  , aggregate
  , verifyAggregation
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)

import Consensus.Types (XmssPubkey (..), XmssSignature (..), LeanMultisigProof (..))
import Crypto.Error (CryptoError (..))
import Crypto.Hashing (sha256)
import qualified Crypto.LeanSig as LeanSig

-- | Prover context (stub: no resources needed).
data ProverContext = ProverContext
  deriving (Show)

-- | Verifier context (stub: no resources needed).
data VerifierContext = VerifierContext
  deriving (Show)

setupProver :: IO ProverContext
setupProver = pure ProverContext

setupVerifier :: IO VerifierContext
setupVerifier = pure VerifierContext

teardownProver :: ProverContext -> IO ()
teardownProver _ = pure ()

teardownVerifier :: VerifierContext -> IO ()
teardownVerifier _ = pure ()

-- | Aggregate multiple signatures into a single proof.
-- Each signature is first verified individually (like a real prover).
-- Returns an error if inputs are empty or any signature is invalid.
aggregate :: ProverContext -> [(XmssPubkey, XmssSignature)] -> ByteString
          -> IO (Either CryptoError LeanMultisigProof)
aggregate _ [] _ = pure (Left (AggregationFailed "empty input"))
aggregate _ signers message = do
  -- Verify each signature individually
  case mapM (verifySigner message) signers of
    Left e -> pure (Left e)
    Right digests -> do
      let count = fromIntegral (length signers) :: Word32
          pubkeyBytes = map (\(XmssPubkey p, _) -> p) signers
          aggregateHash = computeAggregateHash pubkeyBytes digests message count
          proof = aggregateHash <> encodeLE32 count <> BS.concat digests
      pure (Right (LeanMultisigProof proof))

-- | Verify an aggregation proof against a set of public keys and message.
verifyAggregation :: VerifierContext -> LeanMultisigProof -> [XmssPubkey] -> ByteString
                  -> IO (Either CryptoError Bool)
verifyAggregation _ (LeanMultisigProof proof) pubkeys message = pure $ do
  -- Parse the proof
  if BS.length proof < 36
    then Right False
    else do
      let storedHash = BS.take 32 proof
          count = decodeLE32 (BS.take 4 (BS.drop 32 proof))
          digestsBytes = BS.drop 36 proof
          expectedDigestsLen = fromIntegral count * 32
      if BS.length digestsBytes /= expectedDigestsLen
        then Right False
        else if fromIntegral count /= length pubkeys
          then Right False
          else do
            let digests = chunksOf 32 digestsBytes
                pubkeyBs = map (\(XmssPubkey p) -> p) pubkeys
                expectedHash = computeAggregateHash pubkeyBs digests message count
            Right (storedHash == expectedHash)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Verify a single signer and produce a signature digest.
verifySigner :: ByteString -> (XmssPubkey, XmssSignature) -> Either CryptoError ByteString
verifySigner message (pubkey, sig) =
  case LeanSig.verify pubkey message sig of
    Left e -> Left e
    Right False -> Left InvalidSignature
    Right True ->
      -- sigDigest = SHA-256(pubkey ++ sig[0..63])
      let sigDigest = sha256 (unXmssPubkey pubkey <> BS.take 64 (unXmssSignature sig))
      in  Right sigDigest

-- | Compute the aggregate hash from pubkeys, digests, message, and count.
computeAggregateHash :: [ByteString] -> [ByteString] -> ByteString -> Word32 -> ByteString
computeAggregateHash pubkeyBytes digests message count =
  sha256 ("AGG" <> BS.concat pubkeyBytes <> BS.concat digests <> message <> encodeLE32 count)

encodeLE32 :: Word32 -> ByteString
encodeLE32 w = BS.pack
  [ fromIntegral w
  , fromIntegral (w `div` 256)
  , fromIntegral (w `div` 65536)
  , fromIntegral (w `div` 16777216)
  ]

decodeLE32 :: ByteString -> Word32
decodeLE32 bs =
  fromIntegral (BS.index bs 0)
  + fromIntegral (BS.index bs 1) * 256
  + fromIntegral (BS.index bs 2) * 65536
  + fromIntegral (BS.index bs 3) * 16777216

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf _ bs | BS.null bs = []
chunksOf n bs = BS.take n bs : chunksOf n (BS.drop n bs)
