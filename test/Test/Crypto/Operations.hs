module Test.Crypto.Operations (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

import Consensus.Constants (Domain)
import Consensus.Types
import Crypto.Error (CryptoError (..))
import Crypto.KeyManager (newManagedKey)
import Crypto.LeanMultisig (setupProver, setupVerifier)
import Crypto.LeanSig (generateKeyPair)
import Crypto.Operations
import SSZ.Common (zeroN, mkBytesN)
import SSZ.List (mkSszList)

-- | Unwrap a Right or fail.
unsafeRight :: (Show e) => Either e a -> a
unsafeRight (Right x) = x
unsafeRight (Left e)  = error ("unexpected Left: " <> show e)

-- | Helper: create a test domain.
testDomain :: Domain
testDomain = zeroN @32

-- | Helper: create test attestation data.
testAttData :: AttestationData
testAttData = AttestationData
  { adSlot   = 42
  , adHead   = Checkpoint (zeroN @32) 0
  , adTarget = Checkpoint (zeroN @32) 1
  , adSource = Checkpoint (zeroN @32) 0
  }

-- | Helper: create a test beacon block.
testBlock :: BeaconBlock
testBlock = BeaconBlock
  { bbSlot = 42
  , bbProposerIndex = 0
  , bbParentRoot = zeroN @32
  , bbStateRoot = zeroN @32
  , bbBody = BeaconBlockBody
    { bbbAttestations = unsafeRight $ mkSszList []
    }
  }

tests :: TestTree
tests = testGroup "Crypto.Operations"
  [ testCase "sign and verify attestation" $
      withSystemTempDirectory "ops-test" $ \tmpDir -> do
        let keyPath = tmpDir </> "key.dat"
            (pk, pub) = unsafeRight $ generateKeyPair 10 "test-seed"
        mk <- newManagedKey pk pub
        sa <- unsafeRight <$> signAttestation mk keyPath testAttData 0 testDomain
        let valid = unsafeRight $ verifyAttestation sa pub testDomain
        valid @?= True

  , testCase "attestation with wrong domain fails" $
      withSystemTempDirectory "ops-test" $ \tmpDir -> do
        let keyPath = tmpDir </> "key.dat"
            (pk, pub) = unsafeRight $ generateKeyPair 10 "test-seed"
            wrongDomain = unsafeRight $ mkBytesN @32 (BS.replicate 31 0 <> BS.singleton 1)
        mk <- newManagedKey pk pub
        sa <- unsafeRight <$> signAttestation mk keyPath testAttData 0 testDomain
        let valid = unsafeRight $ verifyAttestation sa pub wrongDomain
        valid @?= False

  , testCase "sign and verify block" $
      withSystemTempDirectory "ops-test" $ \tmpDir -> do
        let keyPath = tmpDir </> "key.dat"
            (pk, pub) = unsafeRight $ generateKeyPair 10 "test-seed"
        mk <- newManagedKey pk pub
        sbb <- unsafeRight <$> signBlock mk keyPath testBlock testDomain
        let valid = unsafeRight $ verifyBlock sbb pub testDomain
        valid @?= True

  , testCase "block with wrong domain fails" $
      withSystemTempDirectory "ops-test" $ \tmpDir -> do
        let keyPath = tmpDir </> "key.dat"
            (pk, pub) = unsafeRight $ generateKeyPair 10 "test-seed"
            wrongDomain = unsafeRight $ mkBytesN @32 (BS.replicate 31 0 <> BS.singleton 1)
        mk <- newManagedKey pk pub
        sbb <- unsafeRight <$> signBlock mk keyPath testBlock testDomain
        let valid = unsafeRight $ verifyBlock sbb pub wrongDomain
        valid @?= False

  , testCase "aggregate attestations: sign, aggregate, verify" $
      withSystemTempDirectory "ops-test" $ \tmpDir -> do
        prover <- setupProver
        verifier <- setupVerifier
        -- Create 3 validators
        let seeds = ["seed-" <> BS.pack [fromIntegral i] | i <- [0..2 :: Int]]
            keyPairs = map (\s -> unsafeRight $ generateKeyPair 10 s) seeds
            pubs = map snd keyPairs
        -- Sign attestations
        signedAtts <- mapM (\i -> do
          let (pk, _pub) = keyPairs !! i
          mk <- newManagedKey pk (snd (keyPairs !! i))
          let keyPath = tmpDir </> ("key-" <> show i <> ".dat")
          unsafeRight <$> signAttestation mk keyPath testAttData (fromIntegral i) testDomain
          ) [0..2]
        -- Aggregate
        result <- aggregateAttestations prover signedAtts pubs testDomain
        case result of
          Left err -> assertFailure ("aggregation failed: " <> show err)
          Right (aggAtt, aggProof) -> do
            -- Build SignedAggregatedAttestation and verify
            let saa = SignedAggregatedAttestation aggAtt aggProof
            valid <- unsafeRight <$> verifyAggregatedAttestation verifier saa pubs testDomain
            valid @?= True

  , testCase "duplicate validator indices in aggregation are rejected" $
      withSystemTempDirectory "ops-test" $ \tmpDir -> do
        prover <- setupProver
        let (pk, pub) = unsafeRight $ generateKeyPair 10 "test-seed"
        mk <- newManagedKey pk pub
        let keyPath = tmpDir </> "key.dat"
        sa1 <- unsafeRight <$> signAttestation mk keyPath testAttData 0 testDomain
        sa2 <- unsafeRight <$> signAttestation mk keyPath testAttData 0 testDomain  -- same index
        result <- aggregateAttestations prover [sa1, sa2] [pub] testDomain
        case result of
          Left (AggregationFailed _) -> pure ()
          other -> assertFailure ("expected AggregationFailed, got: " <> show other)

  , testCase "mixed AttestationData in aggregation -> error" $
      withSystemTempDirectory "ops-test" $ \tmpDir -> do
        prover <- setupProver
        let (pk1, pub1) = unsafeRight $ generateKeyPair 10 "seed-1"
            (pk2, pub2) = unsafeRight $ generateKeyPair 10 "seed-2"
            attData2 = testAttData { adSlot = 99 }
        mk1 <- newManagedKey pk1 pub1
        mk2 <- newManagedKey pk2 pub2
        sa1 <- unsafeRight <$> signAttestation mk1 (tmpDir </> "k1.dat") testAttData 0 testDomain
        sa2 <- unsafeRight <$> signAttestation mk2 (tmpDir </> "k2.dat") attData2 1 testDomain
        result <- aggregateAttestations prover [sa1, sa2] [pub1, pub2] testDomain
        case result of
          Left (AggregationFailed msg) ->
            assertBool "should mention mixed" ("mixed" `isInfixOf` msg)
          Left e -> assertFailure ("expected AggregationFailed, got: " <> show e)
          Right _ -> assertFailure "expected Left, got Right"

  , testCase "out-of-range validator index causes error" $
      withSystemTempDirectory "ops-test" $ \tmpDir -> do
        prover <- setupProver
        let (pk, pub) = unsafeRight $ generateKeyPair 10 "test-seed"
        mk <- newManagedKey pk pub
        sa <- unsafeRight <$> signAttestation mk (tmpDir </> "k.dat") testAttData 999 testDomain
        -- Index 999 with committee size 1 causes an index error during aggregation
        result <- try (aggregateAttestations prover [sa] [pub] testDomain)
        case (result :: Either SomeException (Either CryptoError (AggregatedAttestation, AggregatedSignatureProof))) of
          Left _  -> pure ()  -- exception is expected
          Right (Left _) -> pure ()  -- CryptoError is also acceptable
          Right (Right _) -> assertFailure "expected error for out-of-range index"
  ]
