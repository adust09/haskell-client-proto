module Test.Crypto.LeanMultisig (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString as BS
import Consensus.Types (AggregatedSignatureProof (..), LeanMultisigProof (..))
import Crypto.Error (CryptoError (..))
import Crypto.LeanSig (generateKeyPair, sign)
import Crypto.LeanMultisig

-- | Unwrap a Right or fail.
unsafeRight :: (Show e) => Either e a -> a
unsafeRight (Right x) = x
unsafeRight (Left e)  = error ("unexpected Left: " <> show e)

tests :: TestTree
tests = testGroup "Crypto.LeanMultisig"
  [ testCase "single signer: sign, aggregate, verify" $ do
      prover <- setupProver
      verifier <- setupVerifier
      let (pk, pub) = unsafeRight $ generateKeyPair 10 "seed-1"
          sig = unsafeRight $ sign pk "hello world" 0
      proof <- unsafeRight <$> aggregate prover [(pub, sig)] "hello world"
      valid <- unsafeRight <$> verifyAggregation verifier proof [pub] "hello world"
      valid @?= True
      teardownProver prover
      teardownVerifier verifier

  , testCase "multiple signers (3): sign, aggregate, verify" $ do
      prover <- setupProver
      verifier <- setupVerifier
      let seeds = ["seed-1", "seed-2", "seed-3"] :: [BS.ByteString]
          pairs = map (\s -> unsafeRight $ generateKeyPair 10 s) seeds
          pubs = map snd pairs
          sigs = map (\(pk, _) -> unsafeRight $ sign pk "shared message" 0) pairs
          signers = zip pubs sigs
      proof <- unsafeRight <$> aggregate prover signers "shared message"
      valid <- unsafeRight <$> verifyAggregation verifier proof pubs "shared message"
      valid @?= True
      teardownProver prover
      teardownVerifier verifier

  , testCase "tampered proof fails verification" $ do
      prover <- setupProver
      verifier <- setupVerifier
      let (pk, pub) = unsafeRight $ generateKeyPair 10 "seed-1"
          sig = unsafeRight $ sign pk "hello" 0
      asp <- unsafeRight <$> aggregate prover [(pub, sig)] "hello"
      -- Flip a byte in the proof data
      let proofBytes = unLeanMultisigProof (aspProof asp)
          tamperedBytes = BS.take 5 proofBytes <> BS.singleton (BS.index proofBytes 5 + 1) <> BS.drop 6 proofBytes
          tampered = asp { aspProof = LeanMultisigProof tamperedBytes }
      valid <- unsafeRight <$> verifyAggregation verifier tampered [pub] "hello"
      valid @?= False
      teardownProver prover
      teardownVerifier verifier

  , testCase "wrong pubkey set fails verification" $ do
      prover <- setupProver
      verifier <- setupVerifier
      let (pk1, pub1) = unsafeRight $ generateKeyPair 10 "seed-1"
          (_pk2, pub2) = unsafeRight $ generateKeyPair 10 "seed-2"
          sig = unsafeRight $ sign pk1 "hello" 0
      proof <- unsafeRight <$> aggregate prover [(pub1, sig)] "hello"
      valid <- unsafeRight <$> verifyAggregation verifier proof [pub2] "hello"
      valid @?= False
      teardownProver prover
      teardownVerifier verifier

  , testCase "wrong message fails verification" $ do
      prover <- setupProver
      verifier <- setupVerifier
      let (pk, pub) = unsafeRight $ generateKeyPair 10 "seed-1"
          sig = unsafeRight $ sign pk "hello" 0
      proof <- unsafeRight <$> aggregate prover [(pub, sig)] "hello"
      valid <- unsafeRight <$> verifyAggregation verifier proof [pub] "wrong"
      valid @?= False
      teardownProver prover
      teardownVerifier verifier

  , testCase "empty input returns error" $ do
      prover <- setupProver
      result <- aggregate prover [] "hello"
      result @?= Left (AggregationFailed "empty input")
      teardownProver prover
  ]
