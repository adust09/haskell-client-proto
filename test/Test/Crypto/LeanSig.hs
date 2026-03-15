module Test.Crypto.LeanSig (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString as BS
import Consensus.Constants (xmssSignatureSize, xmssPubkeySize)
import Consensus.Types (XmssPubkey (..), XmssSignature (..))
import Crypto.Error (CryptoError (..))
import Crypto.LeanSig

-- | Unwrap a Right or fail.
unsafeRight :: (Show e) => Either e a -> a
unsafeRight (Right x) = x
unsafeRight (Left e)  = error ("unexpected Left: " <> show e)

tests :: TestTree
tests = testGroup "Crypto.LeanSig"
  [ testGroup "Key generation"
    [ testCase "valid tree height produces keypair" $ do
        let (pk, pub) = unsafeRight $ generateKeyPair 10 "test-seed"
        privateKeyTreeHeight pk @?= 10
        BS.length (unXmssPubkey pub) @?= xmssPubkeySize
    , testCase "tree height 0 is rejected" $
        case generateKeyPair 0 "test-seed" of
          Left (InvalidTreeHeight 0) -> pure ()
          other -> assertFailure ("expected InvalidTreeHeight 0, got: " <> show (fmap snd other))
    , testCase "tree height 32 is rejected" $
        case generateKeyPair 32 "test-seed" of
          Left (InvalidTreeHeight 32) -> pure ()
          other -> assertFailure ("expected InvalidTreeHeight 32, got: " <> show (fmap snd other))
    , testCase "tree height 1 is accepted" $ do
        let (pk, _) = unsafeRight $ generateKeyPair 1 "test-seed"
        privateKeyTreeHeight pk @?= 1
    , testCase "tree height 31 is accepted" $ do
        let (pk, _) = unsafeRight $ generateKeyPair 31 "test-seed"
        privateKeyTreeHeight pk @?= 31
    ]
  , testGroup "Sign and verify"
    [ testCase "sign then verify succeeds" $ do
        let (pk, pub) = unsafeRight $ generateKeyPair 10 "test-seed"
            sig = unsafeRight $ sign pk "hello world" 0
            valid = unsafeRight $ verify pub "hello world" sig
        valid @?= True
    , testCase "signature is exactly 3112 bytes" $ do
        let (pk, _) = unsafeRight $ generateKeyPair 10 "test-seed"
            (XmssSignature sigBytes) = unsafeRight $ sign pk "msg" 0
        BS.length sigBytes @?= xmssSignatureSize
    , testCase "wrong message fails verification" $ do
        let (pk, pub) = unsafeRight $ generateKeyPair 10 "test-seed"
            sig = unsafeRight $ sign pk "hello" 0
            valid = unsafeRight $ verify pub "wrong" sig
        valid @?= False
    , testCase "wrong pubkey fails verification" $ do
        let (pk1, _pub1) = unsafeRight $ generateKeyPair 10 "seed-1"
            (_pk2, pub2) = unsafeRight $ generateKeyPair 10 "seed-2"
            sig = unsafeRight $ sign pk1 "hello" 0
            valid = unsafeRight $ verify pub2 "hello" sig
        valid @?= False
    , testCase "tampered signature fails verification" $ do
        let (pk, pub) = unsafeRight $ generateKeyPair 10 "test-seed"
            (XmssSignature sigBytes) = unsafeRight $ sign pk "hello" 0
            tampered = XmssSignature (BS.take 5 sigBytes <> BS.singleton (BS.index sigBytes 5 + 1) <> BS.drop 6 sigBytes)
            valid = unsafeRight $ verify pub "hello" tampered
        valid @?= False
    , testCase "different leaf indices produce different signatures" $ do
        let (pk, _) = unsafeRight $ generateKeyPair 10 "test-seed"
            (XmssSignature sig0) = unsafeRight $ sign pk "msg" 0
            (XmssSignature sig1) = unsafeRight $ sign pk "msg" 1
        sig0 /= sig1 @? "signatures with different leaf indices should differ"
    ]
  , testGroup "Key serialization"
    [ testCase "roundtrip preserves key" $ do
        let (pk, _pub) = unsafeRight $ generateKeyPair 10 "test-seed"
            serialized = serializePrivateKey pk
            pk' = unsafeRight $ deserializePrivateKey serialized
        privateKeyTreeHeight pk' @?= 10
        -- Verify the deserialized key produces identical signatures
        let sig1 = unsafeRight $ sign pk "test" 0
            sig2 = unsafeRight $ sign pk' "test" 0
        sig1 @?= sig2
    , testCase "invalid bytes rejected" $
        case deserializePrivateKey "too short" of
          Left InvalidKeyFormat -> pure ()
          other -> assertFailure ("expected InvalidKeyFormat, got: " <> show (fmap (const ()) other))
    ]
  ]
