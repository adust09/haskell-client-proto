module Test.Crypto.KeyManager (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent.Async (mapConcurrently)
import qualified Data.ByteString as BS
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

import Crypto.Error (CryptoError (..))
import Crypto.LeanSig (generateKeyPair)
import Crypto.KeyManager

-- | Unwrap a Right or fail.
unsafeRight :: (Show e) => Either e a -> a
unsafeRight (Right x) = x
unsafeRight (Left e)  = error ("unexpected Left: " <> show e)

tests :: TestTree
tests = testGroup "Crypto.KeyManager"
  [ testCase "leaf index increments after each sign" $
      withSystemTempDirectory "km-test" $ \tmpDir -> do
        let keyPath = tmpDir </> "key.dat"
            (pk, pub) = unsafeRight $ generateKeyPair 10 "test-seed"
        mk <- newManagedKey pk pub
        r0 <- remainingLeaves mk
        r0 @?= 1024  -- 2^10

        sig0 <- managedSign mk keyPath "msg0"
        assertBool "sign 0 should succeed" (isRight sig0)
        r1 <- remainingLeaves mk
        r1 @?= 1023

        sig1 <- managedSign mk keyPath "msg1"
        assertBool "sign 1 should succeed" (isRight sig1)
        r2 <- remainingLeaves mk
        r2 @?= 1022

  , testCase "KeyExhausted after 2^h signatures (h=1)" $
      withSystemTempDirectory "km-test" $ \tmpDir -> do
        let keyPath = tmpDir </> "key.dat"
            (pk, pub) = unsafeRight $ generateKeyPair 1 "test-seed"
        mk <- newManagedKey pk pub
        r0 <- remainingLeaves mk
        r0 @?= 2  -- 2^1

        sig0 <- managedSign mk keyPath "msg0"
        assertBool "sign 0 should succeed" (isRight sig0)
        sig1 <- managedSign mk keyPath "msg1"
        assertBool "sign 1 should succeed" (isRight sig1)

        result <- managedSign mk keyPath "msg2"
        result @?= Left KeyExhausted

  , testCase "persist and reload preserves state" $
      withSystemTempDirectory "km-test" $ \tmpDir -> do
        let keyPath = tmpDir </> "key.dat"
            (pk, pub) = unsafeRight $ generateKeyPair 10 "test-seed"
        mk <- newManagedKey pk pub
        sig <- managedSign mk keyPath "msg0"
        assertBool "sign should succeed" (isRight sig)

        -- Load from persisted file
        mk' <- unsafeRight <$> loadManagedKey keyPath
        r <- remainingLeaves mk'
        r @?= 1023  -- used 1 leaf

        -- Can continue signing
        sig2 <- managedSign mk' keyPath "msg1"
        assertBool "sign after reload should succeed" (isRight sig2)
        r' <- remainingLeaves mk'
        r' @?= 1022

  , testCase "concurrent signing produces no errors" $
      withSystemTempDirectory "km-test" $ \tmpDir -> do
        let keyPath = tmpDir </> "key.dat"
            (pk, pub) = unsafeRight $ generateKeyPair 10 "test-seed"
        mk <- newManagedKey pk pub

        -- Sign from 8 concurrent threads, 10 signatures each
        let numThreads = 8 :: Int
            sigsPerThread = 10 :: Int
        results <- mapConcurrently
          (\threadId -> do
            sigs <- sequence
              [ managedSign mk keyPath (BS.pack [fromIntegral threadId, fromIntegral i])
              | i <- [0 :: Int .. sigsPerThread - 1]
              ]
            pure sigs
          )
          [0 .. numThreads - 1]

        let allResults = concat results
            successes = filter isRight allResults
        -- All should succeed (80 < 1024)
        length successes @?= numThreads * sigsPerThread

        -- Verify remaining leaves
        r <- remainingLeaves mk
        r @?= 1024 - fromIntegral (numThreads * sigsPerThread)

  , testCase "load nonexistent file returns error" $
      withSystemTempDirectory "km-test" $ \tmpDir -> do
        let keyPath = tmpDir </> "nonexistent.dat"
        result <- loadManagedKey keyPath
        case result of
          Left InvalidKeyFormat -> pure ()
          Left e -> assertFailure ("expected InvalidKeyFormat, got: Left " <> show e)
          Right _ -> assertFailure "expected Left, got Right"
  ]

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False
