module Test.SSZ.Bitlist (tests) where

import qualified Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit
import SSZ.Common
import SSZ.Bitlist

-- | Helper: unwrap a Right or fail the test.
unsafeRight :: (Show e) => Either e a -> a
unsafeRight (Right a) = a
unsafeRight (Left e)  = error ("expected Right, got Left: " ++ show e)

tests :: TestTree
tests = testGroup "SSZ.Bitlist"
  [ testGroup "mkBitlist"
      [ testCase "accepts within capacity" $ do
          case mkBitlist @64 (replicate 10 True) of
            Right bl -> bitlistLen bl @?= 10
            Left err -> assertFailure (show err)
      , testCase "rejects exceeding capacity" $ do
          case mkBitlist @4 (replicate 5 True) of
            Left (InvalidLength 4 5) -> pure ()
            other -> assertFailure ("expected InvalidLength 4 5, got " ++ show other)
      ]
  , testGroup "Ssz metadata"
      [ testCase "Bitlist is variable-size" $
          sszIsFixedSize @(Bitlist 64) @?= False
      ]
  , testGroup "encode/decode"
      [ testCase "empty bitlist encodes as 0x01 (just sentinel)" $ do
          let bl = unsafeRight $ mkBitlist @64 []
          sszEncode bl @?= BS.singleton 0x01
      , testCase "roundtrip empty bitlist" $ do
          let bl = unsafeRight $ mkBitlist @64 []
          fmap bitlistLen (sszDecode @(Bitlist 64) (sszEncode bl)) @?= Right 0
      , testCase "roundtrip 3 bits [T,F,T]" $ do
          let bits = [True, False, True]
              bl = unsafeRight $ mkBitlist @64 bits
          case sszDecode @(Bitlist 64) (sszEncode bl) of
            Right bl' -> do
              bitlistLen bl' @?= 3
              [getBitlistBit bl' i | i <- [0..2]] @?= bits
            Left err -> assertFailure (show err)
      , testCase "roundtrip 8 bits (sentinel in next byte)" $ do
          let bits = replicate 8 True
              bl = unsafeRight $ mkBitlist @64 bits
              encoded = sszEncode bl
          BS.length encoded @?= 2  -- 8 data bits + sentinel = 9 bits = 2 bytes
          fmap bitlistLen (sszDecode @(Bitlist 64) encoded) @?= Right 8
      , testCase "roundtrip 7 bits (sentinel in same byte)" $ do
          let bits = replicate 7 True
              bl = unsafeRight $ mkBitlist @64 bits
              encoded = sszEncode bl
          BS.length encoded @?= 1  -- 7 data bits + sentinel = 8 bits = 1 byte
          fmap bitlistLen (sszDecode @(Bitlist 64) encoded) @?= Right 7
      , testCase "decode rejects missing sentinel (all zeros)" $ do
          case sszDecode @(Bitlist 64) (BS.singleton 0x00) of
            Left InvalidSentinel -> pure ()
            other -> assertFailure ("expected InvalidSentinel, got " ++ show other)
      , testCase "decode rejects empty input" $ do
          case sszDecode @(Bitlist 64) BS.empty of
            Left InvalidSentinel -> pure ()
            other -> assertFailure ("expected InvalidSentinel, got " ++ show other)
      , testCase "decode rejects exceeding capacity" $ do
          let bits = replicate 10 False
              bl = unsafeRight $ mkBitlist @64 bits
              encoded = sszEncode bl
          case sszDecode @(Bitlist 5) encoded of
            Left (InvalidLength 5 10) -> pure ()
            other -> assertFailure ("expected InvalidLength 5 10, got " ++ show other)
      ]
  ]
