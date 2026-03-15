module Test.SSZ.Bitvector (tests) where

import qualified Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit
import SSZ.Common
import SSZ.Bitvector

-- | Helper: unwrap a Right or fail the test.
unsafeRight :: (Show e) => Either e a -> a
unsafeRight (Right a) = a
unsafeRight (Left e)  = error ("expected Right, got Left: " ++ show e)

tests :: TestTree
tests = testGroup "SSZ.Bitvector"
  [ testGroup "mkBitvector"
      [ testCase "accepts correct bit count" $ do
          case mkBitvector @8 (replicate 8 False) of
            Right _  -> pure ()
            Left err -> assertFailure (show err)
      , testCase "rejects wrong bit count" $ do
          case mkBitvector @8 (replicate 7 False) of
            Left (InvalidLength 8 7) -> pure ()
            other -> assertFailure ("expected InvalidLength 8 7, got " ++ show other)
      ]
  , testGroup "Ssz metadata"
      [ testCase "Bitvector 16 is fixed-size" $
          sszIsFixedSize @(Bitvector 16) @?= True
      , testCase "Bitvector 16 fixed size == 2" $
          sszFixedSize @(Bitvector 16) @?= Just 2
      , testCase "Bitvector 10 fixed size == 2" $
          sszFixedSize @(Bitvector 10) @?= Just 2
      ]
  , testGroup "encode/decode"
      [ testCase "encode 8 bits all false" $ do
          let bv = unsafeRight $ mkBitvector @8 (replicate 8 False)
          sszEncode bv @?= BS.singleton 0x00
      , testCase "encode 8 bits all true" $ do
          let bv = unsafeRight $ mkBitvector @8 (replicate 8 True)
          sszEncode bv @?= BS.singleton 0xFF
      , testCase "encode bit 0 set (LSB first)" $ do
          let bv = unsafeRight $ mkBitvector @8 (True : replicate 7 False)
          sszEncode bv @?= BS.singleton 0x01
      , testCase "roundtrip 16 bits" $ do
          let bits = [True, False, True, True, False, False, True, False,
                      False, True, False, False, True, True, False, False]
              bv = unsafeRight $ mkBitvector @16 bits
          case sszDecode @(Bitvector 16) (sszEncode bv) of
            Right bv' -> [getBit bv' i | i <- [0..15]] @?= bits
            Left err  -> assertFailure (show err)
      , testCase "decode rejects non-zero unused bits" $ do
          -- Bitvector 10 uses 2 bytes, but only 10 bits
          case sszDecode @(Bitvector 10) (BS.pack [0xFF, 0xFF]) of
            Left (ExtraBytes _) -> pure ()
            other -> assertFailure ("expected ExtraBytes, got " ++ show other)
      ]
  ]
