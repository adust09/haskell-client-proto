module Test.SSZ.Vector (tests) where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data.Word (Word64)
import Test.Tasty
import Test.Tasty.HUnit
import SSZ.Common
import SSZ.Vector

-- | Helper: unwrap a Right or fail the test.
unsafeRight :: (Show e) => Either e a -> a
unsafeRight (Right a) = a
unsafeRight (Left e)  = error ("expected Right, got Left: " ++ show e)

tests :: TestTree
tests = testGroup "SSZ.Vector"
  [ testGroup "mkSszVector"
      [ testCase "accepts correct length" $ do
          case mkSszVector @4 (V.fromList ([1, 2, 3, 4] :: [Word64])) of
            Right _  -> pure ()
            Left err -> assertFailure (show err)
      , testCase "rejects wrong length" $ do
          case mkSszVector @4 (V.fromList ([1, 2, 3] :: [Word64])) of
            Left (InvalidLength 4 3) -> pure ()
            other -> assertFailure ("expected InvalidLength 4 3, got " ++ show other)
      ]
  , testGroup "Ssz metadata"
      [ testCase "SszVector 4 Word64 is fixed-size" $
          sszIsFixedSize @(SszVector 4 Word64) @?= True
      , testCase "SszVector 4 Word64 fixed size == 32" $
          sszFixedSize @(SszVector 4 Word64) @?= Just 32
      ]
  , testGroup "encode/decode"
      [ testCase "encode 4 Word64s" $ do
          let v = unsafeRight $ mkSszVector @4 (V.fromList ([1, 2, 3, 4] :: [Word64]))
          BS.length (sszEncode v) @?= 32
      , testCase "roundtrip 4 Word64s" $ do
          let v = unsafeRight $ mkSszVector @4 (V.fromList ([42, 0, 999, 1] :: [Word64]))
              decoded = sszDecode @(SszVector 4 Word64) (sszEncode v)
          fmap (V.toList . unSszVector) decoded @?= Right [42, 0, 999, 1]
      , testCase "decode rejects wrong length" $ do
          let decoded = sszDecode @(SszVector 4 Word64) (BS.replicate 16 0)
          case decoded of
            Left (InvalidLength 32 16) -> pure ()
            other -> assertFailure ("expected InvalidLength 32 16, got " ++ show other)
      ]
  ]
