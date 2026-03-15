module Test.SSZ.List (tests) where

import qualified Data.ByteString as BS
import Data.Word (Word64)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import SSZ.Common
import SSZ.List

-- | Helper: unwrap a Right or fail the test.
unsafeRight :: (Show e) => Either e a -> a
unsafeRight (Right a) = a
unsafeRight (Left e)  = error ("expected Right, got Left: " ++ show e)

tests :: TestTree
tests = testGroup "SSZ.List"
  [ testGroup "mkSszList"
      [ testCase "accepts within capacity" $ do
          case mkSszList @10 ([1, 2, 3] :: [Word64]) of
            Right _  -> pure ()
            Left err -> assertFailure (show err)
      , testCase "rejects exceeding capacity" $ do
          case mkSszList @2 ([1, 2, 3] :: [Word64]) of
            Left (InvalidLength 2 3) -> pure ()
            other -> assertFailure ("expected InvalidLength 2 3, got " ++ show other)
      , testCase "accepts empty list" $ do
          case mkSszList @10 ([] :: [Word64]) of
            Right l  -> unSszList l @?= []
            Left err -> assertFailure (show err)
      ]
  , testGroup "Ssz metadata"
      [ testCase "SszList is variable-size" $
          sszIsFixedSize @(SszList 10 Word64) @?= False
      ]
  , testGroup "encode/decode fixed-size elements"
      [ testCase "encode [1,2,3] as Word64" $ do
          let l = unsafeRight $ mkSszList @10 ([1, 2, 3] :: [Word64])
          BS.length (sszEncode l) @?= 24  -- 3 * 8 bytes
      , testCase "roundtrip [42, 0, 999]" $ do
          let l = unsafeRight $ mkSszList @100 ([42, 0, 999] :: [Word64])
          fmap unSszList (sszDecode @(SszList 100 Word64) (sszEncode l)) @?= Right [42, 0, 999]
      , testCase "roundtrip empty list" $ do
          let l = unsafeRight $ mkSszList @10 ([] :: [Word64])
          fmap unSszList (sszDecode @(SszList 10 Word64) (sszEncode l)) @?= Right []
      , testCase "decode rejects exceeding capacity" $ do
          let l = unsafeRight $ mkSszList @10 ([1, 2, 3, 4, 5] :: [Word64])
              decoded = sszDecode @(SszList 3 Word64) (sszEncode l)
          case decoded of
            Left (InvalidLength 3 5) -> pure ()
            other -> assertFailure ("expected InvalidLength 3 5, got " ++ show other)
      ]
  , testGroup "roundtrip properties"
      [ testProperty "Word64 list roundtrip" $ \(xs :: [Word64]) ->
          let capped = take 50 xs
          in  case mkSszList @100 capped of
                Right l  -> fmap unSszList (sszDecode @(SszList 100 Word64) (sszEncode l)) == Right capped
                Left _   -> False
      ]
  ]
