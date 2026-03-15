module Test.SSZ.Common (tests) where

import qualified Data.ByteString as BS
import Data.Word (Word32, Word64)
import SSZ.Common
import Test.SSZ.Support
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "SSZ.Common"
  [ testGroup "mkBytesN"
      [ testCase "accepts correct length" $
          case mkBytesN @32 (BS.replicate 32 0) of
            Right _  -> pure ()
            Left err -> assertFailure ("unexpected error: " <> show err)
      , testCase "rejects wrong length" $
          case mkBytesN @32 (BS.replicate 31 0) of
            Left (InvalidLength 32 31) -> pure ()
            Left err  -> assertFailure ("unexpected error shape: " <> show err)
            Right _   -> assertFailure "should have rejected wrong length"
      ]
  , testGroup "Ssz metadata"
      [ testCase "sszFixedSize @Word64 == Just 8" $
          sszFixedSize @Word64 @?= Just 8
      , testCase "sszFixedSize @Bool == Just 1" $
          sszFixedSize @Bool @?= Just 1
      , testCase "sszFixedSize @Bytes32 == Just 32" $
          sszFixedSize @Bytes32 @?= Just 32
      , testCase "sszIsFixedSize @Word32 is True" $
          sszIsFixedSize @Word32 @?= True
      ]
  , testGroup "encode"
      [ testCase "Word64 42 little-endian" $
          sszEncode (42 :: Word64) @?= BS.pack [42, 0, 0, 0, 0, 0, 0, 0]
      , testCase "Bool True == 0x01" $
          sszEncode True @?= BS.singleton 0x01
      , testCase "Bool False == 0x00" $
          sszEncode False @?= BS.singleton 0x00
      ]
  , testGroup "decode"
      [ testCase "Word64 roundtrip 42" $
          sszDecode (BS.pack [42, 0, 0, 0, 0, 0, 0, 0]) @?= Right (42 :: Word64)
      , testCase "Bool invalid byte" $
          sszDecode @Bool (BS.singleton 0x02) @?= Left (InvalidBool 0x02)
      , testCase "Word64 wrong length" $
          sszDecode @Word64 (BS.pack [1, 2, 3]) @?= Left (InvalidLength 8 3)
      ]
  , testGroup "roundtrip properties"
      [ testProperty "Word8 roundtrip" $ \(ArbWord8 w) -> roundtripProp w
      , testProperty "Word16 roundtrip" $ \(ArbWord16 w) -> roundtripProp w
      , testProperty "Word32 roundtrip" $ \(ArbWord32 w) -> roundtripProp w
      , testProperty "Word64 roundtrip" $ \(ArbWord64 w) -> roundtripProp w
      , testProperty "Bool roundtrip" $ \(ArbBool b) -> roundtripProp b
      , testProperty "BytesN 32 roundtrip" $ \(ArbBytesN @32 bn) -> roundtripProp bn
      , testProperty "BytesN 4 roundtrip" $ \(ArbBytesN @4 bn) -> roundtripProp bn
      , testProperty "BytesN 96 roundtrip" $ \(ArbBytesN @96 bn) -> roundtripProp bn
      ]
  ]
