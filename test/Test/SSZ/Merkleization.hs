module Test.SSZ.Merkleization (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data.Word (Word64)
import Test.Tasty
import Test.Tasty.HUnit
import SSZ.Common
import SSZ.Merkleization

-- | Helper to convert hex string to ByteString.
hexToBS :: String -> ByteString
hexToBS = BS.pack . go
  where
    go [] = []
    go (a:b:rest) = fromIntegral (hexDigit a * 16 + hexDigit b) : go rest
    go _ = error "hexToBS: odd length"
    hexDigit c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
      | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
      | otherwise = error ("hexToBS: invalid char " ++ [c])

-- | Access a pre-computed zero hash by depth.
zeroHash :: Int -> ByteString
zeroHash i = SSZ.Merkleization.zeroHashes V.! i

tests :: TestTree
tests = testGroup "SSZ.Merkleization"
  [ testGroup "sha256"
      [ testCase "sha256 empty string matches NIST vector" $
          sha256 BS.empty @?=
            hexToBS "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      , testCase "sha256 \"abc\" matches NIST vector" $
          sha256 (BS.pack [0x61, 0x62, 0x63]) @?=
            hexToBS "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
      , testCase "sha256Pair a b == sha256 (a <> b)" $ do
          let a = BS.pack [1, 2, 3]
              b = BS.pack [4, 5, 6]
          sha256Pair a b @?= sha256 (a <> b)
      ]
  , testGroup "pack"
      [ testCase "empty input produces one zero chunk" $ do
          let chunks = pack []
          length chunks @?= 1
          head chunks @?= BS.replicate 32 0
      , testCase "32 bytes produces one chunk" $ do
          let input = BS.replicate 32 0xAB
              chunks = pack [input]
          length chunks @?= 1
          head chunks @?= input
      , testCase "48 bytes produces two chunks (padded)" $ do
          let input = BS.replicate 48 0xCD
              chunks = pack [input]
          length chunks @?= 2
          (chunks !! 0) @?= BS.replicate 32 0xCD
          (chunks !! 1) @?= (BS.replicate 16 0xCD <> BS.replicate 16 0)
      ]
  , testGroup "merkleize"
      [ testCase "single chunk limit=1 is identity" $ do
          let chunk = sha256 (BS.pack [42])
          merkleize [chunk] 1 @?= chunk
      , testCase "single chunk limit=2 pads with zero hash" $ do
          let chunk = BS.replicate 32 0xAA
              expected = sha256Pair chunk (zeroHash 0)
          merkleize [chunk] 2 @?= expected
      , testCase "two chunks limit=2 hashes pair" $ do
          let c0 = BS.replicate 32 0x11
              c1 = BS.replicate 32 0x22
          merkleize [c0, c1] 2 @?= sha256Pair c0 c1
      , testCase "single chunk limit=4 builds depth-2 tree" $ do
          let chunk = BS.replicate 32 0xBB
              z0 = zeroHash 0
              z1 = zeroHash 1
              left = sha256Pair chunk z0
              expected = sha256Pair left z1
          merkleize [chunk] 4 @?= expected
      ]
  , testGroup "mixInLength"
      [ testCase "mixes root with length" $ do
          let root = BS.replicate 32 0
              expected = sha256Pair root (sszEncode (3 :: Word64))
          mixInLength root 3 @?= expected
      ]
  , testGroup "hashTreeRoot"
      [ testCase "Word64 0 hash tree root" $ do
          hashTreeRoot (0 :: Word64) @?= BS.replicate 32 0
      , testCase "Bytes32 is identity" $ do
          let bs = BS.pack [1..32]
          case mkBytesN @32 bs of
            Right b32 -> hashTreeRoot b32 @?= bs
            Left err  -> assertFailure (show err)
      , testCase "Bool True hash tree root" $ do
          let expected = BS.singleton 1 <> BS.replicate 31 0
          hashTreeRoot True @?= expected
      ]
  ]
