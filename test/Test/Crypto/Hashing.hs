module Test.Crypto.Hashing (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Word (Word8, Word64)
import qualified Data.ByteString as BS
import Crypto.Hashing (sha256, sha256Pair)
import Crypto.SigningRoot (computeDomain, computeSigningRoot)
import Consensus.Constants (Domain)
import SSZ.Common (mkBytesN, unBytesN, zeroN)

-- | Convert a ByteString to hex string for comparison.
toHex :: BS.ByteString -> String
toHex = concatMap toHexByte . BS.unpack
  where
    toHexByte :: Word8 -> String
    toHexByte w =
      let (hi, lo) = w `divMod` 16
      in  [hexChar hi, hexChar lo]
    hexChar :: Word8 -> Char
    hexChar n
      | n < 10    = toEnum (fromIntegral n + fromEnum '0')
      | otherwise  = toEnum (fromIntegral n - 10 + fromEnum 'a')

-- | Unwrap a Right or fail the test.
unsafeRight :: (Show e) => Either e a -> a
unsafeRight (Right x) = x
unsafeRight (Left e)  = error ("unexpected Left: " <> show e)

tests :: TestTree
tests = testGroup "Crypto.Hashing"
  [ testGroup "SHA-256"
    [ testCase "empty string — NIST vector" $ do
        let result = toHex (sha256 "")
        result @?= "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    , testCase "abc — NIST vector" $ do
        let result = toHex (sha256 "abc")
        result @?= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    , testCase "sha256Pair consistency" $ do
        let a = "hello"
            b = "world"
        sha256Pair a b @?= sha256 (a <> b)
    ]
  , testGroup "computeDomain"
    [ testCase "produces 32-byte domain starting with domain type" $ do
        let domainType = unsafeRight $ mkBytesN @4 (BS.pack [0x07, 0x00, 0x00, 0x00])
            version = unsafeRight $ mkBytesN @4 (BS.pack [0x00, 0x00, 0x00, 0x01])
            genesisRoot = zeroN @32
            domain = computeDomain domainType version genesisRoot
        BS.length (unBytesN domain) @?= 32
        -- First 4 bytes must be the domain type
        BS.take 4 (unBytesN domain) @?= BS.pack [0x07, 0x00, 0x00, 0x00]
    , testCase "different fork versions produce different domains" $ do
        let domainType = unsafeRight $ mkBytesN @4 (BS.pack [0x07, 0x00, 0x00, 0x00])
            v1 = unsafeRight $ mkBytesN @4 (BS.pack [0x00, 0x00, 0x00, 0x01])
            v2 = unsafeRight $ mkBytesN @4 (BS.pack [0x00, 0x00, 0x00, 0x02])
            genesisRoot = zeroN @32
            d1 = computeDomain domainType v1 genesisRoot
            d2 = computeDomain domainType v2 genesisRoot
        d1 /= d2 @? "different fork versions should produce different domains"
    ]
  , testGroup "computeSigningRoot"
    [ testCase "produces 32-byte root" $ do
        let val = (42 :: Word64)
            domain = zeroN @32 :: Domain
            root = computeSigningRoot val domain
        BS.length (unBytesN root) @?= 32
    , testCase "different domains produce different roots" $ do
        let val = (42 :: Word64)
            d1 = zeroN @32 :: Domain
            d2 = unsafeRight $ mkBytesN @32 (BS.replicate 31 0 <> BS.singleton 1)
        computeSigningRoot val d1 /= computeSigningRoot val d2 @?
          "different domains should produce different signing roots"
    , testCase "different values produce different roots" $ do
        let domain = zeroN @32 :: Domain
        computeSigningRoot (1 :: Word64) domain /= computeSigningRoot (2 :: Word64) domain @?
          "different values should produce different signing roots"
    ]
  ]
