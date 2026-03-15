module Test.SSZ.Derive (tests) where

import qualified Data.ByteString as BS
import Data.Word (Word64)
import GHC.Generics (Generic, Rep)
import Test.Tasty
import Test.Tasty.HUnit
import SSZ.Common
import SSZ.Derive
import SSZ.List (SszList, mkSszList)
import SSZ.Merkleization

-- ---------------------------------------------------------------------------
-- Test container: all fixed fields
-- ---------------------------------------------------------------------------

data TestFixed = TestFixed
  { tfSlot :: !Word64
  , tfRoot :: !(BytesN 32)
  } deriving stock (Generic, Eq, Show)

instance Ssz TestFixed where
  sszFixedSize = genericSszFixedSize @(Rep TestFixed)

instance SszEncode TestFixed where
  sszEncode = genericSszEncode

instance SszDecode TestFixed where
  sszDecode = genericSszDecode

instance SszHashTreeRoot TestFixed where
  hashTreeRoot = genericHashTreeRoot

-- ---------------------------------------------------------------------------
-- Test container: mixed fixed and variable fields
-- ---------------------------------------------------------------------------

data TestMixed = TestMixed
  { tmSlot  :: !Word64
  , tmItems :: !(SszList 100 Word64)
  , tmFlag  :: !Bool
  } deriving stock (Generic, Eq, Show)

instance Ssz TestMixed where
  sszFixedSize = genericSszFixedSize @(Rep TestMixed)

instance SszEncode TestMixed where
  sszEncode = genericSszEncode

instance SszDecode TestMixed where
  sszDecode = genericSszDecode

-- ---------------------------------------------------------------------------
-- Test container: nested container
-- ---------------------------------------------------------------------------

data TestNested = TestNested
  { tnInner :: !TestFixed
  , tnValue :: !Word64
  } deriving stock (Generic, Eq, Show)

instance Ssz TestNested where
  sszFixedSize = genericSszFixedSize @(Rep TestNested)

instance SszEncode TestNested where
  sszEncode = genericSszEncode

instance SszDecode TestNested where
  sszDecode = genericSszDecode

instance SszHashTreeRoot TestNested where
  hashTreeRoot = genericHashTreeRoot

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

unsafeRight :: (Show e) => Either e a -> a
unsafeRight (Right a) = a
unsafeRight (Left e)  = error ("expected Right, got Left: " ++ show e)

tests :: TestTree
tests = testGroup "SSZ.Derive"
  [ testGroup "fixed container (TestFixed)"
      [ testCase "Ssz metadata: fixed-size 40 bytes" $ do
          sszFixedSize @TestFixed @?= Just 40
          sszIsFixedSize @TestFixed @?= True
      , testCase "encode produces 40 bytes" $ do
          let root = unsafeRight $ mkBytesN @32 (BS.replicate 32 0xAA)
              tf = TestFixed 42 root
          BS.length (sszEncode tf) @?= 40
      , testCase "roundtrip" $ do
          let root = unsafeRight $ mkBytesN @32 (BS.pack [1..32])
              tf = TestFixed 12345 root
          sszDecode (sszEncode tf) @?= Right tf
      , testCase "hashTreeRoot matches manual computation" $ do
          let root = unsafeRight $ mkBytesN @32 (BS.replicate 32 0)
              tf = TestFixed 0 root
              -- Field 0: hashTreeRoot(Word64 0) = 32 zero bytes
              -- Field 1: hashTreeRoot(Bytes32 zero) = 32 zero bytes
              -- merkleize([zero, zero], 2) = sha256(zero ++ zero)
              fieldRoot0 = hashTreeRoot (0 :: Word64)
              fieldRoot1 = hashTreeRoot root
              expected = merkleize [fieldRoot0, fieldRoot1] 2
          hashTreeRoot tf @?= expected
      ]
  , testGroup "mixed container (TestMixed)"
      [ testCase "Ssz metadata: variable-size" $ do
          sszIsFixedSize @TestMixed @?= False
          sszFixedSize @TestMixed @?= Nothing
      , testCase "roundtrip with non-empty list" $ do
          let items = unsafeRight $ mkSszList @100 ([10, 20, 30] :: [Word64])
              tm = TestMixed 99 items True
          sszDecode (sszEncode tm) @?= Right tm
      , testCase "roundtrip with empty list" $ do
          let items = unsafeRight $ mkSszList @100 ([] :: [Word64])
              tm = TestMixed 0 items False
          sszDecode (sszEncode tm) @?= Right tm
      ]
  , testGroup "nested container (TestNested)"
      [ testCase "roundtrip" $ do
          let root = unsafeRight $ mkBytesN @32 (BS.pack [1..32])
              inner = TestFixed 777 root
              tn = TestNested inner 888
          sszDecode (sszEncode tn) @?= Right tn
      , testCase "hashTreeRoot" $ do
          let root = unsafeRight $ mkBytesN @32 (BS.replicate 32 0xFF)
              inner = TestFixed 1 root
              tn = TestNested inner 2
              innerRoot = hashTreeRoot inner
              valueRoot = hashTreeRoot (2 :: Word64)
              expected = merkleize [innerRoot, valueRoot] 2
          hashTreeRoot tn @?= expected
      ]
  ]
