module Test.SSZ.Support
  ( ArbWord8 (..)
  , ArbWord16 (..)
  , ArbWord32 (..)
  , ArbWord64 (..)
  , ArbBool (..)
  , ArbBytesN (..)
  , roundtripProp
  ) where

import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.TypeNats (KnownNat, Nat, natVal)
import SSZ.Common (BytesN, SszDecode (..), SszEncode (..), mkBytesN)
import Test.QuickCheck (Arbitrary (..), Property, counterexample, (===), vectorOf, choose)

newtype ArbWord8 = ArbWord8 Word8
  deriving stock (Show, Eq)

instance Arbitrary ArbWord8 where
  arbitrary = ArbWord8 <$> arbitrary

newtype ArbWord16 = ArbWord16 Word16
  deriving stock (Show, Eq)

instance Arbitrary ArbWord16 where
  arbitrary = ArbWord16 <$> arbitrary

newtype ArbWord32 = ArbWord32 Word32
  deriving stock (Show, Eq)

instance Arbitrary ArbWord32 where
  arbitrary = ArbWord32 <$> arbitrary

newtype ArbWord64 = ArbWord64 Word64
  deriving stock (Show, Eq)

instance Arbitrary ArbWord64 where
  arbitrary = ArbWord64 <$> arbitrary

newtype ArbBool = ArbBool Bool
  deriving stock (Show, Eq)

instance Arbitrary ArbBool where
  arbitrary = ArbBool <$> arbitrary

newtype ArbBytesN (n :: Nat) = ArbBytesN (BytesN n)
  deriving stock (Show, Eq)

instance KnownNat n => Arbitrary (ArbBytesN n) where
  arbitrary = do
    let len = fromIntegral (natVal (Proxy @n))
    bytes <- BS.pack <$> vectorOf len (choose (0, 255))
    case mkBytesN bytes of
      Right bn -> pure (ArbBytesN bn)
      Left _   -> error "ArbBytesN: impossible — length mismatch"

-- | Property that checks SSZ roundtrip: decode . encode == identity.
roundtripProp :: (SszEncode a, SszDecode a, Eq a, Show a) => a -> Property
roundtripProp x =
  counterexample ("encoded: " <> show (sszEncode x)) $
    sszDecode (sszEncode x) === Right x
