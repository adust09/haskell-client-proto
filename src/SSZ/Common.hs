{-# LANGUAGE AllowAmbiguousTypes #-}

module SSZ.Common
  ( -- * Error types
    SszError (..)
    -- * Typeclasses
  , Ssz (..)
  , sszIsFixedSize
  , SszEncode (..)
  , SszDecode (..)
    -- * BytesN (constructor hidden)
  , BytesN
  , mkBytesN
  , unBytesN
  , zeroN
    -- * Type aliases
  , Bytes4
  , Bytes20
  , Bytes32
  , Bytes48
  , Bytes96
  , Uint128
  , Uint256
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString, word16LE, word32LE, word64LE)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.TypeNats (KnownNat, Nat, natVal)

-- | SSZ decoding errors.
data SszError
  = InvalidLength Int Int -- ^ expected, actual
  | InvalidOffset Word32
  | ExtraBytes Int
  | InvalidBool Word8
  | InvalidSentinel
  | CustomError String
  deriving stock (Show, Eq)

-- | Typeclass for SSZ metadata. Returns the fixed size of a type, or
-- 'Nothing' for variable-size types.
class Ssz a where
  sszFixedSize :: Maybe Word32

-- | Convenience function: check whether a type is fixed-size.
sszIsFixedSize :: forall a. Ssz a => Bool
sszIsFixedSize = isJust (sszFixedSize @a)

-- | Typeclass for SSZ encoding.
class Ssz a => SszEncode a where
  sszEncode :: a -> ByteString

-- | Typeclass for SSZ decoding.
class Ssz a => SszDecode a where
  sszDecode :: ByteString -> Either SszError a

-- | Fixed-length byte array, parameterised by its size at the type level.
-- The constructor is intentionally not exported; use 'mkBytesN' to create
-- values.
newtype BytesN (n :: Nat) = UnsafeBytesN ByteString
  deriving stock (Eq, Ord, Show)

-- | Smart constructor for 'BytesN'. Returns 'Left' if the input length does
-- not match @n@.
mkBytesN :: forall n. KnownNat n => ByteString -> Either SszError (BytesN n)
mkBytesN bs
  | BS.length bs == len = Right (UnsafeBytesN bs)
  | otherwise           = Left (InvalidLength len (BS.length bs))
  where
    len = fromIntegral (natVal (Proxy @n))

-- | Unwrap a 'BytesN' to a raw 'ByteString'.
unBytesN :: BytesN n -> ByteString
unBytesN (UnsafeBytesN bs) = bs

-- | Create a zero-filled 'BytesN'.
zeroN :: forall n. KnownNat n => BytesN n
zeroN = UnsafeBytesN (BS.replicate (fromIntegral (natVal (Proxy @n))) 0)

-- Type aliases
type Bytes4  = BytesN 4
type Bytes20 = BytesN 20
type Bytes32 = BytesN 32
type Bytes48 = BytesN 48
type Bytes96 = BytesN 96
type Uint128 = BytesN 16
type Uint256 = BytesN 32

-- ---------------------------------------------------------------------------
-- Ssz instances
-- ---------------------------------------------------------------------------

instance Ssz Word8 where
  sszFixedSize = Just 1

instance Ssz Word16 where
  sszFixedSize = Just 2

instance Ssz Word32 where
  sszFixedSize = Just 4

instance Ssz Word64 where
  sszFixedSize = Just 8

instance Ssz Bool where
  sszFixedSize = Just 1

instance KnownNat n => Ssz (BytesN n) where
  sszFixedSize = Just (fromIntegral (natVal (Proxy @n)))

-- ---------------------------------------------------------------------------
-- SszEncode instances
-- ---------------------------------------------------------------------------

instance SszEncode Word8 where
  sszEncode w = BS.singleton w

instance SszEncode Word16 where
  sszEncode = LBS.toStrict . toLazyByteString . word16LE

instance SszEncode Word32 where
  sszEncode = LBS.toStrict . toLazyByteString . word32LE

instance SszEncode Word64 where
  sszEncode = LBS.toStrict . toLazyByteString . word64LE

instance SszEncode Bool where
  sszEncode True  = BS.singleton 0x01
  sszEncode False = BS.singleton 0x00

instance KnownNat n => SszEncode (BytesN n) where
  sszEncode = unBytesN

-- ---------------------------------------------------------------------------
-- SszDecode instances
-- ---------------------------------------------------------------------------

instance SszDecode Word8 where
  sszDecode bs
    | BS.length bs == 1 = Right (BS.index bs 0)
    | otherwise         = Left (InvalidLength 1 (BS.length bs))

instance SszDecode Word16 where
  sszDecode bs
    | BS.length bs == 2 =
        Right $  fromIntegral (BS.index bs 0)
            .|. (fromIntegral (BS.index bs 1) `shiftL` 8)
    | otherwise = Left (InvalidLength 2 (BS.length bs))

instance SszDecode Word32 where
  sszDecode bs
    | BS.length bs == 4 =
        Right $  fromIntegral (BS.index bs 0)
            .|. (fromIntegral (BS.index bs 1) `shiftL` 8)
            .|. (fromIntegral (BS.index bs 2) `shiftL` 16)
            .|. (fromIntegral (BS.index bs 3) `shiftL` 24)
    | otherwise = Left (InvalidLength 4 (BS.length bs))

instance SszDecode Word64 where
  sszDecode bs
    | BS.length bs == 8 =
        Right $  fromIntegral (BS.index bs 0)
            .|. (fromIntegral (BS.index bs 1) `shiftL` 8)
            .|. (fromIntegral (BS.index bs 2) `shiftL` 16)
            .|. (fromIntegral (BS.index bs 3) `shiftL` 24)
            .|. (fromIntegral (BS.index bs 4) `shiftL` 32)
            .|. (fromIntegral (BS.index bs 5) `shiftL` 40)
            .|. (fromIntegral (BS.index bs 6) `shiftL` 48)
            .|. (fromIntegral (BS.index bs 7) `shiftL` 56)
    | otherwise = Left (InvalidLength 8 (BS.length bs))

instance SszDecode Bool where
  sszDecode bs
    | BS.length bs /= 1 = Left (InvalidLength 1 (BS.length bs))
    | otherwise = case BS.index bs 0 of
        0x00 -> Right False
        0x01 -> Right True
        w    -> Left (InvalidBool w)

instance KnownNat n => SszDecode (BytesN n) where
  sszDecode = mkBytesN
