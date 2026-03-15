{-# LANGUAGE AllowAmbiguousTypes #-}

-- | SSZ fixed-length bit array.
module SSZ.Bitvector
  ( Bitvector
  , mkBitvector
  , unBitvector
  , bitvectorBits
  , getBit
  ) where

import Data.Bits (setBit, shiftR, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.TypeNats (KnownNat, Nat, natVal)
import SSZ.Common

-- | Fixed-length bit array of @n@ bits. Constructor is hidden.
newtype Bitvector (n :: Nat) = UnsafeBitvector ByteString
  deriving stock (Eq, Ord, Show)

-- | Smart constructor from a list of bools.
mkBitvector :: forall n. KnownNat n => [Bool] -> Either SszError (Bitvector n)
mkBitvector bits
  | length bits /= numBits = Left (InvalidLength numBits (length bits))
  | otherwise              = Right (UnsafeBitvector (packBits bits))
  where
    numBits = fromIntegral (natVal (Proxy @n))

-- | Raw bytes.
unBitvector :: Bitvector n -> ByteString
unBitvector (UnsafeBitvector bs) = bs

-- | Number of bits.
bitvectorBits :: forall n. KnownNat n => Bitvector n -> Int
bitvectorBits _ = fromIntegral (natVal (Proxy @n))

-- | Get bit at index (0-based, LSB first within each byte).
getBit :: Bitvector n -> Int -> Bool
getBit (UnsafeBitvector bs) i =
  let byteIdx = i `div` 8
      bitIdx  = i `mod` 8
  in  testBit (BS.index bs byteIdx) bitIdx

-- | Pack bits into bytes, LSB first.
packBits :: [Bool] -> ByteString
packBits bits =
  let numBytes = (length bits + 7) `div` 8
      buildByte byteIdx =
        foldl (\acc bitIdx ->
          let globalIdx = byteIdx * 8 + bitIdx
          in  if globalIdx < length bits && bits !! globalIdx
                then setBit acc bitIdx
                else acc
        ) (0 :: Word8) [0..7]
  in  BS.pack [buildByte i | i <- [0 .. numBytes - 1]]

-- | Serialized size: ceil(n / 8) bytes. Fixed-size.
instance KnownNat n => Ssz (Bitvector n) where
  sszFixedSize = Just (fromIntegral ((natVal (Proxy @n) + 7) `div` 8))

instance KnownNat n => SszEncode (Bitvector n) where
  sszEncode (UnsafeBitvector bs) = bs

instance KnownNat n => SszDecode (Bitvector n) where
  sszDecode bs = do
    let numBits = fromIntegral (natVal (Proxy @n)) :: Int
        expectedBytes = (numBits + 7) `div` 8
    if BS.length bs /= expectedBytes
      then Left (InvalidLength expectedBytes (BS.length bs))
      else do
        -- Verify unused high bits in last byte are zero
        let lastByte = BS.index bs (expectedBytes - 1)
            usedBits = numBits `mod` 8
        if usedBits /= 0 && lastByte `shiftR` usedBits /= 0
          then Left (ExtraBytes (fromIntegral lastByte))
          else Right (UnsafeBitvector bs)
