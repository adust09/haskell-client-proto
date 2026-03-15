{-# LANGUAGE AllowAmbiguousTypes #-}

-- | SSZ variable-length bit array with sentinel bit.
module SSZ.Bitlist
  ( Bitlist
  , mkBitlist
  , unBitlist
  , bitlistLen
  , getBitlistBit
  ) where

import Data.Bits (clearBit, setBit, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.TypeNats (KnownNat, Nat, natVal)
import SSZ.Common

-- | Variable-length bit array with max capacity @n@. Constructor is hidden.
-- Internally stores the bits WITHOUT the sentinel (just the data bits packed).
-- The sentinel is added/removed during encode/decode.
data Bitlist (n :: Nat) = UnsafeBitlist
  { blBits   :: !ByteString  -- ^ packed data bits (no sentinel)
  , blLength :: !Int          -- ^ number of data bits
  }
  deriving stock (Eq, Show)

-- | Smart constructor from a list of bools.
mkBitlist :: forall n. KnownNat n => [Bool] -> Either SszError (Bitlist n)
mkBitlist bits
  | numBits > maxCap = Left (InvalidLength maxCap numBits)
  | otherwise        = Right (UnsafeBitlist (packBitsRaw bits) numBits)
  where
    numBits = length bits
    maxCap  = fromIntegral (natVal (Proxy @n))

-- | Raw bytes (without sentinel).
unBitlist :: Bitlist n -> ByteString
unBitlist = blBits

-- | Number of data bits.
bitlistLen :: Bitlist n -> Int
bitlistLen = blLength

-- | Get bit at index.
getBitlistBit :: Bitlist n -> Int -> Bool
getBitlistBit (UnsafeBitlist bs _) i =
  let byteIdx = i `div` 8
      bitIdx  = i `mod` 8
  in  testBit (BS.index bs byteIdx) bitIdx

-- | Pack bits into bytes, LSB first (no sentinel).
packBitsRaw :: [Bool] -> ByteString
packBitsRaw bits =
  let numBytes = (length bits + 7) `div` 8
      buildByte byteIdx =
        foldl (\acc bitIdx ->
          let globalIdx = byteIdx * 8 + bitIdx
          in  if globalIdx < length bits && bits !! globalIdx
                then setBit acc bitIdx
                else acc
        ) (0 :: Word8) [0..7]
  in  BS.pack [buildByte i | i <- [0 .. numBytes - 1]]

-- | Variable-size.
instance Ssz (Bitlist n) where
  sszFixedSize = Nothing

-- | Encode: pack bits + append sentinel bit.
instance SszEncode (Bitlist n) where
  sszEncode (UnsafeBitlist _ numBits) | numBits == 0 =
    -- Empty bitlist: just the sentinel byte 0x01
    BS.singleton 0x01
  sszEncode bl@(UnsafeBitlist _packed numBits) =
    -- Re-pack all bits including the sentinel
    let sentinelIdx = numBits  -- sentinel goes right after last data bit
        totalBits   = numBits + 1
        numBytes    = (totalBits + 7) `div` 8
        buildByte byteIdx =
          foldl (\acc bitIdx ->
            let globalIdx = byteIdx * 8 + bitIdx
            in  if globalIdx == sentinelIdx
                  then setBit acc bitIdx   -- sentinel bit
                  else if globalIdx < numBits && getBitlistBit bl globalIdx
                    then setBit acc bitIdx
                    else acc
          ) (0 :: Word8) [0..7]
    in  BS.pack [buildByte i | i <- [0 .. numBytes - 1]]

-- | Decode: find sentinel bit, everything below is data.
instance KnownNat n => SszDecode (Bitlist n) where
  sszDecode bs = do
    let maxCap = fromIntegral (natVal (Proxy @n)) :: Int
    if BS.null bs
      then Left InvalidSentinel
      else do
        -- Find the sentinel: highest set bit in the last byte
        let lastByte = BS.index bs (BS.length bs - 1)
        sentinelBitInByte <- findHighestSetBit lastByte
        let numBits = (BS.length bs - 1) * 8 + sentinelBitInByte
        if numBits > maxCap
          then Left (InvalidLength maxCap numBits)
          else do
            -- Clear the sentinel bit and keep just the data
            let cleared = BS.init bs `BS.snoc` clearBit lastByte sentinelBitInByte
                dataBytes = BS.take ((numBits + 7) `div` 8) cleared
            Right (UnsafeBitlist dataBytes numBits)

-- | Find the index of the highest set bit in a byte. Returns error if 0.
findHighestSetBit :: Word8 -> Either SszError Int
findHighestSetBit 0 = Left InvalidSentinel
findHighestSetBit b = Right (go 7)
  where
    go i
      | testBit b i = i
      | otherwise   = go (i - 1)
