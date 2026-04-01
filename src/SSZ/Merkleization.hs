{-# LANGUAGE AllowAmbiguousTypes #-}

-- | SSZ Merkleization: hash_tree_root computation using SHA-256.
module SSZ.Merkleization
  ( -- * Hashing
    sha256
  , sha256Pair
    -- * Chunking
  , pack
  , packBits
    -- * Merkle tree
  , merkleize
  , mixInLength
  , zeroHashes
    -- * Typeclass
  , SszHashTreeRoot (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Crypto.Hashing (sha256, sha256Pair)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.TypeNats (KnownNat, natVal)
import SSZ.Bitvector (Bitvector, unBitvector)
import SSZ.Bitlist (Bitlist, unBitlist, bitlistLen)
import SSZ.Common
import SSZ.List (SszList, unSszList)
import SSZ.Vector (SszVector, unSszVector)

-- ---------------------------------------------------------------------------
-- Chunking
-- ---------------------------------------------------------------------------

-- | Zero-pad and split into 32-byte chunks.
-- If input is empty, returns one zero chunk.
pack :: [ByteString] -> [ByteString]
pack bss =
  let concatenated = BS.concat bss
      padded = zeroPadTo32 concatenated
  in  chunksOf 32 padded
  where
    zeroPadTo32 bs
      | BS.null bs = BS.replicate 32 0
      | otherwise  =
          let remainder = BS.length bs `mod` 32
          in  if remainder == 0
                then bs
                else bs <> BS.replicate (32 - remainder) 0

-- | Pack bits into 32-byte chunks (for Bitvector/Bitlist).
packBits :: [Bool] -> [ByteString]
packBits bits =
  let -- Pack bits into bytes, LSB first
      numBytes = (length bits + 7) `div` 8
      buildByte byteIdx =
        foldl (\acc bitIdx ->
          let globalIdx = byteIdx * 8 + bitIdx
              bit = if globalIdx < length bits && bits !! globalIdx then 1 else 0
          in  acc + bit * (2 ^ bitIdx)
        ) (0 :: Word8) [0..7]
      packed = BS.pack [buildByte i | i <- [0 .. numBytes - 1]]
  in  pack [packed]

-- | Split a ByteString into chunks of a given size.
chunksOf :: Int -> ByteString -> [ByteString]
chunksOf _ bs | BS.null bs = []
chunksOf n bs = BS.take n bs : chunksOf n (BS.drop n bs)

-- ---------------------------------------------------------------------------
-- Merkle tree
-- ---------------------------------------------------------------------------

-- | Pre-computed zero hashes. zeroHashes[0] = 32 zero bytes,
-- zeroHashes[i] = sha256(zeroHashes[i-1] ++ zeroHashes[i-1]).
zeroHashes :: V.Vector ByteString
zeroHashes = V.generate 64 go
  where
    go 0 = BS.replicate 32 0
    go i = let prev = zeroHashes V.! (i - 1)
           in  sha256Pair prev prev

-- | Compute the Merkle root of chunks with a given limit.
-- The tree depth is determined by the limit (next power of 2).
-- Chunks are padded with zero hashes to fill the tree.
merkleize :: [ByteString] -> Word64 -> ByteString
merkleize chunks limit =
  let depth = if limit <= 1 then 0 else ceilLog2 limit
      -- Empty chunk list: start with a zero chunk so the tree is well-formed
      effectiveChunks = if null chunks then [zeroHashes V.! 0] else chunks
  in  go effectiveChunks depth 0
  where
    -- Recursively hash pairs up to the root
    go [single] 0 _ = single
    go _ 0 _level = error "merkleize: too many chunks for depth 0"
    go cs d level =
      let padded = padToEven cs level
          paired = pairHash padded
      in  go paired (d - 1) (level + 1)

    -- Pad the chunk list to an even number, using zero hashes at the given level
    padToEven cs level
      | even (length cs) = cs
      | otherwise        = cs ++ [zeroHashes V.! level]

    -- Hash adjacent pairs
    pairHash [] = []
    pairHash [x] = [x]  -- shouldn't happen after padding
    pairHash (x : y : rest) = sha256Pair x y : pairHash rest

    -- Ceiling log2
    ceilLog2 :: Word64 -> Int
    ceilLog2 1 = 0
    ceilLog2 n = 1 + ceilLog2 ((n + 1) `div` 2)

-- | Mix in the length for variable-size types.
-- result = SHA-256(root ++ little_endian_64(length))
mixInLength :: ByteString -> Word64 -> ByteString
mixInLength root len =
  sha256Pair root (sszEncode len)

-- ---------------------------------------------------------------------------
-- SszHashTreeRoot typeclass
-- ---------------------------------------------------------------------------

-- | Compute the hash tree root of an SSZ value.
class Ssz a => SszHashTreeRoot a where
  hashTreeRoot :: a -> ByteString

-- Basic types: merkleize(pack([encode(value)]))
instance SszHashTreeRoot Word8 where
  hashTreeRoot = head . pack . pure . sszEncode

instance SszHashTreeRoot Word16 where
  hashTreeRoot = head . pack . pure . sszEncode

instance SszHashTreeRoot Word32 where
  hashTreeRoot = head . pack . pure . sszEncode

instance SszHashTreeRoot Word64 where
  hashTreeRoot = head . pack . pure . sszEncode

instance SszHashTreeRoot Bool where
  hashTreeRoot = head . pack . pure . sszEncode

-- BytesN: merkleize(pack([value]), limit=ceil(n/32))
instance KnownNat n => SszHashTreeRoot (BytesN n) where
  hashTreeRoot bn =
    let n = natVal (Proxy @n)
        limit = fromIntegral ((n + 31) `div` 32)
        chunks = pack [unBytesN bn]
    in  merkleize chunks limit

-- Bitvector: merkleize(packBits(bits), limit=ceil(n/256))
instance KnownNat n => SszHashTreeRoot (Bitvector n) where
  hashTreeRoot bv =
    let n = natVal (Proxy @n)
        limit = fromIntegral ((n + 255) `div` 256)
        rawBytes = unBitvector bv
        bits = [BS.index rawBytes (i `div` 8) `testBitAt` (i `mod` 8)
               | i <- [0 .. fromIntegral n - 1]]
        chunks = packBits bits
    in  merkleize chunks limit

-- Bitlist: mixInLength(merkleize(packBits(bits), limit=ceil(n/256)), len)
instance KnownNat n => SszHashTreeRoot (Bitlist n) where
  hashTreeRoot bl =
    let n = natVal (Proxy @n)
        limit = fromIntegral ((n + 255) `div` 256)
        len = bitlistLen bl
        rawBytes = unBitlist bl
        bits = [BS.index rawBytes (i `div` 8) `testBitAt` (i `mod` 8)
               | i <- [0 .. len - 1]]
        chunks = packBits bits
        root = merkleize chunks limit
    in  mixInLength root (fromIntegral len)

-- SszVector (fixed-size elements): merkleize(pack(map encode elems), limit=chunkLimit)
-- SszVector (composite elements): merkleize(map hashTreeRoot elems, limit=n)
-- For fixed-size elements, the limit must be in chunks: (N * size + 31) / 32
instance (KnownNat n, SszHashTreeRoot a, SszEncode a, Ssz a)
      => SszHashTreeRoot (SszVector n a) where
  hashTreeRoot sv =
    let n = fromIntegral (natVal (Proxy @n))
        elems = V.toList (unSszVector sv)
    in  case sszFixedSize @a of
          Just s ->
            let chunks = pack (map sszEncode elems)
                chunkLimit = (n * fromIntegral s + 31) `div` 32
            in  merkleize chunks chunkLimit
          Nothing ->
            merkleize (map hashTreeRoot elems) n

-- SszList (fixed-size elements): mixInLength(merkleize(pack(map encode elems), limit=chunkLimit), len)
-- SszList (composite elements): mixInLength(merkleize(map hashTreeRoot elems, limit=n), len)
-- For fixed-size elements, the limit must be in chunks: (N * size + 31) / 32
instance (KnownNat n, SszHashTreeRoot a, SszEncode a, Ssz a)
      => SszHashTreeRoot (SszList n a) where
  hashTreeRoot sl =
    let n = fromIntegral (natVal (Proxy @n))
        elems = unSszList sl
        len = fromIntegral (length elems)
        root = case sszFixedSize @a of
          Just s ->
            let chunks = pack (map sszEncode elems)
                chunkLimit = (n * fromIntegral s + 31) `div` 32
            in  merkleize chunks chunkLimit
          Nothing ->
            merkleize (map hashTreeRoot elems) n
    in  mixInLength root len

-- Helper: test a bit in a byte
testBitAt :: Word8 -> Int -> Bool
testBitAt byte bit = byte `div` (2 ^ bit) `mod` 2 == 1
