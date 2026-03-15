{-# LANGUAGE AllowAmbiguousTypes #-}

-- | SSZ fixed-length vector.
module SSZ.Vector
  ( SszVector
  , mkSszVector
  , unSszVector
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Data.Word (Word32)
import GHC.TypeNats (KnownNat, Nat, natVal)
import SSZ.Common

-- | Fixed-length homogeneous collection. Constructor is hidden; use 'mkSszVector'.
newtype SszVector (n :: Nat) a = UnsafeSszVector (V.Vector a)
  deriving stock (Eq, Show)

-- | Smart constructor. Validates that length equals @n@.
mkSszVector :: forall n a. KnownNat n => V.Vector a -> Either SszError (SszVector n a)
mkSszVector v
  | V.length v == expected = Right (UnsafeSszVector v)
  | otherwise              = Left (InvalidLength expected (V.length v))
  where
    expected = fromIntegral (natVal (Proxy @n))

-- | Unwrap.
unSszVector :: SszVector n a -> V.Vector a
unSszVector (UnsafeSszVector v) = v

-- | Fixed-size iff element type is fixed-size.
instance (KnownNat n, Ssz a) => Ssz (SszVector n a) where
  sszFixedSize = case sszFixedSize @a of
    Just elemSize -> Just (fromIntegral (natVal (Proxy @n)) * elemSize)
    Nothing       -> Nothing

-- | Encode: fixed elements → concatenate; variable elements → offset table + data.
instance (KnownNat n, SszEncode a, Ssz a) => SszEncode (SszVector n a) where
  sszEncode (UnsafeSszVector v) =
    case sszFixedSize @a of
      Just _  -> BS.concat (map sszEncode (V.toList v))
      Nothing ->
        let encoded = map sszEncode (V.toList v)
            numElems = V.length v
            offsetTableSize = numElems * 4
            offsets = scanl (\acc bs -> acc + BS.length bs) offsetTableSize encoded
            offsetBytes = map (sszEncode . (fromIntegral :: Int -> Word32)) (take numElems offsets)
        in  BS.concat offsetBytes <> BS.concat encoded

-- | Decode: fixed elements → split by element size; variable elements → offset table.
instance (KnownNat n, SszDecode a, Ssz a) => SszDecode (SszVector n a) where
  sszDecode bs = do
    let expected = fromIntegral (natVal (Proxy @n)) :: Int
    xs <- case sszFixedSize @a of
      Just elemSize ->
        let es = fromIntegral elemSize
        in  if BS.length bs /= es * expected
              then Left (InvalidLength (es * expected) (BS.length bs))
              else mapM sszDecode (chunksOf es bs)
      Nothing -> decodeVariableElements expected bs
    if length xs /= expected
      then Left (InvalidLength expected (length xs))
      else Right (UnsafeSszVector (V.fromList xs))

-- | Split a ByteString into chunks of a given size.
chunksOf :: Int -> ByteString -> [ByteString]
chunksOf _ bs | BS.null bs = []
chunksOf n bs = BS.take n bs : chunksOf n (BS.drop n bs)

-- | Decode variable-size elements from an offset table.
decodeVariableElements :: SszDecode a => Int -> ByteString -> Either SszError [a]
decodeVariableElements expected bs
  | expected == 0 && BS.null bs = Right []
  | BS.length bs < expected * 4 = Left (InvalidLength (expected * 4) (BS.length bs))
  | otherwise = do
      offsets <- mapM (\i -> sszDecode @Word32 (BS.take 4 (BS.drop (i * 4) bs)))
                      [0 .. expected - 1]
      let ends = map fromIntegral (drop 1 offsets) ++ [BS.length bs]
          starts = map fromIntegral offsets
          slices = zipWith (\s e -> BS.take (e - s) (BS.drop s bs)) starts ends
      mapM sszDecode slices
