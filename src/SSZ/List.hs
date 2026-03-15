{-# LANGUAGE AllowAmbiguousTypes #-}

-- | SSZ variable-length list with max capacity.
module SSZ.List
  ( SszList
  , mkSszList
  , unSszList
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import GHC.TypeNats (KnownNat, Nat, natVal)
import SSZ.Common

-- | Variable-length list with max capacity @n@ (type-level).
-- Constructor is hidden; use 'mkSszList'.
newtype SszList (n :: Nat) a = UnsafeSszList [a]
  deriving stock (Eq, Show)

-- | Smart constructor. Validates that length does not exceed max capacity.
mkSszList :: forall n a. KnownNat n => [a] -> Either SszError (SszList n a)
mkSszList xs
  | length xs > maxCap = Left (InvalidLength maxCap (length xs))
  | otherwise          = Right (UnsafeSszList xs)
  where
    maxCap = fromIntegral (natVal (Proxy @n))

-- | Unwrap.
unSszList :: SszList n a -> [a]
unSszList (UnsafeSszList xs) = xs

-- SszList is always variable-size.
instance Ssz (SszList n a) where
  sszFixedSize = Nothing

-- | Encode: fixed-size elements → concatenate; variable-size → offset table + data.
instance (SszEncode a, Ssz a) => SszEncode (SszList n a) where
  sszEncode (UnsafeSszList xs) =
    case sszFixedSize @a of
      Just _  -> BS.concat (map sszEncode xs)
      Nothing ->
        let encoded = map sszEncode xs
            numElems = length xs
            offsetTableSize = numElems * 4
            -- Each offset = offsetTableSize + sum of encoded sizes before this element
            offsets = scanl (\acc bs -> acc + BS.length bs) offsetTableSize encoded
            -- scanl produces numElems+1 entries; take first numElems
            offsetBytes = map (sszEncode . (fromIntegral :: Int -> Word32)) (take numElems offsets)
        in  BS.concat offsetBytes <> BS.concat encoded

-- | Decode: fixed-size elements → split by element size; variable-size → offset table.
instance (KnownNat n, SszDecode a, Ssz a) => SszDecode (SszList n a) where
  sszDecode bs = do
    let maxCap = fromIntegral (natVal (Proxy @n)) :: Int
    xs <- case sszFixedSize @a of
      Just elemSize ->
        let es = fromIntegral elemSize
        in  if not (BS.null bs) && BS.length bs `mod` es /= 0
              then Left (InvalidLength es (BS.length bs))
              else mapM sszDecode (chunksOf es bs)
      Nothing -> decodeVariableList bs
    if length xs > maxCap
      then Left (InvalidLength maxCap (length xs))
      else Right (UnsafeSszList xs)

-- | Split a ByteString into chunks of a given size.
chunksOf :: Int -> ByteString -> [ByteString]
chunksOf _ bs | BS.null bs = []
chunksOf n bs = BS.take n bs : chunksOf n (BS.drop n bs)

-- | Decode a list of variable-size elements using offset table.
decodeVariableList :: SszDecode a => ByteString -> Either SszError [a]
decodeVariableList bs
  | BS.null bs = Right []
  | BS.length bs < 4 = Left (InvalidLength 4 (BS.length bs))
  | otherwise = do
      firstOffset <- sszDecode @Word32 (BS.take 4 bs)
      let numElems = fromIntegral firstOffset `div` 4
      if numElems == 0
        then Right []
        else do
          offsets <- mapM (\i -> sszDecode @Word32 (BS.take 4 (BS.drop (i * 4) bs)))
                         [0 .. numElems - 1]
          let ends = map fromIntegral (drop 1 offsets) ++ [BS.length bs]
              starts = map fromIntegral offsets
              slices = zipWith (\s e -> BS.take (e - s) (BS.drop s bs)) starts ends
          mapM sszDecode slices
