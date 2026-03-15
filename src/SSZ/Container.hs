{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Reusable two-pass container encoding and decoding helpers.
-- Used by both manual container instances and GHC.Generics auto-derivation.
module SSZ.Container
  ( -- * Encoder
    SszEncoder
  , emptyEncoder
  , addFixedField
  , addVariableField
  , finalizeEncoder
    -- * Decoder
  , FieldKind (..)
  , decodeContainer
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)
import SSZ.Common (SszDecode (..), SszEncode (..), SszError (..))

-- ---------------------------------------------------------------------------
-- Two-pass encoder
-- ---------------------------------------------------------------------------

-- | A field descriptor: either fixed bytes inline or variable bytes deferred.
data FieldEntry
  = FixedEntry !ByteString
  | VariableEntry !ByteString

-- | Collects field entries. Offsets are computed only in 'finalizeEncoder'.
newtype SszEncoder = SszEncoder [FieldEntry]

emptyEncoder :: SszEncoder
emptyEncoder = SszEncoder []

-- | Append a fixed-size field.
addFixedField :: SszEncode a => a -> SszEncoder -> SszEncoder
addFixedField a (SszEncoder fs) = SszEncoder (fs ++ [FixedEntry (sszEncode a)])

-- | Append a variable-size field.
addVariableField :: SszEncode a => a -> SszEncoder -> SszEncoder
addVariableField a (SszEncoder fs) = SszEncoder (fs ++ [VariableEntry (sszEncode a)])

-- | Compute offsets and produce the final serialized container.
finalizeEncoder :: SszEncoder -> ByteString
finalizeEncoder (SszEncoder fs) =
  let -- Total fixed region size: sum of fixed field sizes + 4 bytes per variable field
      fixedRegionSize = sum
        [ case f of
            FixedEntry bs  -> fromIntegral (BS.length bs)
            VariableEntry _ -> 4
        | f <- fs
        ]
      -- Build fixed and variable regions
      (fixedParts, varParts, _) = foldl go ([], [], fixedRegionSize) fs
      go (fixed, var, varOffset) (FixedEntry bs) =
        (fixed ++ [bs], var, varOffset)
      go (fixed, var, varOffset) (VariableEntry bs) =
        let offsetBs = sszEncode (varOffset :: Word32)
        in  (fixed ++ [offsetBs], var ++ [bs], varOffset + fromIntegral (BS.length bs))
  in  BS.concat fixedParts <> BS.concat varParts

-- ---------------------------------------------------------------------------
-- Container decoder
-- ---------------------------------------------------------------------------

-- | Describes whether a field in a container is fixed or variable size.
data FieldKind
  = FixedField !Word32  -- ^ field size in bytes
  | VariableField       -- ^ variable-size (4-byte offset in fixed region)

-- | Decode a container: given the field layout, split input into per-field
-- ByteStrings that can then be decoded individually.
decodeContainer :: [FieldKind] -> ByteString -> Either SszError [ByteString]
decodeContainer fields bs = do
  let fixedRegionSize = sum
        [ case f of FixedField n -> n; VariableField -> 4
        | f <- fields
        ]
  if fromIntegral fixedRegionSize > BS.length bs
    then Left (InvalidLength (fromIntegral fixedRegionSize) (BS.length bs))
    else extractFields fields 0 (fromIntegral fixedRegionSize) bs

extractFields :: [FieldKind] -> Int -> Int -> ByteString -> Either SszError [ByteString]
extractFields [] _ _ _ = Right []
extractFields (FixedField n : rest) cursor totalFixed input = do
  let slice = BS.take (fromIntegral n) (BS.drop cursor input)
  remaining <- extractFields rest (cursor + fromIntegral n) totalFixed input
  Right (slice : remaining)
extractFields (VariableField : rest) cursor totalFixed input = do
  -- Read the 4-byte offset at cursor
  offset <- sszDecode @Word32 (BS.take 4 (BS.drop cursor input))
  -- End of this variable field = next variable field's offset or end of input
  varEnd <- findVarEnd rest (cursor + 4) totalFixed input
  let start = fromIntegral offset
      slice = BS.take (varEnd - start) (BS.drop start input)
  remaining <- extractFields rest (cursor + 4) totalFixed input
  Right (slice : remaining)

-- | Find where the current variable field ends: either the offset of the
-- next variable field, or the end of the input.
findVarEnd :: [FieldKind] -> Int -> Int -> ByteString -> Either SszError Int
findVarEnd [] _ _ input = Right (BS.length input)
findVarEnd (FixedField n : rest) cursor totalFixed input =
  findVarEnd rest (cursor + fromIntegral n) totalFixed input
findVarEnd (VariableField : _) cursor _ input = do
  offset <- sszDecode @Word32 (BS.take 4 (BS.drop cursor input))
  Right (fromIntegral offset)
