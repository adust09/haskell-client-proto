{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | GHC.Generics-based auto-derivation for SSZ Container types.
--
-- Usage:
--
-- @
-- data Checkpoint = Checkpoint { cpSlot :: Word64, cpRoot :: Bytes32 }
--   deriving stock (Generic, Eq, Show)
--
-- instance Ssz Checkpoint where
--   sszFixedSize = genericSszFixedSize \@(Rep Checkpoint)
--
-- instance SszEncode Checkpoint where
--   sszEncode = genericSszEncode
--
-- instance SszDecode Checkpoint where
--   sszDecode = genericSszDecode
--
-- instance SszHashTreeRoot Checkpoint where
--   hashTreeRoot = genericHashTreeRoot
-- @
module SSZ.Derive
  ( genericSszFixedSize
  , genericSszEncode
  , genericSszDecode
  , genericHashTreeRoot
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Word (Word32)
import GHC.Generics
import SSZ.Common
import SSZ.Container
import SSZ.Merkleization (SszHashTreeRoot (..), merkleize)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Compute the fixed size of a Generic container.
-- Returns 'Nothing' if any field is variable-size.
-- Each variable-size field contributes 4 bytes (offset) to the fixed region.
genericSszFixedSize :: forall f. GSsz f => Maybe Word32
genericSszFixedSize = gsszFixedSize @f

-- | Encode a Generic container using two-pass serialization.
genericSszEncode :: (Generic a, GEncode (Rep a)) => a -> ByteString
genericSszEncode a = finalizeEncoder (gAddFields (from a) emptyEncoder)

-- | Decode a Generic container from bytes.
genericSszDecode :: forall a. (Generic a, GDecode (Rep a)) => ByteString -> Either SszError a
genericSszDecode bs = do
  let kinds = gFieldKinds @(Rep a)
  slices <- decodeContainer kinds bs
  (rep, _) <- gFromSlices slices
  Right (to rep)

-- | Compute hashTreeRoot of a Generic container:
-- merkleize(map hashTreeRoot fields).
genericHashTreeRoot :: (Generic a, GHashTreeRoot (Rep a)) => a -> ByteString
genericHashTreeRoot a =
  let roots = gFieldRoots (from a)
      numFields = fromIntegral (length roots)
  in  merkleize roots numFields

-- ---------------------------------------------------------------------------
-- GSsz: Generic metadata
-- ---------------------------------------------------------------------------

class GSsz (f :: Type -> Type) where
  -- | Container fixed size: sum of field sizes (4 for variable-size fields).
  -- Returns Nothing if any field is variable-size.
  gsszFixedSize :: Maybe Word32
  -- | Total fixed region size (counts 4 bytes per variable field).
  gsszFixedRegionSize :: Word32

instance GSsz f => GSsz (M1 i c f) where
  gsszFixedSize = gsszFixedSize @f
  gsszFixedRegionSize = gsszFixedRegionSize @f

instance (GSsz f, GSsz g) => GSsz (f :*: g) where
  gsszFixedSize = case (gsszFixedSize @f, gsszFixedSize @g) of
    (Just a, Just b) -> Just (a + b)
    _                -> Nothing
  gsszFixedRegionSize = gsszFixedRegionSize @f + gsszFixedRegionSize @g

instance Ssz a => GSsz (K1 i a) where
  gsszFixedSize = sszFixedSize @a
  gsszFixedRegionSize = case sszFixedSize @a of
    Just n  -> n
    Nothing -> 4  -- offset placeholder

instance GSsz U1 where
  gsszFixedSize = Just 0
  gsszFixedRegionSize = 0

-- ---------------------------------------------------------------------------
-- GEncode: Generic encoding
-- ---------------------------------------------------------------------------

class GEncode (f :: Type -> Type) where
  gAddFields :: f p -> SszEncoder -> SszEncoder

instance GEncode f => GEncode (M1 i c f) where
  gAddFields (M1 x) = gAddFields x

instance (GEncode f, GEncode g) => GEncode (f :*: g) where
  gAddFields (l :*: r) = gAddFields r . gAddFields l

instance (SszEncode a, Ssz a) => GEncode (K1 i a) where
  gAddFields (K1 a) enc = case sszFixedSize @a of
    Just _  -> addFixedField a enc
    Nothing -> addVariableField a enc

instance GEncode U1 where
  gAddFields U1 = id

-- ---------------------------------------------------------------------------
-- GDecode: Generic decoding
-- ---------------------------------------------------------------------------

class GDecode (f :: Type -> Type) where
  gFieldKinds :: [FieldKind]
  gFromSlices :: [ByteString] -> Either SszError (f p, [ByteString])

instance GDecode f => GDecode (M1 i c f) where
  gFieldKinds = gFieldKinds @f
  gFromSlices slices = do
    (x, rest) <- gFromSlices @f slices
    Right (M1 x, rest)

instance (GDecode f, GDecode g) => GDecode (f :*: g) where
  gFieldKinds = gFieldKinds @f ++ gFieldKinds @g
  gFromSlices slices = do
    (l, rest1) <- gFromSlices @f slices
    (r, rest2) <- gFromSlices @g rest1
    Right (l :*: r, rest2)

instance (SszDecode a, Ssz a) => GDecode (K1 i a) where
  gFieldKinds = case sszFixedSize @a of
    Just n  -> [FixedField n]
    Nothing -> [VariableField]
  gFromSlices [] = Left (CustomError "not enough fields to decode")
  gFromSlices (s : rest) = do
    a <- sszDecode s
    Right (K1 a, rest)

instance GDecode U1 where
  gFieldKinds = []
  gFromSlices slices = Right (U1, slices)

-- ---------------------------------------------------------------------------
-- GHashTreeRoot: Generic hashTreeRoot for containers
-- ---------------------------------------------------------------------------

class GHashTreeRoot (f :: Type -> Type) where
  gFieldRoots :: f p -> [ByteString]

instance GHashTreeRoot f => GHashTreeRoot (M1 i c f) where
  gFieldRoots (M1 x) = gFieldRoots x

instance (GHashTreeRoot f, GHashTreeRoot g) => GHashTreeRoot (f :*: g) where
  gFieldRoots (l :*: r) = gFieldRoots l ++ gFieldRoots r

instance SszHashTreeRoot a => GHashTreeRoot (K1 i a) where
  gFieldRoots (K1 a) = [hashTreeRoot a]

instance GHashTreeRoot U1 where
  gFieldRoots U1 = []
