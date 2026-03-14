# SSZ Implementation Guide

## Overview

Simple Serialize (SSZ) is the wire format for all Lean Consensus messages. This document specifies exactly what to implement, based on the [SSZ spec](https://github.com/ethereum/consensus-specs/blob/dev/ssz/simple-serialize.md) and the existing [ssz-hs](https://github.com/michaelsproul/ssz-hs) library by Michael Sproul.

## Decision: Fork ssz-hs vs. Write from Scratch

**Recommendation: Write from scratch**, referencing ssz-hs for design patterns.

Rationale:
- ssz-hs covers ~20-25% of what we need
- Missing features (Bitlist, Bitvector, Vector, Merkleization, Bytes32) are substantial
- The library is 9 commits, described as "mostly for fun"
- Writing from scratch with our exact needs is cleaner than adapting incomplete code
- We can reuse the same architectural patterns (two-pass encoder, GHC.Generics derivation)

### Patterns to borrow from ssz-hs

1. **Two-pass encoding** (`SszEncoder`): Fixed parts + offset table first, then variable parts
2. **GHC.Generics derivation**: `GSsz`, `GEncode`, `GDecode` typeclasses walking generic `Rep`
3. **Typeclass design**: `Ssz` (metadata), `Encode` (serialization), `Decode` (deserialization) as separate classes

---

## Type System

### Basic Types

| SSZ Type | Haskell Type | Size | Encoding |
|----------|-------------|------|----------|
| `uint8` | `Word8` | 1 byte | identity |
| `uint16` | `Word16` | 2 bytes | little-endian |
| `uint32` | `Word32` | 4 bytes | little-endian |
| `uint64` | `Word64` | 8 bytes | little-endian |
| `uint128` | `Word128` (custom) | 16 bytes | little-endian |
| `uint256` | `Word256` (custom) | 32 bytes | little-endian |
| `boolean` | `Bool` | 1 byte | 0x00 or 0x01 |

**Note on uint128/uint256**: Use `ByteString` wrappers with smart constructors. No arithmetic needed — these are used only as opaque identifiers (roots, hashes).

### Fixed-Size Byte Types

```haskell
-- Phantom-typed wrapper for fixed-length byte arrays
newtype BytesN (n :: Nat) = BytesN { unBytesN :: ByteString }

type Bytes4  = BytesN 4
type Bytes20 = BytesN 20
type Bytes32 = BytesN 32   -- Most common: block roots, state roots
type Bytes48 = BytesN 48   -- Public keys (not used in XMSS, but spec-compatible)
type Bytes96 = BytesN 96   -- BLS signatures (not used, but spec-compatible)
```

`Bytes32` is critical — used for all roots and hashes in the consensus types.

### Composite Types

#### Container (struct)

```haskell
-- Containers map to Haskell records
-- Fixed-size fields are serialized inline
-- Variable-size fields get offset pointers (4 bytes each)
data Checkpoint = Checkpoint
    { cpSlot :: Word64
    , cpRoot :: Bytes32
    }
```

Encoding algorithm (two-pass):
1. **Pass 1**: Write fixed parts + 4-byte offsets for variable parts
2. **Pass 2**: Append variable parts in field order

#### List (variable-length, max capacity N)

```haskell
-- Type-level max length for Merkleization tree depth
newtype SszList (n :: Nat) a = SszList { unSszList :: [a] }
```

- Variable-size: always uses offset encoding
- Max length `N` determines Merkleization tree depth: `next_power_of_two(N)`
- Encoding: just concatenate elements (fixed-size) or offset-table + elements (variable-size)

#### Vector (fixed-length N)

```haskell
-- Fixed-length homogeneous collection
newtype SszVector (n :: Nat) a = SszVector { unSszVector :: Data.Vector.Vector a }
```

- Fixed-size if element type is fixed-size
- Variable-size if element type is variable-size
- Length is compile-time constant, not serialized

#### Bitvector (fixed-length N bits)

```haskell
-- Fixed-length bit array, N bits
newtype Bitvector (n :: Nat) = Bitvector { unBitvector :: ByteString }
-- Serialized size: ceil(N / 8) bytes
-- Fixed-size type
```

Encoding:
- Pack bits into bytes, LSB first
- Unused high bits in last byte are zero
- Size: `ceil(N / 8)` bytes, known at compile time

#### Bitlist (variable-length, max N bits)

```haskell
-- Variable-length bit array, max N bits
newtype Bitlist (n :: Nat) = Bitlist { unBitlist :: ByteString }
-- Serialized: bits + sentinel bit (1-bit) to mark length
-- Variable-size type
```

Encoding:
- Pack bits into bytes, LSB first
- Append a sentinel `1` bit after the last data bit
- Size: `ceil((len + 1) / 8)` bytes
- Decoding: find the highest set bit in last byte — that's the sentinel; everything below is data

---

## Typeclasses

```haskell
-- Metadata: is this type fixed or variable size?
class Ssz a where
    -- True if the type has a fixed serialized size
    sszIsFixedSize :: proxy a -> Bool
    -- The fixed size in bytes (only valid when sszIsFixedSize is True)
    sszFixedSize :: proxy a -> Word32

-- Encoding
class Ssz a => SszEncode a where
    sszEncode :: a -> ByteString

-- Decoding
class Ssz a => SszDecode a where
    sszDecode :: ByteString -> Either SszError a

-- Errors
data SszError
    = InvalidLength { expected :: Int, actual :: Int }
    | InvalidOffset Word32
    | ExtraBytes Int
    | InvalidBool Word8
    | InvalidSentinel
    | CustomError String
    deriving (Show, Eq)
```

---

## Encoding Algorithm

### Fixed-size types

Direct little-endian serialization. No length prefix.

### Variable-size Container

```
serialize(container) =
    fixed_parts = []
    variable_parts = []
    for field in container.fields:
        if is_fixed_size(field):
            fixed_parts.append(serialize(field))
        else:
            fixed_parts.append(offset_bytes)  -- 4 bytes, little-endian
            variable_parts.append(serialize(field))

    -- offsets point from start of fixed region
    offset = sum(len(p) for p in fixed_parts)
    -- fix up offset values
    return concat(fixed_parts) ++ concat(variable_parts)
```

### Variable-size List (fixed-size elements)

```
serialize(list) = concat(serialize(elem) for elem in list)
-- Length inferred from total bytes / element size
```

### Variable-size List (variable-size elements)

```
serialize(list) =
    offsets = [4 * len(list) + sum(len(serialize(e)) for e in list[:i]) for i in range(len(list))]
    return concat(encode_u32(offset) for offset in offsets)
        ++ concat(serialize(elem) for elem in list)
```

---

## Decoding Algorithm

### Fixed-size types

Read exactly `sszFixedSize` bytes, interpret as little-endian.

### Container

1. Read fixed region: `sum(fixedSize(f) if fixed else 4 for f in fields)` bytes
2. Extract offsets for variable fields
3. Use offsets to slice the remaining bytes into variable parts
4. Decode each part

### List (fixed-size elements)

```
decode(bytes) =
    element_size = sszFixedSize @a
    assert(len(bytes) % element_size == 0)
    return [decode(bytes[i:i+element_size]) for i in range(0, len(bytes), element_size)]
```

### List (variable-size elements)

```
decode(bytes) =
    first_offset = decode_u32(bytes[0:4])
    num_elements = first_offset / 4
    offsets = [decode_u32(bytes[i*4:(i+1)*4]) for i in range(num_elements)]
    offsets.append(len(bytes))  -- sentinel
    return [decode(bytes[offsets[i]:offsets[i+1]]) for i in range(num_elements)]
```

---

## Merkleization

### Constants

```haskell
bytesPerChunk :: Int
bytesPerChunk = 32  -- SHA-256 output size

bytesPerLengthOffset :: Int
bytesPerLengthOffset = 4
```

### Core Functions

```haskell
-- Zero-pad a ByteString to a multiple of 32 bytes, then split into 32-byte chunks
pack :: [ByteString] -> [Bytes32]

-- Pack bits into 32-byte chunks (for Bitvector/Bitlist)
packBits :: [Bool] -> [Bytes32]

-- Given chunks, compute the Merkle root
-- Pad to next_power_of_two with zero-chunks, then hash pairwise up to root
merkleize :: [Bytes32] -> Word64 -> Bytes32
-- limit parameter determines the tree depth (next_power_of_two(limit))
-- For fixed-size types: limit = number of chunks
-- For variable-size types: limit = chunk_count(max_capacity)

-- Mix in the length for variable-size types
mixInLength :: Bytes32 -> Word64 -> Bytes32
-- SHA-256(root || encode_u64_le(length))

-- Mix in a type index for Union types
mixInType :: Bytes32 -> Word8 -> Bytes32
```

### hash_tree_root by Type

| Type | hash_tree_root |
|------|----------------|
| `uint8..uint256` | `merkleize(pack([encode(value)]))` |
| `Bool` | `merkleize(pack([encode(value)]))` |
| `Bytes32` | identity (already 32 bytes) |
| `BytesN n` | `merkleize(pack([value]))` |
| `Bitvector n` | `merkleize(packBits(bits), limit=ceil(n/256))` |
| `Bitlist n` | `mixInLength(merkleize(packBits(bits), limit=ceil(n/256)), len(bits))` |
| `SszVector n a` (fixed a) | `merkleize(pack(map encode elems), limit=n)` |
| `SszVector n a` (variable a) | `merkleize(map hashTreeRoot elems, limit=n)` |
| `SszList n a` (fixed a) | `mixInLength(merkleize(pack(map encode elems), limit=n), len(elems))` |
| `SszList n a` (variable a) | `mixInLength(merkleize(map hashTreeRoot elems, limit=n), len(elems))` |
| `Container` | `merkleize(map hashTreeRoot fields)` |

### SHA-256 via crypton

```haskell
import Crypto.Hash (hash, SHA256, Digest)
import qualified Data.ByteArray as BA

sha256 :: ByteString -> ByteString
sha256 bs = BA.convert (hash bs :: Digest SHA256)
```

---

## GHC.Generics Auto-Derivation

The goal is to let users write:

```haskell
data Checkpoint = Checkpoint
    { cpSlot :: Word64
    , cpRoot :: Bytes32
    } deriving (Generic)

instance Ssz Checkpoint
instance SszEncode Checkpoint
instance SszDecode Checkpoint
```

### Implementation Strategy

Use `GHC.Generics` to walk the product structure of a record type:

```haskell
class GSsz (f :: Type -> Type) where
    gsszIsFixedSize :: proxy f -> Bool
    gsszFixedSize :: proxy f -> Word32

class GSsz f => GEncode (f :: Type -> Type) where
    gSszEncode :: f p -> Builder

class GSsz f => GDecode (f :: Type -> Type) where
    gSszDecode :: ByteString -> Either SszError (f p)
```

Walk `M1`, `(:*:)`, `K1` constructors to handle metadata, products, and leaf fields.

Alternatively, use Template Haskell (as ssz-hs does) to generate instances with less runtime overhead.

---

## Test Strategy

### Unit Tests (tasty-hunit)

For each type:
1. Encode known value → check exact bytes
2. Decode known bytes → check exact value
3. Edge cases: empty lists, max-length values, zero values

### Roundtrip Tests (tasty-quickcheck)

```haskell
prop_roundtrip :: (SszEncode a, SszDecode a, Eq a) => a -> Bool
prop_roundtrip x = sszDecode (sszEncode x) == Right x
```

For every SSZ type with Arbitrary instances.

### Merkleization Tests

Compare `hashTreeRoot` output against known test vectors from leanSpec:

```bash
# Generate test vectors
cd leanSpec/
uv run fill --clean --fork=devnet --scheme=test
```

### Spec Compliance

- Validate against [consensus-spec-tests SSZ static tests](https://github.com/ethereum/consensus-spec-tests)
- Adapt for Lean Consensus-specific types once leanSpec vectors are available
