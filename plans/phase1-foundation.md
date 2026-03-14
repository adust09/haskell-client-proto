# Phase 1: Foundation — SSZ Serialization + Consensus Types

## Goal

Implement the SSZ (Simple Serialize) wire format from scratch and define all consensus types needed for pq-devnet-3. By end of phase, `cabal build` and `cabal test` pass with full roundtrip property tests and correct `hashTreeRoot` output for all types.

## Prerequisites

- GHC 9.6.7 + Cabal 3.14.2 installed (via ghcup)
- No external dependencies beyond Hackage packages
- Reference: [SSZ spec](https://github.com/ethereum/consensus-specs/blob/dev/ssz/simple-serialize.md), ssz-hs design patterns

---

## Step 1: Project Scaffold

### Files to create

- `lean-consensus.cabal` — full cabal config with library, executable, test-suite
- `cabal.project` — single-package project file
- `app/Main.hs` — minimal `main = putStrLn "lean-consensus"`
- `src/SSZ.hs` — re-export module (empty initially)
- `test/Main.hs` — tasty test runner entry point

### Key decisions

- Default language: `GHC2021`
- Default extensions: `DataKinds`, `TypeFamilies`, `GADTs`, `DerivingStrategies`, `OverloadedStrings`, `ScopedTypeVariables`, `TypeApplications`
- GHC options: `-Wall -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints`

### Dependencies (Phase 1)

```
base >= 4.18, bytestring >= 0.11, crypton >= 0.34, memory >= 0.18,
vector >= 0.13, containers >= 0.6, text >= 2.0
```

Test deps: `tasty`, `tasty-hunit`, `tasty-quickcheck`, `QuickCheck`

### Tests (TDD)

- `cabal build` compiles with zero warnings
- `cabal test` runs and reports 0 tests (no test cases yet)

### Completion criteria

- Directory structure matches scaffold spec
- `cabal build` succeeds
- `cabal test` runs (empty test suite)

---

## Step 2: SSZ Common — Typeclasses and Primitives

### Files to create

- `src/SSZ/Common.hs`

### Types and signatures

```haskell
-- Metadata typeclass
class Ssz a where
    sszIsFixedSize :: proxy a -> Bool
    sszFixedSize   :: proxy a -> Word32

-- Encoding typeclass
class Ssz a => SszEncode a where
    sszEncode :: a -> ByteString

-- Decoding typeclass
class Ssz a => SszDecode a where
    sszDecode :: ByteString -> Either SszError a

-- Error type
data SszError
    = InvalidLength { expected :: Int, actual :: Int }
    | InvalidOffset Word32
    | ExtraBytes Int
    | InvalidBool Word8
    | InvalidSentinel
    | CustomError String
    deriving (Show, Eq)

-- Fixed-length byte array with phantom type-level length
newtype BytesN (n :: Nat) = BytesN { unBytesN :: ByteString }
    deriving (Eq, Ord, Show)

-- Common aliases
type Bytes4  = BytesN 4
type Bytes20 = BytesN 20
type Bytes32 = BytesN 32
type Bytes48 = BytesN 48
type Bytes96 = BytesN 96

-- Smart constructor enforcing length
mkBytesN :: forall n. KnownNat n => ByteString -> Either SszError (BytesN n)

-- Opaque identifier wrappers (no arithmetic, ByteString-backed)
newtype Word128 = Word128 { unWord128 :: ByteString }  -- 16 bytes
newtype Word256 = Word256 { unWord256 :: ByteString }  -- 32 bytes
```

### Algorithm description

- `BytesN n` wraps a `ByteString` with a phantom `Nat` parameter. The smart constructor `mkBytesN` checks `BS.length bs == natVal @n` and returns `Left (InvalidLength ...)` on mismatch.
- `Word128`/`Word256` are opaque identifiers — no arithmetic operations, just SSZ encode/decode as 16/32 little-endian bytes. Construct with length-checked smart constructors.
- `Ssz` instances for primitive types: `Word8` (1, fixed), `Word16` (2, fixed), `Word32` (4, fixed), `Word64` (8, fixed), `Bool` (1, fixed), `BytesN n` (n, fixed), `Word128` (16, fixed), `Word256` (32, fixed).

### Tests (TDD)

- `mkBytesN` accepts correct-length `ByteString`, rejects wrong length
- `Ssz` metadata: `sszIsFixedSize @Word64 == True`, `sszFixedSize @Word64 == 8`
- `Ssz` metadata for `BytesN 32`: fixed size = 32

### Completion criteria

- All primitive `Ssz` instances compile
- Smart constructors enforce lengths
- Metadata queries return correct values

---

## Step 3: SSZ Encode

### Files to create

- `src/SSZ/Encode.hs`
- `test/Test/SSZ/Encode.hs`

### Types and signatures

```haskell
-- Two-pass encoder state
data SszEncoder = SszEncoder
    { encFixedParts    :: Builder  -- fixed parts + offset placeholders
    , encVariableParts :: Builder  -- variable parts appended in order
    , encFixedLen      :: Word32   -- running count of fixed region size
    }

-- Internal helpers
emptyEncoder :: SszEncoder
addFixedField :: SszEncode a => a -> SszEncoder -> SszEncoder
addVariableField :: SszEncode a => a -> SszEncoder -> SszEncoder
finalize :: SszEncoder -> ByteString
```

### Algorithm description

**Fixed-size encoding**: Direct little-endian serialization.
- `Word8`: identity byte
- `Word16`/`Word32`/`Word64`: `Data.ByteString.Builder.word16LE` etc.
- `Bool`: `0x00` for False, `0x01` for True
- `BytesN n`: raw bytes (identity)
- `Word128`/`Word256`: raw bytes (already stored little-endian)

**Two-pass container encoding**:
1. Iterate fields. For fixed-size fields, append encoded bytes to `encFixedParts`. For variable-size fields, append a 4-byte offset placeholder to `encFixedParts` and the encoded bytes to `encVariableParts`.
2. After all fields: compute actual offsets (each offset = total fixed region size + cumulative variable data before this field).
3. Patch offset placeholders with actual values.
4. Concatenate: `fixedRegion ++ variableRegion`.

**List encoding (fixed-size elements)**: Concatenate `sszEncode` of each element. Length inferred from `totalBytes / elementSize` on decode.

**List encoding (variable-size elements)**: Write `N` 4-byte offsets first (pointing past offset table into variable region), then concatenate serialized elements.

### Tests (TDD)

```
sszEncode (42 :: Word64) == "\x2a\x00\x00\x00\x00\x00\x00\x00"
sszEncode True == "\x01"
sszEncode False == "\x00"
sszEncode (BytesN @32 (BS.replicate 32 0xff)) == BS.replicate 32 0xff
```

- Encode known `Checkpoint` value and check exact byte output
- Encode list of `Word64` values and verify concatenation

### Completion criteria

- All primitive `SszEncode` instances produce correct little-endian bytes
- Two-pass encoder handles containers with mixed fixed/variable fields
- List encoding works for both fixed and variable-size element types

---

## Step 4: SSZ Decode

### Files to create

- `src/SSZ/Decode.hs`
- `test/Test/SSZ/Decode.hs`
- `test/Test/SSZ/Roundtrip.hs`

### Types and signatures

```haskell
-- SszDecode instances for all basic types
-- Container decoding via field-by-field cursor

-- Internal decoder state
data Cursor = Cursor
    { curBytes  :: ByteString
    , curOffset :: Int
    }

-- Helpers
takeFixed :: Int -> Cursor -> Either SszError (ByteString, Cursor)
readOffset :: Cursor -> Either SszError (Word32, Cursor)
sliceVariable :: [Word32] -> ByteString -> Either SszError [ByteString]
```

### Algorithm description

**Fixed-size decoding**: Read exactly `sszFixedSize` bytes, interpret as little-endian.
- `Word8`: single byte
- `Word16`/`Word32`/`Word64`: little-endian conversion
- `Bool`: `0x00 → False`, `0x01 → True`, anything else → `InvalidBool`
- `BytesN n`: take `n` bytes, verify length

**Container decoding**:
1. Compute fixed region size: `sum(sszFixedSize f | if fixed(f), else 4 for f in fields)`.
2. Read fixed region. Extract inline fixed fields and 4-byte offsets for variable fields.
3. Use offsets to slice the variable region: offset[i] to offset[i+1] (last offset to end-of-input).
4. Decode each variable-size slice.

**List decoding (fixed-size elements)**:
- `len(bytes) % elementSize == 0` or error
- Split into chunks of `elementSize`, decode each

**List decoding (variable-size elements)**:
- First 4 bytes = first offset → `numElements = firstOffset / 4`
- Read `numElements` offsets, append `len(bytes)` as sentinel
- Slice by offset pairs, decode each

### Tests (TDD)

- Decode known byte sequences to expected values (inverse of Encode tests)
- **Roundtrip property tests** for all types:
  ```haskell
  prop_roundtrip :: (SszEncode a, SszDecode a, Eq a, Arbitrary a) => a -> Bool
  prop_roundtrip x = sszDecode (sszEncode x) == Right x
  ```
- `Arbitrary` instances for `Word8`, `Word16`, `Word32`, `Word64`, `Bool`, `BytesN n`
- Error cases: truncated input, extra bytes, invalid bool byte, bad offsets

### Completion criteria

- All primitive `SszDecode` instances pass
- Roundtrip property: `sszDecode . sszEncode == Right` for all basic types
- Error paths return appropriate `SszError` variants

---

## Step 5: SSZ Composite Types — Vector, Bitvector, Bitlist

### Files to create

- `src/SSZ/Vector.hs`
- `src/SSZ/Bitvector.hs`
- `src/SSZ/Bitlist.hs`
- `test/Test/SSZ/Vector.hs`
- `test/Test/SSZ/Bitvector.hs`
- `test/Test/SSZ/Bitlist.hs`

### Types and signatures

```haskell
-- Fixed-length homogeneous collection
newtype SszVector (n :: Nat) a = SszVector { unSszVector :: Data.Vector.Vector a }
-- Fixed-size iff element type is fixed-size
-- Length is compile-time constant (not serialized)

-- Fixed-length bit array
newtype Bitvector (n :: Nat) = Bitvector { unBitvector :: ByteString }
-- Serialized size: ceil(n / 8) bytes
-- Fixed-size type

-- Variable-length bit array with max capacity
newtype Bitlist (n :: Nat) = Bitlist { unBitlist :: ByteString }
-- Serialized: bits + sentinel bit
-- Variable-size type

-- SszList: variable-length list with max capacity
newtype SszList (n :: Nat) a = SszList { unSszList :: [a] }
-- Variable-size type
-- Max length n determines Merkleization tree depth
```

### Algorithm description

**SszVector encoding/decoding**:
- Fixed-size elements: concatenate encodings. Decode by splitting into `n` chunks of `elementSize`.
- Variable-size elements: offset table + variable parts (same as container pattern). Decode via offsets.
- `Ssz` instance: `sszIsFixedSize = sszIsFixedSize @a`, `sszFixedSize = n * sszFixedSize @a`.

**Bitvector encoding**:
- Pack `n` bits into bytes, LSB-first within each byte.
- Unused high bits in last byte are zero-padded.
- Size always `ceil(n / 8)` bytes (fixed).

**Bitvector decoding**:
- Read `ceil(n / 8)` bytes.
- Verify unused bits in last byte are zero.

**Bitlist encoding**:
- Pack bits into bytes, LSB-first.
- Append sentinel `1` bit immediately after last data bit.
- Size: `ceil((numBits + 1) / 8)` bytes (variable).

**Bitlist decoding**:
- Find highest set bit in last byte — that's the sentinel.
- Everything below the sentinel is data.
- Verify `numBits <= maxCapacity` (from type-level `n`).

**SszList encoding/decoding**:
- Same as raw list encoding, but track max length for Merkleization.
- Enforce `length <= maxCapacity` on decode.

### Tests (TDD)

- `SszVector 4 Word64`: encode 4 values, verify exact bytes, roundtrip
- `Bitvector 16`: encode/decode 16 bits, check byte layout, roundtrip
- `Bitvector 10`: verify unused 6 bits in byte 2 are zero
- `Bitlist 64`: encode varying lengths (0, 1, 63, 64 bits), check sentinel detection
- `Bitlist` edge case: empty bitlist (just sentinel bit)
- `SszList 100 Word64`: roundtrip with 0, 50, 100 elements
- Reject `SszList 10 Word64` with 11 elements

### Completion criteria

- All composite types encode/decode correctly
- Roundtrip property tests pass for all composite types with `Arbitrary` instances
- Bit-level operations (pack, sentinel) are correct at boundaries
- Max-length enforcement works for `SszList` and `Bitlist`

---

## Step 6: SSZ Merkleization

### Files to create

- `src/SSZ/Merkleization.hs`
- `test/Test/SSZ/Merkleization.hs`

### Types and signatures

```haskell
-- Constants
bytesPerChunk :: Int  -- 32

-- SHA-256 via crypton
sha256 :: ByteString -> ByteString

-- Core Merkleization functions
pack :: [ByteString] -> [Bytes32]
-- Zero-pad concatenated input to multiple of 32 bytes, split into chunks

packBits :: [Bool] -> [Bytes32]
-- Pack bits into 32-byte chunks for Bitvector/Bitlist

merkleize :: [Bytes32] -> Word64 -> Bytes32
-- Pad chunks to next_power_of_two(limit) with zero-chunks, hash pairwise to root

mixInLength :: Bytes32 -> Word64 -> Bytes32
-- SHA-256(root ++ encode_u64_le(length))

-- Typeclass for hash_tree_root
class Ssz a => SszHashTreeRoot a where
    hashTreeRoot :: a -> Bytes32

-- Pre-computed zero hashes (optimization)
zeroHashes :: Vector Bytes32  -- zeroHashes[i] = merkle root of 2^i zero chunks
```

### Algorithm description

**`pack`**: Concatenate all input `ByteString`s, zero-pad to next multiple of 32 bytes, split into 32-byte `Bytes32` chunks. If input is empty, return one zero chunk.

**`merkleize`**: Given chunks and a limit:
1. Compute `depth = ceil(log2(max(limit, 1)))`.
2. Pad chunk list to `2^depth` with zero chunks.
3. Hash pairwise: `hash(chunk[2i] ++ chunk[2i+1])` until single root.
4. Optimization: use pre-computed `zeroHashes` table to avoid hashing zero pairs.

**`mixInLength`**: `SHA-256(root ++ littleEndian64(length))` — used for variable-length types (List, Bitlist) to commit to the actual length.

**`hashTreeRoot` by type**:
| Type | Computation |
|------|-------------|
| `uintN` | `merkleize(pack([encode(value)]), limit=1)` |
| `Bool` | `merkleize(pack([encode(value)]), limit=1)` |
| `Bytes32` | identity (already 32 bytes, one chunk) |
| `BytesN n` | `merkleize(pack([rawBytes]), limit=ceil(n/32))` |
| `Bitvector n` | `merkleize(packBits(bits), limit=ceil(n/256))` |
| `Bitlist n` | `mixInLength(merkleize(packBits(bits), limit=ceil(n/256)), len)` |
| `SszVector n a` (fixed) | `merkleize(pack(map encode elems), limit=n*ceil(sszFixedSize/32))` |
| `SszVector n a` (composite) | `merkleize(map hashTreeRoot elems, limit=n)` |
| `SszList n a` (fixed) | `mixInLength(merkleize(pack(map encode elems), limit=chunkCount(n)), len)` |
| `SszList n a` (composite) | `mixInLength(merkleize(map hashTreeRoot elems, limit=n), len)` |
| Container | `merkleize(map hashTreeRoot fields)` |

### Tests (TDD)

- `sha256` of known input matches expected digest
- `pack` of empty input → one zero chunk
- `pack` of 48 bytes → two chunks (32 + 16 zero-padded)
- `merkleize` of single chunk with limit=1 → identity
- `merkleize` of two chunks → `SHA-256(chunk0 ++ chunk1)`
- `hashTreeRoot` of `Word64 0` matches SSZ spec test vector
- `hashTreeRoot` of `Bytes32` is identity
- Cross-validate with leanSpec test vectors (when available)
- `mixInLength` spot-checks

### Completion criteria

- `hashTreeRoot` produces correct output for all basic and composite types
- Pre-computed zero hashes optimization works
- Merkleization validated against known test vectors

---

## Step 7: SSZ Auto-Derivation via GHC.Generics

### Files to create

- `src/SSZ/Derive.hs`

### Types and signatures

```haskell
-- Generic metadata
class GSsz (f :: Type -> Type) where
    gsszIsFixedSize :: proxy f -> Bool
    gsszFixedSize   :: proxy f -> Word32

-- Generic encoding
class GSsz f => GEncode (f :: Type -> Type) where
    gSszEncode :: f p -> Builder
    gEncodeFixedParts :: f p -> (Builder, [Builder])
    -- Returns (fixed parts with offset placeholders, list of variable parts)

-- Generic decoding
class GSsz f => GDecode (f :: Type -> Type) where
    gSszDecode :: ByteString -> Int -> Either SszError (f p, Int)
    -- Cursor-based decoding returning consumed offset

-- Instances for GHC.Generics constructors:
-- M1 (metadata wrapper)    — delegate to inner
-- (:*:) (product)          — combine left and right
-- K1 (leaf field)          — delegate to SszEncode/SszDecode
-- U1 (unit / no fields)    — empty encoding
```

### Algorithm description

**Container encoding via Generics**:
1. Walk the `Rep a` structure (which is a product of `K1` leaves).
2. For each field: if `sszIsFixedSize`, emit encoded bytes to fixed region. If variable, emit 4-byte offset placeholder to fixed region and encoded bytes to variable region.
3. After walking all fields, compute offsets: `fixedRegionSize + cumulativeVariableOffset` for each variable field.
4. Replace placeholders with computed offsets.
5. Concatenate fixed + variable regions.

**Container decoding via Generics**:
1. Compute total fixed region size by summing `sszFixedSize` (or 4 for variable) per field.
2. Split input into fixed region and variable region.
3. Walk fields: fixed fields read inline from fixed region cursor; variable fields read a 4-byte offset, then slice the variable region using offset pairs.
4. Decode each field and reconstruct the product type.

**hashTreeRoot for Containers via Generics**:
- Walk fields, compute `hashTreeRoot` for each field, collect into list of `Bytes32`.
- `merkleize(fieldRoots)`.

### Tests (TDD)

- Derive `Ssz`, `SszEncode`, `SszDecode` for `Checkpoint` → roundtrip test
- Derive for `AttestationData` (nested container) → roundtrip test
- Derive for a container with mixed fixed/variable fields → roundtrip test
- `hashTreeRoot` for `Checkpoint` via derived instance matches manual computation

### Completion criteria

- Empty instances `instance Ssz T` / `instance SszEncode T` / `instance SszDecode T` work for any `Generic`-derived record type
- Handles nested containers (container within container)
- Handles containers with variable-size fields (lists, bitlists)

---

## Step 8: Consensus Types

### Files to create

- `src/Consensus/Constants.hs`
- `src/Consensus/Types.hs`
- `test/Test/Consensus/Types.hs`
- `test/Test/Consensus/Constants.hs`

### Types and signatures

```haskell
-- Consensus.Constants
module Consensus.Constants where

-- Primitive aliases
type Slot           = Word64
type Epoch          = Word64
type ValidatorIndex = Word64
type CommitteeIndex = Word64
type SubnetId       = Word64
type Gwei           = Word64
type Root           = Bytes32
type Domain         = Bytes32
type Version        = Bytes4

-- Timing
slotDuration :: Int           -- 4_000_000 microseconds
networkDelayBound :: Int      -- 800_000 microseconds

-- Finality
slotsToFinality :: Word64     -- 3
finalityThreshold :: Rational -- 2 % 3

-- Type-level constants for SSZ collections
type MAX_VALIDATORS_PER_SUBNET = 256
type MAX_ATTESTATIONS          = 128
type MAX_ATTESTATIONS_STATE    = 4096
type SLOTS_PER_HISTORICAL_ROOT = 64
type VALIDATOR_REGISTRY_LIMIT  = 1024

-- Networking constants
gossipsubMeshSize :: Int      -- 8
gossipsubHeartbeatMs :: Int   -- 700
```

```haskell
-- Consensus.Types
module Consensus.Types where

-- Crypto primitives (opaque, sized ByteString wrappers)
newtype XmssSignature = XmssSignature { unXmssSignature :: ByteString }
-- SSZ: fixed-size, 3112 bytes
xmssSignatureSize :: Int  -- 3112

newtype XmssPubkey = XmssPubkey { unXmssPubkey :: ByteString }
-- SSZ: fixed-size (TBD: 32 or 64 bytes, depends on leanSig params)

newtype LeanMultisigProof = LeanMultisigProof { unLeanMultisigProof :: ByteString }
-- SSZ: variable-size (length-delimited, ~146-380 KiB)

-- Core consensus types (all derive Generic + SSZ instances)
data Checkpoint = Checkpoint
    { cpSlot :: Slot, cpRoot :: Root }
-- SSZ: fixed, 40 bytes

data AttestationData = AttestationData
    { adSlot :: Slot, adHeadRoot :: Root
    , adSourceCheckpoint :: Checkpoint, adTargetCheckpoint :: Checkpoint }
-- SSZ: fixed, 120 bytes

data SignedAttestation = SignedAttestation
    { saData :: AttestationData, saValidatorIndex :: ValidatorIndex
    , saSignature :: XmssSignature }
-- SSZ: fixed (120 + 8 + 3112 = 3240 bytes)

data SignedAggregatedAttestation = SignedAggregatedAttestation
    { saaData :: AttestationData
    , saaAggregationBits :: Bitlist MAX_VALIDATORS_PER_SUBNET
    , saaAggregationProof :: LeanMultisigProof }
-- SSZ: variable (due to Bitlist and proof)

data BeaconBlockBody = BeaconBlockBody
    { bbbAttestations :: SszList MAX_ATTESTATIONS SignedAggregatedAttestation }
-- SSZ: variable

data BeaconBlock = BeaconBlock
    { bbSlot :: Slot, bbProposerIndex :: ValidatorIndex
    , bbParentRoot :: Root, bbStateRoot :: Root, bbBody :: BeaconBlockBody }
-- SSZ: variable

data SignedBeaconBlock = SignedBeaconBlock
    { sbbBlock :: BeaconBlock, sbbSignature :: XmssSignature }
-- SSZ: variable

data BeaconBlockHeader = BeaconBlockHeader
    { bbhSlot :: Slot, bbhProposerIndex :: ValidatorIndex
    , bbhParentRoot :: Root, bbhStateRoot :: Root, bbhBodyRoot :: Root }
-- SSZ: fixed, 112 bytes

data Validator = Validator
    { vPubkey :: XmssPubkey, vEffectiveBalance :: Gwei, vSlashed :: Bool
    , vActivationSlot :: Slot, vExitSlot :: Slot, vWithdrawableSlot :: Slot }
-- SSZ: fixed

data BeaconState = BeaconState
    { bsSlot :: Slot, bsLatestBlockHeader :: BeaconBlockHeader
    , bsBlockRoots :: SszVector SLOTS_PER_HISTORICAL_ROOT Root
    , bsStateRoots :: SszVector SLOTS_PER_HISTORICAL_ROOT Root
    , bsValidators :: SszList VALIDATOR_REGISTRY_LIMIT Validator
    , bsBalances :: SszList VALIDATOR_REGISTRY_LIMIT Gwei
    , bsJustifiedCheckpoint :: Checkpoint, bsFinalizedCheckpoint :: Checkpoint
    , bsCurrentAttestations :: SszList MAX_ATTESTATIONS_STATE SignedAggregatedAttestation }
-- SSZ: variable

-- Fork choice types (internal, not SSZ-serialized)
data Store = Store
    { stJustifiedCheckpoint :: Checkpoint, stFinalizedCheckpoint :: Checkpoint
    , stBlocks :: Map Root BeaconBlock, stBlockStates :: Map Root BeaconState
    , stLatestMessages :: Map ValidatorIndex LatestMessage, stCurrentSlot :: Slot }

data LatestMessage = LatestMessage { lmSlot :: Slot, lmRoot :: Root }
```

### Build order

Follow the type dependency graph bottom-up:
1. `BytesN` → `Bytes32` (Root)
2. `XmssSignature`, `XmssPubkey`, `LeanMultisigProof`
3. `Checkpoint`
4. `AttestationData`
5. `SignedAttestation`
6. `SignedAggregatedAttestation`
7. `BeaconBlockBody` → `BeaconBlock` → `SignedBeaconBlock`
8. `BeaconBlockHeader`
9. `Validator`
10. `BeaconState`
11. `Store`, `LatestMessage` (non-SSZ)

### Tests (TDD)

- SSZ roundtrip property for each type (needs `Arbitrary` instances)
- `hashTreeRoot` for `Checkpoint`, `BeaconBlockHeader` against manually computed values
- `Ssz` metadata: `sszIsFixedSize @Checkpoint == True`, `sszFixedSize @Checkpoint == 40`
- `sszIsFixedSize @BeaconState == False` (due to lists)
- Constants: verify `slotDuration == 4_000_000`, `slotsToFinality == 3`

### Completion criteria

- All consensus types compile with derived SSZ instances
- Roundtrip tests pass for every type
- `hashTreeRoot` correct for at least `Checkpoint`, `BeaconBlockHeader`, `AttestationData`
- Constants module compiles and exports correct values

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| GHC.Generics derivation complexity for nested variable-size containers | Blocks Step 7, delays Step 8 | Start with manual instances for complex types; add Generics derivation incrementally |
| XMSS signature/pubkey sizes may change when leanSig library is obtained | Rework `XmssSignature`/`XmssPubkey` definitions | Use `ByteString` wrappers with size constants that are easy to update |
| leanSpec test vectors not yet available | Cannot cross-validate Merkleization | Use SSZ consensus-spec-tests for standard types; defer Lean-specific validation |
| Type-level natural programming (`KnownNat` constraints) may create ergonomic issues | Verbose type signatures, confusing errors | Provide helper functions and type aliases to hide complexity |
| `Bitlist` sentinel bit edge cases at byte boundaries | Incorrect encode/decode | Extensive property tests with boundary values (n=0, n=7, n=8, n=255) |

---

## Exit Criteria

- [ ] `cabal build` succeeds with zero warnings
- [ ] `cabal test` passes all tests
- [ ] SSZ encode/decode roundtrip property tests for: `Word8`, `Word16`, `Word32`, `Word64`, `Bool`, `BytesN n`, `Word128`, `Word256`, `SszVector`, `SszList`, `Bitvector`, `Bitlist`
- [ ] SSZ encode/decode roundtrip for all consensus types: `Checkpoint`, `AttestationData`, `SignedAttestation`, `SignedAggregatedAttestation`, `BeaconBlock`, `SignedBeaconBlock`, `BeaconBlockHeader`, `Validator`, `BeaconState`
- [ ] GHC.Generics auto-derivation works for record types with mixed fixed/variable fields
- [ ] `hashTreeRoot` produces correct output for basic types and at least 3 consensus types
- [ ] Pre-computed zero hashes table works
- [ ] All error paths tested (truncated input, extra bytes, invalid booleans, bad offsets, exceeded max lengths)
