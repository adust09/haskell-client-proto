# Library & Technology Decisions

## Summary Table

| Decision | Choice | Alternatives Considered | Rationale |
|----------|--------|------------------------|-----------|
| SSZ library | New implementation (referencing ssz-hs patterns) | Fork ssz-hs | ssz-hs covers ~20%; cleaner to write from scratch with exact needs |
| Hashing | `crypton` | `cryptohash-sha256`, `hashable` | Active maintenance (fork of cryptonite), full SHA-256, ByteArray API |
| Networking | rust-libp2p via FFI | hs-libp2p, go-libp2p sidecar, pure Haskell | rust-libp2p has gossipsub v2.0 + QUIC; most battle-tested |
| Storage | `rocksdb-haskell` + STM TVar | `lmdb`, `sqlite-simple`, pure in-memory | KV model matches ethlambda; RocksDB proven in Ethereum clients |
| Concurrency | `async` + `forkIO` + STM | `typed-process`, `brick` actors | GHC lightweight threads natural fit for actor model |
| JSON/YAML | `aeson` + `yaml` | `tomland`, hand-rolled | Standard Haskell ecosystem choice |
| Testing | `tasty` + `tasty-hunit` + `tasty-quickcheck` | `hspec`, `hedgehog` | Composable, supports both unit and property tests |
| Metrics | `prometheus-client` | `ekg`, custom | leanMetrics standard uses Prometheus |
| Compression | `snappy` or `snappy-c` | `zlib`, `lz4` | libp2p message compression uses Snappy |
| Byte operations | `bytestring` | `byteslice`, `bytes` | Standard, mature, sufficient |

---

## Detailed Decisions

### 1. SSZ: New Implementation

**Decision**: Write SSZ from scratch in `src/SSZ/`, referencing ssz-hs design patterns.

**ssz-hs analysis** (https://github.com/michaelsproul/ssz-hs):

What it has (~20-25%):
- `Ssz`, `Encode`, `Decode` typeclasses
- `Word8`, `Word64` encoding/decoding (little-endian)
- Variable-length `[a]` lists (both fixed and variable element)
- Container auto-derivation via GHC.Generics + Template Haskell
- Two-pass offset-based encoding for mixed fixed/variable containers
- QuickCheck roundtrip tests

What it's missing (~75-80%):
- `Word16`, `Word32`, `Bool` instances
- `Word128`, `Word256` (uint128, uint256)
- `BytesN n` / `Bytes32` (fixed-length byte arrays)
- `SszVector n a` (fixed-length typed collection)
- `Bitvector n` (fixed-length bit array)
- `Bitlist n` (variable-length bit array with sentinel)
- Union types
- Max-length enforcement on lists
- **Entire Merkleization layer** (hash_tree_root, pack, merkleize, mixInLength)

**Key patterns to reuse**:
- Two-pass encoder architecture (fixed parts → offsets → variable parts)
- GHC.Generics walking for Container derivation
- Separate Ssz/Encode/Decode typeclasses

### 2. Hashing: crypton

**Decision**: Use `crypton` for SHA-256.

```haskell
-- crypton API
import Crypto.Hash (hash, SHA256, Digest)
import qualified Data.ByteArray as BA

sha256 :: ByteString -> ByteString
sha256 bs = BA.convert (hash bs :: Digest SHA256)
```

- `crypton` is the actively maintained fork of `cryptonite` (after the original was archived)
- Provides constant-time operations and hardware acceleration where available
- Also has Keccak-256 if needed later
- The `memory` package (ByteArray) is a required transitive dependency

### 3. Networking: rust-libp2p FFI

**Decision**: Use rust-libp2p exposed through a thin C ABI wrapper, consumed via Haskell FFI.

**Architecture**:
```
Haskell (Network.P2P)
    │
    │ foreign import ccall
    ▼
C ABI wrapper (lean_p2p.h / lean_p2p.so)
    │
    │ Rust FFI
    ▼
rust-libp2p (gossipsub, QUIC, discv5)
```

**Why not alternatives**:
- **hs-libp2p** (MatrixAI): Immature, no gossipsub v2.0, no QUIC
- **go-libp2p sidecar**: Adds Go runtime dependency; IPC latency in 4s slot time
- **Pure Haskell gossipsub**: Extremely high effort; no QUIC implementation

**Trade-offs**:
- (+) Battle-tested gossipsub + QUIC from rust-libp2p ecosystem
- (+) Same library used by Lighthouse, ethlambda
- (-) Rust toolchain required for builds
- (-) FFI boundary adds complexity for error handling and memory management

**Fallback**: If FFI proves too complex initially, use a sidecar process with stdin/stdout JSON-RPC.

### 4. Storage: rocksdb-haskell + STM

**Decision**: RocksDB for persistent state, STM TVar for in-memory hot cache.

```haskell
-- Persistent layer
import Database.RocksDB (DB, get, put)

-- Hot cache (fork choice store, current state)
import Control.Concurrent.STM (TVar, readTVar, writeTVar, atomically)
```

**Data partitioning**:
- Hot (TVar): Current `BeaconState`, `Store` (fork choice), recent blocks
- Cold (RocksDB): Historical states, all blocks by root, finalized chain

### 5. Concurrency: GHC Lightweight Threads + STM

**Decision**: Actor-like model using `forkIO` + `TQueue` for message passing.

```haskell
-- Each actor is a lightweight thread with its own message queue
data Actor msg = Actor
    { actorQueue  :: TQueue msg
    , actorThread :: Async ()
    }

-- No shared mutable state between actors — communicate via messages
-- STM transactions for atomic state updates within an actor
```

Maps naturally to ethlambda's actor model:
- P2P Actor ↔ `forkIO` thread reading from network
- Blockchain Actor ↔ `forkIO` thread processing blocks
- Validator Actor ↔ `forkIO` thread managing duties

### 6. XMSS Key State Management

**Decision**: Use `MVar` for thread-safe stateful key management.

XMSS signatures are **stateful** — each leaf key can only be used once. Reuse enables forgery.

```haskell
data KeyState = KeyState
    { ksPrivateKey   :: XmssPrivateKey
    , ksNextLeafIdx  :: Word32        -- next unused leaf
    , ksTreeHeight   :: Word32        -- determines max signatures = 2^h
    }

-- Thread-safe, prevents concurrent signing with same leaf
type ManagedKey = MVar KeyState
```

Critical invariants:
- `ksNextLeafIdx` is monotonically increasing
- `ksNextLeafIdx < 2^ksTreeHeight` (exhaustion check before signing)
- Persist `ksNextLeafIdx` to disk on every sign operation (crash recovery)
- **Never** decrement or reset the leaf index

---

## Dependency Version Constraints

All versions verified against GHC 9.6.7 and Cabal 3.14.2:

```
base          >= 4.18  && < 5        -- GHC 9.6 base
bytestring    >= 0.11  && < 0.13
crypton       >= 0.34  && < 1.1
memory        >= 0.18  && < 0.19
vector        >= 0.13  && < 0.14
containers    >= 0.6   && < 0.8
text          >= 2.0   && < 2.2
tasty         >= 1.4   && < 1.6
tasty-hunit   >= 0.10  && < 0.11
tasty-quickcheck >= 0.10 && < 0.12
QuickCheck    >= 2.14  && < 2.16
```
