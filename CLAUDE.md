# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Haskell implementation of an Ethereum Lean Consensus (formerly Beam Chain) client, targeting participation in **pq-devnet-3**. The client implements 3-Slot Finality (3SF-mini) with 4-second slots, XMSS post-quantum signatures, and leanMultisig zkVM aggregation.

The primary reference implementation is [ethlambda](https://github.com/lambdaclass/ethlambda) (Rust by LambdaClass). The formal spec is [leanSpec](https://github.com/leanEthereum/leanSpec) (Python).

## Build & Test Commands

```bash
export PATH="$HOME/.ghcup/bin:$PATH"

cabal build              # Build the project
cabal test               # Run all tests
cabal test --test-option='-p /pattern/'  # Run tests matching pattern
cabal run lean-consensus # Run the executable
```

- **GHC**: 9.6.7 (via ghcup)
- **Cabal**: 3.14.2
- **Language**: GHC2021 with DataKinds, TypeFamilies, GADTs, DerivingStrategies, OverloadedStrings

## Architecture

The project name is `lean-consensus`. Source lives in `src/`, tests in `test/`, entry point in `app/Main.hs`.

### Module Structure (maps to ethlambda crates)

| Module | Responsibility | ethlambda Crate |
|--------|---------------|-----------------|
| `SSZ.*` | Simple Serialize wire format (encode/decode/merkleize) | `types` (SSZ parts) |
| `Consensus.Types` | BeaconState, BeaconBlock, Attestation, etc. | `types` |
| `Consensus.Constants` | Slot time (4s), finality thresholds, type-level naturals | `types` |
| `Consensus.ForkChoice` | 3SF-mini fork choice rule, head selection | `fork_choice` |
| `Consensus.StateTransition` | Per-slot/block processing, validity checks | `state_transition` |
| `Crypto.LeanSig` | XMSS signature FFI bindings (sign/verify) | `crypto` |
| `Crypto.LeanMultisig` | Aggregation proof FFI bindings | `crypto` |
| `Crypto.Hash` | SHA-256 (crypton), Poseidon2 (FFI) | `crypto` |
| `Network.P2P` | rust-libp2p FFI (gossipsub, QUIC, discv5) | `p2p` |
| `Network.RPC` | HTTP REST API | `rpc` |
| `Storage` | RocksDB + TVar in-memory cache | `storage` |
| `Metrics` | Prometheus counters/gauges | `metrics` |

### SSZ Subsystem

SSZ is written from scratch (not forking ssz-hs). Key design patterns:
- **Three typeclasses**: `Ssz` (metadata: fixed/variable), `SszEncode`, `SszDecode`
- **Two-pass encoding**: fixed parts + offset table first, then variable parts
- **Auto-derivation**: via `GHC.Generics` — types derive `Generic` then get empty `Ssz`/`SszEncode`/`SszDecode` instances
- **Merkleization**: `hash_tree_root` using SHA-256 (crypton), with `pack`, `merkleize`, `mixInLength`
- Key SSZ types: `BytesN n` (phantom-typed fixed bytes), `SszVector n a`, `SszList n a`, `Bitvector n`, `Bitlist n`

### Concurrency Model

Actor-based using GHC lightweight threads (`forkIO` + `async`), communicating via `TQueue` message passing. Each actor (P2P, Blockchain, Validator, RPC) owns its state — no shared mutable state between actors. STM for atomic updates within an actor.

### Crypto FFI Pattern

Both leanSig (XMSS) and leanMultisig use C ABI wrappers consumed via `foreign import ccall`. XMSS keys are **stateful** — leaf indices must be monotonically increasing and never reused. Key state is managed via `MVar` for thread safety and persisted to disk on every sign operation.

### Networking

rust-libp2p exposed through a thin C ABI wrapper → Haskell FFI. Gossipsub topics: `attestation_{subnet_id}`, `aggregation`, `beacon_block`. Fallback: IPC sidecar if FFI proves too complex.

## Testing

- Framework: `tasty` + `tasty-hunit` (unit) + `tasty-quickcheck` (property)
- SSZ roundtrip property tests for all types: `sszDecode (sszEncode x) == Right x`
- Merkleization validated against leanSpec test vectors
- Test vectors from leanSpec: `cd leanSpec/ && uv run fill --clean --fork=devnet --scheme=test`

## Implementation Phases

The project follows a 5-phase roadmap (see `docs/dev/04-implementation-roadmap.md`):
1. **Foundation** — SSZ + consensus types (current phase)
2. **Cryptography FFI** — leanSig/leanMultisig bindings
3. **Consensus Logic** — state transition + fork choice
4. **Networking** — rust-libp2p gossipsub
5. **Infrastructure** — storage, genesis, main loop, devnet integration

## Documentation

- `docs/00-10*.md` — Research & protocol background (3SF, XMSS, leanMultisig, devnet specs)
- `docs/dev/` — Actionable implementation guides (scaffold, SSZ spec, types, roadmap, library decisions)

## Key Technical Notes

- `Bytes32` is the most common type — used for all roots and hashes
- `Word128`/`Word256` are `ByteString` wrappers (opaque identifiers, no arithmetic)
- Hashing: `crypton` (maintained fork of `cryptonite`) — `Crypto.Hash.hash` with `SHA256` digest
- The `memory` package (`Data.ByteArray`) is a required transitive dependency of crypton
