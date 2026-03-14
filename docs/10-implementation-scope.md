# Haskell Implementation Scope

## Target: pq-devnet-3

Our Haskell consensus client targets participation in pq-devnet-3. This document defines the required implementation scope and key technical decisions.

## Required Implementation (7 Components)

### 1. 3SF-mini Consensus
- Fork choice rule (3SF-mini variant of LMD-GHOST)
- Head selection algorithm
- Vote processing and weight calculation
- Reference: `fork_choice` crate in ethlambda

### 2. State Transition
- Per-slot state updates
- Per-block state updates (including attestation processing)
- Validity checks (block, attestation, signature)
- Reference: `state_transition` crate in ethlambda

### 3. leanSig (XMSS Signatures)
- FFI bindings to leanSig C library
- Sign and verify operations
- Key state management (track used leaf indices)
- Reference: `crypto` crate in ethlambda

### 4. leanMultisig (Aggregation)
- FFI bindings to leanMultisig C library
- `xmss_aggregate()` — proof generation
- `xmss_verify_aggregation()` — proof verification
- Prover/verifier setup
- Reference: `crypto` crate in ethlambda

### 5. Aggregator Role
- Collect attestations from subscribed subnets
- Run leanMultisig aggregation
- Publish `SignedAggregatedAttestation` to `aggregation` topic
- Optional role (`--is-aggregator` flag)

### 6. Gossipsub Networking
- `attestation_{subnet_id}` — individual attestation propagation
- `aggregation` — aggregated attestation propagation
- `beacon_block` — block propagation
- libp2p QUIC/UDP transport

### 7. State Management
- BeaconState storage and retrieval
- Fork choice store (justified/finalized checkpoints, block tree)
- In-memory caching + persistent storage

## Technology Considerations

### Cryptography (FFI)
- leanSig and leanMultisig are C/Rust libraries
- Haskell FFI (`foreign import ccall`) for C bindings
- Alternatively, use `inline-c` or write a thin Rust wrapper exposed as C ABI
- Must handle stateful XMSS key management safely

### Serialization (SSZ)
- Simple Serialize (SSZ) is the wire format for all consensus messages
- No mature Haskell SSZ library exists — likely need to implement
- Core types: `uint64`, `bytes32`, `Container`, `List`, `Vector`, `Bitlist`, `Bitvector`
- Merkleization for hash tree roots

### Networking (libp2p)
- No production-quality Haskell libp2p implementation
- Options:
  - FFI bindings to rust-libp2p or go-libp2p
  - Sidecar process communicating via IPC
  - Implement minimal gossipsub in Haskell (high effort)

### Concurrency (STM + async)
- GHC lightweight threads + `forkIO` for actor-like concurrency
- STM (`TVar`, `TChan`, `TQueue`) for safe state sharing
- `async` library for structured concurrency
- Natural fit for actor model used by ethlambda

### Storage (RocksDB)
- `rocksdb-haskell` package for persistent state
- In-memory `TVar`-based cache for hot state
- Key-value model matching ethlambda's storage crate

## ethlambda → Haskell Module Mapping

| ethlambda Crate | Haskell Module | Key Types/Functions |
|-----------------|----------------|---------------------|
| `types` | `Consensus.Types` | `BeaconState`, `BeaconBlock`, `Attestation` |
| `fork_choice` | `Consensus.ForkChoice` | `onBlock`, `onAttestation`, `getHead` |
| `state_transition` | `Consensus.StateTransition` | `processSlot`, `processBlock` |
| `crypto` | `Crypto.LeanSig` | `sign`, `verify`, `KeyState` |
| `crypto` | `Crypto.LeanMultisig` | `aggregate`, `verifyAggregation` |
| `p2p` | `Network.P2P` | `gossipPublish`, `gossipSubscribe` |
| `rpc` | `Network.RPC` | HTTP API handlers |
| `storage` | `Storage` | `getState`, `putState`, `getBlock` |
| `metrics` | `Metrics` | Prometheus counters/gauges |

## Reference Links

- [Lean Consensus Roadmap](https://leanroadmap.org/)
- [leanMultisig Overview](https://deepwiki.com/leanEthereum/leanMultisig/1-overview)
- [ethlambda Blog Post](https://blog.lambdaclass.com/ethlambda-building-a-post-quantum-ethereum-client-with-the-help-of-shared-tooling/)
- [ethlambda Architecture](https://blog.lambdaclass.com/building-a-minimalist-post-quantum-ethereum-client-ethlambdas-architecture/)
- [ethlambda GitHub](https://github.com/lambdaclass/ethlambda)
- [leanSpec GitHub](https://github.com/leanEthereum/leanSpec)
- [3SF Research Paper](https://ethresear.ch/t/3-slot-finality-ssf-is-not-about-single-slot/20927)
- [3SF Arxiv Paper](https://arxiv.org/abs/2411.00558)
- [Lean Consensus 2026 Plan (HackMD)](https://hackmd.io/@tcoratger/ryS1ElrWbx)
- [pq-devnet-3 Spec](https://github.com/leanEthereum/pm/blob/main/breakout-rooms/leanConsensus/pq-interop/pq-devnet-3.md)
