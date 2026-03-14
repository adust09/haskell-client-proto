# ethlambda Architecture (Reference Implementation)

## Overview

ethlambda is a Lean Consensus client built by LambdaClass in **Rust**. It serves as the primary reference implementation for our Haskell client. Note: the Elixir-based `lambda_ethereum_consensus` is a separate, older project for the current Beacon Chain and is not directly relevant.

## 10-Crate Architecture

```
ethlambda/
├── bin/ethlambda           — Entry point & CLI
├── blockchain/
│   ├── fork_choice         — LMD GHOST (3SF-mini variant)
│   └── state_transition    — Slot/block/attestation processing
├── common/
│   ├── types               — Data structures (SSZ-serializable)
│   ├── crypto              — leanSig/leanMultisig wrapper
│   └── metrics             — Prometheus (leanMetrics-compliant)
├── net/
│   ├── p2p                 — libp2p gossipsub + request-response
│   └── rpc                 — HTTP REST API
└── storage/                — RocksDB + in-memory stores
```

### Crate Responsibilities

| Crate | Responsibility |
|-------|---------------|
| `bin/ethlambda` | CLI argument parsing, actor orchestration, startup |
| `fork_choice` | 3SF-mini fork choice rule, head selection, vote processing |
| `state_transition` | BeaconState updates per slot/block, validity checks |
| `types` | `BeaconState`, `BeaconBlock`, `Attestation`, SSZ codecs |
| `crypto` | FFI wrappers around leanSig (XMSS) and leanMultisig |
| `metrics` | Prometheus exporters following leanMetrics standard |
| `p2p` | libp2p networking, gossipsub mesh, peer management |
| `rpc` | HTTP API for external tools and monitoring |
| `storage` | State persistence (RocksDB) and caching (in-memory) |

## Actor-Based Concurrency Model

ethlambda uses an actor model where each component runs as an independent task:

```
┌──────────────┐     ┌───────────────────┐     ┌───────────┐
│   P2P Actor  │────→│  Blockchain Actor  │────→│  Storage  │
│  (gossipsub) │     │  (fork choice +    │     │  (RocksDB)│
│              │←────│   state transition)│←────│           │
└──────────────┘     └───────────────────┘     └───────────┘
       ↑                      ↑
       │                      │
┌──────────────┐     ┌───────────────────┐
│  RPC Actor   │     │ Validator Actor    │
│  (HTTP API)  │     │ (duties, signing)  │
└──────────────┘     └───────────────────┘
```

- Components communicate via message passing (channels)
- No shared mutable state — each actor owns its data
- `spawned` runtime for lightweight task management

## Crypto Integration

### leanSig (XMSS)
- Rust FFI to the leanSig C library
- Sign/verify operations
- Key management (stateful — tracks used leaf indices)

### leanMultisig
- Rust FFI to the leanMultisig C library
- `xmss_aggregate()` — generate aggregation proof
- `xmss_verify_aggregation()` — verify aggregation proof
- Prover/verifier setup at initialization

## Network Layer

| Parameter | Value |
|-----------|-------|
| Transport | libp2p QUIC/UDP |
| Discovery | discv5 |
| Pubsub | Gossipsub v2.0 |
| Mesh size | 8 |
| Heartbeat interval | 700ms |
| Topics | `attestation_{subnet_id}`, `aggregation`, `beacon_block` |

### Request-Response Protocols
- `beacon_blocks_by_range` — Sync historical blocks
- `beacon_blocks_by_root` — Fetch specific blocks
- `status` — Exchange chain status with peers

## Design Principles

| Principle | Detail |
|-----------|--------|
| Minimal codebase | < 5,000 lines total (vs 200k+ for other clients) |
| Trait minimization | Only 12 traits (serialization, storage, crypto) |
| Macro minimization | Only 4 macros |
| No inheritance | Composition over inheritance throughout |
| Explicit errors | No panics in library code; `Result<T, E>` everywhere |

## Haskell Module Correspondence

| ethlambda Crate | Haskell Module (proposed) |
|-----------------|---------------------------|
| `types` | `Consensus.Types` |
| `fork_choice` | `Consensus.ForkChoice` |
| `state_transition` | `Consensus.StateTransition` |
| `crypto` | `Crypto.LeanSig` / `Crypto.LeanMultisig` |
| `p2p` | `Network.P2P` |
| `rpc` | `Network.RPC` |
| `storage` | `Storage` |
| `metrics` | `Metrics` |

See [10-implementation-scope.md](10-implementation-scope.md) for detailed mapping.
