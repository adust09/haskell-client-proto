# Lean Consensus Overview

## What is Lean Consensus?

Lean Consensus (formerly known as **Beam Chain**) is a clean-slate redesign of Ethereum's consensus layer, announced by Justin Drake at Devcon 2024. It aims to replace the current Beacon Chain with a fundamentally simpler, more secure, and faster protocol.

## Four Pillars

| Pillar | Goal |
|--------|------|
| **Security Hardening** | Post-quantum cryptography migration (BLS → XMSS hash-based signatures) |
| **Decentralization** | Lower staking requirements (32 ETH → 1 ETH target) |
| **Rapid Finality** | Finality in seconds, not minutes (3-Slot Finality) |
| **Minimalism** | Formally verifiable, simple specification |

## ETH 2.0 vs Lean Consensus

| Item | ETH 2.0 (Beacon Chain) | Lean Consensus |
|------|------------------------|----------------|
| Finality | ~15 min (2 epochs) | ~12 sec (3 slots) |
| Slot time | 12 sec | 4 sec |
| Signature scheme | BLS12-381 | XMSS (hash-based, PQ-safe) |
| Signature aggregation | BLS pairing aggregation | leanMultisig (zkVM proof) |
| Fork choice | LMD-GHOST + Casper FFG | 3-layer decoupled protocol |
| Minimum stake | 32 ETH | 1 ETH (target) |
| Network | libp2p/gossipsub | Gossipsub v2.0 + QUIC |

## Document Navigation

| File | Topic |
|------|-------|
| [01-motivation.md](01-motivation.md) | Why redesign the Beacon Chain |
| [02-3sf-decoupled-protocol.md](02-3sf-decoupled-protocol.md) | 3-layer decoupled architecture |
| [03-3sf-mini.md](03-3sf-mini.md) | 3SF-mini for devnet |
| [04-xmss-signatures.md](04-xmss-signatures.md) | Post-quantum signatures (XMSS) |
| [05-lean-multisig-architecture.md](05-lean-multisig-architecture.md) | leanMultisig zkVM stack |
| [06-devnet-roadmap.md](06-devnet-roadmap.md) | pq-devnet-0 to 4 roadmap |
| [07-devnet-3-spec.md](07-devnet-3-spec.md) | pq-devnet-3 detailed spec |
| [08-ethlambda-architecture.md](08-ethlambda-architecture.md) | ethlambda (Rust) reference impl |
| [09-leanspec-reference.md](09-leanspec-reference.md) | leanSpec repository reference |
| [10-implementation-scope.md](10-implementation-scope.md) | Haskell implementation scope |
