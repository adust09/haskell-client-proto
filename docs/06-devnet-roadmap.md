# Devnet Roadmap (pq-devnet-0 to 4)

## Overview

Lean Consensus development follows an incremental devnet strategy. Each devnet adds capabilities while maintaining multi-client interoperability.

## Timeline

```
pq-devnet-0  Oct 2025   ██████████ Complete
pq-devnet-1  Dec 2025   ██████████ Complete
pq-devnet-2  Jan 2026   ██████████ Complete
pq-devnet-3  Feb 2026   ████████░░ Target ← Current
pq-devnet-4  Mar 2026   ░░░░░░░░░░ Planned
```

## pq-devnet-0 (October 2025) — Foundation

- leanSpec framework established
- Multi-client interop **without** PQ signatures
- 4-second slots + QUIC transport + 3SF-mini
- **Clients**: Ream, Zeam, Qlean

## pq-devnet-1 (December 2025) — Basic PQ Signatures

- leanSig (XMSS) signing and verification integrated
- Basic signature aggregation (concatenation method — no ZK proofs)
- **Clients**: + Lantern, Grandine

## pq-devnet-2 (January 2026) — ZK Aggregation

- leanMultisig aggregation integrated
- Baseline PQ aggregation performance measurement
- **Clients**: + ethlambda

## pq-devnet-3 (February 2026) — Aggregator Separation ← Target

- **Key change**: Aggregator role separated from block production
- Independent Aggregator nodes collect and aggregate attestations
- New gossipsub topics: `attestation_{subnet_id}`, `aggregation`
- Validators assigned to attestation subnets
- Proposers include pre-aggregated signatures in blocks
- **Recursive aggregation NOT yet implemented**

See [07-devnet-3-spec.md](07-devnet-3-spec.md) for detailed specification.

## pq-devnet-4 (March 2026) — Recursive Aggregation

- leanVM recursive PQ signature aggregation
- Proposer-side recursive aggregation → reduced in-block signature size
- Consolidate to one aggregation per message
- Further performance optimizations

## Feature Matrix

| Feature | dn-0 | dn-1 | dn-2 | dn-3 | dn-4 |
|---------|-------|-------|-------|-------|-------|
| 3SF-mini consensus | ✓ | ✓ | ✓ | ✓ | ✓ |
| 4-sec slots + QUIC | ✓ | ✓ | ✓ | ✓ | ✓ |
| leanSig (XMSS) | — | ✓ | ✓ | ✓ | ✓ |
| leanMultisig (ZK) | — | — | ✓ | ✓ | ✓ |
| Aggregator separation | — | — | — | ✓ | ✓ |
| Recursive aggregation | — | — | — | — | ✓ |

## Participating Clients

| Client | Language | Since |
|--------|----------|-------|
| Ream | Rust | dn-0 |
| Zeam | Zig | dn-0 |
| Qlean | Go | dn-0 |
| Lantern | C++ | dn-1 |
| Grandine | Rust | dn-1 |
| ethlambda | Rust | dn-2 |
