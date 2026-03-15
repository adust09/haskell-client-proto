# Development Documentation

Implementation guides for the Haskell Lean Consensus client (pq-devnet-3).

## Documents

| # | Document | Contents |
|---|----------|----------|
| 01 | [Project Scaffold](01-project-scaffold.md) | Directory structure, cabal config, dependencies, GHC/toolchain setup |
| 02 | [SSZ Implementation Guide](02-ssz-implementation-guide.md) | Type system, encoding/decoding algorithms, Merkleization spec, auto-derivation strategy |
| 03 | [Consensus Types](03-consensus-types.md) | All Haskell type definitions: Checkpoint, Attestation, BeaconBlock, BeaconState, etc. |
| 04 | [Implementation Roadmap](04-implementation-roadmap.md) | Phase 1–5 task breakdown with checkboxes, exit criteria, risk register |
| 05 | [Library Decisions](05-library-decisions.md) | SSZ (new vs fork), crypton, rust-libp2p FFI, RocksDB, concurrency model |
| 06 | [FFI ABI Proposal](06-ffi-abi-proposal.md) | leanSig/leanMultisig C ABI wrapper design, error codes, memory ownership, open questions |

## Quick Reference

- **GHC**: 9.6.7 via ghcup (`~/.ghcup/bin`)
- **Cabal**: 3.14.2
- **Target**: pq-devnet-3 (4s slots, 3-slot finality, XMSS signatures, leanMultisig aggregation)
- **Reference implementations**: [ethlambda](https://github.com/lambdaclass/ethlambda) (Rust), [leanSpec](https://github.com/leanEthereum/leanSpec) (Python)

## Relationship to Research Docs

The `docs/` directory contains two categories:
- `docs/00-10*.md` — Research & background (protocol specs, architecture analysis)
- `docs/dev/` — Implementation guides (this directory, actionable development docs)
