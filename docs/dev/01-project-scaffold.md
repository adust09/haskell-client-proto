# Project Scaffold

## Directory Structure

```
lean-consensus/
├── lean-consensus.cabal
├── cabal.project
├── LICENSE
├── app/
│   └── Main.hs                    -- Entry point & CLI
├── src/
│   ├── SSZ/
│   │   ├── Common.hs              -- Shared types, Ssz typeclass
│   │   ├── Encode.hs              -- Encoding logic (two-pass encoder)
│   │   ├── Decode.hs              -- Decoding logic
│   │   ├── Bitvector.hs           -- Fixed-length bit array
│   │   ├── Bitlist.hs             -- Variable-length bit array
│   │   ├── Vector.hs              -- Fixed-length typed collection
│   │   ├── Merkleization.hs       -- hash_tree_root, pack, merkleize
│   │   └── Derive.hs              -- GHC.Generics / TH auto-derivation
│   ├── SSZ.hs                     -- Re-export module
│   ├── Consensus/
│   │   ├── Types.hs               -- BeaconState, BeaconBlock, Attestation, etc.
│   │   ├── Constants.hs           -- Slot time, finality thresholds, etc.
│   │   ├── ForkChoice.hs          -- 3SF-mini fork choice (Phase 3)
│   │   └── StateTransition.hs     -- Slot/block processing (Phase 3)
│   ├── Crypto/
│   │   ├── LeanSig.hs             -- XMSS FFI bindings (Phase 2)
│   │   ├── LeanMultisig.hs        -- Aggregation FFI bindings (Phase 2)
│   │   └── Hash.hs                -- SHA-256 (crypton), Poseidon2 (FFI)
│   ├── Network/
│   │   ├── P2P.hs                 -- libp2p gossipsub (Phase 4)
│   │   └── RPC.hs                 -- HTTP API (Phase 5)
│   ├── Storage.hs                 -- RocksDB + TVar cache (Phase 5)
│   └── Metrics.hs                 -- Prometheus counters/gauges (Phase 5)
└── test/
    ├── Main.hs                    -- Test runner (tasty)
    └── Test/
        ├── SSZ/
        │   ├── Encode.hs
        │   ├── Decode.hs
        │   ├── Roundtrip.hs
        │   ├── Bitvector.hs
        │   ├── Bitlist.hs
        │   ├── Vector.hs
        │   └── Merkleization.hs
        └── Consensus/
            ├── Types.hs
            └── Constants.hs
```

## Cabal Configuration

```cabal
cabal-version:   3.0
name:            lean-consensus
version:         0.1.0.0
synopsis:        Haskell Lean Consensus client for pq-devnet-3
license:         MIT
build-type:      Simple

common warnings
    ghc-options: -Wall -Wcompat -Widentities -Wincomplete-record-updates
                 -Wincomplete-uni-patterns -Wmissing-home-modules
                 -Wpartial-fields -Wredundant-constraints

library
    import:           warnings
    hs-source-dirs:   src
    default-language: GHC2021
    default-extensions:
        DataKinds
        TypeFamilies
        GADTs
        StandaloneDeriving
        DerivingStrategies
        GeneralizedNewtypeDeriving
        OverloadedStrings
        ScopedTypeVariables
        TypeApplications
    exposed-modules:
        SSZ
        SSZ.Common
        SSZ.Encode
        SSZ.Decode
        SSZ.Bitvector
        SSZ.Bitlist
        SSZ.Vector
        SSZ.Merkleization
        SSZ.Derive
        Consensus.Types
        Consensus.Constants
    build-depends:
        base          >= 4.18  && < 5
      , bytestring    >= 0.11  && < 0.13
      , crypton       >= 0.34  && < 1.1
      , memory        >= 0.18  && < 0.19
      , vector        >= 0.13  && < 0.14
      , containers    >= 0.6   && < 0.8
      , text          >= 2.0   && < 2.2

executable lean-consensus
    import:           warnings
    main-is:          Main.hs
    hs-source-dirs:   app
    default-language: GHC2021
    build-depends:
        base >= 4.18 && < 5
      , lean-consensus

test-suite lean-consensus-test
    import:           warnings
    type:             exitcode-stdio-1.0
    main-is:          Main.hs
    hs-source-dirs:   test
    default-language: GHC2021
    default-extensions:
        DataKinds
        TypeApplications
        ScopedTypeVariables
        OverloadedStrings
    other-modules:
        Test.SSZ.Encode
        Test.SSZ.Decode
        Test.SSZ.Roundtrip
        Test.SSZ.Bitvector
        Test.SSZ.Bitlist
        Test.SSZ.Vector
        Test.SSZ.Merkleization
    build-depends:
        base            >= 4.18 && < 5
      , lean-consensus
      , bytestring
      , vector
      , tasty           >= 1.4  && < 1.6
      , tasty-hunit     >= 0.10 && < 0.11
      , tasty-quickcheck >= 0.10 && < 0.12
      , QuickCheck      >= 2.14 && < 2.16
```

## GHC & Toolchain

- **GHC**: 9.6.7 (installed via ghcup)
- **Cabal**: 3.14.2
- **ghcup path**: `~/.ghcup/bin` (add to PATH)

## Key Dependencies

| Package | Version | Purpose |
|---------|---------|---------|
| `bytestring` | 0.11+ | SSZ byte manipulation |
| `crypton` | 0.34+ | SHA-256 for Merkleization |
| `memory` | 0.18+ | ByteArray abstraction (required by crypton) |
| `vector` | 0.13+ | Efficient arrays for SSZ Vector |
| `containers` | 0.6+ | Map/Set for fork choice store |
| `text` | 2.0+ | String handling |

### Future Dependencies (Phase 2+)

| Package | Purpose | Phase |
|---------|---------|-------|
| `rocksdb-haskell` | State persistence | 5 |
| `stm` | Concurrent state management | 3 |
| `async` | Structured concurrency | 3 |
| `yaml` / `aeson` | Config & genesis parsing | 5 |
| `prometheus-client` | Metrics export | 5 |
| `snappy` | libp2p message compression | 4 |

## Build & Test Commands

```bash
export PATH="$HOME/.ghcup/bin:$PATH"

cabal update
cabal build
cabal test
cabal run lean-consensus
```
