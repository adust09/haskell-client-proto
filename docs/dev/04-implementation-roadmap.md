# Implementation Roadmap

## Phase 1: Foundation

**Goal**: SSZ serialization + consensus types that compile and pass roundtrip tests.

### Step 1.1: Project Scaffold

- [ ] Create `lean-consensus.cabal` per [01-project-scaffold.md](01-project-scaffold.md)
- [ ] Create `cabal.project`
- [ ] Set up directory structure (`src/`, `test/`, `app/`)
- [ ] `app/Main.hs` with minimal stub
- [ ] Verify `cabal build` succeeds with empty modules

### Step 1.2: SSZ Core (Encode + Decode)

Reference: [02-ssz-implementation-guide.md](02-ssz-implementation-guide.md)

- [ ] `SSZ.Common` — `Ssz` typeclass, `SszError`, `BytesN` newtype
- [ ] `SSZ.Encode` — `SszEncode` typeclass, two-pass `SszEncoder`
  - [ ] `Word8`, `Word16`, `Word32`, `Word64` instances
  - [ ] `Bool` instance
  - [ ] `BytesN n` instance
  - [ ] `[a]` list instance (both fixed and variable element)
- [ ] `SSZ.Decode` — `SszDecode` typeclass
  - [ ] Same instances as Encode
  - [ ] Offset-based variable-length decoding
- [ ] Tests: `Test.SSZ.Encode`, `Test.SSZ.Decode`, `Test.SSZ.Roundtrip`

### Step 1.3: SSZ Composite Types

- [ ] `SSZ.Vector` — `SszVector n a`, fixed-length collection
  - [ ] Encode/Decode for fixed-element and variable-element vectors
  - [ ] Test: roundtrip, length enforcement
- [ ] `SSZ.Bitvector` — `Bitvector n`, fixed-length bit array
  - [ ] Encode/Decode (bit packing, LSB first)
  - [ ] Test: roundtrip, bit manipulation helpers
- [ ] `SSZ.Bitlist` — `Bitlist n`, variable-length bit array
  - [ ] Encode/Decode (sentinel bit)
  - [ ] Test: roundtrip, sentinel detection edge cases

### Step 1.4: SSZ Merkleization

- [ ] `SSZ.Merkleization` — hash_tree_root implementation
  - [ ] `sha256` via crypton
  - [ ] `pack` — zero-pad and chunk
  - [ ] `packBits` — bit packing into chunks
  - [ ] `merkleize` — binary Merkle tree, zero-hash padding, limit parameter
  - [ ] `mixInLength` — for List/Bitlist
  - [ ] `hashTreeRoot` typeclass method or standalone function
  - [ ] Pre-computed zero hashes table (optimization)
- [ ] Tests: known vectors, cross-validate with leanSpec

### Step 1.5: SSZ Auto-Derivation

- [ ] `SSZ.Derive` — GHC.Generics-based derivation
  - [ ] `GSsz`, `GEncode`, `GDecode` walking `Rep`
  - [ ] Handle `M1`, `(:*:)`, `K1` for product types
  - [ ] Container offset calculation for mixed fixed/variable fields
- [ ] Test: derive instances for `Checkpoint`, `AttestationData`, verify roundtrip

### Step 1.6: Consensus Types

Reference: [03-consensus-types.md](03-consensus-types.md)

- [ ] `Consensus.Constants` — timing, limits, type-level naturals
- [ ] `Consensus.Types` — all types with SSZ instances
  - [ ] `Checkpoint`
  - [ ] `AttestationData`
  - [ ] `XmssSignature`, `XmssPubkey`
  - [ ] `SignedAttestation`
  - [ ] `SignedAggregatedAttestation`, `LeanMultisigProof`
  - [ ] `BeaconBlockBody`, `BeaconBlock`, `SignedBeaconBlock`
  - [ ] `BeaconBlockHeader`
  - [ ] `Validator`
  - [ ] `BeaconState`
  - [ ] `Store`, `LatestMessage` (non-SSZ, internal)
- [ ] Tests: SSZ roundtrip for each type, `hashTreeRoot` for key types

### Phase 1 Exit Criteria

- `cabal build` and `cabal test` pass
- All SSZ types encode/decode correctly
- Roundtrip property tests for all types
- `hashTreeRoot` produces correct output for basic types
- Consensus types compile with derived SSZ instances

---

## Phase 2: Cryptography FFI

**Goal**: Sign/verify with XMSS, aggregate/verify with leanMultisig.

### Step 2.1: leanSig FFI

- [ ] Obtain leanSig C library (headers + shared lib)
- [ ] `Crypto.LeanSig` — FFI bindings
  - [ ] `foreign import ccall` declarations
  - [ ] `sign :: PrivateKey -> ByteString -> IO XmssSignature`
  - [ ] `verify :: PublicKey -> ByteString -> XmssSignature -> Bool`
  - [ ] Key state management (leaf index tracking via `IORef` or `MVar`)
- [ ] `Crypto.Hash` — SHA-256 (crypton), Poseidon2 (FFI to leanSig internal)
- [ ] Tests: sign → verify roundtrip, known test vectors

### Step 2.2: leanMultisig FFI

- [ ] Obtain leanMultisig C/Rust library
- [ ] `Crypto.LeanMultisig` — FFI bindings
  - [ ] `setupProver :: IO ProverContext`
  - [ ] `setupVerifier :: IO VerifierContext`
  - [ ] `aggregate :: ProverContext -> [SignedAttestation] -> IO LeanMultisigProof`
  - [ ] `verifyAggregation :: VerifierContext -> LeanMultisigProof -> AttestationData -> Bitlist n -> IO Bool`
- [ ] Tests: aggregate → verify roundtrip

### Phase 2 Exit Criteria

- Can sign and verify XMSS signatures
- Can aggregate and verify leanMultisig proofs
- Key state management prevents leaf reuse
- All crypto operations work with SSZ-serialized messages

---

## Phase 3: Consensus Logic

**Goal**: Process slots and blocks, run fork choice.

### Step 3.1: State Transition

Reference: docs/03-3sf-mini.md, leanSpec Python, ethlambda `state_transition` crate

- [ ] `Consensus.StateTransition`
  - [ ] `processSlot :: BeaconState -> BeaconState` — per-slot housekeeping
  - [ ] `processBlock :: BeaconState -> SignedBeaconBlock -> Either Error BeaconState`
  - [ ] `processAttestation :: BeaconState -> SignedAggregatedAttestation -> Either Error BeaconState`
  - [ ] Validity checks: slot, proposer, parent root, signatures
- [ ] Tests: leanSpec state transition test vectors

### Step 3.2: Fork Choice

Reference: docs/03-3sf-mini.md, ethlambda `fork_choice` crate

- [ ] `Consensus.ForkChoice`
  - [ ] `onBlock :: Store -> SignedBeaconBlock -> Store`
  - [ ] `onAttestation :: Store -> SignedAttestation -> Store`
  - [ ] `getHead :: Store -> Root` — 3SF-mini head selection
  - [ ] Justification/finalization updates
  - [ ] Vote weight calculation
- [ ] Tests: ethlambda fork choice test cases

### Phase 3 Exit Criteria

- Can process a sequence of blocks and track state
- Fork choice selects correct head
- Justification and finalization work within 3 slots
- Slashing conditions detected

---

## Phase 4: Networking

**Goal**: Connect to pq-devnet-3 peers, send/receive gossip messages.

### Step 4.1: rust-libp2p FFI

- [ ] Build thin C ABI wrapper around rust-libp2p
  - [ ] gossipsub publish/subscribe
  - [ ] QUIC/UDP transport
  - [ ] discv5 peer discovery
- [ ] `Network.P2P` — Haskell FFI bindings
  - [ ] `startNode :: Config -> IO P2PNode`
  - [ ] `subscribe :: P2PNode -> Topic -> (ByteString -> IO ()) -> IO ()`
  - [ ] `publish :: P2PNode -> Topic -> ByteString -> IO ()`
  - [ ] `requestBlocksByRange :: P2PNode -> Slot -> Slot -> IO [SignedBeaconBlock]`

### Step 4.2: Aggregator Role

- [ ] Attestation collection from subnets
- [ ] leanMultisig proof generation
- [ ] Publish to `aggregation` topic
- [ ] `--is-aggregator` CLI flag

### Phase 4 Exit Criteria

- Can discover and connect to devnet peers
- Receives blocks and attestations via gossipsub
- Aggregator mode produces valid aggregated attestations

---

## Phase 5: Infrastructure & Integration

**Goal**: Persistent storage, genesis loading, main loop, devnet connection.

### Step 5.1: Storage

- [ ] `Storage` — RocksDB for BeaconState, blocks
- [ ] In-memory TVar cache for hot state
- [ ] Fork choice store persistence

### Step 5.2: Genesis & Config

- [ ] YAML/JSON genesis config parsing
- [ ] pq-devnet-3 genesis state initialization
- [ ] CLI argument parsing

### Step 5.3: Main Loop & Actor Model

- [ ] Wire up all components with async + STM
- [ ] Slot timer (4-second ticks)
- [ ] Block proposal duty
- [ ] Attestation duty
- [ ] Message routing between actors

### Step 5.4: Metrics

- [ ] Prometheus counters/gauges via `prometheus-client`
- [ ] Slot processing time, peer count, head slot, finalized slot

### Step 5.5: Integration Testing

- [ ] Connect to pq-devnet-3
- [ ] Sync blocks from genesis
- [ ] Submit attestations
- [ ] Verify finalization

### Phase 5 Exit Criteria

- Full node syncs with pq-devnet-3
- Participates in consensus (attestations accepted by peers)
- Aggregator mode works end-to-end
- Finalization observed within 3 slots

---

## Risk Register

| Risk | Impact | Mitigation |
|------|--------|------------|
| leanSig/leanMultisig C libraries unavailable or undocumented | Blocks Phase 2 | Contact leanEthereum team early; study ethlambda's FFI code |
| SSZ spec differences between Lean Consensus and ETH2 | Incorrect serialization | Use leanSpec test vectors as source of truth |
| rust-libp2p FFI complexity | Delays Phase 4 | Start with IPC sidecar as fallback |
| BeaconState fields differ from estimate | Rework in Phase 1 | Treat types as provisional; add leanSpec as submodule early |
| GHC performance for real-time 4s slots | Missed attestation deadlines | Profile early; use unboxed vectors; consider `compact` regions |

---

## External Dependencies to Obtain

| Dependency | Source | Needed By |
|------------|--------|-----------|
| leanSpec (Python) | https://github.com/leanEthereum/leanSpec | Phase 1 (test vectors) |
| leanSig C library | leanEthereum org | Phase 2 |
| leanMultisig C library | leanEthereum org | Phase 2 |
| pq-devnet-3 genesis config | leanEthereum pm repo | Phase 5 |
| pq-devnet-3 bootnodes | leanEthereum pm repo | Phase 4 |
