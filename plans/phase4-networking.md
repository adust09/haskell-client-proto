# Phase 4: Networking — rust-libp2p FFI + Gossipsub + Aggregator Role

## Goal

Connect to pq-devnet-3 peers via rust-libp2p, exchange blocks and attestations over gossipsub, and implement the Aggregator role that collects individual attestations and produces leanMultisig proofs. By end of phase, the node can discover peers, receive/send gossip messages, and an aggregator node can produce valid aggregated attestations.

## Prerequisites

- **Phase 1-3 complete**: SSZ, crypto, state transition, fork choice all working
- **External**: rust-libp2p C ABI wrapper library (built from Rust source)
- **Toolchain**: Rust toolchain (for building the libp2p wrapper)
- **Dependencies**: `snappy` or `snappy-c` (gossipsub message compression)

---

## Step 1: rust-libp2p C ABI Wrapper

### Files to create

- `cbits/lean_p2p.h` — C header declaring the FFI surface
- `cbits/lean_p2p_wrapper.c` — optional thin C shim if Rust's C ABI needs adaptation
- `rust-libp2p-wrapper/` — Rust crate that wraps rust-libp2p with `#[no_mangle] extern "C"` functions

### C API surface (to be exposed by Rust crate)

```c
// Node lifecycle
typedef struct LeanP2PNode LeanP2PNode;

int lean_p2p_create(LeanP2PConfig* config, LeanP2PNode** out_node);
void lean_p2p_destroy(LeanP2PNode* node);
int lean_p2p_start(LeanP2PNode* node);
int lean_p2p_stop(LeanP2PNode* node);

// Gossipsub
int lean_p2p_subscribe(LeanP2PNode* node, const char* topic,
                       void (*callback)(const uint8_t* data, size_t len, void* ctx),
                       void* ctx);
int lean_p2p_unsubscribe(LeanP2PNode* node, const char* topic);
int lean_p2p_publish(LeanP2PNode* node, const char* topic,
                     const uint8_t* data, size_t len);

// Request-response
int lean_p2p_request_blocks_by_range(LeanP2PNode* node,
                                     const char* peer_id,
                                     uint64_t start_slot, uint64_t count,
                                     uint8_t** out_data, size_t* out_len);
int lean_p2p_request_blocks_by_root(LeanP2PNode* node,
                                    const uint8_t* roots, size_t roots_len,
                                    uint8_t** out_data, size_t* out_len);

// Peer management
int lean_p2p_add_bootnode(LeanP2PNode* node, const char* multiaddr);
int lean_p2p_peer_count(LeanP2PNode* node);
int lean_p2p_get_local_peer_id(LeanP2PNode* node, char* out_id, size_t max_len);

// Memory management
void lean_p2p_free_buffer(uint8_t* buf, size_t len);

// Configuration
typedef struct {
    uint16_t listen_port;
    uint8_t  gossipsub_mesh_size;
    uint16_t gossipsub_heartbeat_ms;
    const char* const* bootnodes;
    size_t bootnode_count;
    const char* private_key_path;  // optional, for stable peer ID
} LeanP2PConfig;
```

### Algorithm description

The Rust wrapper crate uses `rust-libp2p` with:
- **Transport**: QUIC/UDP (primary), TCP/Noise (fallback)
- **Discovery**: discv5 for peer discovery
- **Pubsub**: Gossipsub v2.0 with configurable mesh parameters
- **Compression**: Snappy for gossipsub message payloads

The wrapper manages the libp2p `Swarm` in a Rust async runtime (`tokio`). C callbacks are invoked on the Rust runtime's thread pool — Haskell must handle thread safety at the callback boundary.

### Completion criteria

- Rust wrapper crate builds and produces `liblean_p2p.so`
- C header covers all required operations
- Basic smoke test: create node, add bootnode, start, get peer count

---

## Step 2: Haskell P2P FFI Bindings

### Files to create

- `src/Network/P2P.hs`
- `test/Test/Network/P2P.hs`

### Types and signatures

```haskell
module Network.P2P where

-- Opaque node handle
data CP2PNode
newtype P2PNode = P2PNode (ForeignPtr CP2PNode)

-- Configuration
data P2PConfig = P2PConfig
    { p2pListenPort       :: Word16
    , p2pMeshSize         :: Word8
    , p2pHeartbeatMs      :: Word16
    , p2pBootnodes        :: [String]
    , p2pPrivateKeyPath   :: Maybe FilePath
    }

-- Node lifecycle
createNode :: P2PConfig -> IO P2PNode
startNode  :: P2PNode -> IO ()
stopNode   :: P2PNode -> IO ()

-- Gossipsub
data Topic
    = AttestationTopic SubnetId   -- "attestation_{subnet_id}"
    | AggregationTopic            -- "aggregation"
    | BeaconBlockTopic            -- "beacon_block"
    deriving (Show, Eq)

topicString :: Topic -> String

subscribe :: P2PNode -> Topic -> (ByteString -> IO ()) -> IO ()
-- Subscribe to a topic with a callback for received messages

unsubscribe :: P2PNode -> Topic -> IO ()

publish :: P2PNode -> Topic -> ByteString -> IO ()
-- Publish SSZ-encoded, Snappy-compressed message to topic

-- Request-response
requestBlocksByRange :: P2PNode -> Slot -> Word64 -> IO [ByteString]
-- startSlot → count → SSZ-encoded blocks

requestBlocksByRoot :: P2PNode -> [Root] -> IO [ByteString]
-- roots → SSZ-encoded blocks

-- Peer info
peerCount :: P2PNode -> IO Int
localPeerId :: P2PNode -> IO String

-- FFI declarations
foreign import ccall "lean_p2p_create"
    c_create :: Ptr LeanP2PConfig -> Ptr (Ptr CP2PNode) -> IO CInt

foreign import ccall "lean_p2p_subscribe"
    c_subscribe :: Ptr CP2PNode -> CString
                -> FunPtr (Ptr Word8 -> CSize -> Ptr () -> IO ())
                -> Ptr () -> IO CInt

foreign import ccall "lean_p2p_publish"
    c_publish :: Ptr CP2PNode -> CString -> Ptr Word8 -> CSize -> IO CInt
```

### Algorithm description

**Callback handling**: Gossipsub callbacks arrive on Rust threads. The Haskell callback wrapper:
1. Copies the message bytes into a Haskell `ByteString` (since the Rust buffer is freed after callback returns).
2. Writes the message to a `TQueue` (thread-safe, non-blocking).
3. A dedicated Haskell thread reads from the `TQueue` and processes messages.

This avoids blocking Rust threads with Haskell GC or long-running Haskell computations.

**Message serialization**: All messages on the wire are SSZ-encoded, then Snappy-compressed. On publish: `sszEncode → snappyCompress → c_publish`. On receive: `snappyDecompress → sszDecode`.

**Lifecycle**: `ForeignPtr` with `lean_p2p_destroy` as finalizer. `startNode` spawns the Rust async runtime. `stopNode` signals shutdown and waits.

### Tests (TDD)

- Create and destroy node without crash
- Subscribe/unsubscribe topic without crash
- Two local nodes: node A publishes → node B receives (loopback test)
- Snappy compress/decompress roundtrip
- Invalid message bytes → decode error (doesn't crash)
- `topicString` produces correct strings: `"attestation_0"`, `"aggregation"`, `"beacon_block"`

### Completion criteria

- P2P node can be created, started, and stopped
- Gossipsub subscribe/publish works
- Callback-to-TQueue pipeline handles messages without blocking Rust
- Snappy compression integrated

---

## Step 3: Message Handlers — Block and Attestation Gossip

### Files to create

- `src/Network/MessageHandler.hs`
- `test/Test/Network/MessageHandler.hs`

### Types and signatures

```haskell
module Network.MessageHandler where

-- Incoming message types (deserialized from gossip)
data GossipMessage
    = GossipBlock SignedBeaconBlock
    | GossipAttestation SignedAttestation SubnetId
    | GossipAggregation SignedAggregatedAttestation
    deriving (Show)

-- Message validation result
data ValidationResult
    = Accept       -- valid, propagate to mesh
    | Reject       -- invalid, penalize sender
    | Ignore       -- seen before or irrelevant, don't propagate
    deriving (Show, Eq)

-- Validate incoming gossip message
validateGossipMessage :: BeaconState -> Store -> GossipMessage -> IO ValidationResult

-- Handle validated message (apply to state/store)
handleGossipMessage :: TVar BeaconState -> TVar Store -> GossipMessage -> IO ()

-- Start gossip message processing loop
startMessageHandler :: P2PNode -> TVar BeaconState -> TVar Store -> IO ()
-- Subscribes to all topics, routes messages to handler
```

### Algorithm description

**Block validation** (`GossipBlock`):
1. Check block slot is not too far in the future (≤ current slot + 1).
2. Check parent root exists in store.
3. Verify proposer signature.
4. Run `stateTransition` to verify block validity.
5. If valid: `onBlock` into store, update state, re-publish to mesh.

**Attestation validation** (`GossipAttestation`):
1. Check attestation slot is recent (within acceptable range).
2. Verify XMSS signature.
3. Check validator hasn't already attested for this slot (dedup).
4. If valid: `onAttestation` into store, forward to aggregator if running.

**Aggregation validation** (`GossipAggregation`):
1. Verify leanMultisig proof.
2. Check aggregation bits are consistent with known validator set.
3. If valid: store for inclusion in next block (if proposer).

**Deduplication**: Maintain a seen-message cache (bounded `Map` or LRU) keyed by message hash. `Ignore` duplicates.

### Tests (TDD)

- Valid block message → `Accept`
- Block with bad signature → `Reject`
- Block from too far in future → `Ignore`
- Valid attestation → `Accept`
- Duplicate attestation → `Ignore`
- Valid aggregation → `Accept`
- Aggregation with bad proof → `Reject`

### Completion criteria

- All three message types correctly validated
- Valid messages applied to state and store
- Invalid messages rejected with appropriate result
- Deduplication prevents processing same message twice

---

## Step 4: Request-Response Protocols (Sync)

### Files to modify

- `src/Network/P2P.hs` (add request-response handling)

### Types and signatures

```haskell
-- Respond to incoming requests
data RequestHandler = RequestHandler
    { rhBlocksByRange :: Slot -> Word64 -> IO [SignedBeaconBlock]
    , rhBlocksByRoot  :: [Root] -> IO [SignedBeaconBlock]
    , rhStatus        :: IO StatusMessage
    }

data StatusMessage = StatusMessage
    { smHeadSlot      :: Slot
    , smHeadRoot      :: Root
    , smFinalizedSlot :: Slot
    , smFinalizedRoot :: Root
    } deriving (Generic, Eq, Show)

-- Register request handlers
registerRequestHandlers :: P2PNode -> RequestHandler -> IO ()

-- Initial sync
initialSync :: P2PNode -> Store -> IO Store
-- Request blocks from peers to catch up to chain head

-- Sync strategy
data SyncStatus
    = Synced
    | Syncing { syncTarget :: Slot, syncProgress :: Slot }
    | SyncFailed String
    deriving (Show)
```

### Algorithm description

**`initialSync`**:
1. Request `status` from connected peers to learn their head slot.
2. Determine highest head slot among peers.
3. Request blocks by range from local finalized slot to peer head slot (in batches).
4. For each received block: validate and apply via `stateTransition` + `onBlock`.
5. Repeat until caught up.

**`beacon_blocks_by_range` handler**: Read from storage, return SSZ-encoded blocks for the requested range.

**`beacon_blocks_by_root` handler**: Look up blocks by root in store/storage, return found blocks.

### Tests (TDD)

- `initialSync` with a peer that has 10 more blocks → syncs all 10
- Request handler responds with correct blocks for range
- Request handler responds with correct blocks for roots
- Sync handles peer disconnection gracefully

### Completion criteria

- Node can sync from peers on startup
- Block-by-range and block-by-root request-response works
- Status exchange allows peers to compare chain state

---

## Step 5: Aggregator Role

### Files to create

- `src/Network/Aggregator.hs`
- `test/Test/Network/Aggregator.hs`

### Types and signatures

```haskell
module Network.Aggregator where

-- Aggregator state
data AggregatorState = AggregatorState
    { asProverCtx       :: ProverContext
    , asPendingAttestations :: TVar (Map AttestationData [SignedAttestation])
    , asSubnets         :: [SubnetId]  -- subnets this aggregator is responsible for
    , asCurrentSlot     :: TVar Slot
    }

-- Initialize aggregator
initAggregator :: ProverContext -> [SubnetId] -> IO AggregatorState

-- Receive an individual attestation (from gossip)
onIndividualAttestation :: AggregatorState -> SignedAttestation -> SubnetId -> IO ()
-- Validate and store in pending pool, keyed by AttestationData

-- Produce aggregated attestation (called at end of voting phase)
produceAggregation :: AggregatorState -> AttestationData -> [XmssPubkey]
                   -> IO (Either CryptoError SignedAggregatedAttestation)

-- Run aggregator duty for one slot
runAggregatorDuty :: AggregatorState -> P2PNode -> BeaconState -> IO ()
-- Collect attestations → aggregate → publish to 'aggregation' topic

-- Start aggregator loop
startAggregator :: AggregatorState -> P2PNode -> TVar BeaconState -> GenesisTime -> IO ()
-- Runs continuously: subscribe to attestation subnets, aggregate per slot, publish
```

### Algorithm description

**Attestation collection**:
1. Subscribe to `attestation_{subnet_id}` for each assigned subnet.
2. On receiving a `SignedAttestation`: verify signature, check subnet assignment, deduplicate.
3. Group by `AttestationData` in the pending pool.

**Aggregation duty** (triggered at ConfirmationPhase of each slot):
1. For each unique `AttestationData` in the pending pool:
   a. Collect all valid `SignedAttestation`s for that data.
   b. Call `aggregate` with the prover context and attestation list.
   c. Construct `Bitlist` from participating validator indices.
   d. Build `SignedAggregatedAttestation`.
2. Publish each aggregated attestation to the `aggregation` topic.
3. Clear the pending pool for this slot.

**Timing**: Aggregation must complete before the ViewMerge phase ends (within ~1600ms after voting phase closes). Monitor proof generation time; skip aggregation if it would exceed the deadline.

### Tests (TDD)

- Collect 4 attestations with same data → aggregate → verify → valid
- Collect attestations from wrong subnet → rejected
- Duplicate attestation → ignored (not double-counted in bitfield)
- Aggregation with 1 attestation → valid (single-signature proof)
- Aggregation timing: mock slow prover → skips aggregation gracefully
- Published aggregation can be deserialized by another node

### Completion criteria

- Aggregator collects attestations from assigned subnets
- Produces valid aggregated attestations with correct bitfields
- Publishes aggregations to gossipsub
- Handles edge cases: empty slot (no attestations), single attestation, duplicate attestations
- Respects slot timing constraints

---

## Step 6: End-to-End Network Integration

### Files to create

- `test/Test/Network/Integration.hs`

### Test scenarios

1. **Two-node gossip**: Node A proposes block → Node B receives via gossipsub
2. **Attestation flow**: Validator node signs attestation → publishes to subnet → Aggregator node receives
3. **Aggregation flow**: Aggregator collects attestations → publishes aggregation → Proposer node receives
4. **Full 3-slot finality over network**: 4 validator nodes + 1 aggregator → finalization observed
5. **Sync**: New node joins, syncs from existing peer, reaches same head

### Completion criteria

- Blocks propagate between nodes via gossipsub
- Attestations reach aggregators via subnet topics
- Aggregated attestations reach proposers via aggregation topic
- Initial sync works when a new node joins

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| rust-libp2p C ABI wrapper is complex to build | Delays entire phase | Start with IPC sidecar (stdin/stdout JSON-RPC) as fallback |
| Callback threading issues (Rust → Haskell) | Crashes, deadlocks | Use TQueue callback pattern; never block in callback |
| Snappy compression incompatibility | Message decoding failures | Test with messages from ethlambda reference client |
| discv5 peer discovery fails on devnet | Cannot find peers | Support manual bootnode configuration via CLI |
| Aggregation too slow for slot timing | Missed aggregation deadlines | Profile prover; run on dedicated core; implement timeout-based skip |
| Gossipsub mesh instability with few peers | Message delivery failures | Tune mesh parameters; add peer scoring |

---

## Exit Criteria

- [ ] rust-libp2p wrapper builds and links into Haskell project
- [ ] P2P node can create, start, and stop without errors
- [ ] Gossipsub: can subscribe to topics and receive messages
- [ ] Gossipsub: can publish messages that other nodes receive
- [ ] Snappy compression/decompression integrated
- [ ] Block gossip: receive and validate blocks from peers
- [ ] Attestation gossip: receive and validate individual attestations
- [ ] Aggregation gossip: receive and validate aggregated attestations
- [ ] Request-response: blocks by range and blocks by root
- [ ] Initial sync: node can catch up to chain head from peers
- [ ] Aggregator: collects attestations from subnets
- [ ] Aggregator: produces valid leanMultisig aggregation proofs
- [ ] Aggregator: publishes aggregations to gossipsub
- [ ] Aggregator: respects slot timing constraints
- [ ] `--is-aggregator` CLI flag enables aggregator mode
- [ ] Two-node integration test passes
