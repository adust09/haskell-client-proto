# Phase 5: Infrastructure — RocksDB, Genesis, Actor Model, Main Loop, Devnet Integration

## Goal

Wire all components into a running node: persistent storage, genesis loading, actor-based concurrency, CLI, metrics, and full integration with pq-devnet-3. By end of phase, the client syncs with the devnet, participates in consensus (submitting valid attestations), and observes finalization within 3 slots.

## Prerequisites

- **Phases 1-4 complete**: SSZ, crypto, consensus, networking all working
- **External**: pq-devnet-3 genesis config (YAML/JSON from leanEthereum pm repo)
- **External**: pq-devnet-3 bootnode multiaddrs
- **Dependencies**: `rocksdb-haskell`, `aeson`, `yaml`, `optparse-applicative`, `prometheus-client`, `async`, `stm`

---

## Step 1: Persistent Storage — RocksDB + TVar Cache

### Files to create

- `src/Storage.hs`
- `test/Test/Storage.hs`

### Types and signatures

```haskell
module Storage where

import Database.RocksDB (DB)
import Control.Concurrent.STM (TVar)

-- Storage handle combining hot cache and cold store
data StorageHandle = StorageHandle
    { shDB           :: DB                           -- RocksDB handle
    , shHotState     :: TVar BeaconState             -- current state (fast access)
    , shHotStore     :: TVar Store                   -- fork choice store
    , shRecentBlocks :: TVar (Map Root SignedBeaconBlock)  -- recent block cache
    }

-- Initialize storage
openStorage :: FilePath -> IO StorageHandle
-- Open/create RocksDB database at path, initialize TVars

closeStorage :: StorageHandle -> IO ()

-- Block operations
putBlock :: StorageHandle -> Root -> SignedBeaconBlock -> IO ()
-- Write to both TVar cache and RocksDB

getBlock :: StorageHandle -> Root -> IO (Maybe SignedBeaconBlock)
-- Check TVar cache first, fall back to RocksDB

-- State operations
putState :: StorageHandle -> Root -> BeaconState -> IO ()
-- Persist state snapshot to RocksDB (for finalized states)

getState :: StorageHandle -> Root -> IO (Maybe BeaconState)

-- Finalized chain
getFinalizedBlock :: StorageHandle -> Slot -> IO (Maybe SignedBeaconBlock)
putFinalizedBlock :: StorageHandle -> Slot -> SignedBeaconBlock -> IO ()

-- Hot state access (STM)
readCurrentState :: StorageHandle -> STM BeaconState
writeCurrentState :: StorageHandle -> BeaconState -> STM ()

readForkChoiceStore :: StorageHandle -> STM Store
writeForkChoiceStore :: StorageHandle -> Store -> STM ()

-- Pruning
pruneOldBlocks :: StorageHandle -> Slot -> IO Int
-- Remove blocks older than finalized checkpoint from hot cache
```

### Algorithm description

**Data partitioning**:
- **Hot (TVar)**: Current `BeaconState`, `Store` (fork choice), recent unfinalized blocks. Accessed in STM transactions for atomicity within the blockchain actor.
- **Cold (RocksDB)**: All blocks indexed by root, finalized blocks indexed by slot, periodic state snapshots at finalized checkpoints.

**RocksDB key schema**:
- `b:{root}` → SSZ-encoded `SignedBeaconBlock`
- `s:{root}` → SSZ-encoded `BeaconState`
- `f:{slot}` → block root at finalized slot
- `meta:finalized_slot` → latest finalized slot
- `meta:head_root` → current head root

**Write path**: New blocks go to both TVar cache (immediate) and RocksDB (durable). State snapshots only written at finalized checkpoints (to avoid writing full state every slot).

**Read path**: Check TVar cache first (O(1) map lookup). On miss, read from RocksDB (disk I/O). Recent blocks are almost always in the hot cache.

**Pruning**: Periodically remove blocks older than the finalized checkpoint from the TVar cache (keep them in RocksDB). Prevents unbounded memory growth.

### Tests (TDD)

- `putBlock` → `getBlock` roundtrip
- `putState` → `getState` roundtrip
- Hot cache hit: block in TVar returns immediately
- Cold read: block only in RocksDB → still returned
- `pruneOldBlocks` removes old blocks from hot cache, keeps them in RocksDB
- Concurrent reads/writes don't corrupt data (STM atomicity test)
- `openStorage` → `closeStorage` → `openStorage` preserves data

### Completion criteria

- RocksDB opens, writes, reads, and closes without errors
- TVar cache provides fast access to hot state
- SSZ serialization used for all persisted data
- Pruning keeps memory bounded

---

## Step 2: Genesis & Configuration

### Files to create

- `src/Genesis.hs`
- `src/Config.hs`
- `test/Test/Genesis.hs`

### Types and signatures

```haskell
module Config where

-- Node configuration (from CLI + config file)
data NodeConfig = NodeConfig
    { ncDataDir         :: FilePath
    , ncListenPort      :: Word16
    , ncBootnodes       :: [String]
    , ncGenesisFile     :: FilePath
    , ncValidatorKeyDir :: Maybe FilePath  -- None for non-validator (beacon) node
    , ncIsAggregator    :: Bool
    , ncMetricsPort     :: Maybe Word16
    , ncLogLevel        :: LogLevel
    }

data LogLevel = Debug | Info | Warn | Error
    deriving (Show, Eq, Ord)

-- Parse from CLI arguments
parseConfig :: IO NodeConfig
-- Uses optparse-applicative

-- Parse from YAML config file (optional, CLI overrides)
loadConfigFile :: FilePath -> IO (Maybe NodeConfig)
```

```haskell
module Genesis where

-- Genesis state from config
data GenesisConfig = GenesisConfig
    { gcGenesisTime       :: UTCTime
    , gcValidators        :: [GenesisValidator]
    , gcForkVersion       :: Version
    , gcChainId           :: Word64
    , gcDepositRoot       :: Root
    } deriving (Generic)

data GenesisValidator = GenesisValidator
    { gvPubkey           :: XmssPubkey
    , gvBalance          :: Gwei
    , gvActivationSlot   :: Slot
    } deriving (Generic)

-- Load genesis from JSON/YAML file
loadGenesis :: FilePath -> IO (Either String GenesisConfig)

-- Initialize BeaconState from genesis config
initializeGenesisState :: GenesisConfig -> BeaconState

-- Initialize Store from genesis
initializeGenesisStore :: GenesisConfig -> BeaconState -> Store

-- Genesis block (slot 0, empty body)
genesisBlock :: BeaconBlock
```

### Algorithm description

**`loadGenesis`**: Parse the pq-devnet-3 genesis config file (YAML or JSON, format determined by leanEthereum pm repo). Extract genesis time, initial validator set, fork version, and chain ID. Use `aeson` for JSON or `yaml` for YAML.

**`initializeGenesisState`**:
1. Create empty `BeaconState` at slot 0.
2. Populate `bsValidators` from genesis validators (pubkey, balance, activation slot 0).
3. Populate `bsBalances` from genesis validator balances.
4. Set `bsJustifiedCheckpoint` and `bsFinalizedCheckpoint` to genesis checkpoint (slot 0, genesis block root).
5. Initialize `bsBlockRoots` and `bsStateRoots` vectors with zero roots.
6. Set `bsLatestBlockHeader` from genesis block.

**`initializeGenesisStore`**: Call `initStore` from `Consensus.ForkChoice` with genesis state and genesis block.

**CLI parsing** (via `optparse-applicative`):
```
lean-consensus [OPTIONS]
  --data-dir PATH          Data directory (default: ./data)
  --port PORT              Listen port (default: 9000)
  --bootnode MULTIADDR     Bootnode address (repeatable)
  --genesis FILE           Genesis config file
  --validator-keys DIR     Validator key directory (enables validator mode)
  --is-aggregator          Enable aggregator role
  --metrics-port PORT      Prometheus metrics port
  --log-level LEVEL        Log level: debug|info|warn|error
```

### Tests (TDD)

- Parse sample genesis config → correct `GenesisConfig`
- `initializeGenesisState` produces valid state (slot 0, correct validator count)
- `hashTreeRoot` of genesis state matches expected value
- Genesis block has slot 0 and empty body
- CLI parsing: all flags parsed correctly, defaults applied

### Completion criteria

- Genesis config loads from file
- Genesis state is correctly initialized
- Genesis store is correctly initialized
- CLI argument parsing works with all options
- Config file loading works as fallback

---

## Step 3: Actor Model — Structured Concurrency

### Files to create

- `src/Node.hs` — main node orchestration
- `src/Actor.hs` — actor primitives

### Types and signatures

```haskell
module Actor where

-- Generic actor with typed messages
data Actor msg = Actor
    { actorQueue  :: TQueue msg
    , actorThread :: Async ()
    , actorName   :: String
    }

-- Spawn an actor
spawnActor :: String -> (TQueue msg -> IO ()) -> IO (Actor msg)
-- Creates TQueue, forks thread, returns handle

-- Send message to actor
send :: Actor msg -> msg -> STM ()
-- Write to actor's TQueue

-- Stop actor gracefully
stopActor :: Actor msg -> IO ()
-- Cancel async, wait for cleanup

-- Wait for actor to finish (or crash)
waitActor :: Actor msg -> IO (Either SomeException ())
```

```haskell
module Node where

-- All actors that make up the node
data NodeActors = NodeActors
    { naBlockchain :: Actor BlockchainMsg
    , naP2P        :: Actor P2PMsg
    , naValidator  :: Maybe (Actor ValidatorMsg)  -- only if validator keys present
    , naAggregator :: Maybe (Actor AggregatorMsg) -- only if --is-aggregator
    , naRPC        :: Maybe (Actor RPCMsg)         -- only if --metrics-port set
    }

-- Message types for each actor
data BlockchainMsg
    = BcNewBlock SignedBeaconBlock
    | BcNewAttestation SignedAttestation
    | BcNewAggregation SignedAggregatedAttestation
    | BcSlotTick Slot
    | BcGetHead (TMVar Root)  -- request-response via TMVar

data P2PMsg
    = P2PPublishBlock SignedBeaconBlock
    | P2PPublishAttestation SignedAttestation SubnetId
    | P2PPublishAggregation SignedAggregatedAttestation
    | P2PRequestSync Slot Slot (TMVar [SignedBeaconBlock])

data ValidatorMsg
    = ValSlotTick Slot
    | ValNewHead Root BeaconState

data AggregatorMsg
    = AggNewAttestation SignedAttestation SubnetId
    | AggSlotTick Slot

-- Start all actors
startNode :: NodeConfig -> StorageHandle -> GenesisConfig -> IO NodeActors

-- Stop all actors gracefully
stopNode :: NodeActors -> IO ()

-- Run node until shutdown signal
runNode :: NodeActors -> IO ()
```

### Algorithm description

**Actor model**:
- Each actor owns its state (no shared mutable state between actors).
- Communication via `TQueue` messages (STM, non-blocking enqueue).
- Each actor is a lightweight GHC thread (`forkIO` via `async`).
- Structured concurrency: `startNode` spawns all actors; `stopNode` cancels all; `runNode` waits for any to exit (crash = restart or shutdown).

**Blockchain Actor** (central coordinator):
1. Receives blocks, attestations, aggregations from P2P.
2. Validates and applies to state/store.
3. On slot tick: advance state, check duties.
4. Notifies Validator actor of new head.

**P2P Actor**:
1. Manages libp2p node lifecycle.
2. Routes incoming gossip messages to Blockchain actor.
3. Handles publish requests from other actors.
4. Manages initial sync.

**Validator Actor** (if validator keys present):
1. On slot tick: check if this validator is proposer.
2. If proposer: build block, sign, send to P2P for publishing.
3. If attester: create attestation, sign, send to P2P for publishing to subnet.

**Aggregator Actor** (if `--is-aggregator`):
1. Collects attestations from Blockchain actor.
2. At ConfirmationPhase: run aggregation, send result to P2P for publishing.

### Tests (TDD)

- `spawnActor` creates actor that processes messages
- `send` → actor receives message
- `stopActor` terminates actor cleanly
- Two actors communicating via messages
- Actor crash is detected by `waitActor`

### Completion criteria

- Actor primitives work (spawn, send, stop, wait)
- All actor types instantiated
- Message routing between actors is correct
- Structured shutdown: all actors stop cleanly

---

## Step 4: Main Loop — Slot-Driven Event Processing

### Files to modify

- `app/Main.hs`
- `src/Node.hs`

### Types and signatures

```haskell
-- app/Main.hs
module Main where

main :: IO ()
-- 1. Parse CLI config
-- 2. Load genesis
-- 3. Open storage
-- 4. Initialize state from genesis (or resume from storage)
-- 5. Start all actors
-- 6. Run until shutdown (SIGINT/SIGTERM)
-- 7. Graceful shutdown: stop actors, close storage
```

```haskell
-- Blockchain actor main loop
blockchainLoop :: StorageHandle -> TQueue BlockchainMsg -> GenesisConfig -> IO ()

-- Validator duty scheduling
data ValidatorDuty
    = ProposeBlock Slot
    | AttestBlock Slot SubnetId
    | NoDuty
    deriving (Show, Eq)

getValidatorDuty :: BeaconState -> ValidatorIndex -> Slot -> ValidatorDuty
```

### Algorithm description

**Startup sequence**:
1. `parseConfig` → `NodeConfig`.
2. `loadGenesis genesisFile` → `GenesisConfig`.
3. `openStorage dataDir` → `StorageHandle`.
4. Check if storage has persisted state:
   - Yes: resume from latest finalized state.
   - No: `initializeGenesisState` + `initializeGenesisStore`.
5. `startNode config storage genesis` → `NodeActors`.
6. `runNode actors` — blocks until shutdown signal.
7. `stopNode actors` → `closeStorage storage`.

**Slot-driven event loop** (Blockchain actor):
```
for each slot tick:
  1. processSlots(state, newSlot)
  2. Notify Validator actor of new slot
  3. Process pending messages:
     - BcNewBlock: validate → stateTransition → onBlock → store
     - BcNewAttestation: validate → onAttestation → forward to Aggregator
     - BcNewAggregation: validate → store for block inclusion
  4. processJustificationFinalization
  5. If finalized checkpoint changed: persist to storage, prune hot cache
```

**Validator duty per slot**:
- Check `getProposerIndex(state)`: if this validator → `ProposeBlock`.
- Otherwise → `AttestBlock` on assigned subnet.
- Proposal happens at ProposalPhase; attestation at VotingPhase.

**Graceful shutdown**: Install SIGINT/SIGTERM handlers that send shutdown message to all actors. Each actor drains its queue, persists state, and exits.

### Tests (TDD)

- Full startup/shutdown cycle without crash
- Blockchain actor processes 3 slots correctly
- Validator duty: correct proposer/attester assignment
- Resume from persisted state matches fresh computation
- Graceful shutdown persists state

### Completion criteria

- Node starts from genesis and runs continuously
- Slot ticks drive state transitions
- Validator duties assigned and executed per slot
- Graceful shutdown preserves state

---

## Step 5: HTTP RPC API

### Files to create

- `src/Network/RPC.hs`
- `test/Test/Network/RPC.hs`

### Types and signatures

```haskell
module Network.RPC where

-- Minimal REST API for monitoring and debugging
-- Endpoints:
--   GET /eth/v1/node/health          → 200 if synced
--   GET /eth/v1/node/syncing         → SyncStatus JSON
--   GET /eth/v1/beacon/headers/head  → head block header JSON
--   GET /eth/v1/beacon/states/head/finality_checkpoints → checkpoints JSON
--   GET /eth/v1/node/peers           → connected peers JSON

data RPCServer = RPCServer
    { rpcPort    :: Word16
    , rpcStorage :: StorageHandle
    , rpcP2P     :: P2PNode
    }

startRPC :: RPCServer -> IO ()
stopRPC :: RPCServer -> IO ()
```

### Algorithm description

Use `warp` + `wai` for the HTTP server (standard Haskell web stack). Each endpoint reads from TVars (current state, store) or P2P node (peer info). All responses are JSON (`aeson`).

This is a minimal monitoring API — not the full Ethereum Beacon API. Just enough for devnet debugging and external tools to verify the node is working.

### Tests (TDD)

- `/eth/v1/node/health` returns 200 when synced
- `/eth/v1/node/syncing` returns correct sync status
- `/eth/v1/beacon/headers/head` returns valid JSON

### Completion criteria

- RPC server starts and responds to requests
- Endpoints return correct data from node state
- JSON responses are well-formed

---

## Step 6: Prometheus Metrics

### Files to create

- `src/Metrics.hs`
- `test/Test/Metrics.hs`

### Types and signatures

```haskell
module Metrics where

-- Metrics registry
data NodeMetrics = NodeMetrics
    { mSlotProcessed    :: Counter    -- total slots processed
    , mBlocksReceived   :: Counter    -- blocks received from gossip
    , mBlocksProposed   :: Counter    -- blocks proposed by this node
    , mAttestationsRecv :: Counter    -- individual attestations received
    , mAggregationsRecv :: Counter    -- aggregated attestations received
    , mHeadSlot         :: Gauge      -- current head slot
    , mFinalizedSlot    :: Gauge      -- latest finalized slot
    , mPeerCount        :: Gauge      -- connected peer count
    , mSlotProcessingMs :: Histogram  -- slot processing time
    , mSyncStatus       :: Gauge      -- 0=synced, 1=syncing
    }

-- Initialize metrics
initMetrics :: IO NodeMetrics

-- Start Prometheus HTTP exporter
startMetricsServer :: Word16 -> NodeMetrics -> IO ()

-- Convenience: instrument a slot processing action
withSlotTimer :: NodeMetrics -> IO a -> IO a
```

### Algorithm description

Use `prometheus-client` for metric definitions and HTTP export. Follows the leanMetrics standard naming conventions where applicable.

Metrics are updated by each actor:
- Blockchain actor: slot, block, attestation counters; head/finalized gauges
- P2P actor: peer count gauge
- Validator actor: blocks proposed counter

The metrics HTTP server runs on a dedicated port (separate from RPC).

### Tests (TDD)

- `initMetrics` creates all metrics without error
- Counter increment works
- Gauge set works
- Metrics server responds with Prometheus text format

### Completion criteria

- All metrics defined and initialized
- Metrics updated during normal operation
- Prometheus scrape endpoint works

---

## Step 7: Devnet Integration Testing

### Files to create

- `test/Test/Integration/Devnet.hs` (or run manually)

### Test scenarios

1. **Genesis sync**: Start node with devnet genesis config → syncs blocks from bootnode peers
2. **Block following**: Node receives and validates new blocks as they're proposed
3. **Attestation submission**: Validator node creates and publishes attestations that peers accept
4. **Aggregation**: Aggregator node collects attestations, produces proofs, publishes aggregations
5. **Finalization**: Node observes finalization within 3 slots of sufficient voting
6. **Restart recovery**: Stop node, restart from persisted state, resume participation
7. **Long-running stability**: Node runs for 100+ slots without crash or memory leak

### Operational checklist

- [ ] Connect to at least 1 pq-devnet-3 bootnode
- [ ] Peer count reaches gossipsub mesh size (≥8)
- [ ] Blocks received and validated from slot 1 onward
- [ ] If validator: attestations published and accepted by peers
- [ ] If aggregator: aggregations published and included in blocks
- [ ] Finalized slot advances as expected
- [ ] Prometheus metrics show correct values
- [ ] RPC health endpoint returns 200
- [ ] Node restarts cleanly from persisted state
- [ ] Memory usage stable over extended run (no unbounded growth)

### Completion criteria

- Node participates in pq-devnet-3 consensus
- Attestations from this node are included in blocks by other proposers
- Finalization observed within 3 slots

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| pq-devnet-3 genesis config format undocumented | Cannot initialize | Study ethlambda's genesis loading code; contact leanEthereum team |
| Devnet peers reject our messages (SSZ incompatibility) | Cannot participate | Test SSZ encoding against ethlambda output; use shared test vectors |
| RocksDB binding issues on target platform | No persistence | Fallback: in-memory only (acceptable for devnet testing) |
| Actor deadlock or message queue unbounded growth | Node hangs or OOMs | Bounded queues with backpressure; deadlock detection via async exceptions |
| GHC runtime pauses (GC) cause missed slot deadlines | Attestation/proposal too late | Tune GC settings (`+RTS -A64m -n4m`); use compact regions for large state |
| Devnet bootnodes unavailable | Cannot discover peers | Support manual peer addition; run local testnet for development |
| Memory leak over long runs | Node crashes after hours/days | Profile with `+RTS -hT`; test with 1000+ slots; monitor RSS |

---

## Exit Criteria

- [ ] RocksDB storage: blocks and states persist across restarts
- [ ] TVar hot cache: fast access to current state and fork choice store
- [ ] Genesis: load config from file, initialize correct genesis state
- [ ] CLI: all options parse correctly (`--data-dir`, `--port`, `--bootnode`, `--genesis`, `--validator-keys`, `--is-aggregator`, `--metrics-port`, `--log-level`)
- [ ] Actor model: all actors (Blockchain, P2P, Validator, Aggregator, RPC) spawn and communicate
- [ ] Main loop: slot-driven event processing runs continuously
- [ ] Validator duties: correct proposer/attester assignment and execution
- [ ] Graceful shutdown: state persisted, all actors stopped, no resource leaks
- [ ] RPC: health endpoint responds correctly
- [ ] Metrics: Prometheus endpoint exports all defined metrics
- [ ] Devnet: syncs with pq-devnet-3 from genesis
- [ ] Devnet: attestations accepted by peers
- [ ] Devnet: finalization observed within 3 slots
- [ ] Stability: runs for 100+ slots without crash
- [ ] Memory: no unbounded growth over extended runs
