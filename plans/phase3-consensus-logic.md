# Phase 3: Consensus Logic — State Transition + Fork Choice (3SF-mini)

## Goal

Implement the 3SF-mini consensus protocol: per-slot/block state transitions and fork choice rule. By end of phase, the client can process a sequence of blocks, track the canonical chain head, and achieve justification/finalization within 3 slots.

## Prerequisites

- **Phase 1 complete**: All SSZ types, Merkleization, consensus types
- **Phase 2 complete**: XMSS sign/verify, leanMultisig aggregate/verify
- **Reference**: leanSpec Python spec, ethlambda `state_transition` and `fork_choice` crates
- **Dependencies**: `stm`, `async`, `containers`

---

## Step 1: State Transition — Per-Slot Processing

### Files to create

- `src/Consensus/StateTransition.hs`
- `test/Test/Consensus/StateTransition.hs`

### Types and signatures

```haskell
module Consensus.StateTransition where

-- Error type for state transition failures
data StateTransitionError
    = SlotTooOld { currentSlot :: Slot, blockSlot :: Slot }
    | InvalidProposer { expected :: ValidatorIndex, actual :: ValidatorIndex }
    | InvalidParentRoot { expected :: Root, actual :: Root }
    | InvalidStateRoot { expected :: Root, actual :: Root }
    | InvalidSignature
    | InvalidAttestation String
    | DuplicateAttestation ValidatorIndex
    | SlashableVote ValidatorIndex
    | InvalidBlockBody String
    deriving (Show, Eq)

-- Per-slot state update (called every 4 seconds, even without a block)
processSlot :: BeaconState -> BeaconState
-- Increment slot, update block/state root history

-- Per-slot pipeline (advance state from current slot to target slot)
processSlots :: BeaconState -> Slot -> Either StateTransitionError BeaconState
-- Repeatedly apply processSlot until state.slot == targetSlot

-- Full block processing
processBlock :: BeaconState -> SignedBeaconBlock -> Either StateTransitionError BeaconState

-- State transition entry point
stateTransition :: BeaconState -> SignedBeaconBlock -> Bool -> Either StateTransitionError BeaconState
-- state → block → validateSignatures? → newState
```

### Algorithm description

**`processSlot`** (called once per slot tick):
1. Cache current state root: `bsStateRoots[slot % SLOTS_PER_HISTORICAL_ROOT] = hashTreeRoot(state)`.
2. If there was a block in the previous slot, cache its root: `bsBlockRoots[slot % SLOTS_PER_HISTORICAL_ROOT] = hashTreeRoot(latestBlockHeader)`.
3. Increment `bsSlot`.

**`processSlots`**:
Loop `processSlot` until `bsSlot == targetSlot`. Error if `targetSlot <= bsSlot`.

**`stateTransition`**:
1. `processSlots state (bbSlot block)` — advance to block's slot.
2. `processBlock state block` — apply block contents.
3. If `validateSignatures`: verify block proposer signature.
4. Verify `bbStateRoot == hashTreeRoot(newState)`.

### Tests (TDD)

- `processSlot` increments slot by 1
- `processSlot` caches state root at correct index
- `processSlots` advances multiple slots
- `processSlots` rejects target slot in the past
- `stateTransition` with a valid block produces expected new state

### Completion criteria

- Per-slot processing updates root caches correctly
- Multi-slot advancement works
- State transition pipeline connects slot processing to block processing

---

## Step 2: State Transition — Block Processing

### Files to modify

- `src/Consensus/StateTransition.hs`

### Types and signatures

```haskell
-- Block processing sub-functions
processBlockHeader :: BeaconState -> BeaconBlock -> Either StateTransitionError BeaconState
-- Validate and record block header

processAttestations :: BeaconState -> [SignedAggregatedAttestation]
                    -> Either StateTransitionError BeaconState
-- Process all attestations in the block body

processAttestation :: BeaconState -> SignedAggregatedAttestation
                   -> Either StateTransitionError BeaconState
-- Process a single aggregated attestation

-- Proposer selection
getProposerIndex :: BeaconState -> ValidatorIndex
-- Deterministic proposer selection for the current slot

-- Committee assignment
getAttestationSubnet :: ValidatorIndex -> Word64 -> SubnetId
-- validatorIndex → totalSubnets → subnetId

-- Justification and finalization
processJustificationFinalization :: BeaconState -> BeaconState
-- Check if checkpoints can be justified/finalized based on accumulated votes
```

### Algorithm description

**`processBlockHeader`**:
1. Verify `bbSlot == bsSlot` (block is for current slot).
2. Verify `bbProposerIndex == getProposerIndex state`.
3. Verify `bbParentRoot == hashTreeRoot(bsLatestBlockHeader)`.
4. Update `bsLatestBlockHeader` with new header (state root left zeroed, filled after full processing).

**`processAttestation`** (for each `SignedAggregatedAttestation` in block body):
1. Verify `adSlot < bsSlot` (attestation is from a past slot, included in this block).
2. Verify source checkpoint matches state's justified checkpoint.
3. Verify target checkpoint is on the same chain as the block.
4. Verify aggregation proof via `verifyAggregatedAttestation`.
5. Update `bsCurrentAttestations` with the aggregated attestation.

**`processJustificationFinalization`** (3SF-mini finality):
1. Count FFG votes targeting each checkpoint.
2. If a checkpoint receives ≥ 2/3 of total effective balance in votes → **justify** it.
3. If the justified checkpoint's source was also justified (forming a chain) → **finalize** the source.
4. Update `bsJustifiedCheckpoint` and `bsFinalizedCheckpoint`.

**`getProposerIndex`**: Deterministic function of `(bsSlot, bsValidators)`. Simple modular selection for devnet: `slot % numActiveValidators`. (Full spec uses RANDAO-based shuffle.)

**`getAttestationSubnet`**: `validatorIndex % totalSubnets`. Deterministic assignment for devnet.

### Tests (TDD)

- `processBlockHeader` accepts valid header, rejects wrong slot/proposer/parent
- `processAttestation` accepts valid aggregated attestation
- `processAttestation` rejects attestation with wrong source checkpoint
- `processAttestation` rejects attestation from future slot
- `processJustificationFinalization`: 2/3 votes → justification
- `processJustificationFinalization`: justified chain → finalization
- `getProposerIndex` is deterministic for same state
- Full block processing: construct valid block, process, verify state change

### Completion criteria

- Block header validation catches all invalid cases
- Attestation processing updates state correctly
- Justification/finalization logic follows 3SF-mini rules
- Proposer selection is deterministic

---

## Step 3: State Transition — Slashing Conditions

### Files to modify

- `src/Consensus/StateTransition.hs`

### Types and signatures

```haskell
-- Slashing detection
data SlashingEvidence
    = DoubleVote
        { svSlot :: Slot
        , svValidator :: ValidatorIndex
        , svVote1 :: AttestationData
        , svVote2 :: AttestationData
        }
    | SurroundVote
        { svValidator :: ValidatorIndex
        , svInner :: (Checkpoint, Checkpoint)  -- (source, target) of surrounded vote
        , svOuter :: (Checkpoint, Checkpoint)  -- (source, target) of surrounding vote
        }
    deriving (Show, Eq)

-- Check for slashable behavior
checkSlashingConditions :: BeaconState -> SignedAttestation -> Either SlashingEvidence ()

-- Apply slashing to a validator
slashValidator :: BeaconState -> ValidatorIndex -> BeaconState
-- Set vSlashed = True, reduce effective balance
```

### Algorithm description

**Double voting**: A validator casts two different FFG votes in the same slot. Detected by checking if the validator already has an attestation in the current slot's attestation pool with different data.

**Surround voting**: Vote `(s1, t1)` surrounds `(s2, t2)` if `s1 < s2 && t2 < t1`. Detected by comparing against the validator's historical votes.

**`slashValidator`**: Set `vSlashed = True`, apply balance penalty (reduce effective balance). In devnet, the exact penalty formula may be simplified.

### Tests (TDD)

- Same validator, same slot, different attestation data → `DoubleVote`
- Vote (1, 10) with existing vote (3, 7) → `SurroundVote`
- Vote (3, 7) with existing vote (1, 10) → `SurroundVote`
- Non-overlapping votes → no slashing
- `slashValidator` sets `vSlashed = True` and reduces balance

### Completion criteria

- Both slashing conditions correctly detected
- Slashing updates validator state
- Non-slashable votes pass through cleanly

---

## Step 4: Fork Choice — Store and Head Selection

### Files to create

- `src/Consensus/ForkChoice.hs`
- `test/Test/Consensus/ForkChoice.hs`

### Types and signatures

```haskell
module Consensus.ForkChoice where

-- Initialize store from genesis
initStore :: BeaconState -> BeaconBlock -> Store

-- Process a new block into the store
onBlock :: Store -> SignedBeaconBlock -> BeaconState -> Either ForkChoiceError Store
-- Validate block, add to store, update checkpoints

-- Process an individual attestation (head vote)
onAttestation :: Store -> SignedAttestation -> Either ForkChoiceError Store
-- Update latest message for the validator

-- Get the current chain head
getHead :: Store -> Root
-- 3SF-mini fork choice: filter by finality, then highest weight

-- Get ancestors
getAncestor :: Store -> Root -> Slot -> Maybe Root
-- Walk parent chain to find ancestor at given slot

-- Vote weight for a block
getWeight :: Store -> Root -> Gwei
-- Sum effective balances of validators whose latest message supports this root

-- Fork choice error
data ForkChoiceError
    = BlockNotFound Root
    | InvalidBlock String
    | FutureBlock Slot Slot  -- blockSlot, currentSlot
    deriving (Show, Eq)
```

### Algorithm description

**`initStore`**: Create store from genesis state and block. Set justified and finalized checkpoints to genesis. Add genesis block and state to maps.

**`onBlock`**:
1. Verify block's slot is not in the future (`bbSlot <= stCurrentSlot`).
2. Verify parent root exists in `stBlocks`.
3. Run `stateTransition` to get the post-state.
4. Add block and post-state to store maps.
5. If the post-state's justified checkpoint is newer → update `stJustifiedCheckpoint`.
6. If the post-state's finalized checkpoint is newer → update `stFinalizedCheckpoint`.

**`onAttestation`**:
1. Verify attestation target is a descendant of the finalized checkpoint.
2. Verify attestation slot is not in the future.
3. Update `stLatestMessages[validatorIndex]` if this attestation is newer (higher slot).

**`getHead`** (3SF-mini head selection):
1. Start from justified checkpoint root.
2. Get all children of the current node.
3. Filter children: only those whose subtree includes the finalized checkpoint.
4. Among remaining children, select the one with highest total vote weight.
5. Repeat until reaching a leaf (no children) — that's the head.

**`getWeight`**: For each validator, if their latest message points to a descendant of `root`, add their effective balance to the weight. Ties broken by higher root hash (lexicographic).

**`getAncestor`**: Follow parent chain from `root` until finding a block at `targetSlot`. Return `Nothing` if no ancestor at that slot (shouldn't happen in a valid chain).

### Tests (TDD)

- Genesis-only store: `getHead` returns genesis root
- Add one block: `getHead` returns new block root
- Fork: two blocks at same slot → `getHead` returns the one with more weight
- Attestation updates `latestMessages` and changes head
- Justification update: new justified checkpoint propagated
- Finalization: finalized checkpoint updated after 3-slot sequence
- `getAncestor` finds correct ancestor at each depth
- `onBlock` rejects future blocks
- `onBlock` rejects blocks with unknown parent

### Completion criteria

- Fork choice correctly selects chain head
- Vote weight calculation is correct
- Justification and finalization updates propagate through store
- 3-slot finality works end-to-end in tests

---

## Step 5: Fork Choice — 3-Slot Finality Integration Test

### Files to create

- `test/Test/Consensus/Integration.hs`

### Types and signatures

```haskell
-- Test helpers
mkGenesisState :: Int -> BeaconState
-- Create genesis with N validators

mkBlock :: BeaconState -> [SignedAggregatedAttestation] -> SignedBeaconBlock
-- Create a valid block for the current slot with given attestations

mkAttestation :: ValidatorIndex -> AttestationData -> XmssSignature -> SignedAttestation
-- Create a signed attestation

-- Integration scenario
test3SlotFinality :: TestTree
-- Simulate 3 slots with ≥2/3 votes → verify finalization
```

### Algorithm description

**3-slot finality integration test**:
1. Create genesis state with 4 validators.
2. **Slot 1**: Proposer creates block B1. All 4 validators vote (head = B1, FFG source = genesis, target = B1).
3. **Slot 2**: Proposer creates block B2 including aggregated votes from slot 1. B1's checkpoint receives 4/4 votes → **justified**. Validators vote (source = B1 justified, target = B2).
4. **Slot 3**: Proposer creates block B3 including aggregated votes from slot 2. B2's checkpoint justified. B1's checkpoint **finalized** (justified checkpoint with justified descendant).
5. Verify: `stFinalizedCheckpoint` points to B1's checkpoint.

This test exercises the full stack: SSZ serialization, crypto (or mocked crypto for speed), state transitions, fork choice, and finality.

### Tests (TDD)

- 3-slot finality with 100% participation → finalization
- 3-slot finality with exactly 2/3 participation → finalization
- 3-slot finality with < 2/3 participation → no finalization
- Fork scenario: two competing chains, votes split → correct head selection
- Validator slashed mid-sequence → reduced vote weight

### Completion criteria

- End-to-end 3-slot finality works in test
- Finalization threshold (2/3) is exact
- Fork resolution works correctly
- All state transition + fork choice components work together

---

## Step 6: Slot Phase Timing

### Files to create

- `src/Consensus/SlotTimer.hs`

### Types and signatures

```haskell
module Consensus.SlotTimer where

-- Slot phase within the 4-second slot
data SlotPhase
    = ProposalPhase      -- 0..800ms (1Δ)
    | VotingPhase        -- 800..2400ms (2Δ)
    | ConfirmationPhase  -- 2400..3200ms (1Δ)
    | ViewMergePhase     -- 3200..4000ms (1Δ)
    deriving (Show, Eq, Ord, Enum, Bounded)

-- Get current slot and phase from wall clock
getCurrentSlotPhase :: UTCTime -> GenesisTime -> (Slot, SlotPhase)

-- Wait until a specific phase of the current slot
waitUntilPhase :: GenesisTime -> SlotPhase -> IO ()

-- Slot ticker: fires at the start of each slot
slotTicker :: GenesisTime -> (Slot -> IO ()) -> IO ()
-- Runs callback at the beginning of each new slot
```

### Algorithm description

**Slot calculation**: `slot = floor((now - genesisTime) / slotDuration)`. Phase within slot determined by fractional position:
- `[0, Δ)` → Proposal (800ms)
- `[Δ, 3Δ)` → Voting (1600ms)
- `[3Δ, 4Δ)` → Confirmation (800ms)
- `[4Δ, 5Δ)` → View Merge (800ms)

Where Δ = 800ms and total slot = 5Δ = 4000ms.

**`slotTicker`**: Uses `threadDelay` to sleep until next slot boundary, then invokes callback. Accounts for clock drift by recalculating sleep time each iteration.

### Tests (TDD)

- `getCurrentSlotPhase` at genesis time → slot 0, ProposalPhase
- `getCurrentSlotPhase` at genesis + 1ms → slot 0, ProposalPhase
- `getCurrentSlotPhase` at genesis + 800ms → slot 0, VotingPhase
- `getCurrentSlotPhase` at genesis + 4000ms → slot 1, ProposalPhase
- Phase boundaries are exact (no off-by-one)

### Completion criteria

- Slot/phase calculation is correct for all boundary conditions
- Slot ticker fires at correct intervals
- Phase timing matches 3SF-mini spec (5Δ per slot)

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| leanSpec Python spec differs from our understanding of 3SF-mini | Incorrect state transition logic | Add leanSpec as git submodule; cross-reference each function |
| Fork choice edge cases (reorgs, long forks) | Consensus failures | Port ethlambda's fork choice tests; add extensive property tests |
| Justification/finalization logic is subtle | Silent bugs | Test with exact threshold values (2/3 ± 1 validator) |
| State transition performance with large validator sets | Missed slot deadlines | Profile with realistic validator counts; optimize hot paths |
| Clock drift affects slot phase timing | Phase violations (voting too early/late) | Use NTP-synced clocks; add configurable phase offsets |
| Crypto verification bottleneck in block processing | Slow block processing | Batch verify signatures; parallelize attestation checks |

---

## Exit Criteria

- [ ] `processSlot` correctly updates state root and block root caches
- [ ] `processBlock` validates and applies blocks (header, body, attestations)
- [ ] `processAttestation` validates aggregated attestations with crypto verification
- [ ] Slashing conditions detected: double voting and surround voting
- [ ] `getProposerIndex` deterministically selects proposers
- [ ] `initStore` creates valid store from genesis
- [ ] `onBlock` adds blocks to store and updates checkpoints
- [ ] `onAttestation` updates latest messages for fork choice
- [ ] `getHead` selects correct chain head using vote weights
- [ ] 3-slot finality integration test passes (justification + finalization)
- [ ] Fork resolution works correctly (competing chains, vote splitting)
- [ ] Slot phase timing matches 3SF-mini spec
- [ ] All functions tested against leanSpec test vectors (when available)
