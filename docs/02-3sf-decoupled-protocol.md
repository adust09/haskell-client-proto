# 3-Slot Finality: Decoupled Protocol

## Overview

3-Slot Finality (3SF) achieves economic finality in ~12 seconds (3 slots × 4 seconds) through a **3-layer decoupled architecture**. Each layer runs an independent protocol with different trust assumptions and confirmation speeds.

## Three-Layer Architecture

```
Layer 3: ChFin  (Finalized Chain)  — BFT protocol → Economic finality
Layer 2: ChMaj  (Majority Chain)   — RLMD → Majority-vote justified chain
Layer 1: ChFast (Available Chain)  — Goldfish (GHOST-Eph) → Low-latency block production
```

### Layer 1: ChFast (Available Chain)

- **Protocol**: Goldfish (GHOST-Eph — ephemeral fork choice)
- **Behavior**: Only considers votes from the immediately preceding slot
- **Properties**: High throughput, low latency, but weaker consistency guarantees
- **Analogy**: "Optimistic" view — accepts blocks quickly, may reorg

### Layer 2: ChMaj (Majority Chain)

- **Protocol**: RLMD (Refined Latest Message Driven)
- **Behavior**: Filters ChFast by requiring >50% validator vote weight
- **Properties**: Produces a "justified" chain that is more stable than ChFast
- **Analogy**: "Cautious" view — only acknowledges blocks with majority support

### Layer 3: ChFin (Finalized Chain)

- **Protocol**: BFT (Byzantine Fault Tolerant) consensus
- **Behavior**: Applies slashing conditions to provide economic finality
- **Properties**: Irreversible under economic assumptions (1/3 stake slashed to revert)
- **Analogy**: "Settlement" — finalized blocks cannot be reverted without catastrophic cost

## How the Layers Interact

```
Blocks proposed → ChFast (immediate availability)
                    ↓ filter by >50% votes
                  ChMaj (justified)
                    ↓ BFT finalization
                  ChFin (finalized)
```

Each layer provides a progressively stronger guarantee:
1. **ChFast**: "This block is available and valid" (reorgable)
2. **ChMaj**: "A majority of validators support this chain" (unlikely to reorg)
3. **ChFin**: "Reverting this requires slashing ≥1/3 of total stake" (economic finality)

## Synchronizer Role

A new role distinct from block proposers:

- **Does not produce blocks** — only broadcasts view-synchronization messages
- **Purpose**: Helps validators converge on a consistent view of the chain
- **Key for recovery**: Enables rapid resynchronization after periods of asynchrony
- **Separation of concerns**: Block production is decoupled from network coordination

## ARFG (Asynchrony-Resilient Finality Gadget)

The finality gadget used in ChFin is designed to be resilient to network asynchrony:

- During asynchronous periods, finality may stall but safety is never violated
- Upon return to synchrony, the Synchronizer role helps validators quickly reconverge
- Combines the best properties of Casper FFG (accountable safety) with faster finalization

## Key Design Principles

1. **Separation of concerns**: Each layer handles one aspect (availability, justification, finality)
2. **Graceful degradation**: If finality stalls, ChFast still produces blocks
3. **Independent analysis**: Each protocol layer can be proven correct in isolation
4. **Composability**: Security proofs compose across layers
