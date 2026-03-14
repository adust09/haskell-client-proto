# 3SF-mini (Devnet Implementation)

## Overview

3SF-mini is the simplified implementation of the 3-Slot Finality protocol designed for devnet testing. It captures the core finality mechanism while omitting some features of the full protocol.

## Slot Structure (4 Phases)

Each slot has duration 5Δ (where Δ = network delay bound):

```
Phase 1: Block Proposal      (1Δ)
Phase 2: Head/FFG Voting      (2Δ)  — Head vote + FFG vote simultaneously
Phase 3: Fast Confirmation    (1Δ)
Phase 4: View Merging         (1Δ)
```

With Δ = 800ms, each slot is 4 seconds.

## Finality in 3 Slots

### Slot s — Initial Voting

1. Proposer broadcasts block B
2. Validators cast two simultaneous votes:
   - **Head Vote**: Points to block B (the head of their local chain)
   - **FFG Vote**: Source = last finalized checkpoint, Target = an ancestor of B

### Slot s+1 — Justification

1. New proposer includes the FFG votes from slot s in their block
2. If the target from slot s received ≥2/3 votes, that checkpoint becomes **justified**
3. Validators cast new FFG votes with:
   - Source = the newly justified checkpoint
   - Target = block B (or a descendant)

### Slot s+2 — Finalization

1. New proposer includes the FFG votes from slot s+1
2. Block B is now the **greatest justified checkpoint**
3. With ≥2/3 FFG votes targeting B's descendant:
   - The FFG finalization rule is satisfied
   - Block B becomes **finalized**

```
Slot s          Slot s+1         Slot s+2
┌──────────┐    ┌──────────┐    ┌──────────┐
│ Propose B │    │ Propose  │    │ Propose  │
│ Vote B    │───→│ Justify  │───→│ Finalize │
│ FFG vote  │    │ FFG vote │    │ B final! │
└──────────┘    └──────────┘    └──────────┘
```

## Key Differences from Casper FFG

| Aspect | Casper FFG | 3SF-mini |
|--------|-----------|----------|
| Granularity | Epoch-level (32 slots) | Per-block |
| Finality time | ~15 minutes (2 epochs) | ~12 seconds (3 slots) |
| Vote phases | Separate attestation duties | Head + FFG combined in one phase |
| Target flexibility | Fixed epoch boundary | Any block on the same chain |
| Justification | Epoch boundary checkpoints | Individual block checkpoints |

## Vote Types

### Head Vote
- Points to the validator's view of the chain head
- Used for fork choice (analogous to LMD-GHOST attestations)
- Ephemeral — only the latest vote counts

### FFG Vote
- A (source, target) pair for the finality gadget
- **Source**: The last justified checkpoint known to the validator
- **Target**: A block the validator wants to justify/finalize
- Accumulated across slots to reach the 2/3 threshold

## Slashing Conditions

Validators are slashed for violating BFT safety rules:

1. **Double voting**: Casting two different FFG votes in the same slot
2. **Surround voting**: Casting an FFG vote that "surrounds" a previous vote
   - Vote (s1, t1) surrounds (s2, t2) if s1 < s2 < t2 < t1

These are the same fundamental conditions as Casper FFG, adapted for per-slot operation.

## Differences from Full 3SF

3SF-mini simplifies the full protocol by:

- Combining the 3-layer architecture into a single unified protocol
- Using a simplified view-merge phase instead of the full Synchronizer protocol
- Not implementing the full ARFG asynchrony-resilience features
- Suitable for devnet testing while the full protocol undergoes formal verification
