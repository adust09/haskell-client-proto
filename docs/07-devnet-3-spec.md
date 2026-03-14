# pq-devnet-3 Specification

## Overview

pq-devnet-3 introduces the **separation of the Aggregator role** from block production. This is a critical architectural change that decouples attestation aggregation from the proposer, enabling better scalability and parallel processing.

## Key Change: Aggregator Role

In previous devnets, the block proposer was responsible for aggregating all attestations. In pq-devnet-3:

- **Aggregators** are dedicated nodes that collect and aggregate attestations
- **Proposers** receive pre-aggregated attestations and include them in blocks
- At least one node must run with `--is-aggregator` flag for finalization to work

## Gossipsub Topics

### `attestation_{subnet_id}`

- Carries individual `SignedAttestation` messages
- Validators are assigned to attestation subnets
- Each subnet covers a partition of the validator set

### `aggregation`

- Carries `SignedAggregatedAttestation` messages
- Aggregators publish to this topic after collecting and aggregating attestations from their subnet

## Message Types

### SignedAttestation

Individual validator attestation containing:
- Slot number
- Head vote (beacon block root)
- FFG vote (source checkpoint, target checkpoint)
- Validator index
- XMSS signature

### SignedAggregatedAttestation

Aggregated attestation containing:
- Slot number
- Head vote
- FFG vote
- Aggregation bitfield (which validators are included)
- leanMultisig proof (ZK proof of all included signatures)

## Attestation Flow

```
Step 1: Attest
  Validators → sign attestation → publish to attestation_{subnet_id}

Step 2: Aggregate
  Aggregators → collect attestations from subnet
              → run leanMultisig aggregation
              → publish SignedAggregatedAttestation to 'aggregation' topic

Step 3: Include in Block
  Proposer → collect SignedAggregatedAttestations from 'aggregation' topic
           → include in block body
           → broadcast block
```

```
┌───────────┐     attestation_{subnet_id}     ┌────────────┐
│ Validator  │ ──────────────────────────────→ │ Aggregator │
│ (attester) │                                 │            │
└───────────┘                                  └─────┬──────┘
                                                     │
                                               aggregation topic
                                                     │
                                                     ▼
                                               ┌────────────┐
                                               │  Proposer   │
                                               │ (in block)  │
                                               └────────────┘
```

## Subnet Assignment

- Validators are deterministically assigned to subnets based on their index
- Subnet count is a devnet configuration parameter
- Each validator publishes attestations only to their assigned subnet
- Aggregators subscribe to one or more subnets

## Differences from pq-devnet-4

| Aspect | pq-devnet-3 | pq-devnet-4 |
|--------|-------------|-------------|
| Aggregation location | Aggregator nodes | Aggregator + Proposer |
| Recursive aggregation | No | Yes (leanVM) |
| In-block proof size | One proof per aggregator | One proof per block |
| Proof composition | Flat (single level) | Recursive (tree) |

In pq-devnet-3, the proposer includes multiple aggregated attestations (one per aggregator/subnet). In pq-devnet-4, the proposer further combines these into a single recursive proof, reducing block size.

## Operational Notes

- At least one node must run as an aggregator (`--is-aggregator` flag)
- Without an aggregator, attestations cannot be aggregated and finalization stalls
- Multiple aggregators can run for redundancy
- Aggregators require sufficient CPU for leanMultisig proof generation
