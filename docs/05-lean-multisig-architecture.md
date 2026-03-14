# leanMultisig Architecture

## Purpose

Since XMSS signatures cannot be algebraically aggregated like BLS, Lean Consensus uses a **zkVM-based approach** to aggregate signatures. leanMultisig replaces N individual XMSS signatures with a single constant-size zero-knowledge proof.

## 5-Layer Stack

```
┌─────────────────────────────────────────────┐
│  Application (CLI)                          │
│  - xmss_aggregate(), xmss_verify_aggregation()
├─────────────────────────────────────────────┤
│  Aggregation                                │
│  - Recursive proof composition (2→1)        │
│  - Infinite signature sets → 1 proof        │
├─────────────────────────────────────────────┤
│  Proving System                             │
│  - WHIR polynomial commitments              │
│  - AIR constraints                          │
│  - Logup argument                           │
├─────────────────────────────────────────────┤
│  Compilation & Execution                    │
│  - zkDSL compiler → leanVM bytecode         │
│  - Deterministic execution trace            │
├─────────────────────────────────────────────┤
│  Crypto Backend                             │
│  - KoalaBear prime field                    │
│  - XMSS, Poseidon2                          │
└─────────────────────────────────────────────┘
```

## KoalaBear Prime Field

All arithmetic operates over the **KoalaBear prime**:

```
p = 2^31 - 2^24 + 1 = 2,130,706,433
```

Properties:
- Efficient modular arithmetic on 32-bit hardware
- Two-adic structure enables fast NTT (Number Theoretic Transform)
- Used by the proving system for polynomial operations

## Aggregation Flow

### Step 1: Individual Signature Verification

```
N XMSS signatures → zkDSL program verifies each → execution trace
```

Each signature verification is compiled into a zkDSL program that:
- Validates the Winternitz hash chains
- Verifies the Merkle authentication path
- Checks the message binding

### Step 2: ZK Proof Generation

```
Execution trace → AIR constraints → WHIR proof
```

The execution trace is converted into an algebraic intermediate representation (AIR), then proven using the WHIR polynomial commitment scheme.

### Step 3: Recursive Composition (2→1)

```
Proof A + Proof B → Single combined proof
```

Two proofs are recursively combined into one. This is applied in a tree structure:
- N proofs → N/2 proofs → N/4 proofs → ... → 1 proof
- The final proof attests to the validity of all N original signatures
- Proof size remains constant regardless of N

## Performance Benchmarks (M4 Max, CPU-only)

| Operation | Current | Target |
|-----------|---------|--------|
| XMSS aggregation | 554 signatures/sec | 1,000 signatures/sec |
| 2→1 recursive proof | 1.35 sec | 0.25 sec |
| Proof size (proven regime) | ~380 KiB | — |
| Proof size (conjectured regime) | ~146 KiB | — |

## Security Parameters

| Parameter | Value |
|-----------|-------|
| Field | KoalaBear (p = 2^31 - 2^24 + 1) |
| Security level | 123.92 bits (target: 128 bits) |

### Two Security Regimes

1. **JohnsonBound (Proven)**
   - Security based on proven bounds for list-decodable codes
   - Larger proof size (~380 KiB)
   - Conservative, formally verified security

2. **CapacityBound (Conjectured)**
   - Security based on conjectured capacity bounds
   - Smaller proof size (~146 KiB)
   - Widely believed to be secure but not formally proven

## Main API

```
-- Proof generation
xmss_aggregate()               -- Generate aggregation proof from N signatures
setup_prover()                 -- Initialize prover context

-- Proof verification
xmss_verify_aggregation()      -- Verify an aggregation proof
setup_verifier()               -- Initialize verifier context

-- Compilation
compile_program_with_flags()   -- Compile zkDSL program to leanVM bytecode
execute_bytecode()             -- Execute bytecode and generate trace
```

## Implications for Haskell Implementation

- leanMultisig is implemented in Rust — Haskell clients will use **FFI bindings**
- The aggregation API is relatively simple (aggregate / verify)
- Prover setup is expensive; should be done once at startup
- The Aggregator role (see [07-devnet-3-spec.md](07-devnet-3-spec.md)) is responsible for running the prover
