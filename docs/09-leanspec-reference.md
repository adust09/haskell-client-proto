# leanSpec Reference

## Overview

[leanSpec](https://github.com/leanEthereum/leanSpec) is the executable specification for Lean Consensus, written in Python. It serves as the authoritative reference for all client implementations.

## Repository Structure

```
src/lean_spec/
  └── subspecs/
      ├── poseidon2/       — Poseidon2 hash function specification
      └── ...              — Other sub-specifications
tests/
  ├── consensus/
  │   └── devnet/
  │       ├── fc/                  — Fork choice tests
  │       └── state_transition/    — State transition tests
  └── lean_spec/                   — Spec-level tests
```

## Key Artifacts

### `lean_consensus.pdf`
- The **formal specification** document
- Defines all data structures, state transitions, and protocol rules
- Primary reference for implementers (alongside the Python code)

### Python Executable Specification
- Located in `src/lean_spec/`
- Can be executed directly to validate behavior
- Serves as the "ground truth" for protocol semantics

### Test Vectors
- Generated from the executable spec
- Located in `tests/consensus/devnet/`
- Cover fork choice and state transition logic

## Test Vector Generation

```bash
# Generate test vectors
uv run fill --clean --fork=devnet

# With specific signature scheme
uv run fill --clean --fork=devnet --scheme=test   # Test signatures (fast)
uv run fill --clean --fork=devnet --scheme=prod   # Production XMSS signatures
```

### Signature Schemes
- `--scheme=test`: Uses simplified test signatures for fast iteration
- `--scheme=prod`: Uses real XMSS signatures (slower but production-accurate)

## CI Integration Pattern

ethlambda and other clients integrate leanSpec test vectors into their CI pipelines:

1. **Fetch test vectors** from leanSpec (or generate locally)
2. **Deserialize** test fixtures (SSZ-encoded states and blocks)
3. **Execute** state transitions / fork choice operations
4. **Compare** output states against expected results

```
leanSpec test vectors → SSZ decode → client state transition → assert match
```

### Recommended Approach for Haskell

1. Add leanSpec as a git submodule or download test vectors in CI
2. Implement SSZ deserialization for test fixture types
3. Run state transition tests against vectors
4. Run fork choice tests against vectors
5. Use `--scheme=test` for fast CI, `--scheme=prod` for periodic full validation

## What to Use from leanSpec

| Artifact | Usage |
|----------|-------|
| `lean_consensus.pdf` | Primary specification reference |
| Python spec source | Clarify ambiguities, understand edge cases |
| Fork choice test vectors | Validate fork choice implementation |
| State transition test vectors | Validate state transition implementation |
| Devnet configuration parameters | Network/consensus config values |
| Sub-specifications (Poseidon2, etc.) | Crypto implementation details |
