# XMSS Signatures (Post-Quantum)

## Why Replace BLS?

BLS12-381, the signature scheme used by the current Beacon Chain, relies on elliptic curve pairings. These are vulnerable to quantum computers:

- **Shor's algorithm** can solve the elliptic curve discrete logarithm problem in polynomial time
- A sufficiently large quantum computer would break BLS signatures entirely
- BLS aggregation (the key property enabling Beacon Chain scalability) is algebraically tied to the curve

Lean Consensus replaces BLS with **XMSS** — a hash-based signature scheme with quantum resistance.

## XMSS (eXtended Merkle Signature Scheme)

XMSS is a stateful hash-based signature scheme standardized in [RFC 8391](https://datatracker.ietf.org/doc/html/rfc8391).

### Construction

XMSS combines two primitives:

1. **Winternitz One-Time Signature (WOTS+)**
   - Signs a single message using hash chains
   - Each key pair can only be used once (one-time signature)
   - Security relies solely on hash function properties

2. **Merkle Tree**
   - Organizes 2^h WOTS+ key pairs into a binary tree
   - The root of the tree is the public key
   - Authentication path proves a leaf belongs to the tree
   - Height h determines how many signatures can be produced

### Properties

| Property | Value |
|----------|-------|
| Security basis | Hash function collision resistance only |
| Quantum resistance | Yes (no number-theoretic assumptions) |
| Statefulness | Yes — must track which leaf keys have been used |
| Signature size | **3,112 bytes** |
| Public key size | Compact (tree root + parameters) |
| Verification speed | Fast (hash computations only) |

### Comparison with BLS12-381

| Metric | BLS12-381 | XMSS |
|--------|-----------|------|
| Signature size | 96 bytes | 3,112 bytes (~32×) |
| Aggregation | Native (pairing-based) | Not possible algebraically |
| Quantum safe | No | Yes |
| Stateful | No | Yes |
| Standardized | Yes (EIP-2333) | Yes (RFC 8391) |

## Poseidon2 Hash Function

Lean Consensus uses **Poseidon2** as the hash function within XMSS:

- **SNARK-friendly**: Designed for efficient evaluation inside zero-knowledge proof circuits
- Critical because leanMultisig must verify XMSS signatures inside a zkVM
- Operates over the KoalaBear prime field (p = 2^31 - 2^24 + 1)
- Significantly fewer constraints in arithmetic circuits compared to SHA-256 or Keccak

## Statefulness Implications

XMSS is **stateful** — each one-time key in the Merkle tree must be used exactly once:

- Validators must persist their key state to avoid reusing keys
- Key reuse completely breaks security (enables signature forgery)
- This is a significant operational consideration for validator implementations
- The Merkle tree height determines the maximum number of signatures per key

## Signature Size Impact

The ~32× increase in signature size (3,112 bytes vs 96 bytes) would be prohibitive if signatures were naively included in blocks. This is why **leanMultisig** (zkVM-based aggregation) is essential — it replaces N individual signatures with a single constant-size zero-knowledge proof.
