---
title: FFI ABI Proposal
last_updated: 2026-03-15
tags:
  - ffi
  - abi
  - xmss
  - lean-sig
  - lean-multisig
  - cryptography
---

# FFI ABI Proposal

Point-in-time C ABI wrapper design for leanSig (XMSS) and leanMultisig (zkVM aggregation). This proposal is **not ABI-stable** across upstream commits — it captures the intended wrapper surface based on ethlambda analysis and proposed design decisions.

**Target platform**: Linux x86_64 only (LP64, System V AMD64 ABI). Platform-specific assumptions are marked with `[LP64]`.

**Non-authoritative constants rule**: Values from `Constants.hs` or ethlambda are input assumptions, not ABI evidence.

**Scope**: Neither leanSig nor leanMultisig currently exposes a C ABI. This document proposes wrapper functions that would be implemented as thin C ABI shims over the upstream Rust libraries.

---

## Decision Status Labels

| Label | Meaning | Blocks Steps 3/5? |
|-------|---------|-------------------|
| **Confirmed** | Verified against upstream at pinned commit | No |
| **Inferred** | From ethlambda; plausible, not verified | Yes |
| **Proposed** | Our design; no upstream precedent | Yes |
| **Unknown** | No info; design risk with proposed default | Yes |

### Decision Status Table

| Decision | Status | Source |
|----------|--------|--------|
| XmssSignature = 3112 bytes | Inferred | ethlambda `signature.rs` |
| XmssPubkey = 32 bytes | Inferred | `Constants.hs:89` (TBD) |
| leaf_index as explicit param | Proposed | Design choice |
| Separate buffers for aggregate | Proposed | Design candidate |
| Error codes | Proposed | Mapped from ethlambda |
| Thread safety | Unknown | Not verified |
| Key serialization contents | Unknown | Not verified |
| Zeroization on free | Proposed | Security requirement |

---

## ABI Hygiene Rules

### Symbol & Header Rules

1. **Headers**: `lean_sig.h`, `lean_multisig.h`. All declarations wrapped in `#ifdef __cplusplus extern "C" { #endif`.
2. **Symbol naming**: Exported symbols prefixed `lean_sig_` or `lean_multisig_`. Internal symbols `static`.
3. **Symbol visibility**: `__attribute__((visibility("default")))` on exports. Build with `-fvisibility=hidden`. `[LP64]`

### Nullability & Buffer Rules

4. **Non-null default**: Every pointer param is non-null unless stated otherwise. NULL on non-null param → `LEAN_ERR_INVALID_PARAM`.
5. **Empty-input convention**: `(NULL, 0)` is valid for **data input buffers** (message, proof). It is NOT valid for output buffers (output ptr must be non-NULL even with capacity=0, to receive `BUFFER_TOO_SMALL`). Rationale: empty messages may be cryptographically valid; output always needs a size report path.
6. **Free functions**: `lean_sig_free_private_key(NULL)`, `lean_multisig_free_prover(NULL)`, `lean_multisig_free_verifier(NULL)` are explicitly no-ops.

### Out-Param Failure Semantics

7. **Pointer-handle outs** (`void **_out`): set to NULL on failure.
8. **Size outs** (`size_t *_out`): set to 0 on failure, EXCEPT on `BUFFER_TOO_SMALL` where set to required size.
9. **Buffer contents**: undefined on failure.

### Lifetime & Threading

10. **Lifetime**: Opaque handles valid from creation to matching free. Use-after-free is UB.
11. **Free vs in-flight**: Free must not race with in-flight ops on same handle. Haskell uses deterministic `bracket`/explicit free, not `ForeignPtr` finalizers alone.
12. **Distinct handles**: Different handles of the same type MAY be used concurrently from different threads unless documented otherwise. Same handle: NOT safe unless documented.
13. **No global one-time init**: Each context is independently initialized via setup functions. No hidden global state.
14. **Async exceptions**: All FFI calls are blocking/non-cancelable from C side. Haskell MUST mask async exceptions around handle use/free. Long-running ops (aggregate, setup) use `safe` foreign imports.

### Versioning & Provenance

15. **ABI version**: `int32_t lean_ffi_abi_version(uint32_t *major, uint32_t *minor)`. Major = breaking, minor = additive. Haskell checks at startup.
16. **Upstream provenance** (runtime-queryable):

```c
int32_t lean_ffi_upstream_commit(
    const char **leansig_commit_out,      // out: static string, NULL-terminated
    const char **leanmultisig_commit_out  // out: static string, NULL-terminated
);
```

Both strings are compile-time constants baked into the wrapper build.

### Type & Size Rules

17. **C types at boundary** `[LP64]`: `int32_t` for return codes, `uint32_t` for indices/counts with known bounds, `size_t` for byte lengths/capacities. No `int`/`long`.
18. **Tree height / leaf index**: `tree_height` ∈ [1, 31]. `leaf_index` < 2^`tree_height`. Out-of-range → `LEAN_ERR_INVALID_PARAM`.
19. **Size queries**: Return `int32_t` status with out-param for consistency:

```c
int32_t lean_sig_pubkey_size(size_t *out);           // expected 32, Inferred
int32_t lean_sig_signature_size(size_t *out);        // expected 3112, Inferred
int32_t lean_sig_serialized_key_size(size_t *out);   // Unknown
```

These are **process-constant** per build. Callers may cache after first successful query.

20. **Overflow**: All `count * element_size` products use overflow-checked arithmetic. Overflow → `LEAN_ERR_INVALID_PARAM`.
21. **Design intent**: Optimizes for short-term wrapper simplicity. Proposed defaults are expected implementation unless upstream contradicts.

---

## Error Codes

All error codes are **Proposed**.

```c
#define LEAN_OK                        0
#define LEAN_NOT_VALID                 1    // predicate functions only

#define LEAN_ERR_INVALID_PARAM        -1
#define LEAN_ERR_BUFFER_TOO_SMALL     -2
#define LEAN_ERR_SIGNING_FAILED       -3
#define LEAN_ERR_KEY_EXHAUSTED        -4
#define LEAN_ERR_EMPTY_INPUT          -5
#define LEAN_ERR_COUNT_MISMATCH       -6
#define LEAN_ERR_AGGREGATION_FAILED   -7
#define LEAN_ERR_DESERIALIZATION      -8   // invalid format or truncated
#define LEAN_ERR_SETUP_FAILED         -9
#define LEAN_ERR_INTERNAL             -99
```

Codes are consecutive -1 to -9; -99 is a catch-all for unexpected internal errors.

### Predicate Convention

Predicate functions (verify operations) use a `0/1/<0` return pattern:

- `0` → valid (`LEAN_OK`)
- `1` → not valid (`LEAN_NOT_VALID`)
- `<0` → error (one of the `LEAN_ERR_*` codes)

This is intentional but atypical. Haskell bindings MUST use a typed wrapper to prevent misuse:

```haskell
fromPredicateResult :: CInt -> Either CryptoError Bool
-- 0  → Right True   (valid)
-- 1  → Right False  (not valid)
-- <0 → Left err     (error)
```

Each predicate function's header declaration includes a comment block explaining this convention.

---

## leanSig C ABI

### XMSS Statefulness (Highest Risk)

XMSS is a **stateful** signature scheme — each leaf index may only be used once. Leaf reuse breaks security entirely.

- **Proposed**: External leaf index management. The `lean_sig_sign` function takes `leaf_index` as an explicit parameter. The serialized key blob is assumed to contain static material only (tree parameters, key material), not progress state.
- **Unknown**: Whether `lean_sig_sign` can detect leaf reuse at the C level. Proposed: Haskell `MVar` enforces monotonic use; C wrapper returns `LEAN_ERR_KEY_EXHAUSTED` if upstream can detect it. **Blocks Step 3.**

### Key Generation

Separated into two operations to avoid coupling allocation with buffer sizing:

```c
// [Operational] Generates key pair. Both outputs are library-allocated.
// Status: Proposed
int32_t lean_sig_keygen(
    uint32_t tree_height,
    void **private_key_out,       // out: NULL on failure
    void **public_key_out         // out: NULL on failure; free with lean_sig_free_public_key
);

// [Operational] Exports public key bytes into caller buffer.
// Status: Proposed
int32_t lean_sig_export_public_key(
    const void *public_key,
    uint8_t *buf_out, size_t buf_capacity,
    size_t *bytes_written_out     // 0 on failure; required size on BUFFER_TOO_SMALL
);

void lean_sig_free_public_key(void *public_key);  // NULL is a no-op
```

### Signing

```c
// [Operational] Signs a message with the given leaf index.
// Status: Proposed (leaf_index as explicit param)
int32_t lean_sig_sign(
    void *private_key,
    const uint8_t *message, size_t message_len,  // (NULL, 0) valid
    uint32_t leaf_index,
    uint8_t *signature_out, size_t signature_len,
    size_t *sig_written_out       // 0 on failure; required on BUFFER_TOO_SMALL
);
```

### Verification

```c
// [Predicate] 0 = valid, 1 = invalid, <0 = error
// Haskell binding wraps via fromPredicateResult :: CInt -> Either CryptoError Bool
// Status: Proposed
int32_t lean_sig_verify(
    const uint8_t *public_key, size_t public_key_len,
    const uint8_t *message, size_t message_len,   // (NULL, 0) valid
    const uint8_t *signature, size_t signature_len
);
```

### Key Serialization

```c
// [Operational] BUFFER_TOO_SMALL sets bytes_written_out to required size.
// Serialized format may be versioned.
// Status: Unknown (serialized key contents not verified)
int32_t lean_sig_serialize_private_key(
    const void *private_key,
    uint8_t *buf_out, size_t buf_capacity,
    size_t *bytes_written_out
);

// [Operational] All-or-nothing: NULL on failure.
// Status: Unknown
int32_t lean_sig_deserialize_private_key(
    const uint8_t *buf, size_t buf_len,
    void **private_key_out
);
```

### Cleanup

```c
// Best-effort zeroization of wrapper-owned buffers.
// Upstream Rust memory zeroization depends on upstream impl and cannot be guaranteed by wrapper.
void lean_sig_free_private_key(void *private_key);  // NULL is a no-op
```

---

## leanMultisig C ABI

### Context Setup

```c
// [Operational] Initializes prover context (compiles zkDSL program, sets up proving parameters).
// Expected to be expensive — call once at startup.
// Status: Proposed
int32_t lean_multisig_setup_prover(void **prover_ctx_out);

// [Operational] Initializes verifier context (loads verification key).
// Status: Proposed
int32_t lean_multisig_setup_verifier(void **verifier_ctx_out);

void lean_multisig_free_prover(void *prover_ctx);    // NULL is a no-op
void lean_multisig_free_verifier(void *verifier_ctx); // NULL is a no-op
```

### Aggregation

```c
// [Operational] Proof output uses 2-call pattern via BUFFER_TOO_SMALL.
// Status: Proposed
int32_t lean_multisig_aggregate(
    void *prover_ctx,
    const uint8_t *pubkeys, size_t pubkeys_byte_len, size_t pubkey_count,
    const uint8_t *signatures, size_t signatures_byte_len, size_t signature_count,
    const uint8_t *message, size_t message_len,   // (NULL, 0) valid
    uint8_t *proof_out, size_t proof_capacity,
    size_t *proof_len_out
);
```

**Preconditions** (all overflow-checked):

- `pubkey_count == signature_count` (else `LEAN_ERR_COUNT_MISMATCH`)
- `pubkeys_byte_len == pubkey_count * lean_sig_pubkey_size()` (else `LEAN_ERR_INVALID_PARAM`)
- `signatures_byte_len == signature_count * lean_sig_signature_size()` (else `LEAN_ERR_INVALID_PARAM`)

### Verification

```c
// [Predicate] 0 = valid, 1 = invalid, <0 = error
// Status: Proposed
int32_t lean_multisig_verify(
    void *verifier_ctx,
    const uint8_t *proof, size_t proof_len,
    const uint8_t *message, size_t message_len,   // (NULL, 0) valid
    const uint8_t *pubkeys, size_t pubkeys_byte_len, size_t pubkey_count
);
```

**Precondition** (overflow-checked):

- `pubkeys_byte_len == pubkey_count * lean_sig_pubkey_size()` (else `LEAN_ERR_INVALID_PARAM`)

### Thread Safety

**Status: Unknown — design risk.**

- **Proposed default**: assume NOT thread-safe per handle. Distinct handles of the same type may be used concurrently from different threads. Haskell wraps each handle in `MVar` + deterministic `bracket`.
- Must verify before Step 5 implementation.

---

## Memory Ownership

| Resource | Allocator | Freer | On failure |
|----------|-----------|-------|------------|
| Private key | `keygen` / `deserialize` | `free_private_key` | NULL |
| Public key | `keygen` | `free_public_key` | NULL |
| Prover ctx | `setup_prover` | `free_prover` | NULL |
| Verifier ctx | `setup_verifier` | `free_verifier` | NULL |
| Output buffers | Caller | Caller | Contents undefined |

**Allocator boundary**: Opaque pointers freed only by matching library free function. Caller-allocated output buffers are the caller's responsibility regardless of success or failure.

**Zeroization**: Best-effort for wrapper-owned buffers (private keys). Upstream Rust memory zeroization depends on upstream implementation and cannot be guaranteed by the wrapper layer.

---

## Open Questions

### A. Unknown Upstream Semantics (block Steps 3/5)

1. **leanSig**: Internal vs external leaf index state management — does the upstream library track leaf progress internally, or is it purely the caller's responsibility?
2. **leanSig**: Serialized key contents — does the blob contain static material only, or key + progress state?
3. **leanSig**: Leaf reuse detectability — can the C/Rust layer detect and reject a reused leaf index?
4. **Size constants**: Exact sizes for pubkey (expected 32), signature (expected 3112), and serialized key (unknown).
5. **leanMultisig**: Thread safety per handle and across handles — can a single prover/verifier context be used from multiple threads?
6. **leanMultisig**: Proof size bounds — is proof size fixed or variable? What is the maximum?
7. **Poseidon2**: Standalone exposure — likely not needed as it is used internally by XMSS, but needs confirmation.

### B. Environment/Process Blockers (do not block this step)

8. **Upstream repo access**: leanSig and leanMultisig repositories may be private. Access needed for Steps 3/5 implementation.
