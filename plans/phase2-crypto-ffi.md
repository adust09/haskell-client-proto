# Phase 2: Cryptography FFI — leanSig (XMSS) + leanMultisig Bindings

## Goal

Bind to the leanSig (XMSS) and leanMultisig (zkVM aggregation) C libraries via Haskell FFI. By end of phase, the client can sign/verify XMSS signatures and aggregate/verify leanMultisig proofs, with stateful key management that prevents leaf reuse.

## Prerequisites

- **Phase 1 complete**: SSZ types compile, `XmssSignature`, `XmssPubkey`, `LeanMultisigProof` wrappers exist
- **External**: leanSig C library (headers + shared lib) obtained from leanEthereum org
- **External**: leanMultisig C/Rust library obtained from leanEthereum org
- **Toolchain**: Rust toolchain (for building leanMultisig), C compiler (for leanSig)

---

## Step 1: Obtain and Inspect C Libraries

### Tasks

- Obtain leanSig C library: headers (`lean_sig.h`), shared library (`liblean_sig.so`)
- Obtain leanMultisig library: headers (`lean_multisig.h`), shared library (`liblean_multisig.so`)
- Study ethlambda's `crypto` crate FFI code for API patterns
- Document exact function signatures, memory ownership semantics, error handling conventions
- Determine exact sizes: XMSS public key size, signature size (expected 3112 bytes), proof size ranges

### Deliverable

A reference document (`docs/dev/06-ffi-api-reference.md`) listing every C function to bind, parameter types, memory ownership, and error codes.

### Completion criteria

- Both libraries build on target platform
- All C function signatures documented
- Memory ownership rules understood (who allocates, who frees)

---

## Step 2: SHA-256 + Poseidon2 Hashing

### Files to create/modify

- `src/Crypto/Hash.hs`
- `test/Test/Crypto/Hash.hs`

### Types and signatures

```haskell
module Crypto.Hash where

-- SHA-256 via crypton (already available in Phase 1 Merkleization)
sha256 :: ByteString -> ByteString
-- Re-export from SSZ.Merkleization or implement standalone

-- Poseidon2 via FFI to leanSig internals
-- Only needed if Poseidon2 is exposed as a standalone function
-- Otherwise, it's used internally by leanSig and doesn't need a Haskell binding

-- Domain separation for signing
computeSigningRoot :: SszHashTreeRoot a => a -> Domain -> Bytes32
-- SHA-256(hashTreeRoot(object) ++ domain)

computeDomain :: DomainType -> Version -> Root -> Domain
-- Per consensus spec domain computation
```

### Algorithm description

- `sha256` wraps `Crypto.Hash.hash @SHA256` from crypton with `BA.convert` to `ByteString`.
- `computeSigningRoot` computes the message that gets signed: `SHA-256(hashTreeRoot(object) ++ domain)`. This is the standard signing root used in Ethereum consensus.
- Poseidon2 is used internally by XMSS (the hash function within Winternitz chains and the Merkle tree). If the leanSig library exposes it as a C function, bind it; otherwise, no direct Haskell binding needed.

### Tests (TDD)

- `sha256` of known inputs matches expected digests
- `computeSigningRoot` for a `Checkpoint` with known domain matches expected output
- Poseidon2 (if bound): test vector from leanSig documentation

### Completion criteria

- SHA-256 hashing works and matches crypton reference
- Signing root computation verified against known values

---

## Step 3: leanSig FFI Bindings (XMSS Sign/Verify)

### Files to create

- `src/Crypto/LeanSig.hs`
- `cbits/lean_sig_wrapper.c` (if a thin C wrapper is needed)
- `test/Test/Crypto/LeanSig.hs`

### Types and signatures

```haskell
module Crypto.LeanSig where

-- Opaque FFI types (pointers to C structs)
data CXmssPrivateKey
data CXmssPublicKey

-- Haskell-side key types
data XmssKeyPair = XmssKeyPair
    { xkpPublicKey  :: XmssPubkey       -- from Consensus.Types
    , xkpPrivatePtr :: ForeignPtr CXmssPrivateKey  -- prevent GC of C allocation
    , xkpTreeHeight :: Word32           -- determines max signatures = 2^h
    }

-- Key generation
generateKeyPair :: Word32 -> IO XmssKeyPair
-- treeHeight → IO keyPair

-- Raw sign (low-level, does NOT manage state)
foreign import ccall "lean_sig_sign"
    c_lean_sig_sign :: Ptr CXmssPrivateKey -> Ptr Word8 -> CSize
                    -> Word32 -> Ptr Word8 -> IO CInt
-- privateKey → message → messageLen → leafIndex → outSignature → errorCode

-- Raw verify (stateless)
foreign import ccall "lean_sig_verify"
    c_lean_sig_verify :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
                      -> Ptr Word8 -> CSize -> IO CInt
-- pubkey → pubkeyLen → message → messageLen → signature → signatureLen → 0=valid

-- High-level Haskell API
sign :: XmssKeyPair -> Word32 -> ByteString -> IO (Either CryptoError XmssSignature)
-- keyPair → leafIndex → message → signature

verify :: XmssPubkey -> ByteString -> XmssSignature -> IO Bool
-- pubkey → message → signature → valid?

-- Error type
data CryptoError
    = SigningFailed Int          -- C error code
    | VerificationFailed Int
    | KeyExhausted               -- all leaves used
    | InvalidKeyState String
    deriving (Show, Eq)
```

### Algorithm description

**Key generation**: Call C function to generate XMSS key pair with specified tree height. The tree height determines `2^h` available one-time signatures. Wrap the private key in a `ForeignPtr` with a C finalizer for automatic cleanup.

**Signing**: Takes private key pointer, message bytes, and leaf index. The C function writes the 3112-byte signature to a pre-allocated output buffer. Return the signature wrapped in `XmssSignature`.

**Verification**: Takes public key bytes, message bytes, and signature bytes. Returns 0 for valid, non-zero for invalid. Pure from Haskell's perspective (no state mutation).

**Memory management**:
- Private keys: `ForeignPtr` with C finalizer (`lean_sig_free_private_key`)
- Signatures/messages: `ByteString` (Haskell-managed, pinned for FFI via `useAsCStringLen`)
- Public keys: copied into Haskell `ByteString` after generation

### Tests (TDD)

- Generate key pair → sign message → verify → `True`
- Generate key pair → sign → tamper signature → verify → `False`
- Generate key pair → sign → verify with wrong message → `False`
- Sign with different leaf indices produces different signatures
- Verify with wrong public key → `False`

### Completion criteria

- Can generate XMSS key pairs
- Can sign arbitrary messages and verify signatures
- FFI memory management is correct (no leaks, no use-after-free)
- All error codes from C library are handled

---

## Step 4: Stateful Key Management

### Files to modify

- `src/Crypto/LeanSig.hs` (add key state management)

### Types and signatures

```haskell
-- Thread-safe stateful key manager
data KeyState = KeyState
    { ksKeyPair     :: XmssKeyPair
    , ksNextLeafIdx :: !Word32         -- next unused leaf (monotonically increasing)
    , ksMaxLeaves   :: !Word32         -- 2^treeHeight
    }

type ManagedKey = MVar KeyState

-- Create managed key from a fresh key pair
newManagedKey :: XmssKeyPair -> IO ManagedKey

-- Load managed key from persisted state
loadManagedKey :: XmssKeyPair -> Word32 -> IO ManagedKey
-- keyPair → persistedLeafIndex → managed key

-- Sign with automatic leaf index management
managedSign :: ManagedKey -> FilePath -> ByteString -> IO (Either CryptoError XmssSignature)
-- managedKey → stateFilePath → message → signature

-- Query remaining signatures
remainingSignatures :: ManagedKey -> IO Word32

-- Persist key state to disk
persistKeyState :: FilePath -> Word32 -> IO ()

-- Load key state from disk
loadKeyState :: FilePath -> IO (Maybe Word32)
```

### Algorithm description

**`managedSign`**:
1. `takeMVar` to acquire exclusive access to key state.
2. Check `ksNextLeafIdx < ksMaxLeaves` — if not, return `Left KeyExhausted`.
3. Call `sign` with current `ksNextLeafIdx`.
4. On success: increment `ksNextLeafIdx`, persist new index to disk, `putMVar` with updated state.
5. On failure: `putMVar` with unchanged state (leaf index not consumed), return error.

**Critical invariants**:
- `ksNextLeafIdx` is **monotonically increasing** — never decremented or reset.
- Disk persistence happens **before** returning the signature — if the process crashes after signing but before persisting, the leaf is "wasted" (safe) rather than risking reuse (unsafe).
- `MVar` ensures only one thread signs with a given key at a time.

**Disk format**: Simple binary file containing a single `Word32` (little-endian) — the next unused leaf index. Atomic write via write-to-temp + rename.

**Crash recovery**: On load, read persisted leaf index. If file is missing or corrupt, refuse to sign (require manual intervention rather than risking reuse).

### Tests (TDD)

- `managedSign` increments leaf index after each sign
- `managedSign` returns `KeyExhausted` when all leaves are used (test with tree height 2 = 4 signatures)
- Concurrent `managedSign` calls don't reuse leaf indices (use `forkIO` + `MVar` barrier)
- Persisted state survives simulated restart (write → read → sign continues from correct index)
- Crash simulation: sign succeeds but persist "fails" → on reload, leaf index is at safe value (never lower than actual usage)

### Completion criteria

- Leaf indices are never reused under any circumstances
- Thread-safe concurrent signing
- Crash-safe persistence (write + rename pattern)
- Key exhaustion detected and reported

---

## Step 5: leanMultisig FFI Bindings (Aggregate/Verify)

### Files to create

- `src/Crypto/LeanMultisig.hs`
- `test/Test/Crypto/LeanMultisig.hs`

### Types and signatures

```haskell
module Crypto.LeanMultisig where

-- Opaque FFI types
data CProverContext
data CVerifierContext

-- Haskell-side context wrappers
newtype ProverContext  = ProverContext (ForeignPtr CProverContext)
newtype VerifierContext = VerifierContext (ForeignPtr CVerifierContext)

-- Setup (expensive, do once at startup)
setupProver :: IO ProverContext
-- Initializes prover context (compiles zkDSL program, sets up proving parameters)

setupVerifier :: IO VerifierContext
-- Initializes verifier context (loads verification key)

-- Aggregation
aggregate :: ProverContext -> [(XmssPubkey, XmssSignature, ByteString)]
          -> IO (Either CryptoError LeanMultisigProof)
-- prover → [(pubkey, signature, message)] → proof
-- Each tuple is one validator's signed attestation

-- Verification
verifyAggregation :: VerifierContext -> LeanMultisigProof
                  -> AttestationData -> Bitlist n -> [XmssPubkey]
                  -> IO Bool
-- verifier → proof → attestationData → aggregationBits → pubkeys → valid?

-- FFI declarations (actual names from C headers)
foreign import ccall "lean_multisig_setup_prover"
    c_setup_prover :: IO (Ptr CProverContext)

foreign import ccall "lean_multisig_setup_verifier"
    c_setup_verifier :: IO (Ptr CVerifierContext)

foreign import ccall "lean_multisig_aggregate"
    c_aggregate :: Ptr CProverContext -> Ptr Word8 -> CSize
                -> Ptr Word8 -> CSize -> IO CInt

foreign import ccall "lean_multisig_verify"
    c_verify :: Ptr CVerifierContext -> Ptr Word8 -> CSize
             -> Ptr Word8 -> CSize -> IO CInt
```

### Algorithm description

**Setup**: Prover and verifier contexts are expensive to initialize (seconds). Create once at node startup, reuse for all subsequent operations. Wrap in `ForeignPtr` with C finalizer for cleanup on GC.

**Aggregation flow**:
1. Serialize all `(pubkey, signature, message)` tuples into the format expected by C API.
2. Call `c_aggregate` with serialized input and output buffer.
3. Parse output into `LeanMultisigProof` (variable-size `ByteString`).
4. Handle error codes (invalid signatures, prover failure).

**Verification flow**:
1. Serialize proof, attestation data hash, aggregation bits, and public keys.
2. Call `c_verify`.
3. Return `True` if verification succeeds.

**Performance considerations**:
- Aggregation is CPU-intensive (~554 sigs/sec on M4 Max). Run in a dedicated thread.
- Proof generation must complete within the slot time (4 seconds minus network delay).
- Consider using `forkOS` for FFI-bound threads if the C library uses thread-local state.

### Tests (TDD)

- Setup prover and verifier (smoke test — doesn't crash)
- Sign N messages with XMSS → aggregate → verify → `True`
- Aggregate → tamper proof → verify → `False`
- Aggregate → verify with wrong attestation data → `False`
- Aggregate → verify with wrong aggregation bits → `False`
- Empty input (0 signatures) → appropriate error
- Single signature aggregation works

### Completion criteria

- Prover/verifier setup succeeds
- Aggregation of N signatures produces a valid proof
- Verification correctly accepts valid proofs and rejects invalid ones
- Memory management is correct (contexts freed on GC)
- Error codes from C library handled

---

## Step 6: Integration — Crypto with SSZ Types

### Files to modify

- `src/Crypto/LeanSig.hs` — add signing root computation
- `src/Crypto/LeanMultisig.hs` — wire up with `AttestationData`

### Types and signatures

```haskell
-- Sign an attestation
signAttestation :: ManagedKey -> FilePath -> AttestationData -> ValidatorIndex -> Domain
               -> IO (Either CryptoError SignedAttestation)

-- Verify an attestation signature
verifyAttestation :: SignedAttestation -> Domain -> IO Bool

-- Aggregate attestations for a subnet
aggregateAttestations :: ProverContext -> [SignedAttestation]
                      -> IO (Either CryptoError SignedAggregatedAttestation)

-- Verify an aggregated attestation
verifyAggregatedAttestation :: VerifierContext -> SignedAggregatedAttestation
                            -> [XmssPubkey] -> Domain -> IO Bool
```

### Algorithm description

**`signAttestation`**:
1. Compute `signingRoot = computeSigningRoot attestationData domain`.
2. `managedSign key stateFile signingRoot`.
3. Wrap result in `SignedAttestation { saData, saValidatorIndex, saSignature }`.

**`verifyAttestation`**:
1. Compute `signingRoot = computeSigningRoot (saData att) domain`.
2. `verify pubkey signingRoot (saSignature att)`.

**`aggregateAttestations`**:
1. Verify all attestations share the same `AttestationData` (reject if not).
2. Extract `(pubkey, signature, signingRoot)` tuples.
3. Call `aggregate proverCtx tuples`.
4. Build `Bitlist` from validator indices.
5. Return `SignedAggregatedAttestation`.

**`verifyAggregatedAttestation`**:
1. Extract participating pubkeys from `saaAggregationBits` and pubkey list.
2. Compute signing root from `saaData` and domain.
3. Call `verifyAggregation verifierCtx proof attestationData bits pubkeys`.

### Tests (TDD)

- Full roundtrip: generate keys → create attestation data → sign → verify
- Full aggregation roundtrip: sign N attestations → aggregate → verify aggregation
- Verify rejects attestation with wrong domain
- Aggregate rejects mixed attestation data

### Completion criteria

- End-to-end: key gen → sign attestation → verify attestation
- End-to-end: sign N attestations → aggregate → verify aggregation
- SSZ-serialized messages are correctly used as signing inputs
- Domain separation works correctly

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| leanSig/leanMultisig libraries unavailable or undocumented | Blocks entire phase | Contact leanEthereum team early; study ethlambda's FFI code as reference |
| C API memory ownership unclear (who allocates, who frees) | Memory leaks or segfaults | Study ethlambda Rust wrapper; write C test harness first |
| XMSS signature size differs from documented 3112 bytes | SSZ types need resizing | Use constants derived from C headers, not hardcoded values |
| leanMultisig prover setup is slow (>10 seconds) | Delays node startup | Setup prover asynchronously; accept attestations before prover is ready |
| Thread-safety issues with C libraries | Crashes under concurrent use | Test with `forkIO` + high concurrency; use `forkOS` if needed |
| Poseidon2 hash not exposed as standalone C function | Cannot implement `Crypto.Hash.poseidon2` | May not be needed — only used internally by leanSig |

---

## Exit Criteria

- [ ] leanSig FFI: can generate XMSS key pairs
- [ ] leanSig FFI: can sign messages and produce 3112-byte signatures
- [ ] leanSig FFI: can verify XMSS signatures (accept valid, reject invalid)
- [ ] Key state management: leaf indices never reused
- [ ] Key state management: thread-safe concurrent signing
- [ ] Key state management: crash-safe disk persistence
- [ ] leanMultisig FFI: prover and verifier setup succeeds
- [ ] leanMultisig FFI: can aggregate N signatures into one proof
- [ ] leanMultisig FFI: can verify aggregation proofs
- [ ] Integration: sign attestation → verify attestation works end-to-end
- [ ] Integration: sign N attestations → aggregate → verify aggregation works end-to-end
- [ ] No memory leaks (test with extended runs)
- [ ] All crypto operations use SSZ-serialized signing roots
