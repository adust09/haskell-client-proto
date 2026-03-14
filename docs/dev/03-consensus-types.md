# Consensus Types Reference

## Overview

All types are SSZ-serializable. This document defines the exact Haskell type definitions needed for pq-devnet-3, derived from the leanSpec Python specification and the ethlambda Rust reference.

---

## Primitive Aliases

```haskell
type Slot           = Word64
type Epoch          = Word64
type ValidatorIndex = Word64
type CommitteeIndex = Word64
type SubnetId       = Word64
type Gwei           = Word64
type Root           = Bytes32    -- 32-byte Merkle root
type Domain         = Bytes32
type Version        = Bytes4
```

---

## Checkpoint

Used in FFG voting for justification and finalization.

```haskell
data Checkpoint = Checkpoint
    { cpSlot :: Slot      -- Slot of the checkpoint block
    , cpRoot :: Root      -- Block root at checkpoint
    } deriving (Generic, Eq, Show)

-- SSZ: Container, fixed-size (8 + 32 = 40 bytes)
```

---

## Attestation Data

The core content of a vote (without signature).

```haskell
data AttestationData = AttestationData
    { adSlot            :: Slot           -- Slot the attestation is for
    , adHeadRoot        :: Root           -- Head vote: block root of chain head
    , adSourceCheckpoint :: Checkpoint    -- FFG source: last justified
    , adTargetCheckpoint :: Checkpoint    -- FFG target: block to justify
    } deriving (Generic, Eq, Show)

-- SSZ: Container, fixed-size (8 + 32 + 40 + 40 = 120 bytes)
```

---

## Attestation (Individual)

Attestation with validator identity and signature.

```haskell
data SignedAttestation = SignedAttestation
    { saData           :: AttestationData
    , saValidatorIndex :: ValidatorIndex
    , saSignature      :: XmssSignature    -- XMSS signature (3112 bytes)
    } deriving (Generic, Eq, Show)

-- SSZ: Container
-- saData: fixed (120 bytes)
-- saValidatorIndex: fixed (8 bytes)
-- saSignature: variable (XMSS signatures are large but fixed at 3112 bytes)
```

### XmssSignature

```haskell
-- XMSS signature is 3112 bytes fixed
newtype XmssSignature = XmssSignature { unXmssSignature :: ByteString }
    deriving (Eq, Show)

-- SSZ: fixed-size, 3112 bytes
xmssSignatureSize :: Int
xmssSignatureSize = 3112
```

---

## Aggregated Attestation

Produced by Aggregator nodes after collecting individual attestations.

```haskell
data SignedAggregatedAttestation = SignedAggregatedAttestation
    { saaData            :: AttestationData
    , saaAggregationBits :: Bitlist MAX_VALIDATORS_PER_SUBNET
    , saaAggregationProof :: LeanMultisigProof
    } deriving (Generic, Eq, Show)

-- SSZ: Container, variable-size (due to Bitlist and proof)
```

### LeanMultisigProof

```haskell
-- ZK proof from leanMultisig aggregation
-- Size varies by proof regime: ~146-380 KiB
newtype LeanMultisigProof = LeanMultisigProof { unLeanMultisigProof :: ByteString }
    deriving (Eq, Show)

-- SSZ: variable-size (length-delimited)
```

---

## Beacon Block

```haskell
data BeaconBlockBody = BeaconBlockBody
    { bbbAttestations :: SszList MAX_ATTESTATIONS SignedAggregatedAttestation
    } deriving (Generic, Eq, Show)

-- SSZ: Container, variable-size

data BeaconBlock = BeaconBlock
    { bbSlot          :: Slot
    , bbProposerIndex :: ValidatorIndex
    , bbParentRoot    :: Root
    , bbStateRoot     :: Root
    , bbBody          :: BeaconBlockBody
    } deriving (Generic, Eq, Show)

-- SSZ: Container, variable-size (due to body)

data SignedBeaconBlock = SignedBeaconBlock
    { sbbBlock     :: BeaconBlock
    , sbbSignature :: XmssSignature
    } deriving (Generic, Eq, Show)
```

---

## Validator

```haskell
data Validator = Validator
    { vPubkey             :: XmssPubkey       -- XMSS public key
    , vEffectiveBalance   :: Gwei
    , vSlashed            :: Bool
    , vActivationSlot     :: Slot
    , vExitSlot           :: Slot
    , vWithdrawableSlot   :: Slot
    } deriving (Generic, Eq, Show)

-- SSZ: Container, fixed-size
```

### XmssPubkey

```haskell
-- XMSS public key size (TBD: depends on leanSig parameters)
-- Likely 32 or 64 bytes (Merkle tree root + OID)
newtype XmssPubkey = XmssPubkey { unXmssPubkey :: ByteString }
    deriving (Eq, Show)
```

---

## Beacon State

The full consensus state. Updated every slot.

```haskell
data BeaconState = BeaconState
    { bsSlot                  :: Slot
    , bsLatestBlockHeader     :: BeaconBlockHeader
    , bsBlockRoots            :: SszVector SLOTS_PER_HISTORICAL_ROOT Root
    , bsStateRoots            :: SszVector SLOTS_PER_HISTORICAL_ROOT Root
    , bsValidators            :: SszList VALIDATOR_REGISTRY_LIMIT Validator
    , bsBalances              :: SszList VALIDATOR_REGISTRY_LIMIT Gwei
    , bsJustifiedCheckpoint   :: Checkpoint
    , bsFinalizedCheckpoint   :: Checkpoint
    , bsCurrentAttestations   :: SszList MAX_ATTESTATIONS_STATE SignedAggregatedAttestation
    } deriving (Generic, Eq, Show)

-- SSZ: Container, variable-size
```

### BeaconBlockHeader

```haskell
data BeaconBlockHeader = BeaconBlockHeader
    { bbhSlot          :: Slot
    , bbhProposerIndex :: ValidatorIndex
    , bbhParentRoot    :: Root
    , bbhStateRoot     :: Root
    , bbhBodyRoot      :: Root      -- hash_tree_root of block body
    } deriving (Generic, Eq, Show)

-- SSZ: Container, fixed-size (8 + 8 + 32 + 32 + 32 = 112 bytes)
```

---

## Fork Choice Types

These are not SSZ-serialized but used internally for fork choice.

```haskell
data Store = Store
    { stJustifiedCheckpoint  :: Checkpoint
    , stFinalizedCheckpoint  :: Checkpoint
    , stBlocks               :: Map Root BeaconBlock
    , stBlockStates          :: Map Root BeaconState
    , stLatestMessages       :: Map ValidatorIndex LatestMessage
    , stCurrentSlot          :: Slot
    } deriving (Eq, Show)

data LatestMessage = LatestMessage
    { lmSlot :: Slot
    , lmRoot :: Root
    } deriving (Eq, Show)
```

---

## Constants

These are devnet configuration parameters. Exact values come from the pq-devnet-3 config file.

```haskell
module Consensus.Constants where

-- Timing
slotsPerSecond :: Word64
slotsPerSecond = 1  -- 1 slot every 4 seconds

slotDuration :: Int  -- microseconds
slotDuration = 4_000_000

networkDelayBound :: Int  -- microseconds (Δ = 800ms)
networkDelayBound = 800_000

-- Finality
slotsToFinality :: Word64
slotsToFinality = 3

finalityThreshold :: Rational
finalityThreshold = 2 % 3

-- Validator set (devnet params, loaded from config)
-- These are placeholders — actual values from genesis config
type MAX_VALIDATORS_PER_SUBNET = 256
type MAX_ATTESTATIONS = 128
type MAX_ATTESTATIONS_STATE = 4096
type SLOTS_PER_HISTORICAL_ROOT = 64
type VALIDATOR_REGISTRY_LIMIT = 1024

-- Networking
gossipsubMeshSize :: Int
gossipsubMeshSize = 8

gossipsubHeartbeatMs :: Int
gossipsubHeartbeatMs = 700
```

**Note**: Type-level constants (`MAX_VALIDATORS_PER_SUBNET` etc.) are used as phantom parameters in `SszList`, `SszVector`, `Bitlist`, `Bitvector`. Exact values should be loaded from the devnet config; the type-level numbers define upper bounds for SSZ Merkleization.

---

## Type Dependency Graph

```
Bytes32 (Root)
    ↓
Checkpoint
    ↓
AttestationData
    ↓
SignedAttestation ←── XmssSignature
    ↓
SignedAggregatedAttestation ←── Bitlist, LeanMultisigProof
    ↓
BeaconBlockBody
    ↓
BeaconBlock ←── BeaconBlockHeader
    ↓
SignedBeaconBlock
    ↓
BeaconState ←── Validator, SszVector, SszList
```

Build order: bottom-up (Bytes32 → Checkpoint → AttestationData → ...).

---

## Notes

1. **XMSS signature/pubkey sizes**: Exact sizes depend on leanSig parameters (tree height, Winternitz parameter). The values here (3112 bytes for sig) are from docs/04-xmss-signatures.md. Verify against the actual leanSig C library headers.

2. **BeaconState fields**: The field list above is a minimal estimate. The actual leanSpec Python spec may have additional fields. Cross-reference with leanSpec once a git submodule is set up.

3. **Constants as type-level naturals**: Using DataKinds + GHC.TypeNats for compile-time length enforcement. Alternative: runtime-checked with smart constructors if type-level programming becomes too complex.

4. **No Union types needed**: pq-devnet-3 doesn't use SSZ Union. Skip for now.
