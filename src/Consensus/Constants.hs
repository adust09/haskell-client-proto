-- | Consensus protocol constants and type aliases for pq-devnet-3.
module Consensus.Constants
  ( -- * Type aliases
    Slot
  , Epoch
  , ValidatorIndex
  , CommitteeIndex
  , SubnetId
  , Gwei
  , Root
  , Domain
  , Version
  , DomainType
    -- * Timing
  , slotDuration
  , networkDelayBound
    -- * Finality
  , slotsToFinality
    -- * Type-level constants
  , MAX_VALIDATORS_PER_SUBNET
  , MAX_ATTESTATIONS
  , MAX_ATTESTATIONS_STATE
  , SLOTS_PER_HISTORICAL_ROOT
  , VALIDATOR_REGISTRY_LIMIT
    -- * Crypto sizes
  , xmssSignatureSize
  , xmssPubkeySize
    -- * Subnets
  , totalSubnets
    -- * Networking
  , gossipsubMeshSize
  , gossipsubHeartbeatMs
    -- * Slot phases (microseconds)
  , proposalPhaseEnd
  , votingPhaseEnd
  , confirmationPhaseEnd
  ) where

import Data.Word (Word64)
import SSZ.Common (Bytes4, Bytes32)

-- ---------------------------------------------------------------------------
-- Type aliases
-- ---------------------------------------------------------------------------

type Slot           = Word64
type Epoch          = Word64
type ValidatorIndex = Word64
type CommitteeIndex = Word64
type SubnetId       = Word64
type Gwei           = Word64
type Root           = Bytes32
type Domain         = Bytes32
type Version        = Bytes4
type DomainType     = Bytes4

-- ---------------------------------------------------------------------------
-- Timing
-- ---------------------------------------------------------------------------

-- | Slot duration in microseconds (4 seconds).
slotDuration :: Int
slotDuration = 4_000_000

-- | Network delay bound in microseconds (Δ = 800ms).
networkDelayBound :: Int
networkDelayBound = 800_000

-- ---------------------------------------------------------------------------
-- Finality
-- ---------------------------------------------------------------------------

-- | Number of slots to finality (3-slot finality).
slotsToFinality :: Word64
slotsToFinality = 3

-- ---------------------------------------------------------------------------
-- Type-level constants for SSZ collections
-- ---------------------------------------------------------------------------

type MAX_VALIDATORS_PER_SUBNET = 256
type MAX_ATTESTATIONS          = 4096
type MAX_ATTESTATIONS_STATE    = 4096
type SLOTS_PER_HISTORICAL_ROOT = 64
type VALIDATOR_REGISTRY_LIMIT  = 1024

-- ---------------------------------------------------------------------------
-- Crypto sizes
-- ---------------------------------------------------------------------------

-- | XMSS signature size in bytes.
xmssSignatureSize :: Int
xmssSignatureSize = 3112

-- | XMSS public key size in bytes.
-- Defaulting to 32 bytes (Merkle tree root only).
-- TODO: Verify against actual leanSig C library headers when obtained.
xmssPubkeySize :: Int
xmssPubkeySize = 32

-- ---------------------------------------------------------------------------
-- Networking
-- ---------------------------------------------------------------------------

gossipsubMeshSize :: Int
gossipsubMeshSize = 8

gossipsubHeartbeatMs :: Int
gossipsubHeartbeatMs = 700

-- ---------------------------------------------------------------------------
-- Subnets
-- ---------------------------------------------------------------------------

-- | Total number of attestation subnets.
totalSubnets :: Word64
totalSubnets = 4

-- ---------------------------------------------------------------------------
-- Slot phases (microseconds from slot start)
-- ---------------------------------------------------------------------------

-- | End of proposal phase (800ms).
proposalPhaseEnd :: Int
proposalPhaseEnd = 800_000

-- | End of voting phase (2400ms).
votingPhaseEnd :: Int
votingPhaseEnd = 2_400_000

-- | End of confirmation phase (3200ms).
confirmationPhaseEnd :: Int
confirmationPhaseEnd = 3_200_000
