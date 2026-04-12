-- | Consensus protocol constants and type aliases for pq-devnet-3.
-- Aligned with leanSpec formal specification.
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
  , Interval
    -- * Timing
  , slotDuration
  , networkDelayBound
  , INTERVALS_PER_SLOT
    -- * Finality
  , slotsToFinality
  , JUSTIFICATION_LOOKBACK_SLOTS
    -- * Type-level constants
  , MAX_ATTESTATIONS
  , VALIDATOR_REGISTRY_LIMIT
  , HISTORICAL_ROOTS_LIMIT
  , BYTE_LIST_MIB
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
type Interval       = Word64

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
-- Type-level constants for SSZ collections (aligned with leanSpec)
-- ---------------------------------------------------------------------------

-- | Intervals per slot (leanSpec: 5).
type INTERVALS_PER_SLOT = 5

-- | Justification lookback slots (leanSpec: 3).
type JUSTIFICATION_LOOKBACK_SLOTS = 3

-- | Max attestations in a block body / state (leanSpec: 4096).
type MAX_ATTESTATIONS = 4096

-- | Maximum number of validators (leanSpec: 2^12 = 4096).
type VALIDATOR_REGISTRY_LIMIT = 4096

-- | Historical roots limit (leanSpec: 2^18 = 262144).
type HISTORICAL_ROOTS_LIMIT = 262144

-- | ByteList max size: 1 MiB in bytes (leanSpec: proof_data limit).
type BYTE_LIST_MIB = 1048576

-- ---------------------------------------------------------------------------
-- Crypto sizes
-- ---------------------------------------------------------------------------

-- | XMSS signature size in bytes.
xmssSignatureSize :: Int
xmssSignatureSize = 3112

-- | XMSS public key size in bytes (leanSpec: 52 bytes).
xmssPubkeySize :: Int
xmssPubkeySize = 52

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
