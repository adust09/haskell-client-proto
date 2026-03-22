{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Consensus types for pq-devnet-3.
-- All SSZ-serializable types derive Generic and use SSZ.Derive for instances.
module Consensus.Types
  ( -- * Crypto primitives
    XmssSignature (..)
  , mkXmssSignature
  , XmssPubkey (..)
  , mkXmssPubkey
    -- * Aggregation types
  , AggregationBits
  , AggregatedSignatureProof (..)
    -- * Core types
  , Checkpoint (..)
  , AttestationData (..)
  , SignedAttestation (..)
  , AggregatedAttestation (..)
  , BeaconBlockBody (..)
  , BeaconBlock (..)
  , SignedBeaconBlock (..)
  , BeaconBlockHeader (..)
  , Validator (..)
  , BeaconState (..)
    -- * Fork choice (non-SSZ)
  , Store (..)
  , LatestMessage (..)
  , SlashingEvidence (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import Data.Word (Word8)
import GHC.Generics (Generic, Rep)
import SSZ.Bitlist (Bitlist)
import SSZ.Common
import SSZ.Container ()
import SSZ.Derive
import SSZ.List (SszList)
import SSZ.Merkleization (SszHashTreeRoot (..))
import Consensus.Constants

-- ---------------------------------------------------------------------------
-- Crypto primitives (opaque ByteString wrappers)
-- ---------------------------------------------------------------------------

-- | XMSS signature (3112 bytes, fixed-size in SSZ).
newtype XmssSignature = XmssSignature { unXmssSignature :: ByteString }
  deriving stock (Eq, Show)

mkXmssSignature :: ByteString -> Either SszError XmssSignature
mkXmssSignature bs
  | BS.length bs == xmssSignatureSize = Right (XmssSignature bs)
  | otherwise = Left (InvalidLength xmssSignatureSize (BS.length bs))

instance Ssz XmssSignature where
  sszFixedSize = Just (fromIntegral xmssSignatureSize)

instance SszEncode XmssSignature where
  sszEncode = unXmssSignature

instance SszDecode XmssSignature where
  sszDecode bs = mkXmssSignature bs

instance SszHashTreeRoot XmssSignature where
  hashTreeRoot (XmssSignature bs) =
    case mkBytesN @3112 bs of
      Right bn -> hashTreeRoot bn
      Left _   -> error "XmssSignature: invalid length"

-- | XMSS public key (xmssPubkeySize bytes, fixed-size in SSZ).
newtype XmssPubkey = XmssPubkey { unXmssPubkey :: ByteString }
  deriving stock (Eq, Show)

mkXmssPubkey :: ByteString -> Either SszError XmssPubkey
mkXmssPubkey bs
  | BS.length bs == xmssPubkeySize = Right (XmssPubkey bs)
  | otherwise = Left (InvalidLength xmssPubkeySize (BS.length bs))

instance Ssz XmssPubkey where
  sszFixedSize = Just (fromIntegral xmssPubkeySize)

instance SszEncode XmssPubkey where
  sszEncode = unXmssPubkey

instance SszDecode XmssPubkey where
  sszDecode bs = mkXmssPubkey bs

instance SszHashTreeRoot XmssPubkey where
  hashTreeRoot (XmssPubkey bs) =
    case mkBytesN @32 bs of
      Right bn -> hashTreeRoot bn
      Left _   -> error "XmssPubkey: invalid length"

-- | Aggregation bits: global-scope bitlist (VALIDATOR_REGISTRY_LIMIT).
-- Replaces the old subnet-scoped Bitlist MAX_VALIDATORS_PER_SUBNET.
type AggregationBits = Bitlist VALIDATOR_REGISTRY_LIMIT

-- | Structured proof from leanMultisig aggregation (SSZ container).
-- Replaces the old opaque LeanMultisigProof.
data AggregatedSignatureProof = AggregatedSignatureProof
  { aspParticipants :: !AggregationBits
  , aspProofData    :: !(SszList BYTES_PER_MIB Word8)
  } deriving stock (Generic, Eq, Show)

instance Ssz AggregatedSignatureProof where
  sszFixedSize = genericSszFixedSize @(Rep AggregatedSignatureProof)
instance SszEncode AggregatedSignatureProof where
  sszEncode = genericSszEncode
instance SszDecode AggregatedSignatureProof where
  sszDecode = genericSszDecode
instance SszHashTreeRoot AggregatedSignatureProof where
  hashTreeRoot = genericHashTreeRoot

-- ---------------------------------------------------------------------------
-- Core consensus types
-- ---------------------------------------------------------------------------

data Checkpoint = Checkpoint
  { cpSlot :: !Slot
  , cpRoot :: !Root
  } deriving stock (Generic, Eq, Ord, Show)

instance Ssz Checkpoint where
  sszFixedSize = genericSszFixedSize @(Rep Checkpoint)
instance SszEncode Checkpoint where
  sszEncode = genericSszEncode
instance SszDecode Checkpoint where
  sszDecode = genericSszDecode
instance SszHashTreeRoot Checkpoint where
  hashTreeRoot = genericHashTreeRoot

data AttestationData = AttestationData
  { adSlot             :: !Slot
  , adHeadRoot         :: !Root
  , adSourceCheckpoint :: !Checkpoint
  , adTargetCheckpoint :: !Checkpoint
  } deriving stock (Generic, Eq, Ord, Show)

instance Ssz AttestationData where
  sszFixedSize = genericSszFixedSize @(Rep AttestationData)
instance SszEncode AttestationData where
  sszEncode = genericSszEncode
instance SszDecode AttestationData where
  sszDecode = genericSszDecode
instance SszHashTreeRoot AttestationData where
  hashTreeRoot = genericHashTreeRoot

data SignedAttestation = SignedAttestation
  { saData           :: !AttestationData
  , saValidatorIndex :: !ValidatorIndex
  , saSignature      :: !XmssSignature
  } deriving stock (Generic, Eq, Show)

instance Ssz SignedAttestation where
  sszFixedSize = genericSszFixedSize @(Rep SignedAttestation)
instance SszEncode SignedAttestation where
  sszEncode = genericSszEncode
instance SszDecode SignedAttestation where
  sszDecode = genericSszDecode
instance SszHashTreeRoot SignedAttestation where
  hashTreeRoot = genericHashTreeRoot

data AggregatedAttestation = AggregatedAttestation
  { aaData             :: !AttestationData
  , aaSubnetId         :: !SubnetId
  , aaAggregationBits  :: !AggregationBits
  , aaAggregationProof :: !AggregatedSignatureProof
  } deriving stock (Generic, Eq, Show)

instance Ssz AggregatedAttestation where
  sszFixedSize = genericSszFixedSize @(Rep AggregatedAttestation)
instance SszEncode AggregatedAttestation where
  sszEncode = genericSszEncode
instance SszDecode AggregatedAttestation where
  sszDecode = genericSszDecode
instance SszHashTreeRoot AggregatedAttestation where
  hashTreeRoot = genericHashTreeRoot

data BeaconBlockBody = BeaconBlockBody
  { bbbAttestations :: !(SszList MAX_ATTESTATIONS AggregatedAttestation)
  } deriving stock (Generic, Eq, Show)

instance Ssz BeaconBlockBody where
  sszFixedSize = genericSszFixedSize @(Rep BeaconBlockBody)
instance SszEncode BeaconBlockBody where
  sszEncode = genericSszEncode
instance SszDecode BeaconBlockBody where
  sszDecode = genericSszDecode
instance SszHashTreeRoot BeaconBlockBody where
  hashTreeRoot = genericHashTreeRoot

data BeaconBlock = BeaconBlock
  { bbSlot          :: !Slot
  , bbProposerIndex :: !ValidatorIndex
  , bbParentRoot    :: !Root
  , bbStateRoot     :: !Root
  , bbBody          :: !BeaconBlockBody
  } deriving stock (Generic, Eq, Show)

instance Ssz BeaconBlock where
  sszFixedSize = genericSszFixedSize @(Rep BeaconBlock)
instance SszEncode BeaconBlock where
  sszEncode = genericSszEncode
instance SszDecode BeaconBlock where
  sszDecode = genericSszDecode
instance SszHashTreeRoot BeaconBlock where
  hashTreeRoot = genericHashTreeRoot

data SignedBeaconBlock = SignedBeaconBlock
  { sbbBlock     :: !BeaconBlock
  , sbbSignature :: !XmssSignature
  } deriving stock (Generic, Eq, Show)

instance Ssz SignedBeaconBlock where
  sszFixedSize = genericSszFixedSize @(Rep SignedBeaconBlock)
instance SszEncode SignedBeaconBlock where
  sszEncode = genericSszEncode
instance SszDecode SignedBeaconBlock where
  sszDecode = genericSszDecode
instance SszHashTreeRoot SignedBeaconBlock where
  hashTreeRoot = genericHashTreeRoot

data BeaconBlockHeader = BeaconBlockHeader
  { bbhSlot          :: !Slot
  , bbhProposerIndex :: !ValidatorIndex
  , bbhParentRoot    :: !Root
  , bbhStateRoot     :: !Root
  , bbhBodyRoot      :: !Root
  } deriving stock (Generic, Eq, Show)

instance Ssz BeaconBlockHeader where
  sszFixedSize = genericSszFixedSize @(Rep BeaconBlockHeader)
instance SszEncode BeaconBlockHeader where
  sszEncode = genericSszEncode
instance SszDecode BeaconBlockHeader where
  sszDecode = genericSszDecode
instance SszHashTreeRoot BeaconBlockHeader where
  hashTreeRoot = genericHashTreeRoot

data Validator = Validator
  { vPubkey           :: !XmssPubkey
  , vEffectiveBalance :: !Gwei
  , vSlashed          :: !Bool
  , vActivationSlot   :: !Slot
  , vExitSlot         :: !Slot
  , vWithdrawableSlot :: !Slot
  } deriving stock (Generic, Eq, Show)

instance Ssz Validator where
  sszFixedSize = genericSszFixedSize @(Rep Validator)
instance SszEncode Validator where
  sszEncode = genericSszEncode
instance SszDecode Validator where
  sszDecode = genericSszDecode
instance SszHashTreeRoot Validator where
  hashTreeRoot = genericHashTreeRoot

data BeaconState = BeaconState
  { bsSlot                :: !Slot
  , bsLatestBlockHeader   :: !BeaconBlockHeader
  , bsBlockRoots          :: !(SszList HISTORICAL_ROOTS_LIMIT Root)
  , bsStateRoots          :: !(SszList HISTORICAL_ROOTS_LIMIT Root)
  , bsValidators          :: !(SszList VALIDATOR_REGISTRY_LIMIT Validator)
  , bsBalances            :: !(SszList VALIDATOR_REGISTRY_LIMIT Gwei)
  , bsJustifiedCheckpoint :: !Checkpoint
  , bsFinalizedCheckpoint :: !Checkpoint
  , bsCurrentAttestations :: !(SszList MAX_ATTESTATIONS AggregatedAttestation)
  } deriving stock (Generic, Eq, Show)

instance Ssz BeaconState where
  sszFixedSize = genericSszFixedSize @(Rep BeaconState)
instance SszEncode BeaconState where
  sszEncode = genericSszEncode
instance SszDecode BeaconState where
  sszDecode = genericSszDecode
instance SszHashTreeRoot BeaconState where
  hashTreeRoot = genericHashTreeRoot

-- ---------------------------------------------------------------------------
-- Fork choice types (not SSZ-serialized)
-- ---------------------------------------------------------------------------

data Store = Store
  { stJustifiedCheckpoint :: !Checkpoint
  , stFinalizedCheckpoint :: !Checkpoint
  , stBlocks              :: !(Map Root BeaconBlock)
  , stBlockStates         :: !(Map Root BeaconState)
  , stLatestMessages      :: !(Map ValidatorIndex LatestMessage)
  , stCurrentSlot         :: !Slot
  , stVoteHistory         :: !(Map ValidatorIndex [AttestationData])
  } deriving stock (Eq, Show)

data LatestMessage = LatestMessage
  { lmSlot :: !Slot
  , lmRoot :: !Root
  } deriving stock (Eq, Show)

-- | Evidence of a slashable offense.
data SlashingEvidence
  = DoubleVote AttestationData AttestationData
  | SurroundVote AttestationData AttestationData
  deriving stock (Eq, Show)
