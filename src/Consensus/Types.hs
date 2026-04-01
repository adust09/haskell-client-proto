{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Consensus types for pq-devnet-3, aligned with leanSpec.
module Consensus.Types
  ( -- * Crypto primitives
    XmssSignature (..)
  , mkXmssSignature
  , XmssPubkey (..)
  , mkXmssPubkey
  , LeanMultisigProof (..)
    -- * Aggregation types
  , AggregationBits
    -- * Core types
  , Config (..)
  , Checkpoint (..)
  , AttestationData (..)
  , SignedAttestation (..)
  , AggregatedAttestation (..)
  , SignedAggregatedAttestation (..)
  , AggregatedSignatureProof (..)
  , BeaconBlockBody (..)
  , BeaconBlock (..)
  , BlockSignatures (..)
  , SignedBeaconBlock (..)
  , BeaconBlockHeader (..)
  , Validator (..)
  , BeaconState (..)
    -- * Fork choice (non-SSZ)
  , Store (..)
  , currentSlot
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import Data.Word (Word64)
import GHC.Generics (Generic, Rep)
import SSZ.Bitlist (Bitlist)
import SSZ.Common
import SSZ.Container ()
import SSZ.Derive
import SSZ.List (SszList)
import SSZ.Merkleization (SszHashTreeRoot (..), merkleize, mixInLength, pack)
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

-- | ZK proof from leanMultisig aggregation (variable-size).
newtype LeanMultisigProof = LeanMultisigProof { unLeanMultisigProof :: ByteString }
  deriving stock (Eq, Show)

instance Ssz LeanMultisigProof where
  sszFixedSize = Nothing

instance SszEncode LeanMultisigProof where
  sszEncode = unLeanMultisigProof

instance SszDecode LeanMultisigProof where
  sszDecode = Right . LeanMultisigProof

-- | Max capacity for LeanMultisigProof in bytes (32KB).
leanMultisigProofMaxSize :: Int
leanMultisigProofMaxSize = 32768

instance SszHashTreeRoot LeanMultisigProof where
  hashTreeRoot (LeanMultisigProof bs) =
    let chunks = pack [bs]
        limit  = max 1 (fromIntegral ((leanMultisigProofMaxSize + 31) `div` 32))
    in  mixInLength (merkleize chunks limit) (fromIntegral (BS.length bs))

-- | Aggregation bits: global-scope bitlist (VALIDATOR_REGISTRY_LIMIT).
type AggregationBits = Bitlist VALIDATOR_REGISTRY_LIMIT

-- ---------------------------------------------------------------------------
-- Config (leanSpec)
-- ---------------------------------------------------------------------------

data Config = Config
  { cfgGenesisTime :: !Word64
  } deriving stock (Generic, Eq, Show)

instance Ssz Config where
  sszFixedSize = genericSszFixedSize @(Rep Config)
instance SszEncode Config where
  sszEncode = genericSszEncode
instance SszDecode Config where
  sszDecode = genericSszDecode
instance SszHashTreeRoot Config where
  hashTreeRoot = genericHashTreeRoot

-- ---------------------------------------------------------------------------
-- Core consensus types
-- ---------------------------------------------------------------------------

data Checkpoint = Checkpoint
  { cpRoot :: !Root
  , cpSlot :: !Slot
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
  { adSlot   :: !Slot
  , adHead   :: !Checkpoint
  , adTarget :: !Checkpoint
  , adSource :: !Checkpoint
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

-- | Unsigned aggregated attestation (leanSpec: aggregation_bits first, then data).
data AggregatedAttestation = AggregatedAttestation
  { aaAggregationBits :: !AggregationBits
  , aaData            :: !AttestationData
  } deriving stock (Generic, Eq, Show)

instance Ssz AggregatedAttestation where
  sszFixedSize = genericSszFixedSize @(Rep AggregatedAttestation)
instance SszEncode AggregatedAttestation where
  sszEncode = genericSszEncode
instance SszDecode AggregatedAttestation where
  sszDecode = genericSszDecode
instance SszHashTreeRoot AggregatedAttestation where
  hashTreeRoot = genericHashTreeRoot

-- | Signed aggregated attestation (leanSpec: contains data + proof).
data SignedAggregatedAttestation = SignedAggregatedAttestation
  { saaData  :: !AggregatedAttestation
  , saaProof :: !AggregatedSignatureProof
  } deriving stock (Generic, Eq, Show)

instance Ssz SignedAggregatedAttestation where
  sszFixedSize = genericSszFixedSize @(Rep SignedAggregatedAttestation)
instance SszEncode SignedAggregatedAttestation where
  sszEncode = genericSszEncode
instance SszDecode SignedAggregatedAttestation where
  sszDecode = genericSszDecode
instance SszHashTreeRoot SignedAggregatedAttestation where
  hashTreeRoot = genericHashTreeRoot

-- | Aggregated signature proof (leanSpec: BlockSignatures.attestation_signatures element).
data AggregatedSignatureProof = AggregatedSignatureProof
  { aspProof :: !LeanMultisigProof
  } deriving stock (Generic, Eq, Show)

instance Ssz AggregatedSignatureProof where
  sszFixedSize = genericSszFixedSize @(Rep AggregatedSignatureProof)
instance SszEncode AggregatedSignatureProof where
  sszEncode = genericSszEncode
instance SszDecode AggregatedSignatureProof where
  sszDecode = genericSszDecode
instance SszHashTreeRoot AggregatedSignatureProof where
  hashTreeRoot = genericHashTreeRoot

-- | Block body: contains unsigned attestations (leanSpec aligned).
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

-- | Block signatures container (leanSpec: BlockSignatures).
data BlockSignatures = BlockSignatures
  { bsigAttestationSignatures :: !(SszList MAX_ATTESTATION_SIGNATURES AggregatedSignatureProof)
  , bsigProposerSignature     :: !XmssSignature
  } deriving stock (Generic, Eq, Show)

instance Ssz BlockSignatures where
  sszFixedSize = genericSszFixedSize @(Rep BlockSignatures)
instance SszEncode BlockSignatures where
  sszEncode = genericSszEncode
instance SszDecode BlockSignatures where
  sszDecode = genericSszDecode
instance SszHashTreeRoot BlockSignatures where
  hashTreeRoot = genericHashTreeRoot

-- | Signed block (leanSpec: SignedBlock = message + BlockSignatures).
data SignedBeaconBlock = SignedBeaconBlock
  { sbbBlock     :: !BeaconBlock
  , sbbSignature :: !BlockSignatures
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
  { vAttestationPubkey :: !XmssPubkey
  , vProposalPubkey    :: !XmssPubkey
  , vIndex             :: !ValidatorIndex
  } deriving stock (Generic, Eq, Show)

instance Ssz Validator where
  sszFixedSize = genericSszFixedSize @(Rep Validator)
instance SszEncode Validator where
  sszEncode = genericSszEncode
instance SszDecode Validator where
  sszDecode = genericSszDecode
instance SszHashTreeRoot Validator where
  hashTreeRoot = genericHashTreeRoot

-- | Beacon state (leanSpec 10-field model).
data BeaconState = BeaconState
  { bsConfig                   :: !Config
  , bsSlot                     :: !Slot
  , bsLatestBlockHeader        :: !BeaconBlockHeader
  , bsLatestJustified          :: !Checkpoint
  , bsLatestFinalized          :: !Checkpoint
  , bsHistoricalBlockHashes    :: !(SszList HISTORICAL_BLOCK_HASHES_LIMIT Root)
  , bsJustifiedSlots           :: !(Bitlist JUSTIFIED_SLOTS_LIMIT)
  , bsValidators               :: !(SszList VALIDATORS_LIMIT Validator)
  , bsJustificationsRoots      :: !(SszList JUSTIFICATIONS_ROOTS_LIMIT Root)
  , bsJustificationsValidators :: !(Bitlist JUSTIFICATIONS_VALIDATORS_LIMIT)
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
  { stTime                  :: !Word64
  , stConfig                :: !Config
  , stHead                  :: !Checkpoint
  , stSafeTarget            :: !Checkpoint
  , stJustifiedCheckpoint   :: !Checkpoint
  , stFinalizedCheckpoint   :: !Checkpoint
  , stValidatorId           :: !ValidatorIndex
  , stAttestationSignatures :: !(Map Slot AggregatedSignatureProof)
  , stBlocks                :: !(Map Root BeaconBlock)
  , stBlockStates           :: !(Map Root BeaconState)
  } deriving stock (Eq, Show)

-- | Derive current slot from store time and genesis time.
currentSlot :: Store -> Slot
currentSlot store =
  let genesis = cfgGenesisTime (stConfig store)
      slotSec = slotDuration `div` 1_000_000
  in  if stTime store >= genesis
        then (stTime store - genesis) `div` fromIntegral slotSec
        else 0
