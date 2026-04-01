{-# OPTIONS_GHC -Wno-orphans #-}

-- | NFData orphan instances and pre-computed benchmark fixtures.
module Bench.Support.Generators
  ( -- * Benchmark fixtures
    genesisState4
  , genesisState128
  , sampleBlock4
  , sampleHeader
  , sampleCheckpoint
  , sampleSignedBlock4
  , sampleSignedBlock128
  , chunk32
  , chunk32b
  , chunks4
  , chunks64
  , chunks256
  , bs64
  , bs1024
    -- * Chain builders
  , chainStore5
  , chainStore50
  , nextBlock5
  , nextBlock50
  , headRoot5
  , headRoot50
  ) where

import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Word (Word64)

import Consensus.Constants
import Consensus.Types
import Consensus.StateTransition (StateTransitionError, getProposerIndex, processSlot, stateTransition)
import Consensus.ForkChoice (ForkChoiceError, initStore, onBlock, onTick)
import SSZ.Bitlist (Bitlist, unBitlist, bitlistLen, mkBitlist)
import SSZ.Bitvector (Bitvector, unBitvector)
import SSZ.Common (BytesN, SszError, unBytesN, mkBytesN, zeroN)
import SSZ.List (SszList, unSszList, mkSszList)
import SSZ.Merkleization (SszHashTreeRoot (..))
import SSZ.Vector (SszVector, unSszVector)

-- ---------------------------------------------------------------------------
-- NFData orphan instances for SSZ types
-- ---------------------------------------------------------------------------

instance NFData (BytesN n) where
  rnf x = rnf (unBytesN x)

instance NFData a => NFData (SszList n a) where
  rnf x = rnf (unSszList x)

instance NFData a => NFData (SszVector n a) where
  rnf x = rnf (V.toList (unSszVector x))

instance NFData (Bitvector n) where
  rnf x = rnf (unBitvector x)

instance NFData (Bitlist n) where
  rnf x = rnf (unBitlist x) `seq` rnf (bitlistLen x)

-- ---------------------------------------------------------------------------
-- NFData orphan instances for Consensus types
-- ---------------------------------------------------------------------------

instance NFData XmssSignature where
  rnf (XmssSignature bs) = rnf bs

instance NFData XmssPubkey where
  rnf (XmssPubkey bs) = rnf bs

instance NFData LeanMultisigProof where
  rnf (LeanMultisigProof bs) = rnf bs

instance NFData Config
instance NFData Checkpoint
instance NFData AttestationData
instance NFData SignedAttestation
instance NFData AggregatedAttestation
instance NFData SignedAggregatedAttestation
instance NFData AggregatedSignatureProof
instance NFData BeaconBlockBody
instance NFData BeaconBlock
instance NFData BlockSignatures
instance NFData SignedBeaconBlock
instance NFData BeaconBlockHeader
instance NFData Validator
instance NFData BeaconState

-- NFData for Store (no Generic, manual instance)
instance NFData Store where
  rnf (Store t c h st jc fc vi as bs bss) =
    rnf t `seq` rnf c `seq` rnf h `seq` rnf st `seq`
    rnf jc `seq` rnf fc `seq` rnf vi `seq` rnf as `seq`
    rnf bs `seq` rnf bss

-- NFData for error types
instance NFData SszError where
  rnf x = x `seq` ()

instance NFData StateTransitionError where
  rnf x = x `seq` ()

instance NFData ForkChoiceError where
  rnf x = x `seq` ()

-- ---------------------------------------------------------------------------
-- Helper functions
-- ---------------------------------------------------------------------------

forceRight :: Either e a -> a
forceRight (Right a) = a
forceRight (Left _)  = error "forceRight: unexpected Left"

zeroRoot :: Root
zeroRoot = zeroN @32

zeroCheckpoint :: Checkpoint
zeroCheckpoint = Checkpoint zeroRoot 0

zeroSig :: XmssSignature
zeroSig = forceRight $ mkXmssSignature (BS.replicate xmssSignatureSize 0)

mkBlockSignatures :: BlockSignatures
mkBlockSignatures =
  let emptyAttSigs = forceRight $ mkSszList @MAX_ATTESTATION_SIGNATURES []
  in  BlockSignatures emptyAttSigs zeroSig

mkEmptyBody :: BeaconBlockBody
mkEmptyBody = BeaconBlockBody
  { bbbAttestations = forceRight $ mkSszList @MAX_ATTESTATIONS [] }

mkTestValidator :: Int -> Validator
mkTestValidator idx =
  let w = fromIntegral (idx `mod` 256)
      pk = forceRight $ mkXmssPubkey (BS.replicate xmssPubkeySize w)
  in  Validator pk pk (fromIntegral idx)

toRoot :: SszHashTreeRoot a => a -> Root
toRoot a = forceRight $ mkBytesN @32 (hashTreeRoot a)

mkGenesisState :: Int -> BeaconState
mkGenesisState n =
  let vals = map mkTestValidator [0 .. n - 1]
      valList = forceRight $ mkSszList @VALIDATORS_LIMIT vals
      emptyHashes = forceRight $ mkSszList @HISTORICAL_BLOCK_HASHES_LIMIT []
      emptyJSlots = forceRight $ mkBitlist @JUSTIFIED_SLOTS_LIMIT []
      emptyJRoots = forceRight $ mkSszList @JUSTIFICATIONS_ROOTS_LIMIT []
      emptyJVals  = forceRight $ mkBitlist @JUSTIFICATIONS_VALIDATORS_LIMIT []
      bodyRoot = toRoot mkEmptyBody
  in  BeaconState
    { bsConfig                   = Config { cfgGenesisTime = 0 }
    , bsSlot                     = 0
    , bsLatestBlockHeader        = BeaconBlockHeader 0 0 zeroRoot zeroRoot bodyRoot
    , bsLatestJustified          = zeroCheckpoint
    , bsLatestFinalized          = zeroCheckpoint
    , bsHistoricalBlockHashes    = emptyHashes
    , bsJustifiedSlots           = emptyJSlots
    , bsValidators               = valList
    , bsJustificationsRoots      = emptyJRoots
    , bsJustificationsValidators = emptyJVals
    }

advanceToSlot :: BeaconState -> Slot -> BeaconState
advanceToSlot bs target
  | bsSlot bs >= target = bs
  | otherwise = advanceToSlot (processSlot bs) target

mkSignedBlock :: BeaconState -> Slot -> SignedBeaconBlock
mkSignedBlock st targetSlot =
  let st1 = advanceToSlot st targetSlot
      parentRoot = toRoot (bsLatestBlockHeader st1)
      block = BeaconBlock
        { bbSlot          = targetSlot
        , bbProposerIndex = getProposerIndex st1
        , bbParentRoot    = parentRoot
        , bbStateRoot     = zeroRoot
        , bbBody          = mkEmptyBody
        }
  in  SignedBeaconBlock block mkBlockSignatures

-- | Build a chain of N empty blocks, returning all states and signed blocks.
buildChain :: BeaconState -> Int -> ([SignedBeaconBlock], [BeaconState])
buildChain gs n = go gs (1 :: Slot) n [] []
  where
    go _st _slot 0 accBlocks accStates = (reverse accBlocks, reverse accStates)
    go st slot remaining accBlocks accStates =
      let sbb = mkSignedBlock st slot
          st' = forceRight $ stateTransition st sbb False
      in  go st' (slot + 1) (remaining - 1) (sbb : accBlocks) (st' : accStates)

-- | Slot duration in seconds (slotDuration is in microseconds).
slotSec :: Word64
slotSec = 4

-- | Build a Store with a chain of N blocks.
-- Advances store time via onTick before each onBlock to avoid BlockSlotInFuture.
buildStoreWithChain :: Int -> Int -> (Store, SignedBeaconBlock)
buildStoreWithChain numValidators chainLen =
  let gs = mkGenesisState numValidators
      genesisBlock = BeaconBlock 0 0 zeroRoot zeroRoot mkEmptyBody
      store0 = initStore gs genesisBlock
      (blocks, _states) = buildChain gs chainLen
      -- Advance time before each block so currentSlot >= blockSlot
      addBlock s sbb =
        let slot = bbSlot (sbbBlock sbb)
            s' = onTick s (slot * slotSec)
        in  forceRight $ onBlock s' sbb
      store = foldl addBlock store0 blocks
      nextSlot = fromIntegral chainLen + 1
      lastState = case Map.elems (stBlockStates store) of
                    [] -> gs
                    ss -> last ss
      -- Advance time for the next block too
      storeForNext = onTick store (nextSlot * slotSec)
      nextSbb = mkSignedBlock lastState nextSlot
  in  (storeForNext, nextSbb)

-- ---------------------------------------------------------------------------
-- Pre-computed benchmark fixtures (CAFs)
-- ---------------------------------------------------------------------------

genesisState4 :: BeaconState
genesisState4 = mkGenesisState 4

genesisState128 :: BeaconState
genesisState128 = mkGenesisState 128

sampleBlock4 :: BeaconBlock
sampleBlock4 = BeaconBlock
  { bbSlot          = 1
  , bbProposerIndex = 0
  , bbParentRoot    = zeroRoot
  , bbStateRoot     = zeroRoot
  , bbBody          = mkEmptyBody
  }

sampleHeader :: BeaconBlockHeader
sampleHeader = BeaconBlockHeader 1 0 zeroRoot zeroRoot (toRoot mkEmptyBody)

sampleCheckpoint :: Checkpoint
sampleCheckpoint = Checkpoint zeroRoot 0

sampleSignedBlock4 :: SignedBeaconBlock
sampleSignedBlock4 = mkSignedBlock genesisState4 1

sampleSignedBlock128 :: SignedBeaconBlock
sampleSignedBlock128 = mkSignedBlock genesisState128 1

-- Raw byte chunks for merkleization benchmarks
chunk32 :: ByteString
chunk32 = BS.replicate 32 0xAB

chunk32b :: ByteString
chunk32b = BS.replicate 32 0xCD

chunks4 :: [ByteString]
chunks4 = [BS.replicate 32 (fromIntegral i) | i <- [0 :: Int .. 3]]

chunks64 :: [ByteString]
chunks64 = [BS.replicate 32 (fromIntegral (i `mod` 256)) | i <- [0 :: Int .. 63]]

chunks256 :: [ByteString]
chunks256 = [BS.replicate 32 (fromIntegral (i `mod` 256)) | i <- [0 :: Int .. 255]]

bs64 :: ByteString
bs64 = BS.replicate 64 0xEF

bs1024 :: ByteString
bs1024 = BS.replicate 1024 0xDE

-- Fork choice fixtures
chainStore5 :: (Store, SignedBeaconBlock)
chainStore5 = buildStoreWithChain 4 5

chainStore50 :: (Store, SignedBeaconBlock)
chainStore50 = buildStoreWithChain 4 50

nextBlock5 :: SignedBeaconBlock
nextBlock5 = snd chainStore5

nextBlock50 :: SignedBeaconBlock
nextBlock50 = snd chainStore50

headRoot5 :: Root
headRoot5 = case stHead (fst chainStore5) of
  Checkpoint r _ -> r

headRoot50 :: Root
headRoot50 = case stHead (fst chainStore50) of
  Checkpoint r _ -> r
