-- | Shared test helpers for network tests.
module Test.Support.Helpers
  ( forkIOSafe
  , mkTestValidator
  , mkTestGenesisState
  , mkTestGenesisBlock
  , mkTestSignedBlock
  , mkTestAttestation
  , zeroRoot
  , zeroCheckpoint
  , zeroSig
  , mkEmptyBody
  , toRoot
  ) where

import Control.Concurrent (forkIO, ThreadId)
import Control.Exception (SomeException, catch)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import qualified Data.Vector as V

import Consensus.Constants
import Consensus.Types
import Consensus.StateTransition (getProposerIndex, processSlot)
import SSZ.Common (mkBytesN, zeroN)
import SSZ.List (mkSszList)
import SSZ.Merkleization (SszHashTreeRoot (..))
import SSZ.Vector (mkSszVector)

-- | Fork a thread that silently catches exceptions (for test subscribers).
forkIOSafe :: IO () -> IO ThreadId
forkIOSafe action = forkIO (action `catch` (\(_ :: SomeException) -> pure ()))

-- | Convert hashTreeRoot to a Bytes32 Root.
toRoot :: SszHashTreeRoot a => a -> Root
toRoot a = case mkBytesN @32 (hashTreeRoot a) of
  Right r -> r
  Left _  -> error "toRoot: hashTreeRoot did not produce 32 bytes"

zeroRoot :: Root
zeroRoot = zeroN @32

zeroCheckpoint :: Checkpoint
zeroCheckpoint = Checkpoint 0 zeroRoot

zeroSig :: XmssSignature
zeroSig = case mkXmssSignature (BS.replicate xmssSignatureSize 0) of
  Right s -> s
  Left _  -> error "zeroSig"

mkTestValidator :: Word8 -> Gwei -> Validator
mkTestValidator w balance =
  let pk = case mkXmssPubkey (BS.replicate xmssPubkeySize w) of
             Right p -> p
             Left _  -> error "mkTestValidator"
  in  Validator pk balance False 0 maxBound maxBound

mkTestGenesisState :: [Validator] -> BeaconState
mkTestGenesisState vals =
  let emptyRoots = forceRight $
        mkSszVector @SLOTS_PER_HISTORICAL_ROOT (V.replicate 64 zeroRoot)
      valList = forceRight $ mkSszList @VALIDATOR_REGISTRY_LIMIT vals
      balances = forceRight $ mkSszList @VALIDATOR_REGISTRY_LIMIT (map vEffectiveBalance vals)
      emptyAtts = forceRight $ mkSszList @MAX_ATTESTATIONS_STATE []
      bodyRoot = toRoot mkEmptyBody
  in  BeaconState
    { bsSlot                = 0
    , bsLatestBlockHeader   = BeaconBlockHeader 0 0 zeroRoot zeroRoot bodyRoot
    , bsBlockRoots          = emptyRoots
    , bsStateRoots          = emptyRoots
    , bsValidators          = valList
    , bsBalances            = balances
    , bsJustifiedCheckpoint = zeroCheckpoint
    , bsFinalizedCheckpoint = zeroCheckpoint
    , bsCurrentAttestations = emptyAtts
    }

mkTestGenesisBlock :: BeaconBlock
mkTestGenesisBlock = BeaconBlock 0 0 zeroRoot zeroRoot mkEmptyBody

mkEmptyBody :: BeaconBlockBody
mkEmptyBody = BeaconBlockBody
  { bbbAttestations = forceRight $ mkSszList @MAX_ATTESTATIONS [] }

mkTestSignedBlock :: BeaconState -> Slot -> SignedBeaconBlock
mkTestSignedBlock st targetSlot =
  let st1 = advanceToSlot st targetSlot
      parentRoot = toRoot (bsLatestBlockHeader st1)
      block = BeaconBlock
        { bbSlot          = targetSlot
        , bbProposerIndex = getProposerIndex st1
        , bbParentRoot    = parentRoot
        , bbStateRoot     = zeroRoot
        , bbBody          = mkEmptyBody
        }
  in  SignedBeaconBlock block zeroSig

mkTestAttestation :: ValidatorIndex -> Slot -> Root -> Checkpoint -> Checkpoint -> SignedAttestation
mkTestAttestation vi slot headRoot source target =
  SignedAttestation
    { saData = AttestationData slot headRoot source target
    , saValidatorIndex = vi
    , saSignature = zeroSig
    }

advanceToSlot :: BeaconState -> Slot -> BeaconState
advanceToSlot bs target
  | bsSlot bs >= target = bs
  | otherwise = advanceToSlot (processSlot bs) target

forceRight :: Either e a -> a
forceRight (Right a) = a
forceRight (Left _)  = error "forceRight: unexpected Left"
