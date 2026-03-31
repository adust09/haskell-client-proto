-- | Shared test helpers for network tests.
module Test.Support.Helpers
  ( forkIOSafe
  , mkTestValidator
  , mkTestValidatorWithKey
  , mkTestGenesisState
  , mkTestGenesisBlock
  , mkTestGenesis
  , mkTestSignedBlock
  , mkTestSignedBlockWithAtts
  , mkTestAttestation
  , mkSignedTestAttestation
  , buildChain
  , advanceToSlot
  , zeroRoot
  , zeroCheckpoint
  , zeroSig
  , mkEmptyBody
  , mkBlockSignatures
  , toRoot
  ) where

import Control.Concurrent (forkIO, ThreadId)
import Control.Exception (SomeException, catch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Time.Clock (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Data.Word (Word8)

import Consensus.Constants
import Consensus.Types
import Consensus.StateTransition (getProposerIndex, processSlot, stateTransition)
import Crypto.LeanSig (PrivateKey, generateKeyPair, sign)
import Crypto.SigningRoot (computeSigningRoot)
import Genesis (GenesisConfig (..), GenesisValidator (..))
import SSZ.Bitlist (mkBitlist)
import SSZ.Common (mkBytesN, unBytesN, zeroN)
import SSZ.List (mkSszList)
import SSZ.Merkleization (SszHashTreeRoot (..))

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

mkBlockSignatures :: BlockSignatures
mkBlockSignatures =
  let emptyAttSigs = forceRight $ mkSszList @MAX_ATTESTATION_SIGNATURES []
  in  BlockSignatures emptyAttSigs zeroSig

mkTestValidator :: Word8 -> Gwei -> Validator
mkTestValidator w balance =
  let pk = case mkXmssPubkey (BS.replicate xmssPubkeySize w) of
             Right p -> p
             Left _  -> error "mkTestValidator"
  in  Validator pk balance False 0 maxBound maxBound

-- | Create a validator with a real Ed25519-backed key pair for signing tests.
-- Returns (PrivateKey, Validator).
mkTestValidatorWithKey :: Int -> Gwei -> (PrivateKey, Validator)
mkTestValidatorWithKey idx balance =
  let seed = BS8.pack ("test-validator-seed-" <> show idx)
      (privKey, pubKey) = forceRight $ generateKeyPair 10 seed
  in  (privKey, Validator pubKey balance False 0 maxBound maxBound)

-- | Create a properly signed attestation using a real private key.
mkSignedTestAttestation :: PrivateKey -> ValidatorIndex -> Slot -> Root
                        -> Checkpoint -> Checkpoint -> Domain -> SignedAttestation
mkSignedTestAttestation privKey vi slot headRoot source target domain =
  let attData = AttestationData slot headRoot source target
      signingRoot = computeSigningRoot attData domain
      message = unBytesN signingRoot
      sig = forceRight $ sign privKey message 0
  in  SignedAttestation attData vi sig

mkTestGenesisState :: [Validator] -> BeaconState
mkTestGenesisState vals =
  let config = Config { cfgGenesisTime = 0 }
      valList = forceRight $ mkSszList @VALIDATORS_LIMIT vals
      emptyHashes = forceRight $ mkSszList @HISTORICAL_BLOCK_HASHES_LIMIT []
      emptyJSlots = forceRight $ mkBitlist @JUSTIFIED_SLOTS_LIMIT []
      emptyJRoots = forceRight $ mkSszList @JUSTIFICATIONS_ROOTS_LIMIT []
      emptyJVals  = forceRight $ mkBitlist @JUSTIFICATIONS_VALIDATORS_LIMIT []
      bodyRoot = toRoot mkEmptyBody
  in  BeaconState
    { bsConfig                   = config
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

mkTestGenesisBlock :: BeaconBlock
mkTestGenesisBlock = BeaconBlock 0 0 zeroRoot zeroRoot mkEmptyBody

-- | Create a GenesisConfig with 2 test validators for integration testing.
mkTestGenesis :: GenesisConfig
mkTestGenesis =
  let pk1 = case mkXmssPubkey (BS.replicate xmssPubkeySize 0) of
               Right p -> p
               Left _  -> error "mkTestGenesis: pk1"
      pk2 = case mkXmssPubkey (BS.replicate xmssPubkeySize 1) of
               Right p -> p
               Left _  -> error "mkTestGenesis: pk2"
      forkVer = case mkBytesN @4 (BS.pack [0, 0, 0, 1]) of
                  Right v -> v
                  Left _  -> error "mkTestGenesis: forkVersion"
  in  GenesisConfig
    { gcGenesisTime = UTCTime (fromGregorian 2026 1 1) 0
    , gcValidators  = [ GenesisValidator pk1 32000000000
                      , GenesisValidator pk2 32000000000
                      ]
    , gcForkVersion = forkVer
    , gcChainId     = 1337
    }

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
  in  SignedBeaconBlock block mkBlockSignatures

mkTestSignedBlockWithAtts :: BeaconState -> Slot -> [AggregatedAttestation] -> SignedBeaconBlock
mkTestSignedBlockWithAtts st targetSlot atts =
  let st1 = advanceToSlot st targetSlot
      parentRoot = toRoot (bsLatestBlockHeader st1)
      body = BeaconBlockBody
        { bbbAttestations = forceRight $ mkSszList @MAX_ATTESTATIONS atts }
      block = BeaconBlock
        { bbSlot          = targetSlot
        , bbProposerIndex = getProposerIndex st1
        , bbParentRoot    = parentRoot
        , bbStateRoot     = zeroRoot
        , bbBody          = body
        }
  in  SignedBeaconBlock block mkBlockSignatures

-- | Build a chain of empty blocks from genesis, returning signed blocks and post-states.
buildChain :: BeaconState -> Int -> ([SignedBeaconBlock], [BeaconState])
buildChain gs n = go gs (1 :: Slot) n [] []
  where
    go _st _slot 0 accBlocks accStates = (reverse accBlocks, reverse accStates)
    go st slot remaining accBlocks accStates =
      let sbb = mkTestSignedBlock st slot
          st' = forceRight $ stateTransition st sbb False
      in  go st' (slot + 1) (remaining - 1) (sbb : accBlocks) (st' : accStates)

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
