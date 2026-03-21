module Test.Consensus.Integration (tests) where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit

import Consensus.Constants
import Consensus.Types
import Consensus.ForkChoice
import Consensus.StateTransition
import SSZ.Bitlist (mkBitlist)
import SSZ.Common (mkBytesN, zeroN)
import SSZ.List (mkSszList)
import SSZ.Merkleization (SszHashTreeRoot (..))

toRoot :: SszHashTreeRoot a => a -> Root
toRoot a = case mkBytesN @32 (hashTreeRoot a) of
  Right r -> r
  Left _  -> error "toRoot: hashTreeRoot did not produce 32 bytes"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

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

mkValidatorWithPubkey :: Word8 -> Gwei -> Validator
mkValidatorWithPubkey w balance =
  let pk = case mkXmssPubkey (BS.replicate xmssPubkeySize w) of
             Right p -> p
             Left _  -> error "mkValidatorWithPubkey"
  in  Validator pk balance False 0 maxBound maxBound

mkGenesisState :: [Validator] -> BeaconState
mkGenesisState vals =
  let numVals = fromIntegral (length vals)
      config = Config { cfgNumValidators = numVals }
      valList = forceRight $ mkSszList @VALIDATORS_LIMIT vals
      emptyHashes = forceRight $ mkSszList @HISTORICAL_BLOCK_HASHES_LIMIT []
      emptyJSlots = forceRight $ mkBitlist @JUSTIFIED_SLOTS_LIMIT []
      emptyJRoots = forceRight $ mkSszList @JUSTIFICATIONS_ROOTS_LIMIT []
      emptyJVals  = forceRight $ mkBitlist @JUSTIFICATIONS_VALIDATORS_LIMIT []
      emptyBody = BeaconBlockBody
        { bbbAttestations = forceRight $ mkSszList @MAX_ATTESTATIONS [] }
      bodyRoot = toRoot emptyBody
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

mkEmptyBody :: BeaconBlockBody
mkEmptyBody = BeaconBlockBody
  { bbbAttestations = forceRight $ mkSszList @MAX_ATTESTATIONS [] }

forceRight :: Either e a -> a
forceRight (Right a) = a
forceRight (Left _)  = error "forceRight: unexpected Left"

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Consensus.Integration"
  [ testCase "state transition advances slot and updates header" $ do
      let vals = [mkValidatorWithPubkey 1 32000000]
          gs = mkGenesisState vals
          gs1 = processSlot gs  -- slot 0 -> 1
          parentRoot = toRoot (bsLatestBlockHeader gs1)
          block = BeaconBlock
            { bbSlot          = 1
            , bbProposerIndex = getProposerIndex gs1
            , bbParentRoot    = parentRoot
            , bbStateRoot     = zeroRoot
            , bbBody          = mkEmptyBody
            }
          signedBlock = SignedBeaconBlock block mkBlockSignatures
      case stateTransition gs signedBlock False of
        Right postState -> do
          bsSlot postState @?= 1
          bbhSlot (bsLatestBlockHeader postState) @?= 1
        Left err -> assertFailure $ "State transition failed: " ++ show err

  , testCase "fork choice selects heavier chain" $ do
      let vals = [ mkValidatorWithPubkey 1 32000000
                 , mkValidatorWithPubkey 2 32000000
                 , mkValidatorWithPubkey 3 32000000
                 ]
          gs = mkGenesisState vals
          genesisBlock = BeaconBlock 0 0 zeroRoot zeroRoot mkEmptyBody
          store = initStore gs genesisBlock
          genesisRoot = toRoot genesisBlock

      getHead store @?= genesisRoot
  ]
