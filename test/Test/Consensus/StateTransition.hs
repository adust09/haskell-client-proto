module Test.Consensus.StateTransition (tests) where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit

import Consensus.Constants
import Consensus.Types
import Consensus.StateTransition
import SSZ.Bitlist (mkBitlist)
import SSZ.Common (mkBytesN, zeroN)
import SSZ.List (mkSszList, unSszList)
import SSZ.Merkleization (SszHashTreeRoot (..))

toRoot :: SszHashTreeRoot a => a -> Root
toRoot a = case mkBytesN @32 (hashTreeRoot a) of
  Right r -> r
  Left _  -> error "toRoot: hashTreeRoot did not produce 32 bytes"

-- ---------------------------------------------------------------------------
-- Test helpers
-- ---------------------------------------------------------------------------

zeroRoot :: Root
zeroRoot = zeroN @32


zeroPubkey :: XmssPubkey
zeroPubkey = case mkXmssPubkey (BS.replicate xmssPubkeySize 0) of
  Right pk -> pk
  Left _   -> error "zeroPubkey failed"

zeroCheckpoint :: Checkpoint
zeroCheckpoint = Checkpoint zeroRoot 0

zeroSig :: XmssSignature
zeroSig = case mkXmssSignature (BS.replicate xmssSignatureSize 0) of
  Right s -> s
  Left _  -> error "zeroSig"

mkBlockSignatures :: BlockSignatures
mkBlockSignatures =
  let emptyAttSigs = forceRight $ mkSszList @MAX_ATTESTATION_SIGNATURES []
  in  BlockSignatures emptyAttSigs zeroSig

mkValidator :: ValidatorIndex -> Validator
mkValidator idx = Validator
  { vAttestationPubkey = zeroPubkey
  , vProposalPubkey    = zeroPubkey
  , vIndex             = idx
  }

mkValidatorWithPubkey :: Word8 -> ValidatorIndex -> Validator
mkValidatorWithPubkey w idx =
  let pk = case mkXmssPubkey (BS.replicate xmssPubkeySize w) of
             Right p -> p
             Left _  -> error "mkValidatorWithPubkey failed"
  in  Validator pk pk idx

mkGenesisState :: [Validator] -> BeaconState
mkGenesisState vals =
  let config = Config { cfgGenesisTime = 0 }
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
tests = testGroup "Consensus.StateTransition"
  [ testGroup "isActiveValidator"
      [ testCase "always active in leanSpec" $ do
          let v = mkValidator 0
          isActiveValidator v 0 @?= True
          isActiveValidator v 50 @?= True
          isActiveValidator v maxBound @?= True
      ]
  , testGroup "processSlots"
      [ testCase "no-op when target equals current" $ do
          let bs = mkGenesisState [mkValidator 0]
          processSlots bs 0 @?= Right bs
      , testCase "rejects target < current" $ do
          let bs = (mkGenesisState [mkValidator 0]) { bsSlot = 5 }
          case processSlots bs 3 of
            Left (SlotTooOld 3 5) -> pure ()
            other -> assertFailure $ "Expected SlotTooOld, got: " ++ show other
      , testCase "advances slot by 1" $ do
          let bs = mkGenesisState [mkValidator 0]
          case processSlots bs 1 of
            Right bs1 -> bsSlot bs1 @?= 1
            Left err  -> assertFailure $ show err
      , testCase "advances multiple slots" $ do
          let bs = mkGenesisState [mkValidator 0]
          case processSlots bs 5 of
            Right bs5 -> bsSlot bs5 @?= 5
            Left err  -> assertFailure $ show err
      ]
  , testGroup "processSlot"
      [ testCase "increments slot" $ do
          let bs = mkGenesisState [mkValidator 0]
              bs1 = processSlot bs
          bsSlot bs1 @?= 1
      , testCase "appends block hash to historical_block_hashes" $ do
          let bs = mkGenesisState [mkValidator 0]
              bs1 = processSlot bs
              hashes = unSszList (bsHistoricalBlockHashes bs1)
          length hashes @?= 1
      , testCase "accumulates historical block hashes over multiple slots" $ do
          let bs = mkGenesisState [mkValidator 0]
          case processSlots bs 3 of
            Right bs3 -> do
              let hashes = unSszList (bsHistoricalBlockHashes bs3)
              length hashes @?= 3
            Left err -> assertFailure $ show err
      ]
  , testGroup "processBlockHeader"
      [ testCase "valid block header" $ do
          let vals = [mkValidatorWithPubkey 1 0]
              bs = mkGenesisState vals
              bs1 = processSlot bs  -- advance to slot 1
              parentRoot = toRoot (bsLatestBlockHeader bs1)
              block = BeaconBlock
                { bbSlot          = 1
                , bbProposerIndex = getProposerIndex bs1
                , bbParentRoot    = parentRoot
                , bbStateRoot     = zeroRoot
                , bbBody          = mkEmptyBody
                }
          case processBlockHeader bs1 block of
            Right bs2 -> bbhSlot (bsLatestBlockHeader bs2) @?= 1
            Left err  -> assertFailure $ show err
      , testCase "rejects wrong slot" $ do
          let bs = mkGenesisState [mkValidator 0]
              block = BeaconBlock 5 0 zeroRoot zeroRoot mkEmptyBody
          case processBlockHeader bs block of
            Left (InvalidSlot 5 0) -> pure ()
            other -> assertFailure $ "Expected InvalidSlot, got: " ++ show other
      ]
  , testGroup "getProposerIndex"
      [ testCase "deterministic with single active validator" $ do
          let bs = mkGenesisState [mkValidator 0]
          getProposerIndex bs @?= 0
      , testCase "rotates with slot" $ do
          let vals = [ mkValidatorWithPubkey 1 0
                     , mkValidatorWithPubkey 2 1
                     , mkValidatorWithPubkey 3 2
                     ]
              bs0 = mkGenesisState vals
              bs1 = bs0 { bsSlot = 1 }
              bs2 = bs0 { bsSlot = 2 }
          getProposerIndex bs0 @?= 0
          getProposerIndex bs1 @?= 1
          getProposerIndex bs2 @?= 2
      , testCase "all validators always active in leanSpec" $ do
          let vals = [ mkValidator 0
                     , mkValidatorWithPubkey 2 1
                     ]
              bs = mkGenesisState vals
          -- slot 0 mod 2 == 0, so validator 0 is proposer
          getProposerIndex bs @?= 0
      ]
  , testGroup "stateTransition"
      [ testCase "advances slot and updates header" $ do
          let vals = [mkValidatorWithPubkey 1 0]
              bs = mkGenesisState vals
              bs1 = processSlot bs
              parentRoot = toRoot (bsLatestBlockHeader bs1)
              block = BeaconBlock
                { bbSlot          = 1
                , bbProposerIndex = getProposerIndex bs1
                , bbParentRoot    = parentRoot
                , bbStateRoot     = zeroRoot
                , bbBody          = mkEmptyBody
                }
              signedBlock = SignedBeaconBlock block mkBlockSignatures
          case stateTransition bs signedBlock False of
            Right postState -> do
              bsSlot postState @?= 1
              bbhSlot (bsLatestBlockHeader postState) @?= 1
            Left err -> assertFailure $ "State transition failed: " ++ show err
      ]
  , testGroup "processAttestation"
      [ testCase "accepts valid past-slot attestation" $ do
          let vals = [mkValidatorWithPubkey 1 0]
              bs = (mkGenesisState vals) { bsSlot = 2 }
              bits = forceRight $ mkBitlist @VALIDATOR_REGISTRY_LIMIT [True]
              att = AggregatedAttestation
                { aaData = AttestationData 1 zeroRoot zeroCheckpoint zeroCheckpoint
                , aaAggregationBits = bits
                }
          case processAttestation bs att of
            Right _ -> pure ()
            Left err -> assertFailure $ "Expected success, got: " ++ show err
      , testCase "rejects future-slot attestation" $ do
          let vals = [mkValidatorWithPubkey 1 0]
              bs = (mkGenesisState vals) { bsSlot = 1 }
              bits = forceRight $ mkBitlist @VALIDATOR_REGISTRY_LIMIT [True]
              att = AggregatedAttestation
                { aaData = AttestationData 5 zeroRoot zeroCheckpoint zeroCheckpoint
                , aaAggregationBits = bits
                }
          case processAttestation bs att of
            Left (InvalidAttestationSlot 5 1) -> pure ()
            other -> assertFailure $ "Expected InvalidAttestationSlot, got: " ++ show other
      ]
  , testGroup "processJustificationFinalization"
      [ testCase "no justification with no votes" $ do
          let vals = [mkValidatorWithPubkey 1 0]
              bs = mkGenesisState vals
              bs' = processJustificationFinalization bs
          bsLatestJustified bs' @?= zeroCheckpoint
      ]
  ]
