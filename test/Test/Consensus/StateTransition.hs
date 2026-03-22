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
zeroCheckpoint = Checkpoint 0 zeroRoot

zeroSig :: XmssSignature
zeroSig = case mkXmssSignature (BS.replicate xmssSignatureSize 0) of
  Right s -> s
  Left _  -> error "zeroSig"

mkBlockSignatures :: BlockSignatures
mkBlockSignatures =
  let emptyAttSigs = forceRight $ mkSszList @MAX_ATTESTATION_SIGNATURES []
  in  BlockSignatures emptyAttSigs zeroSig

mkValidator :: Gwei -> Slot -> Slot -> Validator
mkValidator balance activationSlot exitSlot = Validator
  { vPubkey           = zeroPubkey
  , vEffectiveBalance = balance
  , vSlashed          = False
  , vActivationSlot   = activationSlot
  , vExitSlot         = exitSlot
  , vWithdrawableSlot = maxBound
  }

mkValidatorWithPubkey :: Word8 -> Gwei -> Validator
mkValidatorWithPubkey w balance =
  let pk = case mkXmssPubkey (BS.replicate xmssPubkeySize w) of
             Right p -> p
             Left _  -> error "mkValidatorWithPubkey failed"
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
tests = testGroup "Consensus.StateTransition"
  [ testGroup "isActiveValidator"
      [ testCase "active at slot within range" $ do
          let v = mkValidator 32000000 0 100
          isActiveValidator v 50 @?= True
      , testCase "inactive before activation" $ do
          let v = mkValidator 32000000 10 100
          isActiveValidator v 5 @?= False
      , testCase "inactive at exit slot" $ do
          let v = mkValidator 32000000 0 100
          isActiveValidator v 100 @?= False
      , testCase "inactive if slashed" $ do
          let v = (mkValidator 32000000 0 100) { vSlashed = True }
          isActiveValidator v 50 @?= False
      , testCase "active at exact activation slot" $ do
          let v = mkValidator 32000000 10 100
          isActiveValidator v 10 @?= True
      , testCase "active one slot before exit" $ do
          let v = mkValidator 32000000 0 100
          isActiveValidator v 99 @?= True
      ]
  , testGroup "processSlots"
      [ testCase "no-op when target equals current" $ do
          let bs = mkGenesisState [mkValidator 32000000 0 maxBound]
          processSlots bs 0 @?= Right bs
      , testCase "rejects target < current" $ do
          let bs = (mkGenesisState [mkValidator 32000000 0 maxBound]) { bsSlot = 5 }
          case processSlots bs 3 of
            Left (SlotTooOld 3 5) -> pure ()
            other -> assertFailure $ "Expected SlotTooOld, got: " ++ show other
      , testCase "advances slot by 1" $ do
          let bs = mkGenesisState [mkValidator 32000000 0 maxBound]
          case processSlots bs 1 of
            Right bs1 -> bsSlot bs1 @?= 1
            Left err  -> assertFailure $ show err
      , testCase "advances multiple slots" $ do
          let bs = mkGenesisState [mkValidator 32000000 0 maxBound]
          case processSlots bs 5 of
            Right bs5 -> bsSlot bs5 @?= 5
            Left err  -> assertFailure $ show err
      ]
  , testGroup "processSlot"
      [ testCase "increments slot" $ do
          let bs = mkGenesisState [mkValidator 32000000 0 maxBound]
              bs1 = processSlot bs
          bsSlot bs1 @?= 1
      , testCase "appends block hash to historical_block_hashes" $ do
          let bs = mkGenesisState [mkValidator 32000000 0 maxBound]
              bs1 = processSlot bs
              hashes = unSszList (bsHistoricalBlockHashes bs1)
          length hashes @?= 1
      , testCase "accumulates historical block hashes over multiple slots" $ do
          let bs = mkGenesisState [mkValidator 32000000 0 maxBound]
          case processSlots bs 3 of
            Right bs3 -> do
              let hashes = unSszList (bsHistoricalBlockHashes bs3)
              length hashes @?= 3
            Left err -> assertFailure $ show err
      ]
  , testGroup "processBlockHeader"
      [ testCase "valid block header" $ do
          let vals = [mkValidatorWithPubkey 1 32000000]
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
          let bs = mkGenesisState [mkValidator 32000000 0 maxBound]
              block = BeaconBlock 5 0 zeroRoot zeroRoot mkEmptyBody
          case processBlockHeader bs block of
            Left (InvalidSlot 5 0) -> pure ()
            other -> assertFailure $ "Expected InvalidSlot, got: " ++ show other
      ]
  , testGroup "getProposerIndex"
      [ testCase "deterministic with single active validator" $ do
          let bs = mkGenesisState [mkValidator 32000000 0 maxBound]
          getProposerIndex bs @?= 0
      , testCase "rotates with slot" $ do
          let vals = [ mkValidatorWithPubkey 1 32000000
                     , mkValidatorWithPubkey 2 32000000
                     , mkValidatorWithPubkey 3 32000000
                     ]
              bs0 = mkGenesisState vals
              bs1 = bs0 { bsSlot = 1 }
              bs2 = bs0 { bsSlot = 2 }
          getProposerIndex bs0 @?= 0
          getProposerIndex bs1 @?= 1
          getProposerIndex bs2 @?= 2
      , testCase "skips inactive validators" $ do
          let vals = [ mkValidator 32000000 10 maxBound   -- inactive at slot 0
                     , mkValidatorWithPubkey 2 32000000   -- active
                     ]
              bs = mkGenesisState vals
          getProposerIndex bs @?= 1
      ]
  , testGroup "stateTransition"
      [ testCase "advances slot and updates header" $ do
          let vals = [mkValidatorWithPubkey 1 32000000]
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
          let vals = [mkValidatorWithPubkey 1 32000000]
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
          let vals = [mkValidatorWithPubkey 1 32000000]
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
          let vals = [mkValidatorWithPubkey 1 32000000]
              bs = mkGenesisState vals
              bs' = processJustificationFinalization bs
          bsLatestJustified bs' @?= zeroCheckpoint
      ]
  ]
