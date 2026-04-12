module Test.Consensus.StateTransition (tests) where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit

import Consensus.Constants
import Consensus.Types
import Consensus.StateTransition
import SSZ.Common (mkBytesN, zeroN)
import SSZ.List (mkSszList, unSszList)
import SSZ.Bitlist (mkBitlist)
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
  let valList = case mkSszList @VALIDATOR_REGISTRY_LIMIT vals of
                  Right sl -> sl
                  Left _   -> error "mkGenesisState: validators"
      emptyHashes = case mkSszList @HISTORICAL_ROOTS_LIMIT [] of
                      Right sl -> sl
                      Left _   -> error "mkGenesisState: hashes"
      emptyJustSlots = case mkBitlist @HISTORICAL_ROOTS_LIMIT [] of
                         Right b -> b
                         Left _  -> error "mkGenesisState: justSlots"
      emptyJustRoots = case mkSszList @HISTORICAL_ROOTS_LIMIT [] of
                         Right sl -> sl
                         Left _   -> error "mkGenesisState: justRoots"
      emptyJustVals = case mkBitlist @1073741824 [] of
                        Right b -> b
                        Left _  -> error "mkGenesisState: justVals"
  in  BeaconState
    { bsConfig                    = Config 0
    , bsSlot                      = 0
    , bsLatestBlockHeader         = BeaconBlockHeader 0 0 zeroRoot zeroRoot zeroRoot
    , bsLatestJustified           = zeroCheckpoint
    , bsLatestFinalized           = zeroCheckpoint
    , bsHistoricalBlockHashes     = emptyHashes
    , bsJustifiedSlots            = emptyJustSlots
    , bsValidators                = valList
    , bsJustificationsRoots       = emptyJustRoots
    , bsJustificationsValidators  = emptyJustVals
    }

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Consensus.StateTransition"
  [ testGroup "processSlots"
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
      , testCase "appends block hash to historical" $ do
          let bs = mkGenesisState [mkValidator 0]
              bs1 = processSlot bs
              hashes = unSszList (bsHistoricalBlockHashes bs1)
          length hashes @?= 1
      ]
  , testGroup "processBlockHeader"
      [ testCase "valid block header" $ do
          let vals = [mkValidatorWithPubkey 1 0]
              bs = mkGenesisState vals
              bs1 = processSlot bs  -- advance to slot 1
              parentRoot = toRoot (bsLatestBlockHeader bs1)
              emptyBody = BeaconBlockBody
                { bbbAttestations = case mkSszList @MAX_ATTESTATIONS [] of
                    Right sl -> sl
                    Left _   -> error "mkSszList"
                }
              block = BeaconBlock
                { bbSlot          = 1
                , bbProposerIndex = getProposerIndex bs1
                , bbParentRoot    = parentRoot
                , bbStateRoot     = zeroRoot
                , bbBody          = emptyBody
                }
          case processBlockHeader bs1 block of
            Right bs2 -> bbhSlot (bsLatestBlockHeader bs2) @?= 1
            Left err  -> assertFailure $ show err
      , testCase "rejects wrong slot" $ do
          let bs = mkGenesisState [mkValidator 0]
              emptyBody = BeaconBlockBody
                { bbbAttestations = case mkSszList @MAX_ATTESTATIONS [] of
                    Right sl -> sl
                    Left _   -> error "mkSszList"
                }
              block = BeaconBlock 5 0 zeroRoot zeroRoot emptyBody
          case processBlockHeader bs block of
            Left (InvalidSlot 5 0) -> pure ()
            other -> assertFailure $ "Expected InvalidSlot, got: " ++ show other
      ]
  , testGroup "getProposerIndex"
      [ testCase "deterministic with single validator" $ do
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
      ]
  , testGroup "processAttestation"
      [ testCase "valid attestation passes" $ do
          let vals = [mkValidator 0]
              bs = mkGenesisState vals
              bs1 = processSlot bs
              ad = AttestationData 0 (Checkpoint zeroRoot 0) (Checkpoint zeroRoot 0) zeroCheckpoint
              bits = case mkBitlist @VALIDATOR_REGISTRY_LIMIT [True] of
                       Right b -> b
                       Left _  -> error "mkBitlist"
              aa = AggregatedAttestation bits ad
          case processAttestation bs1 aa of
            Right _bs2 -> pure ()
            Left err   -> assertFailure $ show err
      , testCase "rejects attestation from current or future slot" $ do
          let vals = [mkValidator 0]
              bs = mkGenesisState vals
              bs1 = processSlot bs  -- slot 1
              ad = AttestationData 1 (Checkpoint zeroRoot 0) (Checkpoint zeroRoot 0) zeroCheckpoint
              bits = case mkBitlist @VALIDATOR_REGISTRY_LIMIT [True] of
                       Right b -> b
                       Left _  -> error "mkBitlist"
              aa = AggregatedAttestation bits ad
          case processAttestation bs1 aa of
            Left (InvalidAttestationSlot 1 1) -> pure ()
            other -> assertFailure $ "Expected InvalidAttestationSlot, got: " ++ show other
      ]
  ]
