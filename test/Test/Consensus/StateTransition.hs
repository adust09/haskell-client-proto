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

mkRoot :: Word8 -> Root
mkRoot w = case mkBytesN @32 (BS.replicate 32 w) of
  Right r -> r
  Left _  -> error "mkRoot failed"

zeroPubkey :: XmssPubkey
zeroPubkey = case mkXmssPubkey (BS.replicate xmssPubkeySize 0) of
  Right pk -> pk
  Left _   -> error "zeroPubkey failed"

zeroCheckpoint :: Checkpoint
zeroCheckpoint = Checkpoint 0 zeroRoot

emptyProof :: AggregatedSignatureProof
emptyProof = AggregatedSignatureProof
  { aspParticipants = case mkBitlist @VALIDATOR_REGISTRY_LIMIT [] of
      Right b -> b
      Left _  -> error "emptyProof: mkBitlist"
  , aspProofData = case mkSszList @BYTES_PER_MIB [] of
      Right l -> l
      Left _  -> error "emptyProof: mkSszList"
  }

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
  let emptyRoots = case mkSszList @HISTORICAL_ROOTS_LIMIT [] of
                     Right sl -> sl
                     Left _   -> error "mkGenesisState: roots"
      valList = case mkSszList @VALIDATOR_REGISTRY_LIMIT vals of
                  Right sl -> sl
                  Left _   -> error "mkGenesisState: validators"
      balances = case mkSszList @VALIDATOR_REGISTRY_LIMIT
                      (map vEffectiveBalance vals) of
                   Right sl -> sl
                   Left _   -> error "mkGenesisState: balances"
      emptyAtts = case mkSszList @MAX_ATTESTATIONS [] of
                    Right sl -> sl
                    Left _   -> error "mkGenesisState: attestations"
  in  BeaconState
    { bsSlot                = 0
    , bsLatestBlockHeader   = BeaconBlockHeader 0 0 zeroRoot zeroRoot zeroRoot
    , bsBlockRoots          = emptyRoots
    , bsStateRoots          = emptyRoots
    , bsValidators          = valList
    , bsBalances            = balances
    , bsJustifiedCheckpoint = zeroCheckpoint
    , bsFinalizedCheckpoint = zeroCheckpoint
    , bsCurrentAttestations = emptyAtts
    }

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
      , testCase "caches state root" $ do
          let bs = mkGenesisState [mkValidator 32000000 0 maxBound]
              bs1 = processSlot bs
              stateRoot = toRoot bs
              storedRoot = head (unSszList (bsStateRoots bs1))
          storedRoot @?= stateRoot
      , testCase "prunes stale attestations" $ do
          let vals = [mkValidator 32000000 0 maxBound]
              bs = mkGenesisState vals
              -- Create an attestation at slot 0
              ad = AttestationData 0 zeroRoot zeroCheckpoint zeroCheckpoint
              bits = case mkBitlist @VALIDATOR_REGISTRY_LIMIT [True] of
                       Right b -> b
                       Left _  -> error "mkBitlist"
              saa = SignedAggregatedAttestation ad 0 bits emptyProof
              bs' = case mkSszList @MAX_ATTESTATIONS [saa] of
                      Right sl -> bs { bsCurrentAttestations = sl }
                      Left _   -> error "mkSszList"
              -- Advance 5 slots (retention window is 4)
          case processSlots bs' 5 of
            Right bs5 -> do
              let atts = unSszList (bsCurrentAttestations bs5)
              length atts @?= 0
            Left err -> assertFailure $ show err
      ]
  , testGroup "processBlockHeader"
      [ testCase "valid block header" $ do
          let vals = [mkValidatorWithPubkey 1 32000000]
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
          let bs = mkGenesisState [mkValidator 32000000 0 maxBound]
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
  , testGroup "slashing"
      [ testCase "double vote detected" $ do
          let ad1 = AttestationData 5 (mkRoot 1) zeroCheckpoint zeroCheckpoint
              ad2 = AttestationData 5 (mkRoot 2) zeroCheckpoint zeroCheckpoint
          case checkSlashingConditions [ad1] ad2 of
            Left (DoubleVote _ _) -> pure ()
            other -> assertFailure $ "Expected DoubleVote, got: " ++ show other
      , testCase "surround vote detected (new surrounds old)" $ do
          -- new: source=1, target=10 surrounds old: source=3, target=8
          let source1 = Checkpoint 3 (mkRoot 10)
              target1 = Checkpoint 8 (mkRoot 1)
              source2 = Checkpoint 1 (mkRoot 11)
              target2 = Checkpoint 10 (mkRoot 2)
              ad1 = AttestationData 5 zeroRoot source1 target1
              ad2 = AttestationData 3 zeroRoot source2 target2
          case checkSlashingConditions [ad1] ad2 of
            Left (SurroundVote _ _) -> pure ()
            other -> assertFailure $ "Expected SurroundVote, got: " ++ show other
      , testCase "surround vote detected (old surrounds new)" $ do
          -- old: source=1, target=10 surrounds new: source=3, target=8
          let source1 = Checkpoint 1 (mkRoot 10)
              target1 = Checkpoint 10 (mkRoot 1)
              source2 = Checkpoint 3 (mkRoot 11)
              target2 = Checkpoint 8 (mkRoot 2)
              ad1 = AttestationData 3 zeroRoot source1 target1
              ad2 = AttestationData 5 zeroRoot source2 target2
          case checkSlashingConditions [ad1] ad2 of
            Left (SurroundVote _ _) -> pure ()
            other -> assertFailure $ "Expected SurroundVote, got: " ++ show other
      , testCase "non-overlapping passes" $ do
          -- source=0, target=5 and source=5, target=10 — no overlap
          let source1 = Checkpoint 0 (mkRoot 10)
              target1 = Checkpoint 5 (mkRoot 1)
              source2 = Checkpoint 5 (mkRoot 11)
              target2 = Checkpoint 10 (mkRoot 2)
              ad1 = AttestationData 1 zeroRoot source1 target1
              ad2 = AttestationData 6 zeroRoot source2 target2
          checkSlashingConditions [ad1] ad2 @?= Right ()
      , testCase "slashValidator sets vSlashed and zero balance" $ do
          let vals = [mkValidator 32000000 0 maxBound]
              bs = mkGenesisState vals
              bs' = slashValidator bs 0
              v = head (unSszList (bsValidators bs'))
          vSlashed v @?= True
          vEffectiveBalance v @?= 0
      ]
  , testGroup "expandAggregationBits"
      [ testCase "single validator in subnet" $ do
          -- Validator 0 is in subnet 0 (0 % 4 == 0)
          let vals = [mkValidator 32000000 0 maxBound]
              valList = case mkSszList @VALIDATOR_REGISTRY_LIMIT vals of
                          Right sl -> sl
                          Left _   -> error "mkSszList"
              bits = case mkBitlist @VALIDATOR_REGISTRY_LIMIT [True] of
                       Right b -> b
                       Left _  -> error "mkBitlist"
          expandAggregationBits valList 0 bits @?= [0]
      , testCase "multiple validators, correct subnet mapping" $ do
          -- 4 validators: 0->subnet0, 1->subnet1, 2->subnet2, 3->subnet3
          -- 4 more: 4->subnet0, 5->subnet1, 6->subnet2, 7->subnet3
          let vals = replicate 8 (mkValidator 32000000 0 maxBound)
              valList = case mkSszList @VALIDATOR_REGISTRY_LIMIT vals of
                          Right sl -> sl
                          Left _   -> error "mkSszList"
              -- Bitlist uses LOCAL positions: subnet 0 has validators [0, 4]
              -- Local position 0 = validator 0, local position 1 = validator 4
              -- Set both local bits
              bits = case mkBitlist @VALIDATOR_REGISTRY_LIMIT [True, True] of
                       Right b -> b
                       Left _  -> error "mkBitlist"
          -- Subnet 0 has validators [0, 4], both local bits set
          expandAggregationBits valList 0 bits @?= [0, 4]
      ]
  ]

