module Test.Consensus.ForkChoice (tests) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit

import Consensus.Constants
import Consensus.Types
import Consensus.ForkChoice
import SSZ.Common (mkBytesN, zeroN)
import SSZ.List (mkSszList)
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

zeroSig :: XmssSignature
zeroSig = case mkXmssSignature (BS.replicate xmssSignatureSize 0) of
  Right s -> s
  Left _  -> error "zeroSig failed"

zeroCheckpoint :: Checkpoint
zeroCheckpoint = Checkpoint 0 zeroRoot

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

mkEmptyBody :: BeaconBlockBody
mkEmptyBody = BeaconBlockBody
  { bbbAttestations = case mkSszList @MAX_ATTESTATIONS [] of
      Right sl -> sl
      Left _   -> error "mkEmptyBody"
  }

mkGenesisBlock :: BeaconBlock
mkGenesisBlock = BeaconBlock 0 0 zeroRoot zeroRoot mkEmptyBody

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Consensus.ForkChoice"
  [ testGroup "initStore"
      [ testCase "genesis head is genesis block root" $ do
          let gs = mkGenesisState [mkValidatorWithPubkey 1 32000000]
              gb = mkGenesisBlock
              store = initStore gs gb
              head' = getHead store
              expectedRoot = toRoot gb
          head' @?= expectedRoot
      ]
  , testGroup "getHead"
      [ testCase "returns genesis with no other blocks" $ do
          let gs = mkGenesisState [mkValidatorWithPubkey 1 32000000]
              gb = mkGenesisBlock
              store = initStore gs gb
          getHead store @?= toRoot gb
      ]
  , testGroup "onAttestation"
      [ testCase "updates latest message" $ do
          let gs = mkGenesisState [mkValidatorWithPubkey 1 32000000]
              gb = mkGenesisBlock
              store = initStore gs gb
              gbRoot = toRoot gb
              ad = AttestationData 0 gbRoot zeroCheckpoint zeroCheckpoint
              sa = SignedAttestation ad 0 zeroSig
          case onAttestation store sa of
            Right store1 -> do
              let msg = Map.lookup 0 (stLatestMessages store1)
              case msg of
                Just lm -> do
                  lmRoot lm @?= gbRoot
                  lmSlot lm @?= 0
                Nothing -> assertFailure "Expected latest message"
            Left err -> assertFailure $ show err
      , testCase "rejects future attestation" $ do
          let gs = mkGenesisState [mkValidatorWithPubkey 1 32000000]
              gb = mkGenesisBlock
              store = initStore gs gb
              ad = AttestationData 5 zeroRoot zeroCheckpoint zeroCheckpoint
              sa = SignedAttestation ad 0 zeroSig
          case onAttestation store sa of
            Left (AttestationSlotInFuture 5 0) -> pure ()
            other -> assertFailure $ "Expected AttestationSlotInFuture, got: " ++ show other
      ]
  , testGroup "getAncestor"
      [ testCase "finds self at same slot" $ do
          let gs = mkGenesisState [mkValidatorWithPubkey 1 32000000]
              gb = mkGenesisBlock
              store = initStore gs gb
              gbRoot = toRoot gb
          getAncestor store gbRoot 0 @?= Just gbRoot
      , testCase "returns Nothing for future slot" $ do
          let gs = mkGenesisState [mkValidatorWithPubkey 1 32000000]
              gb = mkGenesisBlock
              store = initStore gs gb
              gbRoot = toRoot gb
          getAncestor store gbRoot 5 @?= Nothing
      ]
  , testGroup "isDescendant"
      [ testCase "block is descendant of itself" $ do
          let gs = mkGenesisState [mkValidatorWithPubkey 1 32000000]
              gb = mkGenesisBlock
              store = initStore gs gb
              gbRoot = toRoot gb
          isDescendant store gbRoot gbRoot @?= True
      , testCase "unknown root is not a descendant" $ do
          let gs = mkGenesisState [mkValidatorWithPubkey 1 32000000]
              gb = mkGenesisBlock
              store = initStore gs gb
              gbRoot = toRoot gb
          isDescendant store gbRoot (mkRoot 99) @?= False
      ]
  , testGroup "onBlock"
      [ testCase "rejects orphan block" $ do
          let gs = mkGenesisState [mkValidatorWithPubkey 1 32000000]
              gb = mkGenesisBlock
              store = initStore gs gb
              orphanBlock = BeaconBlock 1 0 (mkRoot 99) zeroRoot mkEmptyBody
              signedOrphan = SignedBeaconBlock orphanBlock zeroSig
          case onBlock (store { stCurrentSlot = 1 }) signedOrphan of
            Left OrphanBlock -> pure ()
            other -> assertFailure $ "Expected OrphanBlock, got: " ++ show other
      , testCase "rejects future block" $ do
          let gs = mkGenesisState [mkValidatorWithPubkey 1 32000000]
              gb = mkGenesisBlock
              store = initStore gs gb
              futureBlock = BeaconBlock 5 0 zeroRoot zeroRoot mkEmptyBody
              signedFuture = SignedBeaconBlock futureBlock zeroSig
          case onBlock store signedFuture of
            Left (BlockSlotInFuture 5 0) -> pure ()
            other -> assertFailure $ "Expected BlockSlotInFuture, got: " ++ show other
      ]
  , testGroup "getWeight"
      [ testCase "zero weight with no attestations" $ do
          let gs = mkGenesisState [mkValidatorWithPubkey 1 32000000]
              gb = mkGenesisBlock
              store = initStore gs gb
          getWeight store (toRoot gb) @?= 0
      ]
  ]
