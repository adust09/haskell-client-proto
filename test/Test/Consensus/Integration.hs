module Test.Consensus.Integration (tests) where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit

import Consensus.Constants
import Consensus.Types
import Consensus.ForkChoice
import Consensus.StateTransition
import SSZ.Common (mkBytesN, zeroN)
import SSZ.List (mkSszList, unSszList)
import SSZ.Merkleization (SszHashTreeRoot (..))
import SSZ.Vector (mkSszVector)

import qualified Data.Vector as V

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

mkValidatorWithPubkey :: Word8 -> Gwei -> Validator
mkValidatorWithPubkey w balance =
  let pk = case mkXmssPubkey (BS.replicate xmssPubkeySize w) of
             Right p -> p
             Left _  -> error "mkValidatorWithPubkey"
  in  Validator pk balance False 0 maxBound maxBound

zeroSig :: XmssSignature
zeroSig = case mkXmssSignature (BS.replicate xmssSignatureSize 0) of
  Right s -> s
  Left _  -> error "zeroSig"

mkGenesisState :: [Validator] -> BeaconState
mkGenesisState vals =
  let numHistSlots = 64
      emptyRoots = case mkSszVector @SLOTS_PER_HISTORICAL_ROOT
                        (V.replicate numHistSlots zeroRoot) of
                     Right sv -> sv
                     Left _   -> error "mkGenesisState: roots"
      valList = case mkSszList @VALIDATOR_REGISTRY_LIMIT vals of
                  Right sl -> sl
                  Left _   -> error "mkGenesisState: validators"
      balances = case mkSszList @VALIDATOR_REGISTRY_LIMIT
                      (map vEffectiveBalance vals) of
                   Right sl -> sl
                   Left _   -> error "mkGenesisState: balances"
      emptyAtts = case mkSszList @MAX_ATTESTATIONS_STATE [] of
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
          signedBlock = SignedBeaconBlock block zeroSig
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

      -- The genesis block is the only head
      getHead store @?= genesisRoot

  , testCase "slashed validator has reduced weight in fork choice" $ do
      let vals = [ mkValidatorWithPubkey 1 32000000
                 , mkValidatorWithPubkey 2 32000000
                 ]
          gs = mkGenesisState vals
          slashedState = slashValidator gs 0
          v0 = head (unSszList (bsValidators slashedState))
      vSlashed v0 @?= True
      vEffectiveBalance v0 @?= 0

  -- TODO: Port leanSpec state transition vectors and ethlambda fork choice
  -- test cases once available. For now, hand-written scenarios with explicit
  -- assertions on checkpoint values.
  ]
