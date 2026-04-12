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
zeroCheckpoint = Checkpoint zeroRoot 0

mkValidatorWithPubkey :: Word8 -> ValidatorIndex -> Validator
mkValidatorWithPubkey w idx =
  let pk = case mkXmssPubkey (BS.replicate xmssPubkeySize w) of
             Right p -> p
             Left _  -> error "mkValidatorWithPubkey"
  in  Validator pk pk idx

zeroSig :: XmssSignature
zeroSig = case mkXmssSignature (BS.replicate xmssSignatureSize 0) of
  Right s -> s
  Left _  -> error "zeroSig"

zeroBlockSignatures :: BlockSignatures
zeroBlockSignatures = BlockSignatures
  { bsigAttestationSignatures = case mkSszList @MAX_ATTESTATIONS [] of
      Right sl -> sl
      Left _   -> error "zeroBlockSignatures"
  , bsigProposerSignature = zeroSig
  }

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

mkEmptyBody :: BeaconBlockBody
mkEmptyBody = BeaconBlockBody
  { bbbAttestations = case mkSszList @MAX_ATTESTATIONS [] of
      Right sl -> sl
      Left _   -> error "mkEmptyBody"
  }

cfg :: Config
cfg = Config 0

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Consensus.Integration"
  [ testCase "state transition advances slot and updates header" $ do
      let vals = [mkValidatorWithPubkey 1 0]
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
          signedBlock = SignedBlock block zeroBlockSignatures
      case stateTransition gs signedBlock False of
        Right postState -> do
          bsSlot postState @?= 1
          bbhSlot (bsLatestBlockHeader postState) @?= 1
        Left err -> assertFailure $ "State transition failed: " ++ show err

  , testCase "fork choice selects genesis with no other blocks" $ do
      let vals = [ mkValidatorWithPubkey 1 0
                 , mkValidatorWithPubkey 2 1
                 , mkValidatorWithPubkey 3 2
                 ]
          gs = mkGenesisState vals
          genesisBlock = BeaconBlock 0 0 zeroRoot zeroRoot mkEmptyBody
          store = initStore gs genesisBlock cfg
          genesisRoot = toRoot genesisBlock

      -- The genesis block is the only head
      getHead store @?= genesisRoot

  -- TODO: Port leanSpec state transition vectors and ethlambda fork choice
  -- test cases once available. For now, hand-written scenarios with explicit
  -- assertions on checkpoint values.
  ]
