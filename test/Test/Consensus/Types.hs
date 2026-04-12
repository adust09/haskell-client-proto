module Test.Consensus.Types (tests) where

import qualified Data.ByteString as BS
import Data.Word (Word64)
import Test.Tasty
import Test.Tasty.HUnit
import SSZ.Common
import SSZ.Merkleization (SszHashTreeRoot (..), merkleize)
import Consensus.Constants
import Consensus.Types

-- | Helper: unwrap a Right or fail.
unsafeRight :: (Show e) => Either e a -> a
unsafeRight (Right a) = a
unsafeRight (Left e)  = error ("expected Right, got Left: " ++ show e)

-- | Create a zero Bytes32.
zeroRoot :: Root
zeroRoot = zeroN @32

-- | Create a zero Checkpoint (root before slot per leanSpec).
zeroCheckpoint :: Checkpoint
zeroCheckpoint = Checkpoint zeroRoot 0

-- | Create a zero AttestationData.
zeroAttData :: AttestationData
zeroAttData = AttestationData 0 zeroCheckpoint zeroCheckpoint zeroCheckpoint

-- | Create a zero BeaconBlockHeader.
zeroBlockHeader :: BeaconBlockHeader
zeroBlockHeader = BeaconBlockHeader 0 0 zeroRoot zeroRoot zeroRoot

tests :: TestTree
tests = testGroup "Consensus.Types"
  [ testGroup "Ssz metadata"
      [ testCase "Checkpoint is fixed-size, 40 bytes" $ do
          sszFixedSize @Checkpoint @?= Just 40
          sszIsFixedSize @Checkpoint @?= True
      , testCase "AttestationData is fixed-size" $ do
          -- slot(8) + head(40) + target(40) + source(40) = 128
          sszFixedSize @AttestationData @?= Just 128
          sszIsFixedSize @AttestationData @?= True
      , testCase "BeaconBlockHeader is fixed-size, 112 bytes" $ do
          sszFixedSize @BeaconBlockHeader @?= Just 112
          sszIsFixedSize @BeaconBlockHeader @?= True
      , testCase "SignedAttestation is fixed-size" $
          sszIsFixedSize @SignedAttestation @?= True
      , testCase "SignedAggregatedAttestation is variable-size" $
          sszIsFixedSize @SignedAggregatedAttestation @?= False
      , testCase "BeaconState is variable-size" $
          sszIsFixedSize @BeaconState @?= False
      , testCase "Validator is fixed-size" $
          sszIsFixedSize @Validator @?= True
      , testCase "Validator size uses xmssPubkeySize (3 fields)" $ do
          -- attestation_pubkey(52) + proposal_pubkey(52) + index(8) = 112
          let expectedSize = fromIntegral xmssPubkeySize + fromIntegral xmssPubkeySize + 8
          sszFixedSize @Validator @?= Just expectedSize
      , testCase "Config is fixed-size, 8 bytes" $ do
          sszFixedSize @Config @?= Just 8
          sszIsFixedSize @Config @?= True
      , testCase "AggregatedAttestation is variable-size" $
          sszIsFixedSize @AggregatedAttestation @?= False
      , testCase "AggregatedSignatureProof is variable-size" $
          sszIsFixedSize @AggregatedSignatureProof @?= False
      , testCase "BlockSignatures is variable-size" $
          sszIsFixedSize @BlockSignatures @?= False
      , testCase "SignedBlock is variable-size" $
          sszIsFixedSize @SignedBlock @?= False
      ]
  , testGroup "roundtrip"
      [ testCase "Checkpoint (root before slot)" $ do
          let root = unsafeRight $ mkBytesN @32 (BS.pack [1..32])
              cp = Checkpoint root 42
          sszDecode (sszEncode cp) @?= Right cp
      , testCase "AttestationData" $ do
          let root = unsafeRight $ mkBytesN @32 (BS.pack [1..32])
              cp = Checkpoint root 10
              ad = AttestationData 5 cp cp cp
          sszDecode (sszEncode ad) @?= Right ad
      , testCase "SignedAttestation" $ do
          let sig = unsafeRight $ mkXmssSignature (BS.replicate xmssSignatureSize 0xAB)
              sa = SignedAttestation zeroAttData 7 sig
          sszDecode (sszEncode sa) @?= Right sa
      , testCase "BeaconBlockHeader" $ do
          let root = unsafeRight $ mkBytesN @32 (BS.pack [1..32])
              bbh = BeaconBlockHeader 100 5 root root root
          sszDecode (sszEncode bbh) @?= Right bbh
      , testCase "Validator" $ do
          let pk = unsafeRight $ mkXmssPubkey (BS.replicate xmssPubkeySize 0x01)
              v = Validator pk pk 0
          sszDecode (sszEncode v) @?= Right v
      , testCase "Config" $ do
          let cfg = Config 1234567890
          sszDecode (sszEncode cfg) @?= Right cfg
      ]
  , testGroup "hashTreeRoot"
      [ testCase "Checkpoint hashTreeRoot (root before slot)" $ do
          let cp = Checkpoint zeroRoot 0
              rootRoot = hashTreeRoot zeroRoot
              slotRoot = hashTreeRoot (0 :: Word64)
              expected = merkleize [rootRoot, slotRoot] 2
          hashTreeRoot cp @?= expected
      , testCase "BeaconBlockHeader hashTreeRoot" $ do
          let bbh = zeroBlockHeader
              allRoots = [ hashTreeRoot (bbhSlot bbh)
                         , hashTreeRoot (bbhProposerIndex bbh)
                         , hashTreeRoot (bbhParentRoot bbh)
                         , hashTreeRoot (bbhStateRoot bbh)
                         , hashTreeRoot (bbhBodyRoot bbh)
                         ]
          -- 5 fields -> merkleize with limit=5
          hashTreeRoot bbh @?= merkleize allRoots 5
      ]
  , testGroup "constants"
      [ testCase "slotDuration == 4_000_000" $
          slotDuration @?= 4_000_000
      , testCase "slotsToFinality == 3" $
          slotsToFinality @?= 3
      , testCase "xmssSignatureSize == 3112" $
          xmssSignatureSize @?= 3112
      , testCase "xmssPubkeySize == 52" $
          xmssPubkeySize @?= 52
      ]
  ]
