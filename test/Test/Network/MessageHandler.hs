module Test.Network.MessageHandler (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString as BS

import Consensus.ForkChoice (initStore)
import Consensus.Types
import Network.MessageHandler
import SSZ.Common (mkBytesN)
import Test.Support.Helpers

tests :: TestTree
tests = testGroup "Network.MessageHandler"
  [ seenCacheTests
  , blockValidationTests
  , attestationValidationTests
  ]

cfg :: Config
cfg = Config 0

-- ---------------------------------------------------------------------------
-- SeenCache tests
-- ---------------------------------------------------------------------------

seenCacheTests :: TestTree
seenCacheTests = testGroup "SeenCache"
  [ testCase "new message is not seen" $ do
      let cache = newSeenCache 100
          (seen, _) = markSeen (BS.pack [1,2,3]) cache
      seen @?= False

  , testCase "duplicate message is seen" $ do
      let cache = newSeenCache 100
          msg = BS.pack [1,2,3]
          (_, cache1) = markSeen msg cache
          (seen, _) = markSeen msg cache1
      seen @?= True

  , testCase "different messages are not seen" $ do
      let cache = newSeenCache 100
          (_, cache1) = markSeen (BS.pack [1]) cache
          (seen, _) = markSeen (BS.pack [2]) cache1
      seen @?= False

  , testCase "evicts oldest when full" $ do
      let cache = newSeenCache 2
          (_, c1) = markSeen (BS.pack [1]) cache
          (_, c2) = markSeen (BS.pack [2]) c1
          (_, c3) = markSeen (BS.pack [3]) c2  -- should evict [1]
          (seen1, _) = markSeen (BS.pack [1]) c3
          (seen2, _) = markSeen (BS.pack [2]) c3
      seen1 @?= False  -- [1] was evicted
      seen2 @?= True   -- [2] still present
  ]

-- ---------------------------------------------------------------------------
-- Block validation tests
-- ---------------------------------------------------------------------------

blockValidationTests :: TestTree
blockValidationTests = testGroup "Block validation"
  [ testCase "valid block is accepted" $ do
      let vals = [mkTestValidator 1 0]
          gs = mkTestGenesisState vals
          store = initStore gs mkTestGenesisBlock cfg
          sbb = mkTestSignedBlock gs 1
          store1 = store { stTime = 5 }  -- slot 1 = time 5 / 5
      validateBlock store1 sbb 1 @?= Accept

  , testCase "future block is rejected" $ do
      let vals = [mkTestValidator 1 0]
          gs = mkTestGenesisState vals
          store = initStore gs mkTestGenesisBlock cfg
      let farBlock = SignedBlock (BeaconBlock 100 0 zeroRoot zeroRoot mkEmptyBody) zeroBlockSignatures
      validateBlock store farBlock 0 @?= Reject

  , testCase "orphan block is ignored" $ do
      let vals = [mkTestValidator 1 0]
          gs = mkTestGenesisState vals
          store = initStore gs mkTestGenesisBlock cfg
          orphanParent = case mkBytesN @32 (BS.replicate 32 0xFF) of
                           Right r -> r
                           Left _ -> error "impossible"
          orphanBlock = SignedBlock
            (BeaconBlock 1 0 orphanParent zeroRoot mkEmptyBody) zeroBlockSignatures
      validateBlock store orphanBlock 1 @?= Ignore
  ]

-- ---------------------------------------------------------------------------
-- Attestation validation tests
-- ---------------------------------------------------------------------------

attestationValidationTests :: TestTree
attestationValidationTests = testGroup "Attestation validation"
  [ testCase "valid attestation is accepted" $ do
      let vals = [mkTestValidator 1 0]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock
          store = initStore gs genesisBlock cfg
          genesisRoot = toRoot genesisBlock
          headCp = Checkpoint genesisRoot 0
          att = mkTestAttestation 0 0 headCp zeroCheckpoint zeroCheckpoint
      validateAttestation (store { stTime = 5 }) att 0 1 @?= Accept

  , testCase "future attestation is rejected" $ do
      let vals = [mkTestValidator 1 0]
          gs = mkTestGenesisState vals
          store = initStore gs mkTestGenesisBlock cfg
          headCp = Checkpoint zeroRoot 10
          att = mkTestAttestation 0 10 headCp zeroCheckpoint zeroCheckpoint
      validateAttestation store att 0 0 @?= Reject

  , testCase "old attestation is ignored" $ do
      let vals = [mkTestValidator 1 0]
          gs = mkTestGenesisState vals
          store = initStore gs mkTestGenesisBlock cfg
          headCp = Checkpoint zeroRoot 0
          att = mkTestAttestation 0 0 headCp zeroCheckpoint zeroCheckpoint
      validateAttestation (store { stTime = 50 }) att 0 10 @?= Ignore
  ]
