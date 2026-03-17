-- | End-to-end integration tests for the network layer using MockNetwork.
module Test.Network.Integration (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Consensus.ForkChoice (initStore)
import Consensus.StateTransition (getAttestationSubnet, stateTransition)
import Consensus.Types
import Crypto.LeanMultisig (setupProver, setupVerifier)
import Crypto.Operations (aggregateAttestations)
import Network.Aggregator (newAttestationPool, addAttestation, drainAttestations)
import Network.MessageHandler
import Network.P2P.Types (P2PHandle (..), Topic (..))
import Network.P2P.Wire (encodeWire)
import Network.Sync (SyncEnv (..), SyncStatus (..), runSync)
import SSZ.List (unSszList)
import Test.Support.Helpers
import Test.Support.MockNetwork

-- | Force a Right value, failing with an error on Left.
unsafeRight :: Show e => Either e a -> a
unsafeRight (Right a) = a
unsafeRight (Left e)  = error ("unexpected Left: " <> show e)

tests :: TestTree
tests = testGroup "Network.Integration"
  [ testCase "two-node block gossip via MockNetwork" $ do
      let vals = [mkTestValidator 1 32000000]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      -- Create store for node 2
      store2Var <- newTVarIO (initStore gs genesisBlock)

      -- Create mock network and handle
      mn <- newMockNetwork
      handle2 <- mockP2PHandle mn

      -- Setup message handler for node 2
      cache2 <- newTVarIO (newSeenCache 8192)
      slotVar <- newTVarIO 1
      verifier <- setupVerifier
      let env2 = MessageHandlerEnv store2Var cache2 handle2 verifier slotVar
      startMessageHandler env2

      -- Give subscriber threads time to start
      threadDelay 10_000

      -- Node 1 produces and publishes a block
      let sbb = mkTestSignedBlock gs 1
          wireMsg = encodeWire sbb

      broadcastAll mn TopicBeaconBlock wireMsg

      -- Wait for propagation
      threadDelay 50_000

      -- Verify node 2's store was updated
      store2 <- readTVarIO store2Var
      assertBool "node 2 should have processed the block"
        (Map.size (stBlocks store2) >= 2)

  , testCase "attestation flow: validator -> subnet -> message handler" $ do
      let vals = [mkTestValidator 1 32000000]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock
          genesisRoot = toRoot genesisBlock

      storeVar <- newTVarIO (initStore gs genesisBlock)
      cacheVar <- newTVarIO (newSeenCache 8192)
      slotVar <- newTVarIO 1

      mn <- newMockNetwork
      handle <- mockP2PHandle mn
      verifier <- setupVerifier

      let env = MessageHandlerEnv storeVar cacheVar handle verifier slotVar
      startMessageHandler env
      threadDelay 10_000

      -- Publish attestation to subnet
      let subnet = getAttestationSubnet 0
          att = mkTestAttestation 0 0 genesisRoot zeroCheckpoint zeroCheckpoint
          wireAtt = encodeWire att

      broadcastAll mn (TopicAttestation subnet) wireAtt
      threadDelay 50_000

      -- The attestation should have been processed into the store
      store <- readTVarIO storeVar
      -- onAttestation adds to latestMessages
      assertBool "store should have latest message for validator 0"
        (Map.member 0 (stLatestMessages store))

  , testCase "aggregation flow: pool -> aggregate -> publish to TopicAggregation" $ do
      -- Create 8 validators so subnet 0 has validators [0, 4]
      let keyVals = [ mkTestValidatorWithKey i 32000000 | i <- [0..7] ]
          privKeys = map fst keyVals
          vals = map snd keyVals
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      prover <- setupProver
      verifier <- setupVerifier

      -- Process block 1 to give validators a head to attest to
      let sbb1 = mkTestSignedBlock gs 1
          st1 = unsafeRight $ stateTransition gs sbb1 False
          block1Root = toRoot (sbbBlock sbb1)
          genCp = zeroCheckpoint
          domain = cpRoot genCp

      -- Create attestations for subnet 0 validators (indices 0, 4)
      let pubkeys = [ vPubkey v | v <- unSszList (bsValidators st1) ]
          target1 = Checkpoint 1 block1Root
          subnetVis = [ vi | vi <- [0 .. 7 :: Int], vi `mod` 4 == 0 ]  -- [0, 4]
          atts = [ mkSignedTestAttestation (privKeys !! i) (fromIntegral i)
                     1 block1Root genCp target1 domain
                 | i <- subnetVis ]

      -- Add to attestation pool and drain
      pool <- newAttestationPool
      mapM_ (\a -> atomically $ addAttestation pool a) atts
      groups <- atomically $ drainAttestations pool
      assertBool "pool should have 1 group" (Map.size groups == 1)

      -- Aggregate
      let (_, signedAtts) = head (Map.toList groups)
      aggResult <- aggregateAttestations prover signedAtts pubkeys domain 0
      case aggResult of
        Left err -> assertFailure ("aggregation failed: " <> show err)
        Right saa -> do
          -- Setup message handler subscribing to TopicAggregation
          mn <- newMockNetwork
          handle <- mockP2PHandle mn
          storeVar <- newTVarIO (initStore gs genesisBlock)
          cacheVar <- newTVarIO (newSeenCache 8192)
          slotVar <- newTVarIO 1

          let env = MessageHandlerEnv storeVar cacheVar handle verifier slotVar
          startMessageHandler env
          threadDelay 10_000

          -- Publish via handle (which records to mnPublished)
          p2hPublish handle TopicAggregation (encodeWire saa)
          threadDelay 50_000

          -- Verify: mnPublished should have a TopicAggregation entry
          published <- readTVarIO (mnPublished mn)
          assertBool "TopicAggregation message should have been routed"
            (Map.member TopicAggregation published)

  , testCase "3-slot justification: attestations advance justified checkpoint" $ do
      -- Use 16 validators so indices 0,4,8,12 are all in subnet 0 (vi%4==0).
      -- Attestations at slot 4 (subnet 4%4=0) cover all 4 high-weight validators.
      -- Total active balance = 16 * 32M = 512M. Subnet 0 votes = 4 * 32M = 128M.
      -- That's 25% < 66%, so we need all validators. Instead, we create
      -- attestations for each of 4 different slots (one per subnet) with the correct
      -- validators, using a single-element committee trick.
      --
      -- Alternative: put ALL validators in subnet 0 by using only indices that
      -- are multiples of 4. We pad the committee with dummy entries.
      let nVals = 16  -- need 16 so indices 0..15 exist
          keyVals = [ mkTestValidatorWithKey i 32000000 | i <- [0..nVals - 1] ]
          privKeys = map fst keyVals
          vals = map snd keyVals
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      prover <- setupProver
      verifier <- setupVerifier

      let pubkeys = [ vPubkey v | v <- unSszList (bsValidators gs) ]
          genCp = zeroCheckpoint
          domain = cpRoot genCp

      -- Slot 1: process empty block
      let sbb1 = mkTestSignedBlock gs 1
          st1 = unsafeRight $ stateTransition gs sbb1 False
          block1Root = toRoot (sbbBlock sbb1)
          target1 = Checkpoint 1 block1Root

      -- Empty blocks at slots 2-4
      let sbb2e = mkTestSignedBlock st1 2
          st2e = unsafeRight $ stateTransition st1 sbb2e False
          sbb3e = mkTestSignedBlock st2e 3
          st3e = unsafeRight $ stateTransition st2e sbb3e False
          sbb4e = mkTestSignedBlock st3e 4
          st4e = unsafeRight $ stateTransition st3e sbb4e False

      -- Attestation at slot 4 → subnet 4%4 = 0.
      -- Validators in subnet 0: 0, 4, 8, 12 (4 validators × 32M = 128M)
      -- Total active = 512M. 128M is only 25%. We need more subnets.
      -- Instead, create 4 aggregations at slots 0,1,2,3 (subnets 0,1,2,3).
      -- Each subnet has 4 validators: subnet s = {s, s+4, s+8, s+12}.
      -- Aggregate all 4 validators per subnet. Total = 16 validators = 512M = 100%.
      let mkSubnetAgg subnetId = do
            let subnetVis = [ vi | vi <- [0 .. fromIntegral nVals - 1], vi `mod` 4 == subnetId ]
                atts = [ mkSignedTestAttestation (privKeys !! fromIntegral vi) vi
                           1 block1Root genCp target1 domain
                       | vi <- subnetVis ]
            unsafeRight <$> aggregateAttestations prover atts pubkeys domain subnetId

      saa0 <- mkSubnetAgg 0  -- subnet 0: validators 0,4,8,12
      saa1 <- mkSubnetAgg 1  -- subnet 1: validators 1,5,9,13
      saa2 <- mkSubnetAgg 2  -- subnet 2: validators 2,6,10,14
      saa3 <- mkSubnetAgg 3  -- subnet 3: validators 3,7,11,15

      -- Slot 5: block with all 4 aggregations
      let sbb5 = mkTestSignedBlockWithAtts st4e 5 [saa0, saa1, saa2, saa3]
          st5 = unsafeRight $ stateTransition st4e sbb5 False

      -- Verify justification advanced
      assertBool "justified checkpoint should advance to slot >= 1"
        (cpSlot (bsJustifiedCheckpoint st5) >= 1)

      -- E2E: replay all blocks through MockNetwork + MessageHandler
      mn <- newMockNetwork
      handle <- mockP2PHandle mn
      storeVar <- newTVarIO (initStore gs genesisBlock)
      cacheVar <- newTVarIO (newSeenCache 8192)
      slotVar <- newTVarIO 5

      let env = MessageHandlerEnv storeVar cacheVar handle verifier slotVar
      startMessageHandler env
      threadDelay 10_000

      let allBlocks = [sbb1, sbb2e, sbb3e, sbb4e, sbb5]
      mapM_ (\sbb -> do
        broadcastAll mn TopicBeaconBlock (encodeWire sbb)
        threadDelay 50_000
        ) allBlocks

      store <- readTVarIO storeVar
      assertBool "store justified checkpoint should have advanced"
        (cpSlot (stJustifiedCheckpoint store) >= 1)

  , testCase "sync then live gossip: sync 5 blocks then receive block 6 via gossip" $ do
      let vals = [mkTestValidator 1 32000000]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      verifier <- setupVerifier

      -- Build a 5-block chain
      let (blocks, states) = buildChain gs 5

      -- Encode blocks into blockMap keyed by slot
      let blockMap = Map.fromList
            [ (bbSlot (sbbBlock sbb), encodeWire sbb)
            | sbb <- blocks
            ]

      -- Node B: init store + sync
      mn <- newMockNetwork
      handle <- mockP2PHandleWithBlocks mn blockMap

      storeVar <- newTVarIO (initStore gs genesisBlock)
      statusVar <- newTVarIO Synced

      let syncEnv = SyncEnv handle storeVar statusVar 10
      syncResult <- runSync syncEnv 5
      assertEqual "sync should complete" Synced syncResult

      storeAfterSync <- readTVarIO storeVar
      assertEqual "store should be at slot 5"
        5 (stCurrentSlot storeAfterSync)
      assertEqual "store should have 6 blocks (genesis + 5)"
        6 (Map.size (stBlocks storeAfterSync))

      -- Start message handler for live gossip
      cacheVar <- newTVarIO (newSeenCache 8192)
      slotVar <- newTVarIO 6

      let env = MessageHandlerEnv storeVar cacheVar handle verifier slotVar
      startMessageHandler env
      threadDelay 10_000

      -- Build block 6 from the last post-state
      let st5 = last states
          sbb6 = mkTestSignedBlock st5 6

      broadcastAll mn TopicBeaconBlock (encodeWire sbb6)
      threadDelay 50_000

      -- Node B should now have 7 blocks
      storeFinal <- readTVarIO storeVar
      assertEqual "store should have 7 blocks (genesis + 5 synced + 1 live)"
        7 (Map.size (stBlocks storeFinal))
  ]
