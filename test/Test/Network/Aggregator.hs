module Test.Network.Aggregator (tests) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Consensus.Types
import Network.Aggregator
import Test.Support.Helpers

tests :: TestTree
tests = testGroup "Network.Aggregator"
  [ testCase "addAttestation groups by AttestationData" $ do
      pool <- newAttestationPool
      let genesisRoot = toRoot mkTestGenesisBlock
          headCp = Checkpoint genesisRoot 1
          att1 = mkTestAttestation 0 1 headCp zeroCheckpoint zeroCheckpoint
          att2 = mkTestAttestation 1 1 headCp zeroCheckpoint zeroCheckpoint
          ad1 = saData att1
      atomically $ do
        addAttestation pool att1
        addAttestation pool att2
      m <- readTVarIO pool
      case Map.lookup ad1 m of
        Nothing -> assertFailure "expected attestations for ad1"
        Just atts -> length atts @?= 2

  , testCase "duplicate attestation from same validator is rejected" $ do
      pool <- newAttestationPool
      let genesisRoot = toRoot mkTestGenesisBlock
          headCp = Checkpoint genesisRoot 1
          att = mkTestAttestation 0 1 headCp zeroCheckpoint zeroCheckpoint
      atomically $ do
        addAttestation pool att
        addAttestation pool att  -- duplicate
      m <- readTVarIO pool
      let ad = saData att
      case Map.lookup ad m of
        Nothing -> assertFailure "expected attestations"
        Just atts -> length atts @?= 1

  , testCase "drainAttestations clears pool" $ do
      pool <- newAttestationPool
      let genesisRoot = toRoot mkTestGenesisBlock
          headCp = Checkpoint genesisRoot 1
          att = mkTestAttestation 0 1 headCp zeroCheckpoint zeroCheckpoint
      atomically $ addAttestation pool att
      groups <- atomically $ drainAttestations pool
      Map.null groups @?= False

      -- Pool should be empty now
      m <- readTVarIO pool
      Map.null m @?= True

  , testCase "attestations with different data are in separate groups" $ do
      pool <- newAttestationPool
      let genesisRoot = toRoot mkTestGenesisBlock
          headCp1 = Checkpoint genesisRoot 1
          headCp2 = Checkpoint genesisRoot 2
          att1 = mkTestAttestation 0 1 headCp1 zeroCheckpoint zeroCheckpoint
          att2 = mkTestAttestation 1 2 headCp2 zeroCheckpoint zeroCheckpoint
      atomically $ do
        addAttestation pool att1
        addAttestation pool att2
      m <- readTVarIO pool
      Map.size m @?= 2
  ]
