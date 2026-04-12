module Test.Validator (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM

import Actor (Actor (..), spawnActor)
import Consensus.StateTransition (getProposerIndex, processSlot)
import Consensus.ForkChoice (onTick)
import Consensus.Types (XmssPubkey)
import Crypto.KeyManager (newManagedKey)
import Crypto.LeanSig (PrivateKey, generateKeyPair)
import Crypto.SigningRoot (computeDomain)
import Genesis (initializeGenesis)
import NodeTypes
import SSZ.Common (zeroN)
import Storage (withStorage, writeForkChoiceStore)
import Validator (ValidatorEnv (..), validatorLoop)

import Test.Support.Helpers
    ( mkTestValidator
    , mkTestGenesisState
    , mkTestGenesis
    )

import qualified Data.ByteString.Char8 as BS8
import System.IO.Temp (withSystemTempDirectory)

tests :: TestTree
tests = testGroup "Validator"
  [ testCase "should detect proposal duty correctly" testProposalDutyDetection
  , testCase "should propose block when designated proposer" testBlockProposal
  , testCase "should create attestation on slot tick" testAttestationCreation
  , testCase "should not propose when not the proposer" testNoProposalWhenNotProposer
  , testCase "integration: validator produces attestations through multiple slots" testMultiSlotIntegration
  ]

-- | Test that getProposerIndex correctly identifies the proposer.
testProposalDutyDetection :: IO ()
testProposalDutyDetection = do
  let vals = [ mkTestValidator 0 0
             , mkTestValidator 1 1
             , mkTestValidator 2 2
             , mkTestValidator 3 3
             ]
      gs = mkTestGenesisState vals

  -- At slot 0, proposer should be validator 0 (0 mod 4 == 0)
  getProposerIndex gs @?= 0

  -- At slot 1, proposer should be validator 1
  let gs1 = processSlot gs
  getProposerIndex gs1 @?= 1

  -- At slot 2, proposer should be validator 2
  let gs2 = processSlot gs1
  getProposerIndex gs2 @?= 2

-- | Test that the validator proposes a block when it is the designated proposer.
testBlockProposal :: IO ()
testBlockProposal =
  withSystemTempDirectory "lc-test-val-propose" $ \tmpDir -> do
    let genesis = mkTestGenesis
        (gs, store) = initializeGenesis genesis

    (privKey, pubKey) <- forceKeyPair "test-proposer-seed-0"
    managedKey <- newManagedKey privKey pubKey
    let domain = computeDomain (zeroN @4) (zeroN @4) (zeroN @32)
        keyPath = tmpDir <> "/validator.key"
        storePath = tmpDir <> "/db"

    bcMsgs <- newTVarIO ([] :: [BlockchainMsg])
    p2pMsgs <- newTVarIO ([] :: [P2PMsg])
    bcActor <- spawnActor "test-bc" (trackingLoop bcMsgs)
    p2pActor <- spawnActor "test-p2p" (trackingLoop p2pMsgs)

    withStorage storePath gs store $ \sh -> do
      atomically $ writeForkChoiceStore sh (store { stTime = 5 })

      -- With 2 validators, slot 2 mod 2 == 0, so validator 0 is proposer at slot 2
      atomically $ writeForkChoiceStore sh (store { stTime = 10 })

      let env = ValidatorEnv
            { veStorage        = sh
            , veManagedKey     = managedKey
            , veValidatorIndex = 0
            , veDomain         = domain
            , veKeyPersistPath = keyPath
            , veBcActor        = bcActor
            , veP2PActor       = p2pActor
            }

      valActor <- spawnActor "test-validator" (validatorLoop env)
      atomically $ writeTQueue (actorQueue valActor) (ValSlotTick 2)
      threadDelay 200000

      msgs <- readTVarIO bcMsgs
      assertBool "should have proposed a block" (any isBlockMsg msgs)
      assertBool "should have created an attestation" (any isAttMsg msgs)

      shutdownActors [valActor] bcActor p2pActor

-- | Test that attestations are created every slot.
testAttestationCreation :: IO ()
testAttestationCreation =
  withSystemTempDirectory "lc-test-val-attest" $ \tmpDir -> do
    let genesis = mkTestGenesis
        (gs, store) = initializeGenesis genesis

    (privKey, pubKey) <- forceKeyPair "test-attester-seed-1"
    managedKey <- newManagedKey privKey pubKey
    let domain = computeDomain (zeroN @4) (zeroN @4) (zeroN @32)
        keyPath = tmpDir <> "/validator.key"
        storePath = tmpDir <> "/db"

    bcMsgs <- newTVarIO ([] :: [BlockchainMsg])
    p2pMsgs <- newTVarIO ([] :: [P2PMsg])
    bcActor <- spawnActor "test-bc" (trackingLoop bcMsgs)
    p2pActor <- spawnActor "test-p2p" (trackingLoop p2pMsgs)

    withStorage storePath gs store $ \sh -> do
      atomically $ writeForkChoiceStore sh (store { stTime = 5 })

      let env = ValidatorEnv
            { veStorage        = sh
            , veManagedKey     = managedKey
            , veValidatorIndex = 1
            , veDomain         = domain
            , veKeyPersistPath = keyPath
            , veBcActor        = bcActor
            , veP2PActor       = p2pActor
            }

      valActor <- spawnActor "test-validator" (validatorLoop env)
      atomically $ writeTQueue (actorQueue valActor) (ValSlotTick 1)
      threadDelay 200000

      msgs <- readTVarIO bcMsgs
      assertBool "should have at least one attestation" (any isAttMsg msgs)

      p2ps <- readTVarIO p2pMsgs
      assertBool "should have published attestation to P2P" (any isP2PAttMsg p2ps)

      shutdownActors [valActor] bcActor p2pActor

-- | Test that a non-proposer validator does not propose a block.
testNoProposalWhenNotProposer :: IO ()
testNoProposalWhenNotProposer =
  withSystemTempDirectory "lc-test-val-nopropose" $ \tmpDir -> do
    let genesis = mkTestGenesis
        (gs, store) = initializeGenesis genesis

    (privKey, pubKey) <- forceKeyPair "test-non-proposer-seed"
    managedKey <- newManagedKey privKey pubKey
    let domain = computeDomain (zeroN @4) (zeroN @4) (zeroN @32)
        keyPath = tmpDir <> "/validator.key"
        storePath = tmpDir <> "/db"

    bcMsgs <- newTVarIO ([] :: [BlockchainMsg])
    bcActor <- spawnActor "test-bc" (trackingLoop bcMsgs)
    p2pActor <- spawnActor "test-p2p" (sinkLoop @P2PMsg)

    withStorage storePath gs store $ \sh -> do
      atomically $ writeForkChoiceStore sh (store { stTime = 5 })

      let env = ValidatorEnv
            { veStorage        = sh
            , veManagedKey     = managedKey
            , veValidatorIndex = 99
            , veDomain         = domain
            , veKeyPersistPath = keyPath
            , veBcActor        = bcActor
            , veP2PActor       = p2pActor
            }

      valActor <- spawnActor "test-validator" (validatorLoop env)
      atomically $ writeTQueue (actorQueue valActor) (ValSlotTick 1)
      threadDelay 200000

      msgs <- readTVarIO bcMsgs
      let blockCount = length (filter isBlockMsg msgs)
      blockCount @?= 0

      shutdownActors [valActor] bcActor p2pActor

-- | Integration test: run validator through multiple slots.
testMultiSlotIntegration :: IO ()
testMultiSlotIntegration =
  withSystemTempDirectory "lc-test-val-multi" $ \tmpDir -> do
    let genesis = mkTestGenesis
        (gs, store) = initializeGenesis genesis

    (privKey, pubKey) <- forceKeyPair "test-multi-slot-seed"
    managedKey <- newManagedKey privKey pubKey
    let domain = computeDomain (zeroN @4) (zeroN @4) (zeroN @32)
        keyPath = tmpDir <> "/validator.key"
        storePath = tmpDir <> "/db"

    bcMsgs <- newTVarIO ([] :: [BlockchainMsg])
    bcActor <- spawnActor "test-bc" (trackingLoop bcMsgs)
    p2pActor <- spawnActor "test-p2p" (sinkLoop @P2PMsg)

    withStorage storePath gs store $ \sh -> do
      atomically $ writeForkChoiceStore sh (store { stTime = 25 })

      let env = ValidatorEnv
            { veStorage        = sh
            , veManagedKey     = managedKey
            , veValidatorIndex = 0
            , veDomain         = domain
            , veKeyPersistPath = keyPath
            , veBcActor        = bcActor
            , veP2PActor       = p2pActor
            }

      valActor <- spawnActor "test-validator" (validatorLoop env)

      mapM_ (\slot -> do
        atomically $ writeTQueue (actorQueue valActor) (ValSlotTick slot)
        threadDelay 100000
        ) [1, 2, 3]

      threadDelay 100000

      msgs <- readTVarIO bcMsgs
      let attCount = length (filter isAttMsg msgs)
      assertBool ("should have >= 3 attestations, got " <> show attCount) (attCount >= 3)

      shutdownActors [valActor] bcActor p2pActor

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

forceKeyPair :: String -> IO (PrivateKey, XmssPubkey)
forceKeyPair seedStr =
  case generateKeyPair 10 (BS8.pack seedStr) of
    Right kp -> pure kp
    Left e   -> error ("key gen failed: " <> show e)

-- | A loop that records all messages in a TVar.
trackingLoop :: TVar [msg] -> TQueue msg -> IO ()
trackingLoop var queue = go
  where
    go = do
      msg <- atomically $ readTQueue queue
      atomically $ modifyTVar' var (msg :)
      go

-- | A loop that silently consumes all messages.
sinkLoop :: forall msg. TQueue msg -> IO ()
sinkLoop queue = go
  where
    go = do
      _ <- atomically $ readTQueue queue
      go

shutdownActors :: [Actor ValidatorMsg] -> Actor BlockchainMsg -> Actor P2PMsg -> IO ()
shutdownActors valActors bcActor p2pActor = do
  mapM_ (\v -> atomically $ writeTQueue (actorQueue v) ValShutdown) valActors
  atomically $ writeTQueue (actorQueue bcActor) BcShutdown
  atomically $ writeTQueue (actorQueue p2pActor) P2PShutdown

isBlockMsg :: BlockchainMsg -> Bool
isBlockMsg (BcNewBlock _) = True
isBlockMsg _              = False

isAttMsg :: BlockchainMsg -> Bool
isAttMsg (BcNewAttestation _) = True
isAttMsg _                    = False

isP2PAttMsg :: P2PMsg -> Bool
isP2PAttMsg (P2PPublishAttestation _) = True
isP2PAttMsg _                        = False
