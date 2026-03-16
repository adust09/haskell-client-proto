module Test.Consensus.SlotTimer (tests) where

import Data.Time.Clock (UTCTime (..), addUTCTime, NominalDiffTime)
import Data.Time.Calendar (fromGregorian)
import Test.Tasty
import Test.Tasty.HUnit

import Consensus.SlotTimer

-- | Fixed genesis time for tests.
genesis :: UTCTime
genesis = UTCTime (fromGregorian 2026 1 1) 0

-- | Helper: genesis + N microseconds.
atMicros :: Integer -> UTCTime
atMicros us = addUTCTime (fromIntegral us / 1_000_000 :: NominalDiffTime) genesis

tests :: TestTree
tests = testGroup "Consensus.SlotTimer"
  [ testGroup "getCurrentSlotPhase"
      [ testCase "slot 0, start of proposal phase" $ do
          let (slot, phase) = getCurrentSlotPhase genesis genesis
          slot @?= 0
          phase @?= ProposalPhase
      , testCase "slot 0, mid-proposal (400ms)" $ do
          let (slot, phase) = getCurrentSlotPhase genesis (atMicros 400_000)
          slot @?= 0
          phase @?= ProposalPhase
      , testCase "slot 0, exactly at voting boundary (800ms)" $ do
          let (slot, phase) = getCurrentSlotPhase genesis (atMicros 800_000)
          slot @?= 0
          phase @?= VotingPhase
      , testCase "slot 0, mid-voting (1600ms)" $ do
          let (slot, phase) = getCurrentSlotPhase genesis (atMicros 1_600_000)
          slot @?= 0
          phase @?= VotingPhase
      , testCase "slot 0, exactly at confirmation boundary (2400ms)" $ do
          let (slot, phase) = getCurrentSlotPhase genesis (atMicros 2_400_000)
          slot @?= 0
          phase @?= ConfirmationPhase
      , testCase "slot 0, mid-confirmation (2800ms)" $ do
          let (slot, phase) = getCurrentSlotPhase genesis (atMicros 2_800_000)
          slot @?= 0
          phase @?= ConfirmationPhase
      , testCase "slot 0, exactly at view-merge boundary (3200ms)" $ do
          let (slot, phase) = getCurrentSlotPhase genesis (atMicros 3_200_000)
          slot @?= 0
          phase @?= ViewMergePhase
      , testCase "slot 0, mid-view-merge (3600ms)" $ do
          let (slot, phase) = getCurrentSlotPhase genesis (atMicros 3_600_000)
          slot @?= 0
          phase @?= ViewMergePhase
      , testCase "slot 1 starts at 4000ms" $ do
          let (slot, phase) = getCurrentSlotPhase genesis (atMicros 4_000_000)
          slot @?= 1
          phase @?= ProposalPhase
      , testCase "slot 2, voting phase (8800ms = 8000 + 800)" $ do
          let (slot, phase) = getCurrentSlotPhase genesis (atMicros 8_800_000)
          slot @?= 2
          phase @?= VotingPhase
      , testCase "last microsecond of slot 0 (3999999us)" $ do
          let (slot, phase) = getCurrentSlotPhase genesis (atMicros 3_999_999)
          slot @?= 0
          phase @?= ViewMergePhase
      ]
  ]
