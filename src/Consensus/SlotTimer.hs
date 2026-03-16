-- | Slot phase timing for 3SF-mini 4-second slots.
module Consensus.SlotTimer
  ( SlotPhase (..)
  , getCurrentSlotPhase
  , waitUntilPhase
  , slotTicker
  ) where

import Consensus.Constants
    ( Slot
    , slotDuration
    , proposalPhaseEnd
    , votingPhaseEnd
    , confirmationPhaseEnd
    )
import Control.Concurrent (threadDelay)
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime)

-- | Phases within a 4-second slot.
data SlotPhase
  = ProposalPhase       -- ^ 0–800ms: block proposal
  | VotingPhase         -- ^ 800–2400ms: attestation voting
  | ConfirmationPhase   -- ^ 2400–3200ms: confirmation
  | ViewMergePhase      -- ^ 3200–4000ms: view merge
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Compute the current slot and phase from genesis time and current time.
getCurrentSlotPhase :: UTCTime -> UTCTime -> (Slot, SlotPhase)
getCurrentSlotPhase genesisTime now =
  let elapsed = diffUTCTime now genesisTime
      elapsedMicros = nominalToMicros elapsed
      slotDurMicros = fromIntegral slotDuration :: Integer
      slot = fromIntegral (elapsedMicros `div` slotDurMicros)
      phaseOffset = fromIntegral (elapsedMicros `mod` slotDurMicros) :: Int
      phase
        | phaseOffset < proposalPhaseEnd     = ProposalPhase
        | phaseOffset < votingPhaseEnd       = VotingPhase
        | phaseOffset < confirmationPhaseEnd = ConfirmationPhase
        | otherwise                          = ViewMergePhase
  in  (slot, phase)

-- | Block until the given phase of the current or next slot.
waitUntilPhase :: UTCTime -> SlotPhase -> IO ()
waitUntilPhase genesisTime targetPhase = do
  now <- getCurrentTime
  let elapsed = diffUTCTime now genesisTime
      elapsedMicros = nominalToMicros elapsed
      slotDurMicros = fromIntegral slotDuration :: Integer
      currentSlotStart = (elapsedMicros `div` slotDurMicros) * slotDurMicros
      targetOffset = phaseStartMicros targetPhase
      targetMicros = currentSlotStart + fromIntegral targetOffset
      waitMicros
        | targetMicros > elapsedMicros = targetMicros - elapsedMicros
        | otherwise = targetMicros + slotDurMicros - elapsedMicros
  threadDelay (fromIntegral waitMicros)

-- | Run a callback at the start of each slot, indefinitely.
slotTicker :: UTCTime -> (Slot -> IO ()) -> IO ()
slotTicker genesisTime callback = go
  where
    go = do
      waitUntilPhase genesisTime ProposalPhase
      now <- getCurrentTime
      let (slot, _) = getCurrentSlotPhase genesisTime now
      callback slot
      go

-- | Microsecond offset from slot start for each phase.
phaseStartMicros :: SlotPhase -> Int
phaseStartMicros ProposalPhase     = 0
phaseStartMicros VotingPhase       = proposalPhaseEnd
phaseStartMicros ConfirmationPhase = votingPhaseEnd
phaseStartMicros ViewMergePhase    = confirmationPhaseEnd

-- | Convert NominalDiffTime to microseconds as Integer.
nominalToMicros :: NominalDiffTime -> Integer
nominalToMicros dt = truncate (dt * 1_000_000)
