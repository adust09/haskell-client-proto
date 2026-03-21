-- | Shared message types for node actors.
-- Separated from Node to avoid circular dependencies with Validator.
module NodeTypes
  ( BlockchainMsg (..)
  , P2PMsg (..)
  , ValidatorMsg (..)
  ) where

import Consensus.Constants (Slot)
import Consensus.Types
  ( SignedBeaconBlock
  , SignedAttestation
  , AggregatedAttestation
  )

-- | Messages for the blockchain (fork-choice) actor.
data BlockchainMsg
  = BcSlotTick !Slot
  | BcNewBlock !SignedBeaconBlock
  | BcNewAttestation !SignedAttestation
  | BcNewAggregation !AggregatedAttestation
  | BcShutdown

-- | Messages for the P2P networking actor.
data P2PMsg
  = P2PPublishBlock !SignedBeaconBlock
  | P2PPublishAttestation !SignedAttestation
  | P2PPublishAggregation !AggregatedAttestation
  | P2PShutdown

-- | Messages for the validator duty actor.
data ValidatorMsg
  = ValSlotTick !Slot
  | ValShutdown
