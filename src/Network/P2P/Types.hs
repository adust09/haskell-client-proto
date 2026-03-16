-- | Transport-agnostic P2P abstraction layer.
-- Defines 'P2PHandle' (record-of-functions) so message handlers, sync, and
-- aggregator can be developed and tested against a mock backend independently
-- of the Rust FFI wrapper.
module Network.P2P.Types
  ( -- * Handle
    P2PHandle (..)
  , P2PConfig (..)
    -- * Topics
  , Topic (..)
  , topicString
    -- * Status
  , StatusMessage (..)
  ) where

import Data.ByteString (ByteString)
import Data.Word (Word64)

import Consensus.Constants (Slot, SubnetId, Root)

-- | Transport-agnostic P2P handle.  Every backend (FFI, IPC, mock) constructs
-- one of these; consumers only programme against this interface.
data P2PHandle = P2PHandle
  { p2hPublish        :: Topic -> ByteString -> IO ()
  , p2hSubscribe      :: Topic -> (ByteString -> IO ()) -> IO ()
  , p2hUnsubscribe    :: Topic -> IO ()
  , p2hRequestByRange :: Slot -> Word64 -> IO [ByteString]
  , p2hRequestByRoot  :: [Root] -> IO [ByteString]
  , p2hPeerCount      :: IO Int
  , p2hLocalPeerId    :: IO String
  , p2hStop           :: IO ()
  }

-- | Configuration for a P2P node.
data P2PConfig = P2PConfig
  { p2cListenPort :: !Int
  , p2cBootnodes  :: ![String]
  } deriving stock (Eq, Show)

-- | Gossipsub topic names following the pq-devnet-3 wire spec.
data Topic
  = TopicAttestation !SubnetId
  | TopicAggregation
  | TopicBeaconBlock
  deriving stock (Eq, Ord, Show)

-- | Canonical string representation of a topic for gossipsub.
topicString :: Topic -> String
topicString (TopicAttestation sid) = "attestation_" <> show sid
topicString TopicAggregation       = "aggregation"
topicString TopicBeaconBlock       = "beacon_block"

-- | Status message exchanged during initial peer handshake.
data StatusMessage = StatusMessage
  { smHeadSlot  :: !Slot
  , smHeadRoot  :: !Root
  , smFinalizedSlot :: !Slot
  , smFinalizedRoot :: !Root
  } deriving stock (Eq, Show)
