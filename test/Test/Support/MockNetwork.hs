-- | Mock P2P network for testing: in-process message routing via TVar + TQueue.
module Test.Support.MockNetwork
  ( MockNetwork
  , newMockNetwork
  , mockP2PHandle
  , mockP2PHandleWithBlocks
  , broadcastAll
    -- * Helpers re-exported
  , module Test.Support.Helpers
  ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Consensus.Constants (Slot)
import Network.P2P.Types (P2PHandle (..), Topic)

import Test.Support.Helpers

-- | In-process mock network: a map from Topic to list of subscriber queues.
data MockNetwork = MockNetwork
  { mnSubscribers :: !(TVar (Map Topic [TQueue ByteString]))
  , mnPublished   :: !(TVar (Map Topic [ByteString]))
  , mnBlockStore  :: !(TVar (Map Slot ByteString))
  }

-- | Create a new mock network.
newMockNetwork :: IO MockNetwork
newMockNetwork = MockNetwork
  <$> newTVarIO Map.empty
  <*> newTVarIO Map.empty
  <*> newTVarIO Map.empty

-- | Create a P2PHandle backed by the mock network.
mockP2PHandle :: MockNetwork -> IO P2PHandle
mockP2PHandle mn = do
  peerIdRef <- newTVarIO "mock-peer-0"
  pure P2PHandle
    { p2hPublish = \topic msg -> do
        -- Deliver to all subscribers
        subs <- readTVarIO (mnSubscribers mn)
        case Map.lookup topic subs of
          Nothing -> pure ()
          Just qs -> mapM_ (\q -> atomically $ writeTQueue q msg) qs
        -- Record in published log
        atomically $ modifyTVar' (mnPublished mn) $
          Map.insertWith (++) topic [msg]

    , p2hSubscribe = \topic callback -> do
        q <- newTQueueIO
        atomically $ modifyTVar' (mnSubscribers mn) $
          Map.insertWith (++) topic [q]
        -- Spawn a reader thread
        _ <- forkIOSafe $ subscriberLoop q callback
        pure ()

    , p2hUnsubscribe = \_ -> pure ()

    , p2hRequestByRange = \startSlot count -> do
        blockStore <- readTVarIO (mnBlockStore mn)
        let slots = [startSlot .. startSlot + count - 1]
        pure [ bs | s <- slots, Just bs <- [Map.lookup s blockStore] ]

    , p2hRequestByRoot = \_ -> pure []

    , p2hPeerCount = pure 1

    , p2hLocalPeerId = readTVarIO peerIdRef

    , p2hStop = pure ()
    }

-- | Create a P2PHandle with pre-loaded blocks for sync testing.
mockP2PHandleWithBlocks :: MockNetwork -> Map Slot ByteString -> IO P2PHandle
mockP2PHandleWithBlocks mn blocks = do
  atomically $ writeTVar (mnBlockStore mn) blocks
  mockP2PHandle mn

-- | Broadcast a message to all subscribers of a topic.
broadcastAll :: MockNetwork -> Topic -> ByteString -> IO ()
broadcastAll mn topic msg = do
  subs <- readTVarIO (mnSubscribers mn)
  case Map.lookup topic subs of
    Nothing -> pure ()
    Just qs -> mapM_ (\q -> atomically $ writeTQueue q msg) qs

subscriberLoop :: TQueue ByteString -> (ByteString -> IO ()) -> IO ()
subscriberLoop q callback = do
  msg <- atomically $ readTQueue q
  callback msg
  subscriberLoop q callback
