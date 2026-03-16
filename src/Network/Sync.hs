-- | Initial sync: request blocks from peers in batches and apply them.
module Network.Sync
  ( -- * Types
    SyncStatus (..)
  , SyncEnv (..)
    -- * Sync
  , runSync
  , syncBatch
  ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Word (Word64)

import Consensus.Constants (Slot)
import Consensus.ForkChoice (onBlock)
import Consensus.Types (Store (..), SignedBeaconBlock (..), BeaconBlock (..))
import Network.P2P.Types (P2PHandle (..))
import Network.P2P.Wire (decodeWire)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Current sync status.
data SyncStatus
  = Synced
  | Syncing !Slot !Slot   -- ^ current, target
  | SyncFailed !String
  deriving stock (Eq, Show)

-- | Environment for the sync actor.
data SyncEnv = SyncEnv
  { seP2PHandle   :: !P2PHandle
  , seStore       :: !(TVar Store)
  , seStatus      :: !(TVar SyncStatus)
  , seBatchSize   :: !Word64
  }

-- ---------------------------------------------------------------------------
-- Sync logic
-- ---------------------------------------------------------------------------

-- | Run sync from the current head to the given target slot.
runSync :: SyncEnv -> Slot -> IO SyncStatus
runSync env targetSlot = do
  store <- readTVarIO (seStore env)
  let headSlot = stCurrentSlot store
  if headSlot >= targetSlot
    then do
      atomically $ writeTVar (seStatus env) Synced
      pure Synced
    else do
      atomically $ writeTVar (seStatus env) (Syncing headSlot targetSlot)
      syncLoop env headSlot targetSlot

syncLoop :: SyncEnv -> Slot -> Slot -> IO SyncStatus
syncLoop env currentSlot targetSlot
  | currentSlot >= targetSlot = do
      atomically $ writeTVar (seStatus env) Synced
      pure Synced
  | otherwise = do
      let batchSize = seBatchSize env
          startSlot = currentSlot + 1
      result <- syncBatch env startSlot batchSize
      case result of
        Left err -> do
          let status = SyncFailed err
          atomically $ writeTVar (seStatus env) status
          pure status
        Right newSlot -> do
          atomically $ writeTVar (seStatus env) (Syncing newSlot targetSlot)
          syncLoop env newSlot targetSlot

-- | Request and apply a batch of blocks starting from the given slot.
syncBatch :: SyncEnv -> Slot -> Word64 -> IO (Either String Slot)
syncBatch env startSlot count = do
  rawBlocks <- p2hRequestByRange (seP2PHandle env) startSlot count
  applyBlocks env rawBlocks startSlot

applyBlocks :: SyncEnv -> [ByteString] -> Slot -> IO (Either String Slot)
applyBlocks env [] _lastSlot = do
  store <- readTVarIO (seStore env)
  pure (Right (stCurrentSlot store))
applyBlocks env (raw:rest) _fallbackSlot =
  case decodeWire raw of
    Left err -> pure (Left ("SSZ decode failed: " <> show err))
    Right sbb -> do
      let blockSlot = bbSlot (sbbBlock sbb)
      result <- atomically $ do
        store <- readTVar (seStore env)
        -- Advance stCurrentSlot so onBlock doesn't reject the block as "future"
        let store' = store { stCurrentSlot = max blockSlot (stCurrentSlot store) }
        case onBlock store' sbb of
          Left err -> pure (Left (show err))
          Right store'' -> do
            writeTVar (seStore env) store''
            pure (Right store'')
      case result of
        Left err -> pure (Left ("block application failed: " <> err))
        Right store' -> applyBlocks env rest (stCurrentSlot store')
