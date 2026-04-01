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

import Consensus.Constants (Slot, slotDuration)
import Consensus.ForkChoice (onBlock, onTick)
import Consensus.Types (Store (..), Config (..), SignedBeaconBlock (..), BeaconBlock (..), currentSlot)
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
  let headSlot = currentSlot store
  if headSlot >= targetSlot
    then do
      atomically $ writeTVar (seStatus env) Synced
      pure Synced
    else do
      atomically $ writeTVar (seStatus env) (Syncing headSlot targetSlot)
      syncLoop env headSlot targetSlot

syncLoop :: SyncEnv -> Slot -> Slot -> IO SyncStatus
syncLoop env curSlot targetSlot
  | curSlot >= targetSlot = do
      atomically $ writeTVar (seStatus env) Synced
      pure Synced
  | otherwise = do
      let batchSize = seBatchSize env
          startSlot = curSlot + 1
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
  pure (Right (currentSlot store))
applyBlocks env (raw:rest) _fallbackSlot =
  case decodeWire raw of
    Left err -> pure (Left ("SSZ decode failed: " <> show err))
    Right sbb -> do
      let blockSlot = bbSlot (sbbBlock sbb)
      result <- atomically $ do
        store <- readTVar (seStore env)
        -- Advance time so onBlock doesn't reject the block as "future"
        let slotSec = fromIntegral (slotDuration `div` 1_000_000)
            genesis = cfgGenesisTime (stConfig store)
            blockTime = genesis + blockSlot * slotSec
            store' = onTick store (max blockTime (stTime store))
        case onBlock store' sbb of
          Left err -> pure (Left (show err))
          Right store'' -> do
            writeTVar (seStore env) store''
            pure (Right store'')
      case result of
        Left err -> pure (Left ("block application failed: " <> err))
        Right store' -> applyBlocks env rest (currentSlot store')
