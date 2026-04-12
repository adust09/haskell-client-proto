-- | Persistent storage layer: RocksDB for durability + TVar cache for fast access.
module Storage
  ( StorageHandle (..)
  , withStorage
  , putBlock
  , getBlock
  , putState
  , getState
  , getFinalizedBlock
  , putFinalizedBlock
  , readCurrentState
  , writeCurrentState
  , readForkChoiceStore
  , writeForkChoiceStore
  , pruneOldBlocks
  ) where

import Control.Concurrent.STM
import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Database.RocksDB (BatchOp (..), Config (..), DB)
import qualified Database.RocksDB as Rocks

import Consensus.Constants (Root, Slot)
import Consensus.Types
    ( BeaconBlock (..)
    , BeaconState (..)
    , SignedBlock (..)
    , Store
    )
import SSZ.Common (SszDecode (..), SszEncode (..), mkBytesN, unBytesN)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Handle to the storage subsystem. Holds both persistent (RocksDB) and
-- volatile (TVar) state.
data StorageHandle = StorageHandle
  { shDB           :: !DB
  , shHotState     :: !(TVar BeaconState)
  , shHotStore     :: !(TVar Store)
  , shRecentBlocks :: !(TVar (Map Root SignedBlock))
  }

-- ---------------------------------------------------------------------------
-- Key encoding helpers
-- ---------------------------------------------------------------------------

blockKey :: Root -> ByteString
blockKey root = "b:" <> unBytesN root

stateKey :: Root -> ByteString
stateKey root = "s:" <> unBytesN root

finalizedKey :: Slot -> ByteString
finalizedKey slot = "f:" <> encodeBE64 slot

metaKey :: ByteString -> ByteString
metaKey name = "meta:" <> name

-- | Encode Word64 as 8-byte big-endian for lexicographic ordering in RocksDB.
encodeBE64 :: Word64 -> ByteString
encodeBE64 w = BS.pack
  [ fromIntegral (w `shiftR` 56)
  , fromIntegral (w `shiftR` 48)
  , fromIntegral (w `shiftR` 40)
  , fromIntegral (w `shiftR` 32)
  , fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

dbConfig :: Config
dbConfig = Config
  { createIfMissing = True
  , errorIfExists   = False
  , paranoidChecks  = False
  , maxFiles        = Nothing
  , prefixLength    = Nothing
  , bloomFilter     = True
  }

-- | Open a storage handle backed by RocksDB at the given path. Caller
-- provides the initial 'BeaconState' and 'Store' (e.g. from genesis).
-- The action receives a 'StorageHandle'; RocksDB is closed on exit.
withStorage :: FilePath -> BeaconState -> Store -> (StorageHandle -> IO a) -> IO a
withStorage path initialState initialStore action =
  Rocks.withDB path dbConfig $ \db -> do
    hotState     <- newTVarIO initialState
    hotStore     <- newTVarIO initialStore
    recentBlocks <- newTVarIO Map.empty
    let sh = StorageHandle
          { shDB           = db
          , shHotState     = hotState
          , shHotStore     = hotStore
          , shRecentBlocks = recentBlocks
          }
    action sh

-- ---------------------------------------------------------------------------
-- Block operations
-- ---------------------------------------------------------------------------

-- | Persist a signed block to RocksDB and cache it in the TVar.
-- Write ordering: RocksDB first, then TVar (crash-safe).
putBlock :: StorageHandle -> Root -> SignedBlock -> IO ()
putBlock sh root block = do
  Rocks.put (shDB sh) (blockKey root) (sszEncode block)
  atomically $ modifyTVar' (shRecentBlocks sh) (Map.insert root block)

-- | Retrieve a block: TVar cache first, then RocksDB fallback.
-- Decode errors are treated as missing (returns Nothing).
getBlock :: StorageHandle -> Root -> IO (Maybe SignedBlock)
getBlock sh root = do
  cached <- atomically $ Map.lookup root <$> readTVar (shRecentBlocks sh)
  case cached of
    Just b  -> pure (Just b)
    Nothing -> do
      mbs <- Rocks.get (shDB sh) (blockKey root)
      pure $ mbs >>= either (const Nothing) Just . sszDecode

-- ---------------------------------------------------------------------------
-- State operations
-- ---------------------------------------------------------------------------

-- | Persist a beacon state snapshot to RocksDB (keyed by state root).
putState :: StorageHandle -> Root -> BeaconState -> IO ()
putState sh root st =
  Rocks.put (shDB sh) (stateKey root) (sszEncode st)

-- | Retrieve a beacon state from RocksDB.
getState :: StorageHandle -> Root -> IO (Maybe BeaconState)
getState sh root = do
  mbs <- Rocks.get (shDB sh) (stateKey root)
  pure $ mbs >>= either (const Nothing) Just . sszDecode

-- ---------------------------------------------------------------------------
-- Finalized block index
-- ---------------------------------------------------------------------------

-- | Write a finalized block: stores the block, the slot->root index, and
-- updates the meta:finalized_slot pointer. Uses a batch write for atomicity.
putFinalizedBlock :: StorageHandle -> Slot -> Root -> SignedBlock -> IO ()
putFinalizedBlock sh slot root block =
  Rocks.write (shDB sh)
    [ Put (blockKey root) (sszEncode block)
    , Put (finalizedKey slot) (unBytesN root)
    , Put (metaKey "finalized_slot") (encodeBE64 slot)
    , Put (metaKey "head_root") (unBytesN root)
    ]

-- | Look up a finalized block by slot: slot -> root index, then root -> block.
getFinalizedBlock :: StorageHandle -> Slot -> IO (Maybe SignedBlock)
getFinalizedBlock sh slot = do
  mRootBs <- Rocks.get (shDB sh) (finalizedKey slot)
  case mRootBs of
    Nothing     -> pure Nothing
    Just rootBs -> case mkBytesN rootBs of
      Left _     -> pure Nothing
      Right root -> getBlock sh root

-- ---------------------------------------------------------------------------
-- In-memory state (STM)
-- ---------------------------------------------------------------------------

-- | Read the current beacon state from the hot TVar.
readCurrentState :: StorageHandle -> STM BeaconState
readCurrentState = readTVar . shHotState

-- | Atomically update the current beacon state.
writeCurrentState :: StorageHandle -> BeaconState -> STM ()
writeCurrentState sh = writeTVar (shHotState sh)

-- | Read the fork choice store from the hot TVar.
readForkChoiceStore :: StorageHandle -> STM Store
readForkChoiceStore = readTVar . shHotStore

-- | Atomically update the fork choice store.
writeForkChoiceStore :: StorageHandle -> Store -> STM ()
writeForkChoiceStore sh = writeTVar (shHotStore sh)

-- ---------------------------------------------------------------------------
-- Cache management
-- ---------------------------------------------------------------------------

-- | Remove blocks older than the given slot threshold from the in-memory
-- cache. Blocks remain in RocksDB. Returns the number of evicted entries.
pruneOldBlocks :: StorageHandle -> Slot -> IO Int
pruneOldBlocks sh threshold = atomically $ do
  blocks <- readTVar (shRecentBlocks sh)
  let (old, keep) = Map.partition isOld blocks
  writeTVar (shRecentBlocks sh) keep
  pure (Map.size old)
  where
    isOld sbb = bbSlot (sbMessage sbb) < threshold
