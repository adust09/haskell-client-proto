-- | Tests for the RocksDB + TVar storage layer.
module Test.Storage (tests) where

import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Consensus.ForkChoice (initStore)
import Consensus.Types (BeaconState, SignedBeaconBlock (..), Store)
import Storage
import Test.Support.Helpers
    ( buildChain
    , mkTestGenesisBlock
    , mkTestGenesisState
    , mkTestValidator
    , toRoot
    )

tests :: TestTree
tests = testGroup "Storage"
  [ testCase "putBlock -> getBlock roundtrip" testBlockRoundtrip
  , testCase "putState -> getState roundtrip" testStateRoundtrip
  , testCase "hot cache hit" testHotCacheHit
  , testCase "cold read from RocksDB" testColdRead
  , testCase "pruneOldBlocks removes old entries from TVar" testPrune
  , testCase "concurrent read/write" testConcurrent
  , testCase "persistence across close/open" testPersistence
  ]

mkEnv :: (BeaconState -> Store -> [SignedBeaconBlock] -> [BeaconState] -> IO a) -> IO a
mkEnv action = do
  let vals = [mkTestValidator i (fromIntegral (i - 1)) | i <- [1..4]]
      gs   = mkTestGenesisState vals
      gb   = mkTestGenesisBlock
      store = initStore gs gb
      (blocks, states) = buildChain gs 3
  action gs store blocks states

-- | Store a block, retrieve by root, assert equality.
testBlockRoundtrip :: Assertion
testBlockRoundtrip = mkEnv $ \gs _store blocks _states ->
  withSystemTempDirectory "storage-test" $ \dir ->
    withStorage dir gs (initStore gs mkTestGenesisBlock) $ \sh -> do
      let sbb  = head blocks
          root = toRoot (sbbBlock sbb)
      putBlock sh root sbb
      result <- getBlock sh root
      result @?= Just sbb

-- | Store a BeaconState, retrieve by root, assert equality.
testStateRoundtrip :: Assertion
testStateRoundtrip = mkEnv $ \gs _store _blocks states ->
  withSystemTempDirectory "storage-test" $ \dir ->
    withStorage dir gs (initStore gs mkTestGenesisBlock) $ \sh -> do
      let st   = head states
          root = toRoot st
      putState sh root st
      result <- getState sh root
      result @?= Just st

-- | putBlock caches in shRecentBlocks TVar.
testHotCacheHit :: Assertion
testHotCacheHit = mkEnv $ \gs _store blocks _states ->
  withSystemTempDirectory "storage-test" $ \dir ->
    withStorage dir gs (initStore gs mkTestGenesisBlock) $ \sh -> do
      let sbb  = head blocks
          root = toRoot (sbbBlock sbb)
      putBlock sh root sbb
      cached <- atomically $ Map.lookup root <$> readTVar (shRecentBlocks sh)
      cached @?= Just sbb

-- | putBlock, manually clear TVar cache, getBlock still returns from RocksDB.
testColdRead :: Assertion
testColdRead = mkEnv $ \gs _store blocks _states ->
  withSystemTempDirectory "storage-test" $ \dir ->
    withStorage dir gs (initStore gs mkTestGenesisBlock) $ \sh -> do
      let sbb  = head blocks
          root = toRoot (sbbBlock sbb)
      putBlock sh root sbb
      -- Evict from cache
      atomically $ writeTVar (shRecentBlocks sh) Map.empty
      result <- getBlock sh root
      result @?= Just sbb

-- | Build 3-block chain, prune below slot 2, verify removed from TVar but
-- still available in RocksDB.
testPrune :: Assertion
testPrune = mkEnv $ \gs _store blocks _states ->
  withSystemTempDirectory "storage-test" $ \dir ->
    withStorage dir gs (initStore gs mkTestGenesisBlock) $ \sh -> do
      -- Put all 3 blocks (slots 1, 2, 3)
      let roots = map (\sbb -> (toRoot (sbbBlock sbb), sbb)) blocks
      mapM_ (\(r, sbb) -> putBlock sh r sbb) roots
      -- Prune blocks with slot < 2 (removes slot 1)
      removed <- pruneOldBlocks sh 2
      removed @?= 1
      -- Slot 1 block gone from TVar
      cacheSize <- atomically $ Map.size <$> readTVar (shRecentBlocks sh)
      cacheSize @?= 2
      -- But still in RocksDB
      let (root1, sbb1) = head roots
      result <- do
        -- Clear cache to force RocksDB read
        atomically $ writeTVar (shRecentBlocks sh) Map.empty
        getBlock sh root1
      result @?= Just sbb1

-- | 4 async threads each put+get 10 blocks, no lost writes.
testConcurrent :: Assertion
testConcurrent = do
  let vals = [mkTestValidator i (fromIntegral (i - 1)) | i <- [1..4]]
      gs   = mkTestGenesisState vals
      gb   = mkTestGenesisBlock
      store = initStore gs gb
      -- Build enough blocks for 4 threads x 10 blocks
      (allBlocks, _) = buildChain gs 40
      chunks = chunksOf 10 allBlocks
  withSystemTempDirectory "storage-test" $ \dir ->
    withStorage dir gs store $ \sh -> do
      -- Concurrent writes
      forConcurrently_ chunks $ \chunk ->
        mapM_ (\sbb -> putBlock sh (toRoot (sbbBlock sbb)) sbb) chunk
      -- Verify all blocks readable
      mapM_ (\sbb -> do
        let root = toRoot (sbbBlock sbb)
        result <- getBlock sh root
        result @?= Just sbb
        ) allBlocks

-- | putBlock, close storage, reopen, getBlock returns same block.
testPersistence :: Assertion
testPersistence = mkEnv $ \gs _store blocks _states ->
  withSystemTempDirectory "storage-test" $ \dir -> do
    let sbb  = head blocks
        root = toRoot (sbbBlock sbb)
        store = initStore gs mkTestGenesisBlock
    -- Write in first session
    withStorage dir gs store $ \sh ->
      putBlock sh root sbb
    -- Read in second session
    withStorage dir gs store $ \sh -> do
      result <- getBlock sh root
      result @?= Just sbb

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t
