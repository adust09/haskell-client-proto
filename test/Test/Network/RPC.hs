module Test.Network.RPC (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.IORef (newIORef, writeIORef, readIORef)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Network.HTTP.Types (Status, status200, status404)
import Network.Wai (defaultRequest, Request (..))
import Network.Wai.Internal (Response (..), ResponseReceived (..))

import Consensus.ForkChoice (initStore)
import Consensus.Types (Config (..))
import Network.RPC (rpcApp)
import Storage (StorageHandle, withStorage)
import Test.Support.Helpers (mkTestGenesisState, mkTestGenesisBlock, mkTestValidator)

tests :: TestTree
tests = testGroup "Network.RPC"
  [ testCase "GET /eth/v1/node/health returns 200" $ do
      withTestStorage $ \sh -> do
        (status, body) <- callRpc sh ["eth", "v1", "node", "health"]
        status @?= status200
        assertJsonHasKey body "status"

  , testCase "GET /eth/v1/node/syncing returns head_slot" $ do
      withTestStorage $ \sh -> do
        (status, body) <- callRpc sh ["eth", "v1", "node", "syncing"]
        status @?= status200
        assertJsonHasKey body "head_slot"

  , testCase "GET /eth/v1/beacon/headers/head returns slot" $ do
      withTestStorage $ \sh -> do
        (status, body) <- callRpc sh ["eth", "v1", "beacon", "headers", "head"]
        status @?= status200
        assertJsonHasKey body "slot"
        assertJsonHasKey body "proposer_index"

  , testCase "GET /eth/v1/beacon/states/head/finality_checkpoints returns checkpoints" $ do
      withTestStorage $ \sh -> do
        (status, body) <- callRpc sh ["eth", "v1", "beacon", "states", "head", "finality_checkpoints"]
        status @?= status200
        assertJsonHasKey body "justified"
        assertJsonHasKey body "finalized"

  , testCase "GET /eth/v1/node/peers returns empty list" $ do
      withTestStorage $ \sh -> do
        (status, body) <- callRpc sh ["eth", "v1", "node", "peers"]
        status @?= status200
        assertJsonHasKey body "count"

  , testCase "GET /unknown returns 404" $ do
      withTestStorage $ \sh -> do
        (status, _body) <- callRpc sh ["unknown", "path"]
        status @?= status404
  ]

withTestStorage :: (StorageHandle -> IO a) -> IO a
withTestStorage f = do
  let vals = [mkTestValidator 0, mkTestValidator 1]
      gs = mkTestGenesisState vals
      gb = mkTestGenesisBlock
      store = initStore gs gb (Config 0)
  withStorage "/tmp/lc-test-rpc" gs store f

-- | Call the RPC app with a synthetic request and capture the response.
callRpc :: StorageHandle -> [Text] -> IO (Status, BL.ByteString)
callRpc sh pathParts = do
  let app = rpcApp sh
      req = defaultRequest { pathInfo = pathParts }
  ref <- newIORef (error "no response")
  _ <- app req $ \resp -> do
    let (status, body) = extractResponse resp
    writeIORef ref (status, body)
    pure ResponseReceived
  readIORef ref

extractResponse :: Response -> (Status, BL.ByteString)
extractResponse (ResponseBuilder status _headers builder) =
  (status, Builder.toLazyByteString builder)
extractResponse (ResponseRaw _ fallback) =
  extractResponse fallback
extractResponse (ResponseFile status _headers _fp _part) =
  (status, BL.empty)
extractResponse (ResponseStream status _headers _body) =
  (status, BL.empty)

assertJsonHasKey :: BL.ByteString -> String -> IO ()
assertJsonHasKey bs key =
  case Aeson.decode bs of
    Nothing -> assertFailure $ "Failed to parse JSON response"
    Just (Aeson.Object obj) ->
      assertBool ("Missing key: " <> key) (KM.member (Key.fromString key) obj)
    Just _ -> assertFailure "Expected JSON object"
