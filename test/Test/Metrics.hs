module Test.Metrics (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent.STM
import Data.IORef (newIORef, writeIORef, readIORef)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Network.HTTP.Types (Status, status200)
import Network.Wai (defaultRequest, Request (..))
import Network.Wai.Internal (Response (..), ResponseReceived (..))

import Metrics

tests :: TestTree
tests = testGroup "Metrics"
  [ testCase "should initialize with zero counters" $ do
      nm <- initMetrics
      counters <- readTVarIO (nmCounters nm)
      Map.lookup "beacon_slots_processed_total" counters @?= Just 0

  , testCase "should increment counter" $ do
      nm <- initMetrics
      atomically $ incCounter nm "beacon_slots_processed_total"
      atomically $ incCounter nm "beacon_slots_processed_total"
      counters <- readTVarIO (nmCounters nm)
      Map.lookup "beacon_slots_processed_total" counters @?= Just 2

  , testCase "should set gauge" $ do
      nm <- initMetrics
      atomically $ setGauge nm "beacon_head_slot" 42
      gauges <- readTVarIO (nmGauges nm)
      Map.lookup "beacon_head_slot" gauges @?= Just 42.0

  , testCase "GET /metrics returns prometheus text format" $ do
      nm <- initMetrics
      atomically $ do
        incCounter nm "beacon_blocks_received_total"
        setGauge nm "beacon_head_slot" 10
      (status, body) <- callMetrics nm ["metrics"]
      status @?= status200
      let bodyStr = BL8.unpack body
      assertBool "should contain counter type"
        (isInfixOf' "# TYPE beacon_blocks_received_total counter" bodyStr)
      assertBool "should contain counter value"
        (isInfixOf' "beacon_blocks_received_total 1" bodyStr)
      assertBool "should contain gauge"
        (isInfixOf' "beacon_head_slot 10" bodyStr)

  , testCase "GET / returns OK" $ do
      nm <- initMetrics
      (status, body) <- callMetrics nm []
      status @?= status200
      BL8.unpack body @?= "OK\n"
  ]

callMetrics :: NodeMetrics -> [Text] -> IO (Status, BL.ByteString)
callMetrics nm pathParts = do
  let app = metricsApp nm
      req = defaultRequest { pathInfo = pathParts }
  ref <- newIORef (error "no response")
  _ <- app req $ \resp -> do
    let (s, body) = extractResponse resp
    writeIORef ref (s, body)
    pure ResponseReceived
  readIORef ref

extractResponse :: Response -> (Status, BL.ByteString)
extractResponse (ResponseBuilder s _headers builder) =
  (s, Builder.toLazyByteString builder)
extractResponse (ResponseRaw _ fallback) =
  extractResponse fallback
extractResponse (ResponseFile s _headers _fp _part) =
  (s, BL.empty)
extractResponse (ResponseStream s _headers _body) =
  (s, BL.empty)

isInfixOf' :: String -> String -> Bool
isInfixOf' needle haystack = any (isPrefixOf' needle) (tails' haystack)

isPrefixOf' :: String -> String -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' xs@(_:xs') = xs : tails' xs'
