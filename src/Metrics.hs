-- | Prometheus-compatible metrics for the lean-consensus node.
module Metrics
  ( NodeMetrics (..)
  , initMetrics
  , startMetricsServer
  , incCounter
  , setGauge
  , metricsApp
  ) where

import Control.Concurrent.STM
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word16)
import Network.HTTP.Types (status200, hContentType)
import Network.Wai (Application, pathInfo, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp

-- | Thread-safe metric counters and gauges.
data NodeMetrics = NodeMetrics
  { nmCounters :: !(TVar (Map String Int))
  , nmGauges   :: !(TVar (Map String Double))
  }

-- | Create a fresh metrics instance with pre-registered names.
initMetrics :: IO NodeMetrics
initMetrics = do
  counters <- newTVarIO $ Map.fromList
    [ ("beacon_slots_processed_total", 0)
    , ("beacon_blocks_received_total", 0)
    , ("beacon_attestations_received_total", 0)
    ]
  gauges <- newTVarIO $ Map.fromList
    [ ("beacon_head_slot", 0)
    , ("beacon_finalized_slot", 0)
    , ("beacon_peer_count", 0)
    ]
  pure $ NodeMetrics counters gauges

-- | Increment a named counter by 1.
incCounter :: NodeMetrics -> String -> STM ()
incCounter nm name =
  modifyTVar' (nmCounters nm) (Map.adjust (+ 1) name)

-- | Set a named gauge to an absolute value.
setGauge :: NodeMetrics -> String -> Double -> STM ()
setGauge nm name val =
  modifyTVar' (nmGauges nm) (Map.insert name val)

-- | Start a Warp server serving Prometheus text format at /metrics.
startMetricsServer :: Word16 -> NodeMetrics -> IO ()
startMetricsServer port metrics =
  Warp.run (fromIntegral port) (metricsApp metrics)

-- | WAI application serving the /metrics endpoint.
metricsApp :: NodeMetrics -> Application
metricsApp metrics req respond = do
  case pathInfo req of
    ["metrics"] -> do
      body <- renderMetrics metrics
      respond $ responseLBS status200
        [(hContentType, "text/plain; version=0.0.4; charset=utf-8")]
        (BL8.pack body)
    _ ->
      respond $ responseLBS status200
        [(hContentType, "text/plain")]
        "OK\n"

-- | Render all metrics in Prometheus text exposition format.
renderMetrics :: NodeMetrics -> IO String
renderMetrics nm = do
  counters <- readTVarIO (nmCounters nm)
  gauges <- readTVarIO (nmGauges nm)
  let counterLines = Map.foldlWithKey' (\acc k v ->
        acc <> "# TYPE " <> k <> " counter\n"
            <> k <> " " <> show v <> "\n") "" counters
      gaugeLines = Map.foldlWithKey' (\acc k v ->
        acc <> "# TYPE " <> k <> " gauge\n"
            <> k <> " " <> showDouble v <> "\n") "" gauges
  pure $ counterLines <> gaugeLines

showDouble :: Double -> String
showDouble d
  | d == fromIntegral (round d :: Int) = show (round d :: Int)
  | otherwise = show d
