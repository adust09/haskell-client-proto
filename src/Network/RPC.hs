-- | Minimal HTTP REST API for the lean-consensus node.
module Network.RPC
  ( startRpcServer
  , rpcApp
  ) where

import Control.Concurrent.STM (atomically)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word16)
import Network.HTTP.Types (Status, hContentType, status200, status404)
import Network.Wai (Application, Response, pathInfo, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp

import Consensus.ForkChoice (getHead)
import Consensus.Types
  ( BeaconState (..)
  , BeaconBlockHeader (..)
  , Checkpoint (..)
  , Store (..)
  )
import SSZ.Common (unBytesN)
import Storage (StorageHandle, readCurrentState, readForkChoiceStore)

-- | Start the RPC server on the given port.
startRpcServer :: Word16 -> StorageHandle -> IO ()
startRpcServer port storage =
  Warp.run (fromIntegral port) (rpcApp storage)

-- | WAI application implementing the REST endpoints.
rpcApp :: StorageHandle -> Application
rpcApp storage req respond = do
  let path = pathInfo req
  case path of
    ["eth", "v1", "node", "health"] ->
      respond $ jsonResponse status200 $ object ["status" .= ("OK" :: String)]

    ["eth", "v1", "node", "syncing"] -> do
      state <- atomically $ readCurrentState storage
      respond $ jsonResponse status200 $ object
        [ "head_slot" .= bsSlot state
        , "is_syncing" .= False
        ]

    ["eth", "v1", "beacon", "headers", "head"] -> do
      state <- atomically $ readCurrentState storage
      let hdr = bsLatestBlockHeader state
      respond $ jsonResponse status200 $ object
        [ "slot" .= bbhSlot hdr
        , "proposer_index" .= bbhProposerIndex hdr
        , "parent_root" .= hexEncode (unBytesN (bbhParentRoot hdr))
        , "state_root" .= hexEncode (unBytesN (bbhStateRoot hdr))
        , "body_root" .= hexEncode (unBytesN (bbhBodyRoot hdr))
        ]

    ["eth", "v1", "beacon", "states", "head", "finality_checkpoints"] -> do
      state <- atomically $ readCurrentState storage
      respond $ jsonResponse status200 $ object
        [ "justified" .= checkpointJson (bsLatestJustified state)
        , "finalized" .= checkpointJson (bsLatestFinalized state)
        ]

    ["eth", "v1", "node", "peers"] ->
      respond $ jsonResponse status200 $ object
        [ "count" .= (0 :: Int)
        , "peers" .= ([] :: [Value])
        ]

    ["eth", "v1", "beacon", "head"] -> do
      store <- atomically $ readForkChoiceStore storage
      let headRoot = getHead store
      respond $ jsonResponse status200 $ object
        [ "root" .= hexEncode (unBytesN headRoot)
        , "slot" .= (stTime store `div` 5)
        ]

    _ ->
      respond $ jsonResponse status404 $ object
        [ "error" .= ("Not found" :: String) ]

jsonResponse :: Status -> Value -> Response
jsonResponse status val =
  responseLBS status [(hContentType, "application/json")] (Aeson.encode val)

checkpointJson :: Checkpoint -> Value
checkpointJson cp = object
  [ "slot" .= cpSlot cp
  , "root" .= hexEncode (unBytesN (cpRoot cp))
  ]

hexEncode :: BS.ByteString -> Text
hexEncode bs = "0x" <> TE.decodeUtf8 (Base16.encode bs)
