-- | IPC-based P2P backend: communicates with a libp2p sidecar process
-- via JSON-RPC 2.0 over a Unix domain socket.
module Network.P2P.IPC
  ( connectIPC
  , IPCHandle
  , ipcP2PHandle
  ) where

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar, newEmptyMVar, putMVar, takeMVar, newMVar)
import Control.Exception (SomeException, catch)
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.Socket
    ( Socket, SockAddr (..)
    , socket, connect, close
    , Family (..), SocketType (..)
    , defaultProtocol
    )
import Network.Socket.ByteString (sendAll, recv)

import Network.P2P.Types (P2PHandle (..), topicString)
import SSZ.Common (unBytesN)

-- | Handle to the IPC connection with the P2P sidecar.
data IPCHandle = IPCHandle
  { ipcSocket       :: !Socket
  , ipcNextId       :: !(IORef Int)
  , ipcCallbacks    :: !(MVar (Map String (ByteString -> IO ())))
  , ipcPendingCalls :: !(MVar (Map Int (MVar Aeson.Value)))
  , ipcReaderThread :: !ThreadId
  }

-- | Connect to a P2P sidecar via Unix domain socket.
connectIPC :: FilePath -> IO IPCHandle
connectIPC socketPath = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock (SockAddrUnix socketPath)
  nextId <- newIORef 1
  callbacks <- newMVar Map.empty
  pending <- newMVar Map.empty
  tid <- forkIO (readerLoop sock callbacks pending)
  pure IPCHandle
    { ipcSocket       = sock
    , ipcNextId       = nextId
    , ipcCallbacks    = callbacks
    , ipcPendingCalls = pending
    , ipcReaderThread = tid
    }

-- | Build a P2PHandle from an IPC connection.
ipcP2PHandle :: IPCHandle -> P2PHandle
ipcP2PHandle ipc = P2PHandle
  { p2hPublish = \topic msg -> do
      let topicStr = topicString topic
          dataHex = decodeUtf8 (Base16.encode msg)
      _ <- rpcCall ipc "publish"
        [Aeson.object ["topic" .= topicStr, "data" .= dataHex]]
      pure ()

  , p2hSubscribe = \topic callback -> do
      let topicStr = topicString topic
      modifyMVar_ (ipcCallbacks ipc) $ \m ->
        pure (Map.insert topicStr callback m)
      _ <- rpcCall ipc "subscribe"
        [Aeson.object ["topic" .= topicStr]]
      pure ()

  , p2hUnsubscribe = \topic -> do
      let topicStr = topicString topic
      modifyMVar_ (ipcCallbacks ipc) $ \m ->
        pure (Map.delete topicStr m)
      _ <- rpcCall ipc "unsubscribe"
        [Aeson.object ["topic" .= topicStr]]
      pure ()

  , p2hRequestByRange = \startSlot count -> do
      result <- rpcCall ipc "request_by_range"
        [Aeson.object ["start" .= startSlot, "count" .= count]]
      case Aeson.parseMaybe parseBlockList result of
        Just blocks -> pure blocks
        Nothing     -> pure []

  , p2hRequestByRoot = \roots -> do
      let rootHexes = map (decodeUtf8 . Base16.encode . unBytesN) roots
      result <- rpcCall ipc "request_by_root"
        [Aeson.object ["roots" .= rootHexes]]
      case Aeson.parseMaybe parseBlockList result of
        Just blocks -> pure blocks
        Nothing     -> pure []

  , p2hPeerCount = do
      result <- rpcCall ipc "peer_count" []
      case Aeson.parseMaybe (.: "count") =<< asObject result of
        Just n  -> pure n
        Nothing -> case Aeson.parseMaybe Aeson.parseJSON result of
          Just n  -> pure (n :: Int)
          Nothing -> pure 0

  , p2hLocalPeerId = do
      result <- rpcCall ipc "local_peer_id" []
      case Aeson.parseMaybe Aeson.parseJSON result of
        Just s  -> pure (s :: String)
        Nothing -> pure ""

  , p2hStop = do
      _ <- rpcCall ipc "stop" []
      close (ipcSocket ipc)
  }

-- ---------------------------------------------------------------------------
-- JSON-RPC internals
-- ---------------------------------------------------------------------------

-- | Make a JSON-RPC 2.0 call and wait for the response.
rpcCall :: IPCHandle -> Text -> [Aeson.Value] -> IO Aeson.Value
rpcCall ipc method params = do
  reqId <- atomicModifyIORef' (ipcNextId ipc) (\n -> (n + 1, n))
  responseMVar <- newEmptyMVar

  modifyMVar_ (ipcPendingCalls ipc) $ \m ->
    pure (Map.insert reqId responseMVar m)

  let request = Aeson.object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method"  .= method
        , "params"  .= params
        , "id"      .= reqId
        ]
      encoded = LBS.toStrict (Aeson.encode request) <> "\n"

  sendAll (ipcSocket ipc) encoded
  takeMVar responseMVar

-- | Background reader thread: dispatches responses and notifications.
readerLoop :: Socket -> MVar (Map String (ByteString -> IO ())) -> MVar (Map Int (MVar Aeson.Value)) -> IO ()
readerLoop sock callbacks pending = go BS.empty
  where
    go buffer = do
      chunk <- recv sock 4096 `catch` (\(_ :: SomeException) -> pure BS.empty)
      if BS.null chunk
        then pure ()  -- Connection closed
        else do
          let combined = buffer <> chunk
          rest <- processLines combined
          go rest

    processLines bs =
      case BS.elemIndex 0x0A bs of  -- newline
        Nothing -> pure bs
        Just idx -> do
          let (line, remaining) = BS.splitAt idx bs
              rest = BS.drop 1 remaining  -- skip the newline
          handleLine line callbacks pending
          processLines rest

-- | Handle a single JSON-RPC line (response or notification).
handleLine :: ByteString -> MVar (Map String (ByteString -> IO ())) -> MVar (Map Int (MVar Aeson.Value)) -> IO ()
handleLine line callbacks pending =
  case Aeson.decodeStrict line of
    Nothing -> pure ()
    Just val -> case val of
      Aeson.Object obj -> do
        -- Check if it's a response (has "id" field)
        case Aeson.parseMaybe (.: "id") obj of
          Just (reqId :: Int) -> do
            let result = case Aeson.parseMaybe (.: "result") obj of
                  Just r  -> r
                  Nothing -> Aeson.Null
            pendingMap <- readMVar pending
            case Map.lookup reqId pendingMap of
              Just mvar -> do
                putMVar mvar result
                modifyMVar_ pending $ pure . Map.delete reqId
              Nothing -> pure ()

          Nothing -> do
            -- It's a notification — check for "method" == "message"
            case Aeson.parseMaybe (.: "method") obj of
              Just ("message" :: Text) -> do
                let mParams = Aeson.parseMaybe (.: "params") obj
                case mParams of
                  Just paramObj -> handleNotification paramObj callbacks
                  Nothing -> pure ()
              _ -> pure ()

      _ -> pure ()

-- | Handle a "message" notification from the sidecar.
handleNotification :: Aeson.Object -> MVar (Map String (ByteString -> IO ())) -> IO ()
handleNotification params callbacks = do
  let mTopic = Aeson.parseMaybe (.: "topic") params
      mData  = Aeson.parseMaybe (.: "data") params
  case (mTopic, mData) of
    (Just topic, Just dataHex) -> do
      case Base16.decode (encodeUtf8 (dataHex :: Text)) of
        Left _ -> pure ()
        Right bs -> do
          cbs <- readMVar callbacks
          case Map.lookup (topic :: String) cbs of
            Just cb -> cb bs `catch` (\(_ :: SomeException) -> pure ())
            Nothing -> pure ()
    _ -> pure ()

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

parseBlockList :: Aeson.Value -> Aeson.Parser [ByteString]
parseBlockList = Aeson.withArray "blocks" $ \arr ->
  mapM parseHexBlock (map id (foldr (:) [] arr))

parseHexBlock :: Aeson.Value -> Aeson.Parser ByteString
parseHexBlock = Aeson.withText "hex" $ \t ->
  case Base16.decode (encodeUtf8 t) of
    Left _   -> fail "invalid hex"
    Right bs -> pure bs

asObject :: Aeson.Value -> Maybe Aeson.Object
asObject (Aeson.Object o) = Just o
asObject _                = Nothing
