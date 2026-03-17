module Test.Network.IPC (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, catch)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.Socket
    ( Socket, SockAddr (..)
    , socket, bind, listen, accept, close
    , Family (..), SocketType (..)
    , defaultProtocol, maxListenQueue
    )
import Network.Socket.ByteString (sendAll, recv)
import System.IO.Temp (withSystemTempDirectory)

import Network.P2P.IPC (connectIPC, ipcP2PHandle)
import Network.P2P.Types (P2PHandle (..), Topic (..))

tests :: TestTree
tests = testGroup "Network.P2P.IPC"
  [ testCase "publish sends JSON-RPC request to sidecar" testPublish
  , testCase "subscribe registers callback and receives notifications" testSubscribe
  , testCase "peer_count returns value from sidecar" testPeerCount
  ]

-- | Run a mock sidecar server for the duration of a test.
withMockSidecar
  :: FilePath                          -- ^ socket path
  -> (Socket -> ByteString -> IO ())   -- ^ handler: client socket, received data
  -> IO a                             -- ^ test action (connect after server is ready)
  -> IO a
withMockSidecar sockPath handler action = do
  serverSock <- socket AF_UNIX Stream defaultProtocol
  bind serverSock (SockAddrUnix sockPath)
  listen serverSock maxListenQueue

  ready <- newEmptyMVar
  _ <- forkIO $ do
    putMVar ready ()
    (clientSock, _) <- accept serverSock
    let loop = do
          bs <- recv clientSock 4096 `catch` (\(_ :: SomeException) -> pure BS.empty)
          if BS.null bs
            then close clientSock
            else do
              handler clientSock bs
              loop
    loop `catch` (\(_ :: SomeException) -> pure ())
    close serverSock

  takeMVar ready
  threadDelay 10000  -- let server fully bind
  action

-- | Test that publish sends a proper JSON-RPC request.
testPublish :: IO ()
testPublish =
  withSystemTempDirectory "lc-test-ipc" $ \tmpDir -> do
    let sockPath = tmpDir <> "/test.sock"
    received <- newIORef BS.empty

    withMockSidecar sockPath
      (\clientSock bs -> do
        modifyIORef' received (<> bs)
        -- Send a response
        let response = Aeson.object
              [ "jsonrpc" .= ("2.0" :: Text)
              , "result"  .= Aeson.Null
              , "id"      .= (1 :: Int)
              ]
        sendAll clientSock (LBS.toStrict (Aeson.encode response) <> "\n")
      )
      $ do
        ipc <- connectIPC sockPath
        let handle = ipcP2PHandle ipc
        p2hPublish handle TopicBeaconBlock (BS.pack [1, 2, 3])
        threadDelay 50000

        got <- readIORef received
        -- The received data should contain "publish" and "beacon_block"
        assertBool "should contain method 'publish'"
          (BS.isInfixOf (encodeUtf8 "publish") got)
        assertBool "should contain topic 'beacon_block'"
          (BS.isInfixOf (encodeUtf8 "beacon_block") got)

-- | Test that subscribe receives notifications from sidecar.
testSubscribe :: IO ()
testSubscribe =
  withSystemTempDirectory "lc-test-ipc-sub" $ \tmpDir -> do
    let sockPath = tmpDir <> "/test.sock"
    callbackData <- newEmptyMVar

    withMockSidecar sockPath
      (\clientSock _bs -> do
        -- Respond to subscribe, then send a notification
        let response = Aeson.object
              [ "jsonrpc" .= ("2.0" :: Text)
              , "result"  .= Aeson.Null
              , "id"      .= (1 :: Int)
              ]
        sendAll clientSock (LBS.toStrict (Aeson.encode response) <> "\n")
        threadDelay 50000

        -- Send a message notification
        let dataHex = decodeUtf8 (Base16.encode (BS.pack [0xCA, 0xFE]))
            notification = Aeson.object
              [ "jsonrpc" .= ("2.0" :: Text)
              , "method"  .= ("message" :: Text)
              , "params"  .= Aeson.object
                  [ "topic" .= ("beacon_block" :: Text)
                  , "data"  .= dataHex
                  ]
              ]
        sendAll clientSock (LBS.toStrict (Aeson.encode notification) <> "\n")
      )
      $ do
        ipc <- connectIPC sockPath
        let handle = ipcP2PHandle ipc

        p2hSubscribe handle TopicBeaconBlock $ \msg ->
          putMVar callbackData msg

        -- Wait for the notification to arrive
        threadDelay 200000
        result <- takeMVar callbackData
        result @?= BS.pack [0xCA, 0xFE]

-- | Test that peer_count returns the value from sidecar.
testPeerCount :: IO ()
testPeerCount =
  withSystemTempDirectory "lc-test-ipc-peers" $ \tmpDir -> do
    let sockPath = tmpDir <> "/test.sock"

    withMockSidecar sockPath
      (\clientSock _bs -> do
        let response = Aeson.object
              [ "jsonrpc" .= ("2.0" :: Text)
              , "result"  .= (42 :: Int)
              , "id"      .= (1 :: Int)
              ]
        sendAll clientSock (LBS.toStrict (Aeson.encode response) <> "\n")
      )
      $ do
        ipc <- connectIPC sockPath
        let handle = ipcP2PHandle ipc
        count <- p2hPeerCount handle
        count @?= 42
