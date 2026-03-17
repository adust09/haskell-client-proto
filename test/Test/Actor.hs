module Test.Actor (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM

import Actor

tests :: TestTree
tests = testGroup "Actor"
  [ testCase "should receive messages via send" $ do
      resultVar <- newTVarIO (0 :: Int)
      actor <- spawnActor "counter" $ \queue -> do
        msg <- atomically $ readTQueue queue
        atomically $ writeTVar resultVar msg
      atomically $ send actor (42 :: Int)
      threadDelay 50000  -- 50ms for actor to process
      result <- readTVarIO resultVar
      result @?= 42
      stopActor actor

  , testCase "should stop gracefully via stopActor" $ do
      actor <- spawnActor "blocker" $ \queue -> do
        _ <- atomically $ readTQueue queue  -- blocks forever
        pure ()
      stopActor actor
      result <- waitActor actor
      case result of
        Left _  -> pure ()  -- cancelled, expected
        Right _ -> pure ()  -- also fine

  , testCase "should report crash via waitActor" $ do
      actor <- spawnActor "crasher" $ \_queue -> do
        error "intentional crash"
      threadDelay 50000  -- let it crash
      result <- waitActor actor
      case result of
        Left _  -> pure ()  -- crash propagated as Left
        Right _ -> assertFailure "Expected Left from crashed actor"

  , testCase "should process multiple messages in order" $ do
      resultVar <- newTVarIO ([] :: [Int])
      actor <- spawnActor "collector" $ \queue -> collectN queue resultVar 3
      atomically $ do
        send actor (1 :: Int)
        send actor 2
        send actor 3
      threadDelay 100000  -- 100ms
      result <- readTVarIO resultVar
      reverse result @?= [1, 2, 3]
      stopActor actor
  ]

collectN :: TQueue Int -> TVar [Int] -> Int -> IO ()
collectN _queue _var 0 = pure ()
collectN queue var n = do
  msg <- atomically $ readTQueue queue
  atomically $ modifyTVar' var (msg :)
  collectN queue var (n - 1)
