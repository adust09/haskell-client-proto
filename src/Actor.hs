-- | Lightweight actor framework using GHC threads and STM queues.
module Actor
  ( Actor (..)
  , spawnActor
  , send
  , stopActor
  , waitActor
  ) where

import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.STM
import Control.Exception (SomeException)

-- | An actor wrapping a named async thread communicating via a TQueue.
data Actor msg = Actor
  { actorQueue  :: !(TQueue msg)
  , actorThread :: !(Async ())
  , actorName   :: !String
  }

-- | Spawn a named actor that processes messages from a TQueue.
spawnActor :: String -> (TQueue msg -> IO ()) -> IO (Actor msg)
spawnActor name handler = do
  queue <- newTQueueIO
  thread <- async (handler queue)
  pure $ Actor queue thread name

-- | Enqueue a message for the actor (STM, composable).
send :: Actor msg -> msg -> STM ()
send actor msg = writeTQueue (actorQueue actor) msg

-- | Cancel the actor thread and wait for termination.
stopActor :: Actor msg -> IO ()
stopActor actor = do
  cancel (actorThread actor)
  _ <- waitCatch (actorThread actor)
  pure ()

-- | Wait for the actor to finish and return the result.
waitActor :: Actor msg -> IO (Either SomeException ())
waitActor = waitCatch . actorThread
