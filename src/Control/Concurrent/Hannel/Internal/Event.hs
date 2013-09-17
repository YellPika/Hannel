{-# LANGUAGE DoAndIfThenElse #-}

module Control.Concurrent.Hannel.Internal.Event (
    Event (), EventHandler, create, threadID,
    MonadSync, sync
) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Applicative (Applicative, Alternative, empty, (<|>), pure, (<*>))
import Control.Monad (MonadPlus, mzero, mplus, ap, forM, foldM_, void, when)
import Data.Array.IO (IOArray, newListArray, readArray, writeArray)
import System.Random (randomRIO)
import qualified Data.Map as Map

import Control.Concurrent.Hannel.Internal.Trail (Trail, TrailElement (..))
import qualified Control.Concurrent.Hannel.Internal.Trail as Trail
import qualified Control.Concurrent.Hannel.Internal.SyncLock as SyncLock

type EventHandler a = a -> Trail -> IO ()

-- |When synchronized upon, an event performs synchronous
-- operations with other threads before returning a value.
newtype Event a = Event [Trail -> EventHandler a -> IO ()]

runEvent :: Event a -> Trail -> EventHandler a -> IO ()
runEvent (Event events) trail handler =
    shuffle events >>=
    foldM_ (\i x -> do
        x (Trail.extend trail $ Choose i) handler
        return $ i + 1) 0

shuffle :: [a] -> IO [a]
shuffle xs = do
    let n = length xs
    ar <- newArray n xs
    forM [1 .. n] $ \i -> do
        j <- randomRIO (i, n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj

newArray :: Int -> [a] -> IO (IOArray Int a)
newArray n = newListArray (1, n)

-- Wraps an event invocation function. This prevents an event from
-- running if any of its dependencies have already been synced.
create :: (Trail -> EventHandler a -> IO ()) -> Event a
create invoke = Event [invoke']
  where
    invoke' trail handler = do
        ok <- Trail.isActive trail
        when ok $ invoke trail handler

-- |An event that returns the thread ID of the synchronizing thread.
threadID :: Event ThreadId
threadID = create $ \trail handler ->
    handler (Trail.threadID trail) trail

class Monad m => MonadSync m where
    -- |Blocks the current thread until the specified event yields a value.
    sync :: Event a -> m a

instance MonadSync IO where
    sync event = do
        trail <- Trail.create
        output <- newEmptyMVar

        runEvent event trail $ syncHandler $ putMVar output
        takeMVar output

-- Handler for the sync event.
syncHandler :: (a -> IO ()) -> a -> Trail -> IO ()
syncHandler action value trail = do
    completeValue <- Trail.complete trail $ action value
    commitSet <- Trail.commitSets completeValue
    case map Map.assocs commitSet of
        [] -> return ()
        xs:_ -> -- Each assoc will be in the form (lock, (trail, action)).
            let locks = map fst xs
                actions = map (snd . snd) xs in
            void $ SyncLock.withAll locks $
                sequence_ actions

instance Monad Event where
    return x = create $ \trail handler ->
        handler x trail

    Event xs >>= f = Event $ map bind xs
      where
        bind invoke trail handler =
            invoke trail $ \x trail' ->
                runEvent (f x) trail' handler

instance MonadPlus Event where
    mzero = Event []
    mplus (Event first) (Event second) = Event (first ++ second)

instance Functor Event where
    fmap f (Event xs) = Event $ map fmap' xs
      where
        fmap' invoke trail handler = invoke trail (handler . f)

instance Applicative Event where
    pure = return
    (<*>) = ap

instance Alternative Event where
    empty = mzero
    (<|>) = mplus
