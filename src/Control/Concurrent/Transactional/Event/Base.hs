{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE Safe #-}

module Control.Concurrent.Transactional.Event.Base (
    Event (), forkEvent, sync, syncID, swap, unsafeLiftIO
) where

import Control.Concurrent.Transactional.Event.SyncLock
import Control.Concurrent.Transactional.Event.Trail
import Data.List.Util

import Control.Applicative (Applicative, Alternative, empty, (<|>), pure, (<*>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, withMVar, putMVar, takeMVar)
import Control.Monad (MonadPlus, mzero, mplus, ap, filterM, forM_, void, when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Unique (Unique)

import qualified Data.Map as Map

type EventHandler a = a -> Trail -> IO ()

-- |When synchronized upon, an event performs synchronous
-- operations with other threads before returning a value.
newtype Event a = Event [Trail -> EventHandler a -> IO ()]

-- Wraps an event invocation function. This prevents an event from
-- running if any of its dependencies have already been synced.
newEvent :: (Trail -> EventHandler a -> IO ()) -> Event a
newEvent invoke = Event [invoke']
  where
    invoke' trail handler = do
        ok <- isActive trail
        when ok $ invoke trail handler

runEvent :: Event a -> Trail -> EventHandler a -> IO ()
runEvent (Event events) trail handler =
    fmap (zip [1..]) (shuffle events) >>=
    mapM_ (void . forkIO . searchThread)
  where
    searchThread (index, f) = f (extend trail $ Choose index) handler

-- |Blocks the current thread until the specified event yields a value.
sync :: MonadIO m => Event a -> m a
sync event = liftIO $ do
    emptyTrail <- newTrail
    output <- newEmptyMVar

    runEvent event emptyTrail $ syncHandler $ putMVar output
    takeMVar output

syncHandler :: (a -> IO ()) -> a -> Trail -> IO ()
syncHandler action value trail = do
    completeValue <- complete trail $ action value
    commitSet <- commitSets completeValue

    case commitSet of
        [] -> return ()
        xs:_ -> do
            -- Each assoc will be in the form (lock, (trail, action)).
            let assocs = Map.assocs xs
                locks = map fst assocs
                actions = map (snd . snd) assocs

            result <- syncAll locks
            when result $ sequence_ actions

-- |An event that returns a unique identifier for the initial sync operation.
syncID :: Event Unique
syncID = newEvent $ \trail handler ->
    handler (trailID trail) trail

-- |Lifts an IO action to the Event monad. This function is not unsafe in the
-- normal sense (i.e. unsafePerformIO). Use of this function can break the
-- semantics of the Event type.
unsafeLiftIO :: IO a -> Event a
unsafeLiftIO action = newEvent $ \trail handler -> do
    value <- action
    handler value trail

-- |Concurrently evaluates an event. After synchronization, a new thread will be
-- spawned that executes the resulting IO value.
forkEvent :: Event (IO ()) -> Event Unique
forkEvent event = do
    (waitFront, waitBack) <- swap

    output <- unsafeLiftIO $ do
        emptyTrail <- newTrail

        void $ forkIO $
            runEvent (waitFront () >> event) emptyTrail $
                syncHandler (void . forkIO)

        return $ trailID emptyTrail

    waitBack ()

    return output

instance Monad Event where
    return x = newEvent $ \trail handler ->
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

type SwapData i o = (Trail, EventHandler o, i)

-- |Creates a pair of functions that may be used to swap values between threads.
swap :: Event (i -> Event o, o -> Event i)
swap = unsafeLiftIO $ do
    lock <- newMVar ()
    front <- newIORef []
    back <- newIORef []

    return (swapAction lock front back, swapAction lock back front)

swapAction :: MVar () -> IORef [SwapData f b] -> IORef [SwapData b f] -> f -> Event b
swapAction lock front back value = newEvent $ \trail handler -> do
    remaining <- withMVar lock $ \_ -> do
        -- Enqueue the swap data on the front queue for future swappers.
        modifyIORef front (++ [(trail, handler, value)])

        -- Cleaning helps prevent space leaks.
        void $ clean front
        clean back

    forM_ remaining $ \(trail', handler', value') ->
        -- Incoherent trails will never commit anyways.
        when (isCoherent trail trail') $ do
            ref1 <- newIORef []
            ref2 <- newIORef []

            handler value' $ extend trail $ Swap trail' ref1 ref2
            handler' value $ extend trail' $ Swap trail ref2 ref1

-- Removes inactive trails, and returns the remaining.
clean :: IORef [SwapData i o] -> IO [SwapData i o]
clean queue = do
    value <- readIORef queue
    remaining <- filterM (\(trail, _, _) -> isActive trail) value
    writeIORef queue remaining

    return remaining
