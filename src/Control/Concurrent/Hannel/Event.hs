{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Safe #-}

module Control.Concurrent.Hannel.Event (
    Event, sync, syncID,
    merge, tee, split,
    forkEvent, forkEventCancel, forkEventHandle, forkServer,
    module Control.Concurrent.Hannel.Event.Class,
    module Control.Concurrent.Hannel.Event.Handle,
    module Control.Concurrent.Hannel.Event.Time
) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Hannel.Channel.Swap (newSwapChannel, sendFront, receiveBack, signalFront, signalBack)
import Control.Concurrent.Hannel.Event.Base (Event, runEvent, unsafeLiftIO)
import Control.Concurrent.Hannel.Event.Class
import Control.Concurrent.Hannel.Event.Handle (EventHandle, newEventHandle, touchEventHandle)
import Control.Concurrent.Hannel.Event.Sync (sync, syncHandler, syncID)
import Control.Concurrent.Hannel.Event.Time
import Control.Concurrent.Hannel.Event.Trail (newTrail)
import Control.Monad (join, msum, void)
import Data.Unique (Unique)

import qualified Control.Concurrent.Hannel.Event.Trail as Trail

-- |Forks a new thread that continuously synchronizes on
-- an event parametrized by a state value. The result of
-- the event is fed back into itself as input.
forkServer :: a -> (a -> Event a) -> Event (Unique, EventHandle)
forkServer value step = forkEventHandle $ \close ->
    let closeEvent = fmap return close
        loopIO = join . sync . loopEvent
        loopEvent x = closeEvent <|> do
            x' <- step x
            loopEvent x' <|> return (loopIO x')
    in
        closeEvent <|> return (loopIO value) <|> loopEvent value

-- |Concurrently evaluates an event that will be cancelled when the returned
-- handle goes out of scope.
forkEventHandle :: (Event () -> Event (IO ())) -> Event (Unique, EventHandle)
forkEventHandle event = do
    (handle, close) <- newEventHandle
    output <- forkEvent $ event close
    return (output, handle)

-- |Concurrently evaluates an event that may be cancelled.
forkEventCancel :: (Event () -> Event (IO ())) -> Event (Unique, Event ())
forkEventCancel event = do
    channel <- newSwapChannel
    output <- forkEvent $ event $ signalFront channel
    return (output, signalBack channel)

-- |Concurrently evaluates an event. After synchronization, a new thread will be
-- spawned that executes the resulting IO value.
forkEvent :: Event (IO ()) -> Event Unique
forkEvent event = do
    channel <- newSwapChannel
    let event' = signalFront channel >> event
        action = syncHandler (void . forkIO)

    output <- unsafeLiftIO $ do
        emptyTrail <- newTrail
        void $ forkIO $ runEvent event' emptyTrail action
        return $ Trail.syncID emptyTrail
    signalBack channel
    return output

-- |Merges a list of events. The resulting event will wait for all the source
-- events to synchronize before returning a value. Unlike
-- 'Control.Monad.sequence', the resulting event will accept the source events
-- in any order, preventing possible deadlocks.
merge :: [Event a] -> Event [a]
merge [] = return []
merge xs = do
    (l, c, r) <- msum $
                 map (\(l, c, r) -> fmap (\c' -> (l, c', r)) c) $
                 splits xs
    l' <- merge l
    r' <- merge r
    return $ l' ++ (c : r')

splits :: [a] -> [([a], a, [a])]
splits [] = []
splits (x:xs) = ([], x, xs) : map (\(l, c, r) -> (x:l, c, r)) (splits xs)

-- |Splits an event into two events that will be notified
-- simultaneously when the original event is fired.
tee :: Event a -> Event (Event a, Event a)
tee event = do
    channel <- newSwapChannel

    let client = receiveBack channel
        server = do
            x <- event
            sendFront channel x
            return x
        output = client <|> server

    return (output, output)

-- |Splits an event into a user defined number of events that will be
-- notified simultaneously when the original event is fired.
split :: Int -> Event a -> Event [Event a]
split 0 _ = return []
split count event = do
    (x, y) <- tee event
    xs <- split (count - 1) y
    return (x:xs)
