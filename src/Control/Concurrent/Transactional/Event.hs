{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE Safe #-}

module Control.Concurrent.Transactional.Event (
    Event,
    module Control.Concurrent.Transactional.Event.Class,

    -- * Synchronization
    sync, syncID,

    -- * Swapping
    swap,

    -- * Merging and Splitting
    merge, tee, split,

    -- * Threading
    forkEvent, forkEventCancel, forkEventHandle, forkServer,

    -- * Time
    module Control.Concurrent.Transactional.Event.Time,

    -- * Unsafe
    unsafeLiftIO,
) where

import Control.Concurrent.Transactional.Event.Base
import Control.Concurrent.Transactional.Event.Class
import Control.Concurrent.Transactional.Event.Time
import Control.Concurrent.Transactional.EventHandle
import Data.List.Util

import Control.Applicative ((<|>))
import Control.Monad (join, msum)
import Data.Unique (Unique)

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
    (waitFront, waitBack) <- swap
    output <- forkEvent $ event $ waitFront ()
    return (output, waitBack ())

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

-- |Splits an event into two events that will be notified
-- simultaneously when the original event is fired.
tee :: Event a -> Event (Event a, Event a)
tee event = do
    (send, receive) <- swap

    let client = receive ()
        server = do
            x <- event
            send x
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
