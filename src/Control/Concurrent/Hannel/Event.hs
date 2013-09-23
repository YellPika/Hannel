{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE Trustworthy #-}

module Control.Concurrent.Hannel.Event (
    Event, sync, merge, syncID, tee, split,
    forkEvent, delayUntil, delayFor, timeoutAt, timeout,
    module Control.Concurrent.Hannel.Event.Class
) where

import safe Control.Concurrent (forkIO, yield)
import safe Control.Concurrent.Hannel.Channel.Swap (newSwapChannel, swap, signal, signalOther)
import safe Control.Concurrent.Hannel.Event.Base (Event, runEvent, newEvent)
import safe Control.Concurrent.Hannel.Event.Class
import safe Control.Concurrent.Hannel.Event.SyncLock (withAll)
import safe Control.Concurrent.Hannel.Event.Trail (Trail, newTrail, complete, commitSets)
import safe Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import safe Control.Monad (mplus, msum, void)
import safe Control.Monad.Trans (MonadIO, liftIO)
import Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime, addUTCTime)
import safe Data.Unique (Unique)

import safe qualified Control.Concurrent.Hannel.Event.Trail as Trail
import safe qualified Data.Map as Map

-- Blocks the current thread until the specified event yields a value.
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
        xs:_ ->
            -- Each assoc will be in the form (lock, (trail, action)).
            let assocs = Map.assocs xs
                locks = map fst assocs
                actions = map (snd . snd) assocs in

            void $ withAll locks $ sequence_ actions

-- |Concurrently evaluates an event. The event will only synchronize when the
-- returned event is synchronized. After synchronization, a new thread will be
-- spawned for the specified action.
forkEvent :: Event a -> (a -> IO ()) -> Event Unique
forkEvent event action = do
    channel <- newSwapChannel
    output <- unsafeLiftIO $ do
        emptyTrail <- newTrail
        let event' = signalOther channel >> event
        runEvent event' emptyTrail $ syncHandler (void . forkIO . action)
        return $ Trail.syncID emptyTrail
    signal channel
    return output

-- |An event that returns a unique identifier for the initial sync operation.
syncID :: Event Unique
syncID = newEvent $ \trail handler ->
    handler (Trail.syncID trail) trail

-- |Behaves like return, but waits the specified
-- time before the event becomes available.
delayUntil :: UTCTime -> a -> Event a
delayUntil time value = newEvent $ \trail handler ->
    void $ forkIO $ do
        let wait = do
            current <- getCurrentTime
            if current <= time
            then yield >> wait
            else handler value trail

        wait

-- |Behaves like return, but waits for the specified interval of time
-- after synchronization begins before the event becomes available.
delayFor :: NominalDiffTime -> a -> Event a
delayFor interval value = do
    start <- unsafeLiftIO getCurrentTime
    delayUntil (addUTCTime interval start) value

-- |Defines an event that times out at a specific time.
timeoutAt :: UTCTime -> Event a -> Event (Maybe a)
timeoutAt time = mplus (delayUntil time Nothing) . fmap Just

-- |Defines an event that times out after a certain interval of time.
timeout :: NominalDiffTime -> Event a -> Event (Maybe a)
timeout interval = mplus (delayFor interval Nothing) . fmap Just

unsafeLiftIO :: IO a -> Event a
unsafeLiftIO action = newEvent $ \trail handler -> do
    value <- action
    handler value trail

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

    let client = signalOther channel
        server = do
            x <- event
            swap channel x
            return x
        output = client `mplus` server

    return (output, output)

-- |Splits an event into a user defined number of events that will be
-- notified simultaneously when the original event is fired.
split :: Int -> Event a -> Event [Event a]
split 0 _ = return []
split count event = do
    (x, y) <- tee event
    xs <- split (count - 1) y
    return (x:xs)
