module Control.Concurrent.Event (
    Event, sync, merge, threadID, tee, split,
    module Control.Concurrent.Event.Class
) where

import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId)
import Control.Concurrent.Channel.Swap (newSwapChannel, swap, signalOther)
import Control.Concurrent.Event.Base (Event, runEvent, newEvent)
import Control.Concurrent.Event.Class
import Control.Concurrent.Event.SyncLock (withAll)
import Control.Concurrent.Event.Trail (newTrail, complete, commitSets)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (msum, void)
import Control.Monad.Trans (MonadIO, liftIO)

import qualified Control.Concurrent.Event.Trail as Trail
import qualified Data.Map as Map

-- Blocks the current thread until the specified event yields a value.
sync :: MonadIO m => Event a -> m a
sync event = liftIO $ do
    emptyTrail <- newTrail
    output <- newEmptyMVar

    runEvent event emptyTrail $ \value trail -> do
        completeValue <- complete trail $ putMVar output value
        commitSet <- commitSets completeValue

        case commitSet of
            [] -> return ()
            xs:_ ->
                -- Each assoc will be in the form (lock, (trail, action)).
                let assocs = Map.assocs xs
                    locks = map fst assocs
                    actions = map (snd . snd) assocs in

                void $ withAll locks $ sequence_ actions

    takeMVar output

-- |An event that returns the thread ID of the synchronizing thread.
threadID :: Event ThreadId
threadID = newEvent $ \trail handler ->
    handler (Trail.threadID trail) trail

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
