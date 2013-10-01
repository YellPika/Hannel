{-# LANGUAGE Safe #-}

module Control.Concurrent.Hannel.Event.Sync (
    sync, syncHandler, syncID
) where

import Control.Concurrent.Hannel.Event.Base (Event, newEvent, runEvent)
import Control.Concurrent.Hannel.Event.SyncLock (withAll)
import Control.Concurrent.Hannel.Event.Trail (Trail, commitSets, complete, newTrail)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Unique (Unique)

import qualified Data.Map as Map
import qualified Control.Concurrent.Hannel.Event.Trail as Trail

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
        xs:_ ->
            -- Each assoc will be in the form (lock, (trail, action)).
            let assocs = Map.assocs xs
                locks = map fst assocs
                actions = map (snd . snd) assocs in

            void $ withAll locks $ sequence_ actions

-- |An event that returns a unique identifier for the initial sync operation.
syncID :: Event Unique
syncID = newEvent $ \trail handler ->
    handler (Trail.syncID trail) trail
