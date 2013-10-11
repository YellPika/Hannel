{-# LANGUAGE Safe #-}

module Control.Concurrent.Transactional.EventHandle (
    EventHandle (), newEventHandle, wrapEvent
) where

import Control.Concurrent.Transactional.Event.Base

import Control.Monad (void)
import Data.IORef (IORef, newIORef, mkWeakIORef)

-- |Event handles are values that fire an event when they are garbage collected.
data EventHandle = EventHandle { unEventHandle :: IORef () }

-- |Creates a new event handle, and returns it along with an event that becomes
-- available when the event handle is garbage collected.
newEventHandle :: Event (EventHandle, Event ())
newEventHandle = do
    (waitFront, waitBack) <- swap

    unsafeLiftIO $ do
        ref <- newIORef ()
        void $ mkWeakIORef ref $ sync $ waitFront ()
        return (EventHandle ref, waitBack ())

-- |Associates an event handle with an event. As long as the event remains
-- alive, so will the event handle.
wrapEvent :: EventHandle -> Event a -> Event a
wrapEvent = fmap . seq . unEventHandle
