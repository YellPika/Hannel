module Control.Concurrent.Hannel.Event.Handle (
    EventHandle (), newEventHandle, touchEventHandle
) where

import Control.Concurrent.Hannel.Channel.Swap (newSwapChannel, signalFront, signalBack)
import Control.Concurrent.Hannel.Event.Base (Event, unsafeLiftIO)
import Control.Concurrent.Hannel.Event.Sync (sync)
import Control.Monad (void)
import Data.IORef (IORef, newIORef, mkWeakIORef)

-- |Defines a reference to a cancellation event.
data EventHandle = EventHandle (IORef ())

-- |Creates a new event handle, and returns it along with an event that becomes
-- available when the event handle is garbage collected.
newEventHandle :: Event (EventHandle, Event ())
newEventHandle = do
    channel <- newSwapChannel
    unsafeLiftIO $ do
        ref <- newIORef ()
        void $ mkWeakIORef ref $ sync $ signalFront channel
        return (EventHandle ref, signalBack channel)

-- |Prevents an event handle from going out of scope.
touchEventHandle :: EventHandle -> Event a -> Event a
touchEventHandle (EventHandle ref) = fmap $ seq ref
