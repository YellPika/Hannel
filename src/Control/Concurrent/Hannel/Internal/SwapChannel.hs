module Control.Concurrent.Hannel.Internal.SwapChannel (
    SwapChannel (), create, other, swap
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (filterM, forM_, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)

import Control.Concurrent.Hannel.Internal.Event (Event, EventHandler)
import Control.Concurrent.Hannel.Internal.Trail (Trail, TrailElement (Swap))
import qualified Control.Concurrent.Hannel.Internal.Event as Event
import qualified Control.Concurrent.Hannel.Internal.Trail as Trail

type SwapData i o = (Trail, EventHandler o, i)

-- |A one-to-one synchronous swap channel.
data SwapChannel f b = SwapChannel {
    lock :: MVar (),
    front :: IORef [SwapData f b],
    back :: IORef [SwapData b f]
}

-- |Creates a new synchronous channel.
create :: Event (SwapChannel i o)
create = Event.create $ \trail handler -> do
    lock' <- newMVar ()
    front' <- newIORef []
    back' <- newIORef []

    let output = SwapChannel { lock = lock', front = front', back = back' }
    handler output trail

-- |Gets the other side of a channel.
other :: SwapChannel f b -> SwapChannel b f
other channel = channel { front = back channel, back = front channel }

-- |Swaps a value with the other side of a channel.
swap :: SwapChannel f b -> f -> Event b
swap channel value = Event.create $ \trail handler -> do
    remaining <- withMVar (lock channel) $ \_ -> do
        -- Enqueue the swap data on the front queue for future swappers.
        modifyIORef (front channel) (++ [(trail, handler, value)])

        -- Cleaning helps prevent space leaks.
        void $ clean $ front channel
        clean $ back channel


    forM_ remaining $ \(trail', handler', value') ->
        -- Incoherent trails will never commit anyways.
        when (Trail.isCoherent trail trail') $ do
            ref1 <- newIORef []
            ref2 <- newIORef []

            handler value' $ Trail.extend trail $ Swap trail' ref1 ref2
            handler' value $ Trail.extend trail' $ Swap trail ref2 ref1

-- Removes inactive trails, and returns the remaining.
clean :: IORef [SwapData i o] -> IO [SwapData i o]
clean queue = do
    value <- readIORef queue
    remaining <- filterM (\(trail, _, _) -> Trail.isActive trail) value
    writeIORef queue remaining

    return remaining
