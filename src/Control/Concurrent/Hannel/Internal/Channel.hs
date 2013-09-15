module Control.Concurrent.Hannel.Internal.Channel (
    Channel (), create, other, swap
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (filterM, forM_, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)

import Control.Concurrent.Hannel.Internal.Event (Event, EventHandler)
import Control.Concurrent.Hannel.Internal.Trail (Trail, TrailElement (Swap))
import qualified Control.Concurrent.Hannel.Internal.Event as Event
import qualified Control.Concurrent.Hannel.Internal.Trail as Trail

type SwapData i o = (Trail, EventHandler o, i)

data Channel f b = Channel {
    lock :: MVar (),
    front :: IORef [SwapData f b],
    back :: IORef [SwapData b f]
}

create :: Event (Channel i o)
create = Event.create $ \trail handler -> do
    lock' <- newMVar ()
    front' <- newIORef []
    back' <- newIORef []

    let output = Channel { lock = lock', front = front', back = back' }
    handler output trail

other :: Channel f b -> Channel b f
other channel = channel { front = back channel, back = front channel }

swap :: Channel f b -> f -> Event b
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
