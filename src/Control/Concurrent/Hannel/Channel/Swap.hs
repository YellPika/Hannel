{-# LANGUAGE Safe #-}

module Control.Concurrent.Hannel.Channel.Swap (
    SwapChannel (), newSwapChannel,
    swapFront, swapBack,
    signalFront, signalBack,
    sendFront, sendBack,
    receiveFront, receiveBack
) where

import Control.Concurrent.Hannel.Event.Base (Event, EventHandler, newEvent)
import Control.Concurrent.Hannel.Event.Trail (Trail, TrailElement (Swap), extend, isActive, isCoherent)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (filterM, forM_, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)

type SwapData i o = (Trail, EventHandler o, i)

-- |A one-to-one synchronous swap channel.
data SwapChannel f b = SwapChannel {
    lock :: MVar (),
    front :: IORef [SwapData f b],
    back :: IORef [SwapData b f]
}

-- |Creates a new synchronous channel.
newSwapChannel :: Event (SwapChannel i o)
newSwapChannel = newEvent $ \trail handler -> do
    lock' <- newMVar ()
    front' <- newIORef []
    back' <- newIORef []

    let output = SwapChannel { lock = lock', front = front', back = back' }
    handler output trail

-- |Sends a value to the front side of a channel.
sendBack :: SwapChannel () b -> b -> Event ()
sendBack = swapBack

-- |Sends a value to the back side of a channel.
sendFront :: SwapChannel f () -> f -> Event ()
sendFront = swapFront

-- |Receives a value from the back side of a channel.
receiveBack :: SwapChannel f () -> Event f
receiveBack = flip swapBack ()

-- |Receives a value from the front side of a channel.
receiveFront :: SwapChannel () b -> Event b
receiveFront = flip swapFront ()

-- |Waits for the front side of a channel.
signalBack :: SwapChannel () () -> Event ()
signalBack = flip swapBack ()

-- |Waits for the back side of a channel.
signalFront :: SwapChannel () () -> Event ()
signalFront = flip swapFront ()

-- |Sends a value to and receives a value from the front side of a channel.
swapBack :: SwapChannel f b -> b -> Event f
swapBack channel = swapFront channel {
    front = back channel,
    back = front channel
}

-- |Sends a value to and receives a value from the back side of a channel.
swapFront :: SwapChannel f b -> f -> Event b
swapFront channel value = newEvent $ \trail handler -> do
    remaining <- withMVar (lock channel) $ \_ -> do
        -- Enqueue the swap data on the front queue for future swappers.
        modifyIORef (front channel) (++ [(trail, handler, value)])

        -- Cleaning helps prevent space leaks.
        void $ clean $ front channel
        clean $ back channel


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
