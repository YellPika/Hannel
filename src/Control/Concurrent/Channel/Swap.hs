{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module Control.Concurrent.Channel.Swap (
    SwapChannel (), newSwapChannel, other,
    swap, signal, swapOther, signalOther
) where

import Control.Concurrent.Channel.Class (Channel, swap, signal)
import Control.Concurrent.Event.Base (Event, EventHandler, newEvent)
import Control.Concurrent.Event.Trail (Trail, TrailElement (Swap), extend, isActive, isCoherent)
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

-- |Gets the other side of a channel.
other :: SwapChannel f b -> SwapChannel b f
other channel = channel { front = back channel, back = front channel }

-- |Swaps a value with the other side of a channel.
instance Channel (SwapChannel f b) f b where
    swap channel value = newEvent $ \trail handler -> do
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

-- |Swaps a value with this side of a channel.
swapOther :: SwapChannel f b -> b -> Event f
swapOther = swap . other

-- |Receives a value from this side of a channel.
signalOther :: SwapChannel f () -> Event f
signalOther = signal . other
