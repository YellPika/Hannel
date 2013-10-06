{-# LANGUAGE Safe #-}

module Control.Concurrent.Hannel.Channel.Swap (
    SwapChannel (), newSwapChannel,
    swapFront, swapBack,
    signalFront, signalBack,
    sendFront, sendBack,
    receiveFront, receiveBack
) where

import Control.Concurrent.Hannel.Event.Base

-- |A one-to-one synchronous swap channel.
data SwapChannel f b = SwapChannel (f -> Event b) (b -> Event f)

-- |Creates a new synchronous channel.
newSwapChannel :: Event (SwapChannel i o)
newSwapChannel = fmap (uncurry SwapChannel) swap

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
swapBack (SwapChannel _ back) = back

-- |Sends a value to and receives a value from the back side of a channel.
swapFront :: SwapChannel f b -> f -> Event b
swapFront (SwapChannel front _) = front
