module Control.Concurrent.Hannel.SwapChannel (
    SwapChannel, create, other, swapOther, signalOther
) where

import Control.Concurrent.Hannel.Channel (swap, signal)
import Control.Concurrent.Hannel.Event (Event)
import Control.Concurrent.Hannel.Internal.SwapChannel (SwapChannel, create, other)

-- |Swaps a value with the front side of a channel.
swapOther :: SwapChannel f b -> b -> Event f
swapOther = swap . other

-- |Waits for a value on the back side of a channel.
signalOther :: SwapChannel f () -> Event f
signalOther = signal . other
