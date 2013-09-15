module Control.Concurrent.Hannel.SwapChannel (
    SwapChannel, create, other, swapOther
) where

import Control.Concurrent.Hannel.Channel (swap)
import Control.Concurrent.Hannel.Event (Event)
import Control.Concurrent.Hannel.Internal.SwapChannel (SwapChannel, create, other)

-- |Swaps a value with the front side of a channel.
swapOther :: SwapChannel f b -> b -> Event f
swapOther = swap . other
