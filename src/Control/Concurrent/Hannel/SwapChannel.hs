module Control.Concurrent.Hannel.SwapChannel (
    SwapChannel, create, other, swap, swapOther
) where

import Control.Concurrent.Hannel.Event (Event)
import Control.Concurrent.Hannel.Internal.SwapChannel (SwapChannel, create, other, swap)

-- |Swaps a value with the front side of a channel.
swapOther :: SwapChannel f b -> b -> Event f
swapOther = swap . other
