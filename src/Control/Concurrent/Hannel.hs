module Control.Concurrent.Hannel (
    module Control.Concurrent.Hannel.Channel,
    module Control.Concurrent.Hannel.Event,
    module Control.Concurrent.Hannel.Process,
    module Control.Concurrent.Hannel.RoundChannel,
    module Control.Concurrent.Hannel.SwapChannel,
    createSwapChannel, createRoundChannel
) where

import Control.Concurrent.Hannel.Channel
import Control.Concurrent.Hannel.Event
import Control.Concurrent.Hannel.Process
import Control.Concurrent.Hannel.RoundChannel hiding (create)
import Control.Concurrent.Hannel.SwapChannel hiding (create)

import qualified Control.Concurrent.Hannel.RoundChannel as RoundChannel
import qualified Control.Concurrent.Hannel.SwapChannel as SwapChannel

createSwapChannel :: Event (SwapChannel f b)
createSwapChannel = SwapChannel.create

createRoundChannel :: Int -> Event (RoundChannel a)
createRoundChannel = RoundChannel.create
