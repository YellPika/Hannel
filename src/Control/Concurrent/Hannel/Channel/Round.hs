{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module Control.Concurrent.Hannel.Channel.Round (
    RoundChannel (), newRoundChannel, swapRound, signalRound
) where

import Control.Applicative ((<$), (<$>), (<|>))
import Control.Arrow (second)
import Control.Concurrent.Hannel.Channel.Swap (SwapChannel, newSwapChannel, sendFront, sendBack, receiveFront, receiveBack)
import Control.Concurrent.Hannel.Event.Base (Event)
import Control.Monad (replicateM, forM, guard)
import Data.Unique (Unique)

import qualified Control.Concurrent.Hannel.Event as Event

-- |A round channel is designed for performing an n-way swap.
data RoundChannel a = RoundChannel Int (SwapChannel () (a, Unique, SwapChannel [a] ()))

-- |Creates a new round channel that waits on the given number of participants.
newRoundChannel :: Int -> Event (RoundChannel a)
newRoundChannel count = RoundChannel count <$> newSwapChannel

-- |Waits for all participants in a round channel.
signalRound :: RoundChannel () -> Event ()
signalRound channel = () <$ swapRound channel ()

-- |Sends a value to every other participant on the channel, and
-- receives a list of all values sent by the other participants.
swapRound :: RoundChannel a -> a -> Event [a]
swapRound channel value = client' <|> server'
  where
    client' = client channel value
    server' = server channel value

client :: RoundChannel a -> a -> Event [a]
client (RoundChannel _ channel) value = do
    response <- newSwapChannel
    syncID <- Event.syncID
    sendBack channel (value, syncID, response)
    receiveBack response

server :: RoundChannel a -> a -> Event [a]
server (RoundChannel count channel) value = do
    syncID <- Event.syncID

    clients <- replicateM (count - 1) $ do
        (value', syncID', response) <- receiveFront channel
        guard (syncID < syncID') -- Smallest syncID becomes the server.
        return (value', response)

    forM (deletions clients) $ \((value', response), remaining) -> do
        let values = map fst remaining
        sendFront response (value:values)
        return value'

deletions :: [a] -> [(a, [a])]
deletions [] = []
deletions (x:xs) = (x, xs) : map (second (x :)) (deletions xs)
