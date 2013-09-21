{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Concurrent.Channel.Round (
    RoundChannel (), create, swap, signal
) where

import Control.Applicative ((<$>), (<|>))
import Control.Concurrent (ThreadId)
import Control.Concurrent.Channel.Class (Channel, swap, signal)
import Control.Concurrent.Channel.Swap (SwapChannel, newSwapChannel, other)
import Control.Concurrent.Event.Base (Event)
import Control.Monad (replicateM, forM, guard)

import qualified Control.Concurrent.Event as Event

data RoundChannel a = RoundChannel Int (SwapChannel () (a, ThreadId, SwapChannel [a] ()))

create :: Int -> Event (RoundChannel a)
create count = RoundChannel count <$> newSwapChannel

instance Channel (RoundChannel a) a [a] where
    swap channel value = client' <|> server'
      where
        client' = client channel value
        server' = server channel value

client :: RoundChannel a -> a -> Event [a]
client (RoundChannel _ channel) value = do
    response <- newSwapChannel
    threadID <- Event.threadID
    swap (other channel) (value, threadID, response)
    swap (other response) ()

server :: RoundChannel a -> a -> Event [a]
server (RoundChannel count channel) value = do
    threadID <- Event.threadID

    clients <- replicateM (count - 1) $ do
        (value', threadID', response) <- swap channel ()
        guard (threadID < threadID')
        return (value', response)

    forM (deletions clients) $ \((value', response), remaining) -> do
        let values = map fst remaining
        swap response (value:values)
        return value'

deletions :: [a] -> [(a, [a])]
deletions [] = []
deletions (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (deletions xs)
