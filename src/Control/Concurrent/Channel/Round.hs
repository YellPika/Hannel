{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module Control.Concurrent.Channel.Round (
    RoundChannel (), newRoundChannel, swap, signal
) where

import Control.Applicative ((<$>), (<|>))
import Control.Concurrent.Channel.Class (Channel, swap, signal)
import Control.Concurrent.Channel.Swap (SwapChannel, newSwapChannel, other)
import Control.Concurrent.Event.Base (Event)
import Control.Monad (replicateM, forM, guard)
import Data.Unique (Unique)

import qualified Control.Concurrent.Event as Event

data RoundChannel a = RoundChannel Int (SwapChannel () (a, Unique, SwapChannel [a] ()))

newRoundChannel :: Int -> Event (RoundChannel a)
newRoundChannel count = RoundChannel count <$> newSwapChannel

instance Channel (RoundChannel a) a [a] where
    swap channel value = client' <|> server'
      where
        client' = client channel value
        server' = server channel value

client :: RoundChannel a -> a -> Event [a]
client (RoundChannel _ channel) value = do
    response <- newSwapChannel
    syncID <- Event.syncID
    swap (other channel) (value, syncID, response)
    swap (other response) ()

server :: RoundChannel a -> a -> Event [a]
server (RoundChannel count channel) value = do
    syncID <- Event.syncID

    clients <- replicateM (count - 1) $ do
        (value', syncID', response) <- swap channel ()
        guard (syncID < syncID')
        return (value', response)

    forM (deletions clients) $ \((value', response), remaining) -> do
        let values = map fst remaining
        swap response (value:values)
        return value'

deletions :: [a] -> [(a, [a])]
deletions [] = []
deletions (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (deletions xs)
