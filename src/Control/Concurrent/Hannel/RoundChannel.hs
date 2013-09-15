{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Concurrent.Hannel.RoundChannel (
    RoundChannel (), create
) where

import Control.Concurrent (ThreadId)
import Control.Monad (replicateM, mplus, forM, guard)

import Control.Concurrent.Hannel.Channel (Channel, swap)
import Control.Concurrent.Hannel.Event (Event)
import Control.Concurrent.Hannel.SwapChannel (SwapChannel, other)
import qualified Control.Concurrent.Hannel.Event as Event
import qualified Control.Concurrent.Hannel.SwapChannel as SwapChannel

data RoundChannel a = RoundChannel Int (SwapChannel () (a, ThreadId, SwapChannel [a] ()))

create :: Int -> Event (RoundChannel a)
create count = fmap (RoundChannel count) SwapChannel.create

instance Channel (RoundChannel a) a [a] where
    swap (RoundChannel count channel) value = server `mplus` client
      where
        server = do
            threadID <- Event.threadID

            -- Receive count - 1 instances of (value, client).
            clients <- replicateM (count - 1) $ do
                (value', threadID', response) <- swap channel ()

                -- Let the smallest threadID be the server (faster).
                guard (threadID < threadID')

                return (value', response)

            -- Send all the other values to each client.
            forM (deletions clients) $ \((value', response), remaining) -> do
                let values = map fst remaining
                swap response (value:values)
                return value'

        client = do
            response <- SwapChannel.create
            threadID <- Event.threadID
            swap (other channel) (value, threadID, response)
            swap (other response) ()

deletions :: [a] -> [(a, [a])]
deletions [] = []
deletions (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (deletions xs)
