module Control.Concurrent.Hannel.RoundChannel (
    RoundChannel (), create, swap
) where

import Control.Concurrent (ThreadId)
import Control.Monad (replicateM, mplus, forM, guard)

import Control.Concurrent.Hannel.Event (Event)
import Control.Concurrent.Hannel.SwapChannel (SwapChannel)
import qualified Control.Concurrent.Hannel.Event as Event
import qualified Control.Concurrent.Hannel.SwapChannel as SwapChannel

data RoundChannel a = RoundChannel Int (SwapChannel () (a, ThreadId, SwapChannel [a] ()))

create :: Int -> Event (RoundChannel a)
create count = fmap (RoundChannel count) SwapChannel.create

swap :: RoundChannel a -> a -> Event [a]
swap (RoundChannel count channel) value = server `mplus` client
  where
    server = do
        threadID <- Event.threadID

        -- Receive count - 1 instances of (value, client).
        clients <- replicateM (count - 1) $ do
            (value', threadID', response) <- SwapChannel.swap channel ()

            -- Let the smallest threadID be the server (faster).
            guard (threadID < threadID')

            return (value', response)


        -- Send all the other values to each client.
        forM (deletions clients) $ \((value', response), remaining) -> do
            let values = map fst remaining
            SwapChannel.swap response (value:values)
            return value'

    client = do
        response <- SwapChannel.create
        threadID <- Event.threadID
        SwapChannel.swap (SwapChannel.other channel) (value, threadID, response)
        SwapChannel.swapOther response ()

deletions :: [a] -> [(a, [a])]
deletions [] = []
deletions (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (deletions xs)
