module Control.Concurrent.Hannel.Event (
    Event (), sync, merge, tee, split, wrap, post, threadID
) where

import Control.Applicative ((<|>))
import Control.Monad (msum)
import Control.Concurrent.Hannel.Internal.Channel (signal, swap)
import Control.Concurrent.Hannel.Internal.Event (Event (), sync, wrap, post, threadID)
import Control.Concurrent.Hannel.Internal.SwapChannel (create, other)

-- |Merges a list of events. The resulting event will wait for all the source
-- events to synchronize before returning a value. Unlike
-- 'Control.Monad.sequence', the resulting event will accept the source events
-- in any order, preventing possible deadlocks.
merge :: [Event a] -> Event [a]
merge [] = return []
merge xs = do
    (l, c, r) <- msum $
                 map (\(l, c, r) -> fmap (\c' -> (l, c', r)) c) $
                 splits xs
    l' <- merge l
    r' <- merge r
    return $ l' ++ (c : r')

splits :: [a] -> [([a], a, [a])]
splits [] = []
splits (x:xs) = ([], x, xs) : map (\(l, c, r) -> (x:l, c, r)) (splits xs)

-- |Splits an event into two events that will be notified
-- simultaneously when the original event is fired.
tee :: Event a -> Event (Event a, Event a)
tee event = do
    channel <- create

    let client = signal $ other channel
        server = do
            x <- event
            swap channel x
            return x
        output = client <|> server
    
    return (output, output)

-- |Splits an event into a user defined number of events that will be
-- notified simultaneously when the original event is fired.
split :: Int -> Event a -> Event [Event a]
split 0 _ = return []
split count event = do
    (x, y) <- tee event
    xs <- split (count - 1) y
    return (x:xs)
