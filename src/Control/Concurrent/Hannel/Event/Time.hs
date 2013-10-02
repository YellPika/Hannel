{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE Trustworthy #-}

module Control.Concurrent.Hannel.Event.Time (
    timeout, timeoutAt, delayFor, delayUntil
) where

import safe Control.Concurrent (yield)
import safe Control.Applicative ((<|>))
import safe Control.Concurrent.Hannel.Event.Base (Event, unsafeLiftIO)
import safe Control.Monad (when)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)

-- |Defines an event that times out after a certain interval of time.
timeout :: NominalDiffTime -> Event a -> Event (Maybe a)
timeout interval event = fmap Just event <|> delayFor interval Nothing

-- |Defines an event that times out at a specific time.
timeoutAt :: UTCTime -> Event a -> Event (Maybe a)
timeoutAt time event = fmap Just event <|> delayUntil time Nothing

-- |Behaves like return, but waits for the specified interval of time
-- after synchronization begins before the event becomes available.
delayFor :: NominalDiffTime -> a -> Event a
delayFor interval value = do
    start <- unsafeLiftIO getCurrentTime
    delayUntil (addUTCTime interval start) value

-- |Behaves like return, but waits the specified
-- point in time before the event becomes available.
delayUntil :: UTCTime -> a -> Event a
delayUntil time value = do
    unsafeLiftIO wait
    return value
  where
    -- TODO: Use threadDelay
    wait = do
        current <- getCurrentTime
        when (current <= time) $ do
            yield
            wait
