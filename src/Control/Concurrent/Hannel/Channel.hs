{-# LANGUAGE FunctionalDependencies #-}

module Control.Concurrent.Hannel.Channel (
    Channel, swap
) where

import Control.Concurrent.Hannel.Event (Event)

-- |Provides an interface for synchronously sending and receiving values.
class Channel c i o | c -> i o where
    -- |Sends a value through a channel, and receives a value.
    swap :: c -> i -> Event o
