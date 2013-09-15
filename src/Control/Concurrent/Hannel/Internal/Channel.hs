{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Concurrent.Hannel.Internal.Channel (
    Channel, swap, signal
) where

import Control.Concurrent.Hannel.Internal.Event (Event)

-- |Provides an interface for synchronously sending and receiving values.
class Channel c i o | c -> i o where
    -- |Sends a value through a channel, and receives a value.
    swap :: c -> i -> Event o

-- |Receives a value from a channel.
signal :: Channel c () o => c -> Event o
signal channel = swap channel ()
