{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}

module Control.Concurrent.Channel.Class (
    Channel (swap), signal
) where

import Control.Concurrent.Event.Base (Event)

-- |Provides an interface for synchronously sending and receiving values.
class Channel c i o | c -> i o where
    -- |Sends a value through a channel, and receives a value.
    swap :: c -> i -> Event o

-- |Receives a value from a channel.
signal :: Channel c () o => c -> Event o
signal channel = swap channel ()
