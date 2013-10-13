{-# LANGUAGE Safe #-}

module Control.Concurrent.Singular.Event (
    Event (),
    -- * Synchronization
    sync,

    -- * Constructors
    always,

    -- * Pre/Post Synchronization
    guard, wrap,

    -- * Failure
    withNack, wrapAbort
) where

import Control.Concurrent.Singular.Event.Base
import Control.Concurrent.Singular.IVar

withNack :: (Event () -> Event a) -> Event a
withNack selector = guard $ do
    var <- newIVar

    let action = putIVar var ()
        event = selector $ takeIVar var

    return $ wrapAbort action event
