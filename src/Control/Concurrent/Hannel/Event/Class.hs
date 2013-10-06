{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Safe #-}

module Control.Concurrent.Hannel.Event.Class (
    MonadEvent (liftEvent)
) where

import Control.Concurrent.Hannel.Event.Base

import Control.Monad (MonadPlus)
import Control.Monad.Trans (MonadTrans, lift)

-- |Describes a computation based on an event.
class MonadPlus m => MonadEvent m where
    -- |Lifts an event into a computation.
    liftEvent :: Event a -> m a

instance MonadEvent Event where
    liftEvent = id

instance (MonadTrans t, MonadEvent m, MonadPlus (t m)) => MonadEvent (t m) where
    liftEvent = lift . liftEvent
