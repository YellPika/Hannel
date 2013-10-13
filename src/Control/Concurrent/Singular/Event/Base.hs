{-# LANGUAGE Safe #-}

module Control.Concurrent.Singular.Event.Base (
    Event (), newEvent,
    always, guard, wrapAbort, wrap,
    sync
) where

import Control.Concurrent.Singular.Event.Status (StatusRef)
import qualified Control.Concurrent.Singular.Event.Primitive as Primitive

import Control.Arrow (first, second)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Monoid (Monoid, mempty, mappend)

-- |When synchronized upon, an event performs synchronous
-- operations with other threads before returning a value.
newtype Event a = Event {
    runEvent :: IO ([IO ()], Primitive.Event ([IO ()], IO a))
}

newEvent :: IO Bool -> IO (Maybe a) -> (StatusRef -> (a -> IO ()) -> IO ()) -> Event a
newEvent poll commit block = fromPrimitive $ Primitive.newEvent poll commit block

fromPrimitive :: Primitive.Event a -> Event a
fromPrimitive event = Event $ return ([], fmap (\x -> ([], return x)) event)

-- |An event that is always available, and returns the given value.
always :: a -> Event a
always = fromPrimitive . Primitive.always

-- |Specifies a pre-synchronization action.
guard :: IO (Event a) -> Event a
guard event = Event (event >>= runEvent)

-- |Specifies a post-synchronization action.
wrap :: (a -> IO b) -> Event a -> Event b
wrap f = Event . fmap (second (fmap (second (>>= f)))) . runEvent

-- |Specifies an action to perform when an event is not chosen.
wrapAbort :: IO () -> Event a -> Event a
wrapAbort action = Event . fmap (first (action :)) . runEvent

instance Functor Event where
    fmap f = wrap (return . f)

instance Monoid (Event a) where
    mempty = Event $ return ([], mempty)
    mappend x y = Event $ do
        (conds1, event1) <- runEvent x
        (conds2, event2) <- runEvent y
        let conds = conds1 ++ conds2
            choice1 = fmap (first (++ conds2)) event1
            choice2 = fmap (first (++ conds1)) event2
            event = mappend choice1 choice2
        return (conds, event)

-- |Blocks the current thread until the specified event yields a value.
sync :: MonadIO m => Event a -> m a
sync event = liftIO $ do
    (_, base) <- runEvent event
    (nacks, action) <- Primitive.sync base
    sequence_ nacks
    action
