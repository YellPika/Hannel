{-# LANGUAGE Safe #-}

module Control.Concurrent.Singular.Event (
    Event (), fromPrimitive,
    always,
    guard, withNack, wrapAbort, wrap,
    sync
) where

import qualified Control.Concurrent.Singular.Primitive.Condition as Primitive
import qualified Control.Concurrent.Singular.Primitive.Event as Primitive

import Control.Arrow (first, second)
import Data.Monoid (Monoid, mempty, mappend)

type PEvent = Primitive.Event

newtype Event a = Event {
    runEvent :: IO ([IO ()], PEvent ([IO ()], IO a))
}

fromPrimitive :: PEvent a -> Event a
fromPrimitive event = Event {
    runEvent = return ([], fmap (\x -> ([], return x)) event)
}

always :: a -> Event a
always = fromPrimitive . Primitive.always

guard :: IO (Event a) -> Event a
guard event = Event (event >>= runEvent)

withNack :: (Event () -> Event a) -> Event a
withNack selector = guard $ do
    cond <- Primitive.newCondition

    let action = Primitive.signal cond ()
        event = selector $ fromPrimitive $ Primitive.wait cond

    return $ wrapAbort action event

wrapAbort :: IO () -> Event a -> Event a
wrapAbort action = Event . fmap (first (action :)) . runEvent

wrap :: (a -> IO b) -> Event a -> Event b
wrap f = Event . fmap (second (fmap (second (>>= f)))) . runEvent

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

sync :: Event a -> IO a
sync event = do
    (_, base) <- runEvent event
    (nacks, action) <- Primitive.sync base
    sequence_ nacks
    action
