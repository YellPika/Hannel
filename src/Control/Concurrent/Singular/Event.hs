{-# LANGUAGE Safe #-}

module Control.Concurrent.Singular.Event (
    Event (), fromPrimitive,
    always,
    guard, withNack, wrapAbort, wrap,
    sync
) where

import qualified Control.Concurrent.Singular.Primitive.Condition as Primitive
import qualified Control.Concurrent.Singular.Primitive.Event as Primitive

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
wrapAbort action source = Event $ do
    (nacks, event) <- runEvent source
    return (action:nacks, event)

wrap :: (a -> IO b) -> Event a -> Event b
wrap f x = Event $ do
    (nacks, event) <- runEvent x
    return (nacks, fmap (\(nacks', g) -> (nacks', g >>= f)) event)

instance Functor Event where
    fmap f = wrap (return . f)

instance Monoid (Event a) where
    mempty = Event { runEvent = return ([], mempty) }
    mappend first second = Event $ do
        (conds1, event1) <- runEvent first
        (conds2, event2) <- runEvent second
        let conds = conds1 ++ conds2
        let choice1 = fmap (\(conds', action) -> (conds' ++ conds2, action)) event1
        let choice2 = fmap (\(conds', action) -> (conds' ++ conds1, action)) event2
        let event = mappend choice1 choice2
        return (conds, event)

sync :: Event a -> IO a
sync event = do
    (_, base) <- runEvent event
    (nacks, action) <- Primitive.sync base
    sequence_ nacks
    action
