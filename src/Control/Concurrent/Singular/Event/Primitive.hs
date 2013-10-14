{-# LANGUAGE Safe #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Control.Concurrent.Singular.Event.Primitive (
    Event (), newEvent, always, sync
) where

import Control.Concurrent.Singular.Event.Status
import Data.List.Util

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (unless)
import Data.Monoid (Monoid, mempty, mappend)

data BaseEvent a = BaseEvent {
    commit :: !(IO (Maybe a)),
    block :: StatusRef -> (a -> IO ()) -> IO ()
}

instance Functor BaseEvent where
    fmap f (BaseEvent commit' block') = BaseEvent {
        commit = fmap (fmap f) commit',
        block = \status handler -> block' status (handler . f)
    }

newtype Event a = Event { unEvent :: [BaseEvent a] }

newEvent :: IO (Maybe a) -> (StatusRef -> (a -> IO ()) -> IO ()) -> Event a
newEvent commit' block' = Event [BaseEvent commit' block']

always :: a -> Event a
always x = newEvent (return $ Just x) (\ _ _ -> return ())

instance Functor Event where
    fmap f = Event . map (fmap f) . unEvent

instance Monoid (Event a) where
    mempty = Event []
    mappend (Event x) (Event y) = Event (x ++ y)

sync :: Event a -> IO a
sync (Event bases) = shuffle bases >>= commit'
  where
    commit' [] = block'
    commit' (x:xs) = commit x >>= maybe (commit' xs) return
    block' = do
        status <- newStatusRef
        output <- newEmptyMVar

        let block'' [] = return ()
            block'' (x:xs) = do
                block x status $ putMVar output

                status' <- readStatusRef status
                unless (status' == Synced) $
                    block'' xs

        shuffle bases >>= block''
        takeMVar output
