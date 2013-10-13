{-# LANGUAGE Safe #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Control.Concurrent.Singular.Primitive.Event (
    Event (), newEvent, always, sync
) where

import Control.Concurrent.Singular.Primitive.Status
import Data.List.Util

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Monoid (Monoid, mempty, mappend)

data BaseEvent a = BaseEvent {
    poll :: !(IO Bool),
    commit :: !(MaybeT IO a),
    block :: StatusRef -> (a -> IO ()) -> IO ()
}

instance Functor BaseEvent where
    fmap f x = x {
        commit = fmap f $ commit x,
        block = \status handler -> block x status (handler . f)
    }

newtype Event a = Event { unEvent :: [BaseEvent a] }

newEvent :: IO Bool -> MaybeT IO a -> (StatusRef -> (a -> IO ()) -> IO ()) -> Event a
newEvent poll' commit' block' = Event [BaseEvent poll' commit' block']

always :: a -> Event a
always x = newEvent (return True) (return x) (\ _ _ -> return ())

instance Functor Event where
    fmap f = Event . map (fmap f) . unEvent

instance Monoid (Event a) where
    mempty = Event []
    mappend (Event x) (Event y) = Event (x ++ y)

sync :: Event a -> IO a
sync (Event bases) = shuffle bases >>= poll'
  where
    poll' [] = block'
    poll' (x:xs) = do
        let continue = poll' xs

        result <- poll x
        if result
        then runMaybeT (commit x) >>= maybe continue return
        else continue
    block' = do
        status <- newStatusRef
        output <- newEmptyMVar

        let block'' [] = takeMVar output
            block'' (x:xs) = do
                block x status (putMVar output)

                status' <- readStatusRef status
                block'' (if status' == Synced then [] else xs)

        shuffle bases >>= block''
