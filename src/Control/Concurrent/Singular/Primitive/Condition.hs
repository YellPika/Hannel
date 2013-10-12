{-# LANGUAGE Safe #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}

module Control.Concurrent.Singular.Primitive.Condition (
    Condition (), newCondition, signal, wait
) where

import Control.Concurrent.Singular.Primitive.Event
import Control.Concurrent.Singular.Primitive.Status

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (yield)
import Control.Concurrent.MVar (MVar, newEmptyMVar, withMVar)
import Control.Monad (unless)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')

type Listener = (StatusRef, IO ())

data Condition = Condition !(MVar ())
                           !(IORef Bool)
                           !(IORef [Listener])

newCondition :: IO Condition
newCondition = Condition <$> newEmptyMVar
                         <*> newIORef False
                         <*> newIORef []

signal :: Condition -> IO ()
signal (Condition lock value listeners) = do
    signaled <- withMVar lock $ const $
        atomicModifyIORef' value (True,)

    unless signaled $ do
        listeners' <- atomicModifyIORef' listeners ([],)
        mapM_ sync' listeners'

wait :: Condition -> Event ()
wait (Condition lock value listeners) = newEvent poll commit block
  where
    poll = readIORef value
    commit = return $ Just ()
    block status handler = withMVar lock $ \_ -> do
        let listener = (status, handler ())

        signaled <- poll

        if signaled
        then sync' listener
        else atomicModifyIORef' listeners ((, ()) . (listener :))

sync' :: Listener -> IO ()
sync' listener@(status, handler) = do
    result <- casStatusRef status Waiting Synced
    case result of
        Waiting -> handler
        Claimed -> yield >> sync' listener
        Synced -> return ()
