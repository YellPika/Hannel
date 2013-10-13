{-# LANGUAGE Safe #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}

module Control.Concurrent.Singular.Primitive.Condition (
    Condition (), newCondition, signal, wait
) where

import Control.Concurrent.Singular.Primitive.Event
import Control.Concurrent.Singular.Primitive.Status

import Control.Applicative ((<$>), (<$))
import Control.Arrow (first)
import Control.Concurrent (yield)
import Control.Monad.Fix (mfix)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Maybe (fromMaybe, isJust)

type Listener a = (StatusRef, a -> IO ())

newtype Condition a = Condition (IORef (Maybe a, [Listener a]))

newCondition :: IO (Condition a)
newCondition = Condition <$> newIORef (Nothing, [])

signal :: Condition a -> a -> IO ()
signal (Condition state) value =
    atomicModifyIORef' state (first modify) >>=
    mapM_ (`send` value)
  where
    modify = (, []) . Just . fromMaybe value

wait :: Condition a -> Event a
wait (Condition state) = newEvent poll commit block
  where
    poll = isJust <$> commit
    commit = fst <$> readIORef state
    block status handler =
        atomicModifyIORef' state inspect >>=
        maybe (return ()) (send listener)
      where
        listener = (status, handler)
        inspect (x, ls) = ((x, maybe (listener:ls) (const ls) x), x)

send :: Listener a -> a -> IO ()
send (status, handler) value = mfix $ \output -> do
    result <- casStatusRef status Waiting Synced
    case result of
        Waiting -> handler value
        Claimed -> output <$ yield
        Synced -> return ()
