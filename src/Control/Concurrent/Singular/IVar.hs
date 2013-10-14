{-# LANGUAGE Safe #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}

module Control.Concurrent.Singular.IVar (
    IVar (), newIVar, putIVar, takeIVar
) where

import Control.Concurrent.Singular.Event.Base
import Control.Concurrent.Singular.Event.Status

import Control.Applicative ((<$>), (<$))
import Control.Arrow (first)
import Control.Concurrent (yield)
import Control.Monad.Fix (mfix)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Maybe (fromMaybe)

type Listener a = (StatusRef, a -> IO ())

newtype IVar a = IVar (IORef (Maybe a, [Listener a]))

newIVar :: IO (IVar a)
newIVar = IVar <$> newIORef (Nothing, [])

putIVar :: IVar a -> a -> IO ()
putIVar (IVar state) value =
    atomicModifyIORef' state (first modify) >>=
    mapM_ (`send` value)
  where
    modify = (, []) . Just . fromMaybe value

takeIVar :: IVar a -> Event a
takeIVar (IVar state) = newEvent commit block
  where
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
