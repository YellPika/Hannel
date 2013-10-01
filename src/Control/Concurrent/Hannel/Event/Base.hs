{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE Safe #-}

module Control.Concurrent.Hannel.Event.Base (
    Event (), EventHandler, runEvent, newEvent, unsafeLiftIO
) where

import Control.Applicative (Applicative, Alternative, empty, (<|>), pure, (<*>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Hannel.Event.Trail (Trail, TrailElement (..), isActive, extend)
import Control.Monad (MonadPlus, mzero, mplus, ap, forM, foldM_, void, when)
import Data.Array.IO.Safe (IOArray, newListArray, readArray, writeArray)
import System.Random (randomRIO)

type EventHandler a = a -> Trail -> IO ()

-- |When synchronized upon, an event performs synchronous
-- operations with other threads before returning a value.
newtype Event a = Event [Trail -> EventHandler a -> IO ()]

-- Wraps an event invocation function. This prevents an event from
-- running if any of its dependencies have already been synced.
newEvent :: (Trail -> EventHandler a -> IO ()) -> Event a
newEvent invoke = Event [invoke']
  where
    invoke' trail handler = do
        ok <- isActive trail
        when ok $ invoke trail handler

runEvent :: Event a -> Trail -> EventHandler a -> IO ()
runEvent (Event events) trail handler =
    shuffle events >>=
    foldM_ (\i x -> do
        void $ forkIO $ x (extend trail $ Choose i) handler
        return $ i + 1) 0

shuffle :: [a] -> IO [a]
shuffle xs = do
    let n = length xs
    ar <- newArray n xs
    forM [1 .. n] $ \i -> do
        j <- randomRIO (i, n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj

newArray :: Int -> [a] -> IO (IOArray Int a)
newArray n = newListArray (1, n)

unsafeLiftIO :: IO a -> Event a
unsafeLiftIO action = newEvent $ \trail handler -> do
    value <- action
    handler value trail

instance Monad Event where
    return x = newEvent $ \trail handler ->
        handler x trail

    Event xs >>= f = Event $ map bind xs
      where
        bind invoke trail handler =
            invoke trail $ \x trail' ->
                runEvent (f x) trail' handler

instance MonadPlus Event where
    mzero = Event []
    mplus (Event first) (Event second) = Event (first ++ second)

instance Functor Event where
    fmap f (Event xs) = Event $ map fmap' xs
      where
        fmap' invoke trail handler = invoke trail (handler . f)

instance Applicative Event where
    pure = return
    (<*>) = ap

instance Alternative Event where
    empty = mzero
    (<|>) = mplus
