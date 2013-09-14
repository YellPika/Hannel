{-# LANGUAGE DoAndIfThenElse #-}

module Control.Concurrent.Hannel.Event (
    Event (), sync, swap, merge
) where

import Control.Applicative (Applicative, Alternative, empty, (<|>), pure, (<*>))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (MonadPlus, mzero, mplus, msum, ap, forM, forM_, filterM, void, when)
import Data.Array.IO (IOArray, newListArray, readArray, writeArray)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import System.Random (randomRIO)
import qualified Data.Map as Map

import Control.Concurrent.Hannel.Trail (Trail, TrailElement (..))
import qualified Control.Concurrent.Hannel.Trail as Trail
import qualified Control.Concurrent.Hannel.SyncLock as SyncLock

type EventHandler a = a -> Trail -> IO ()

-- |When synchronized upon, an event performs synchronous
-- operations with other threads before returning a value.
newtype Event a = Event [Trail -> EventHandler a -> IO ()]

runEvent :: Event a -> Trail -> EventHandler a -> IO ()
runEvent (Event events) trail handler = shuffle events >>= run trail
  where
    run _ [] = return ()
    run trail' (x:xs) = do
        void $ x (Trail.extend trail' ChooseLeft) handler
        run (Trail.extend trail' ChooseRight) xs

shuffle :: [a] -> IO [a]
shuffle xs = do
    let len = length xs
    ar <- newArray len xs
    forM [1 .. len] $ \i -> do
        j <- randomRIO (i, len)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj

newArray :: Int -> [a] -> IO (IOArray Int a)
newArray n =  newListArray (1,n)

-- Wraps an event invocation function. This prevents an event from
-- running if any of its dependencies have already been synced.
create :: (Trail -> EventHandler a -> IO ()) -> Event a
create invoke = Event [invoke']
  where
    invoke' trail handler = do
        ok <- Trail.isActive trail
        when ok $ invoke trail handler

-- |Blocks the current thread until the specified event yields a value.
sync :: Event a -> IO a
sync event = do
    trail <- Trail.create
    output <- newEmptyMVar

    runEvent event trail $ syncHandler $ putMVar output
    takeMVar output

-- Handler for the sync event.
syncHandler :: (a -> IO ()) -> a -> Trail -> IO ()
syncHandler action value trail = do
    completeValue <- Trail.complete trail $ action value
    commitSet <- Trail.commitSets completeValue
    case map Map.assocs commitSet of
        [] -> return ()
        xs:_ ->
            let locks = map fst xs
                actions = map (snd . snd) xs in
            void $ SyncLock.withAll locks $
                sequence_ actions

-- |Creates a pair of functions forming a swap channel. When supplied with a
-- value, each function returns an event that waits until a value is supplied
-- to the other function. This combinator provides a base for creating other
-- kinds of channels.
swap :: Event (a -> Event b, b -> Event a)
swap = create $ \trail handler -> do
    lock <- newEmptyMVar
    front <- newIORef []
    back <- newIORef []

    let withLock action = do
        putMVar lock ()
        output <- action
        takeMVar lock
        return output

    -- Registers value with the senders, cleans inactive trails from both
    -- queues, and then returns the remaining receivers.
    let clean senders receivers value = withLock $ do
        modifyIORef senders (++ [value])

        let clean' queue = do
            queue' <- readIORef queue
            queue'' <- filterM (\(trail', _, _) -> Trail.isActive trail') queue'
            writeIORef queue queue''
            return queue''

        void $ clean' senders
        clean' receivers

    let swap' senders receivers sendValue = create $ \sendTrail sendHandler -> do
        receivers' <- clean senders receivers (sendTrail, sendHandler, sendValue)
        forM_ receivers' $ \(receiveTrail, receiveHandler, receiveValue) ->
            when (Trail.coherent sendTrail receiveTrail) $ do
                sendRef <- newIORef []
                receiveRef <- newIORef[]

                let sendTrail' =
                        Trail.extend sendTrail $
                        Swap receiveTrail sendRef receiveRef
                    receiveTrail' =
                        Trail.extend receiveTrail $
                        Swap sendTrail receiveRef sendRef

                sendHandler receiveValue sendTrail'
                receiveHandler sendValue receiveTrail'

    handler (swap' front back, swap' back front) trail

-- |Merges a list of events. The resulting event will wait for all the source
-- events to synchronize before returning a value. Unlike
-- 'Control.Monad.sequence', the resulting event will accept the source events
-- in any order, preventing possible deadlocks.
merge :: [Event a] -> Event [a]
merge [] = return []
merge xs = do
    (l, c, r) <- msum $
                 map (\(l, c, r) -> fmap (\c' -> (l, c', r)) c) $
                 splits xs
    l' <- merge l
    r' <- merge r
    return $ l' ++ (c : r')

splits :: [a] -> [([a], a, [a])]
splits [] = []
splits (x:xs) = ([], x, xs) : map (\(l, c, r) -> (x:l, c, r)) (splits xs)

instance Monad Event where
    return x = create $ \trail handler ->
        handler x trail

    Event xs >>= f = Event $ map invoke xs
      where
        invoke x trail handler =
            x trail $ \x' trail' ->
                runEvent (f x') trail' handler

instance MonadPlus Event where
    mzero = Event []
    mplus (Event first) (Event second) = Event (first ++ second)

instance Applicative Event where
    pure = return
    (<*>) = ap

instance Alternative Event where
    empty = mzero
    (<|>) = mplus

instance Functor Event where
    fmap f (Event xs) = Event $ map invoke xs
      where
        invoke x trail handler = x trail (handler . f)
