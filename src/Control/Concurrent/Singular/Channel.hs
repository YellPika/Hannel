{-# LANGUAGE Safe #-}

module Control.Concurrent.Singular.Channel (
    Channel (), newChannel, swapFront, swapBack
) where

import Control.Concurrent.Singular.Event.Base
import Control.Concurrent.Singular.Event.Status

import Control.Applicative ((<$), (<$>), (<*>))
import Control.Concurrent (yield)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Sequence (Seq, (><), (<|), (|>), ViewL (..))
import qualified Data.Sequence as Seq

type QueueItem f b = (StatusRef, b -> IO (), f)
type Queue a = IORef (Seq a)

data Channel f b = Channel !(MVar ())
                           !(Queue (QueueItem f b))
                           !(Queue (QueueItem b f))

newChannel :: IO (Channel i o)
newChannel = Channel <$> newMVar ()
                     <*> newEmptyQueue
                     <*> newEmptyQueue

swapFront :: Channel i o -> i -> Event o
swapFront channel value = newEvent
    (poll channel)
    (commit channel value)
    (\status handler -> block channel (status, handler, value))

swapBack :: Channel i o -> o -> Event i
swapBack (Channel lock front back) = swapFront (Channel lock back front)

poll :: Channel i o -> IO Bool
poll (Channel _ _ back) = not <$> Seq.null <$> readIORef back

commit :: Channel i o -> i -> IO (Maybe o)
commit (Channel lock _ back) input =
    withMVar lock $ const commit'
  where
    commit' =
        dequeue back >>=
        maybe (return Nothing) sync'
    sync' item@(status, handler, output) = do
        result <- casStatusRef status Waiting Synced
        case result of
            Synced -> commit'
            Claimed -> yield >> sync' item
            Waiting -> Just output <$ handler input

block :: Channel i o -> QueueItem i o -> IO ()
block (Channel lock front back) item@(status, handler, value) =
    withMVar lock $ const block'
  where
    notSameEvent (status', _, _) = status /= status'
    block' =
        dequeueIf notSameEvent back >>=
        maybe (enqueue item front) claim'
    claim' item' = do
        result <- casStatusRef status Waiting Claimed
        case result of
            Synced -> undequeue item' back
            _ -> sync' item'
    sync' item'@(status', handler', value') = do
        result <- casStatusRef status' Waiting Synced
        case result of
            Synced -> do
                writeStatusRef status Waiting
                block'
            Claimed -> do
                writeStatusRef status Waiting
                claim' item'
            Waiting -> do
                writeStatusRef status Synced
                handler value'
                handler' value

newEmptyQueue :: IO (Queue a)
newEmptyQueue = newIORef Seq.empty

enqueue :: a -> Queue a -> IO ()
enqueue value = flip atomicModifyIORef' $ \queue -> (queue |> value, ())

dequeue :: Queue a -> IO (Maybe a)
dequeue queue = atomicModifyIORef' queue (dequeue' . Seq.viewl)
  where
    dequeue' Seq.EmptyL = (Seq.empty, Nothing)
    dequeue' (x :< xs) = (xs, Just x)

undequeue :: a -> Queue a -> IO ()
undequeue value = flip atomicModifyIORef' $ \queue -> (value <| queue, ())

dequeueIf :: (a -> Bool) -> Queue a -> IO (Maybe a)
dequeueIf predicate = flip atomicModifyIORef' $ \queue ->
    let (ls, rs) = Seq.breakl predicate queue in
    case Seq.viewl rs of
        Seq.EmptyL -> (ls, Nothing)
        x :< xs -> (ls >< xs, Just x)
