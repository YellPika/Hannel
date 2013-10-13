{-# LANGUAGE Safe #-}

module Control.Concurrent.Singular.Primitive.Channel (
    Channel (..), newChannel, swapFront, swapBack
) where

import Control.Concurrent.Singular.Primitive.Event
import Control.Concurrent.Singular.Primitive.Status

import Control.Applicative ((<$), (<$>), (<*>))
import Control.Concurrent (yield)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
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

commit :: Channel i o -> i -> MaybeT IO o
commit (Channel lock _ back) input =
    MaybeT $ withMVar lock $ const $ runMaybeT commit'
  where
    commit' = dequeue back >>= sync'
    sync' item@(status, handler, output) = do
        result <- casStatusRef status Waiting Synced
        case result of
            Synced -> commit'
            Claimed -> liftIO yield >> sync' item
            Waiting -> output <$ liftIO (handler input)

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

dequeue :: Queue a -> MaybeT IO a
dequeue queue = MaybeT $ atomicModifyIORef' queue (dequeue' . Seq.viewl)
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
