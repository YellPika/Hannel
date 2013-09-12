{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Control.Concurrent.Hannel.Event (
    Event (), sync, swap, merge
) where

import Control.Applicative (Applicative, Alternative, empty, (<|>), pure, (<*>))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (MonadPlus, forM, mzero, mplus, msum, ap, filterM, void, when)
import Data.Foldable (Foldable, forM_, toList)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef, atomicModifyIORef)
import Data.Map (Map)
import Data.List (sort)
import System.Random (randomIO)
import Control.Concurrent.Hannel.SyncLock (SyncLock)
import Control.Concurrent.Hannel.Trail (Trail, PathElement (..), extends)

import qualified Data.Map as Map
import qualified Control.Concurrent.Hannel.Trail as Trail
import qualified Control.Concurrent.Hannel.SyncLock as SyncLock

type EventHandler a = a -> Trail -> IO ()

-- |When synchronized upon, an event performs synchronous
-- operations with other threads before returning a value.
newtype Event a = Event {
    runEvent :: Trail -> EventHandler a -> IO ()
}

-- Wraps an event invocation function. This prevents an event from
-- running if any of its dependencies have already been synced.
create :: (Trail -> EventHandler a -> IO ()) -> Event a
create invoke = Event $ \trail handler -> do
    ok <- active $ Trail.dependencies trail
    when ok $ invoke trail handler

-- Returns True iff exactly none of a trail's dependencies have been synced.
active :: Foldable f => f Trail -> IO Bool
active = allM (fmap not . SyncLock.synced . Trail.syncLock) . toList

-- Just a short-circuiting implementation of a monadic 'all' function.
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ [] = return True
allM f (x:xs) = do
    x' <- f x
    if x' then allM f xs else return False

instance Monad Event where
    return x = create $ \trail handler ->
        handler x trail

    x >>= f = create $ \trail handler ->
        runEvent x trail $ \x' trail' ->
            runEvent (f x') trail' handler

instance MonadPlus Event where
    -- Since mzero doesn't actually do anything,
    -- we can forgo wrapping it with 'create'.
    mzero = Event $ \_ _ -> return ()

    mplus first second = create $ \trail handler -> do
        rand <- randomIO
        let (first', second') = if rand
                                then (first, second)
                                else (second, first)

        runEvent first' (Trail.extend trail ChooseLeft) handler
        runEvent second' (Trail.extend trail ChooseRight) handler

instance Applicative Event where
    pure = return
    (<*>) = ap

instance Alternative Event where
    empty = mzero
    (<|>) = mplus

instance Functor Event where
    -- More efficient than liftM.
    fmap f x = create $ \trail handler ->
        runEvent x trail (handler . f)

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
        let completeValue = (trail, action value)
        forM_ (Trail.path trail) $ register completeValue

        commitSet <- findCommitSet completeValue
        case commitSet of
            set:_ ->
                let xs = Map.assocs set in
                commit (forM_ xs (snd . snd)) (map fst xs)
            [] -> return ()

-- Performs an action after acquiring a list of sync locks.
commit :: IO () -> [SyncLock] -> IO ()
commit action = commit' [] . sort
  where
    commit' obtained [] = do
        action
        forM_ obtained SyncLock.sync
    commit' obtained (x:xs) = do
        handle <- SyncLock.acquire x
        case handle of
            Just handle' -> commit' (handle':obtained) xs
            Nothing -> forM_ obtained SyncLock.release

-- Adds a trail/action pair to a completion ref, if the given element is a swap.
register :: (Trail, IO ()) -> PathElement -> IO ()
register x (Swap _ _ ref) = atomicModifyIORef ref ((,()) . (x :))
register _ _ = return ()

-- Constructs a list of possible sets of commitable threads.
findCommitSet :: (Trail, IO ()) -> IO [Map SyncLock (Trail, IO ())]
findCommitSet completeValue@(trail, _) = do
    let lock = Trail.syncLock trail
    synced <- SyncLock.synced lock
    if synced then
        return []
    else
        findCommitSet' trail $ Map.singleton lock completeValue

-- TODO: Clean this s*** up!
findCommitSet' :: Trail -> Map SyncLock (Trail, IO ()) -> IO [Map SyncLock (Trail, IO ())]
findCommitSet' trail refMap =
    case Trail.path trail of
        [] -> return [refMap]
        element:path ->
            let trail' = trail { Trail.path = path } in
            case element of
                ChooseLeft -> findCommitSet' trail' refMap
                ChooseRight -> findCommitSet' trail' refMap
                Swap trail'' ref1 ref2 -> do
                    let lock'' = Trail.syncLock trail''
                    synced <- SyncLock.synced lock''
                    if synced then
                        return []
                    else
                        case Map.lookup lock'' refMap of
                            Just (trail''', _) -> do
                                if Trail.path trail''' `extends` (Swap trail' ref2 ref1 : Trail.path trail'') then
                                    findCommitSet' trail' refMap
                                else
                                    return []
                            Nothing -> do
                                refs <- readIORef ref1
                                refsMaps <- forM refs $ \ref@(trail''', _) ->
                                    findCommitSet' trail''' $ Map.insert (Trail.syncLock trail''') ref refMap
                                refsMaps' <- forM (concat refsMaps) $ findCommitSet' trail'
                                return $ concat refsMaps'

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

    let clean senders receivers value = withLock $ do
        modifyIORef senders (++ [value])

        let clean' queue = do
            queue' <- readIORef queue
            queue'' <- filterM (\(trail', _, _) -> active $ Trail.dependencies trail') queue'
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

                let sendTrail' = Trail.extend sendTrail $ Swap receiveTrail sendRef receiveRef
                let receiveTrail' = Trail.extend receiveTrail $ Swap sendTrail receiveRef sendRef

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
