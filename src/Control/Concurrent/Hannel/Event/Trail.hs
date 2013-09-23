{-# LANGUAGE Safe #-}

module Control.Concurrent.Hannel.Event.Trail (
    Trail (), TrailElement (..),
    newTrail, extend, complete,
    commitSets, syncID,
    isActive, isCoherent
) where

import Control.Concurrent.Hannel.Event.SyncLock (SyncLock, newSyncLock, isSynced, identifier)
import Control.Monad (guard)
import Control.Monad.Trans (liftIO)
import Control.Monad.List (ListT (..))
import Data.IORef (IORef, readIORef, atomicModifyIORef)
import Data.List (isSuffixOf)
import Data.Map (Map)
import Data.Unique (Unique)

import qualified Data.Map as Map

data TrailElement
    = Choose Integer
    | Swap Trail CompletionRef CompletionRef
  deriving Eq

data Trail = Trail {
    syncLock :: SyncLock,
    path :: Path
}

instance Eq Trail where
    x == y = syncLock x == syncLock y && path x == path y

type Path = [TrailElement]
type CompleteTrail = (Trail, IO ())
type CompletionRef = IORef [CompleteTrail]
type CommitSet = Map SyncLock CompleteTrail

newTrail :: IO Trail
newTrail = do
    syncLock' <- newSyncLock
    return Trail { syncLock = syncLock', path = [] }

extend :: Trail -> TrailElement -> Trail
extend trail element = trail { path = element : path trail }

complete :: Trail -> IO () -> IO CompleteTrail
complete trail action = do
    mapM_ register $ path trail
    return output
  where
    output :: CompleteTrail
    output = (trail, action)

    register :: TrailElement -> IO ()
    register (Swap _ _ ref) = atomicModifyIORef ref (\x -> (output:x, ()))
    register _ = return ()

isActive :: Trail -> IO Bool
isActive = allM (fmap not . isSynced . syncLock) . dependencies

syncID :: Trail -> Unique
syncID = identifier . syncLock

commitSets :: CompleteTrail -> IO [CommitSet]
commitSets completeValue@(trail, _) = runListT $ do
    let lock = syncLock trail
    synced <- liftIO $ isSynced lock
    guard $ not synced

    commitSets' trail $ Map.singleton lock completeValue

commitSets' :: Trail -> CommitSet -> ListT IO CommitSet
commitSets' (Trail _ []) set = return set
commitSets' trail@(Trail _ (_:xs)) set = do
    set' <- updateTrailMap trail set
    commitSets' (trail { path = xs }) set'

updateTrailMap :: Trail -> CommitSet -> ListT IO CommitSet
updateTrailMap trail@(Trail _ (Swap swapTrail ref1 ref2 : xs)) set = do
    let swapLock = syncLock swapTrail
    synced <- liftIO $ isSynced swapLock
    guard $ not synced

    case Map.lookup swapLock set of
        Just (mapTrail, _) -> do
            let swapPth' = Swap (trail { path = xs }) ref2 ref1 : path swapTrail
            guard $ path mapTrail `extends` swapPth'
            return set
        Nothing -> do
            ref@(refTrail, _) <- ListT $ readIORef ref1
            commitSets' refTrail $ Map.insert (syncLock refTrail) ref set
updateTrailMap _ set = return set

extends :: Path -> Path -> Bool
a `extends` b = b `isSuffixOf` a

comparable :: Path -> Path -> Bool
comparable a b = a `extends` b || b `extends` a

dependencies :: Trail -> [Trail]
dependencies trail =
    trail :
    case path trail of
        [] -> []
        Choose _:xs -> dependencies (trail { path = xs })
        Swap partner cRef1 cRef2 : xs ->
            extend partner (Swap (trail { path = xs }) cRef2 cRef1) :
            dependencies trail { path = xs } ++
            dependencies partner

isCoherent :: Trail -> Trail -> Bool
isCoherent front back =
    syncLock front /= syncLock back &&
    foreach frontDeps (sameLock extends back ) &&
    foreach backDeps  (sameLock extends front) &&
    foreach frontDeps (foreach backDeps . sameLock comparable)
  where
    frontDeps = dependencies front
    backDeps = dependencies back

sameLock :: (Path -> Path -> Bool) -> Trail -> Trail -> Bool
sameLock f thread thread' =
    (syncLock thread == syncLock thread') `implies`
    (path thread `f` path thread')

foreach :: [a] -> (a -> Bool) -> Bool
foreach = flip all

implies :: Bool -> Bool -> Bool
x `implies` y = not x || y

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f = allM'
  where
    allM' [] = return True
    allM' (x:xs) = do
        x' <- f x
        if x' then allM' xs else return False
