module Control.Concurrent.Hannel.Trail (
    Trail (), TrailElement (..),
    create, extend, complete, isActive, commitSets, coherent
) where

import Control.Monad (guard)
import Control.Monad.Trans (liftIO)
import Control.Monad.List (ListT (..))
import Data.IORef (IORef, readIORef, atomicModifyIORef) 
import Data.List (isSuffixOf)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Concurrent.Hannel.SyncLock (SyncLock)
import qualified Control.Concurrent.Hannel.SyncLock as SyncLock

data TrailElement
    = ChooseLeft | ChooseRight
    | Swap Trail CompletionRef CompletionRef
  deriving Eq

data Trail = Trail {
    syncLock :: SyncLock,
    path :: Path
} deriving Eq

type Path = [TrailElement]
type CompleteTrail = (Trail, IO ())
type CompletionRef = IORef [CompleteTrail]
type CommitSet = Map SyncLock CompleteTrail

create :: IO Trail
create = do
    syncLock' <- SyncLock.create
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
isActive = allM (fmap not . SyncLock.isSynced . syncLock) . dependencies

commitSets :: CompleteTrail -> IO [CommitSet]
commitSets completeValue@(Trail lock pth, _) = runListT $ do
    synced <- liftIO $ SyncLock.isSynced lock
    guard $ not synced

    commitSets' lock pth $ Map.singleton lock completeValue

commitSets' :: SyncLock -> Path -> CommitSet -> ListT IO CommitSet
commitSets' _ [] trailMap = return trailMap
commitSets' lock (x:xs) trailMap =
    updateTrailMap lock x xs trailMap
    >>= commitSets' lock xs

updateTrailMap :: SyncLock -> TrailElement -> Path -> CommitSet -> ListT IO CommitSet
updateTrailMap lock (Swap (Trail swapLock swapPth) ref1 ref2) pth trailMap = do
    synced <- liftIO $ SyncLock.isSynced swapLock
    guard $ not synced

    case Map.lookup swapLock trailMap of
        Just (Trail _ mapPath, _) -> do
            let swapPth' = Swap (Trail lock pth) ref2 ref1 : swapPth
            guard $ mapPath `extends` swapPth'
            return trailMap
        Nothing -> do
            ref@(Trail refLock refPth, _) <- ListT $ readIORef ref1
            commitSets' refLock refPth $ Map.insert refLock ref trailMap
updateTrailMap _ _ _ trailMap = return trailMap

extends :: Path -> Path -> Bool
a `extends` b = b `isSuffixOf` a

comparable :: Path -> Path -> Bool
comparable a b = a `extends` b || b `extends` a

dependencies :: Trail -> [Trail]
dependencies trail =
    trail :
    case path trail of
        [] -> []
        ChooseLeft:xs -> dependencies (trail { path = xs })
        ChooseRight:xs -> dependencies (trail { path = xs })
        Swap partner cRef1 cRef2 : xs ->
            extend partner (Swap (trail { path = xs }) cRef2 cRef1) :
            dependencies trail { path = xs } ++
            dependencies partner

coherent :: Trail -> Trail -> Bool
coherent front back =
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
