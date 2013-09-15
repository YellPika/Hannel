module Control.Concurrent.Hannel.Internal.Trail (
    Trail (), TrailElement (..),
    create, extend, complete, isActive, commitSets, isCoherent, wrap
) where

import Control.Monad (guard)
import Control.Monad.Trans (liftIO)
import Control.Monad.List (ListT (..))
import Data.IORef (IORef, readIORef, atomicModifyIORef) 
import Data.List (isSuffixOf)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Concurrent.Hannel.Internal.SyncLock (SyncLock)
import qualified Control.Concurrent.Hannel.Internal.SyncLock as SyncLock

data TrailElement
    = ChooseLeft | ChooseRight
    | Swap Trail CompletionRef CompletionRef
  deriving Eq

data Trail = Trail {
    syncLock :: SyncLock,
    path :: Path,
    commitAction :: IO ()
}

instance Eq Trail where
    x == y = syncLock x == syncLock y &&path x == path y

type Path = [TrailElement]
type CompleteTrail = (Trail, IO ())
type CompletionRef = IORef [CompleteTrail]
type CommitSet = Map SyncLock CompleteTrail

create :: IO Trail
create = do
    syncLock' <- SyncLock.create
    return Trail {
        syncLock = syncLock',
        path = [],
        commitAction = return ()
    }

extend :: Trail -> TrailElement -> Trail
extend trail element = trail { path = element : path trail }

wrap :: Trail -> IO () -> Trail
wrap trail action = trail { commitAction = commitAction trail >> action }

complete :: Trail -> IO () -> IO CompleteTrail
complete trail action = do
    mapM_ register $ path trail
    return output
  where
    output :: CompleteTrail
    output = (trail, action >> commitAction trail)

    register :: TrailElement -> IO ()
    register (Swap _ _ ref) = atomicModifyIORef ref (\x -> (output:x, ()))
    register _ = return ()

isActive :: Trail -> IO Bool
isActive = allM (fmap not . SyncLock.isSynced . syncLock) . dependencies

commitSets :: CompleteTrail -> IO [CommitSet]
commitSets completeValue@(trail, _) = runListT $ do
    let lock = syncLock trail
    synced <- liftIO $ SyncLock.isSynced lock
    guard $ not synced

    commitSets' trail $ Map.singleton lock completeValue

commitSets' :: Trail -> CommitSet -> ListT IO CommitSet
commitSets' (Trail _ [] _) set = return set
commitSets' trail@(Trail _ (_:xs) _) set = do
    set' <- updateTrailMap trail set
    commitSets' (trail { path = xs }) set'

updateTrailMap :: Trail -> CommitSet -> ListT IO CommitSet
updateTrailMap trail@(Trail _ (Swap swapTrail ref1 ref2 : xs) _) set = do
    let swapLock = syncLock swapTrail
    synced <- liftIO $ SyncLock.isSynced swapLock
    guard $ not synced

    case Map.lookup swapLock set of
        Just (mapTrail, _) -> do
            let swapPth' = Swap (trail { path = xs }) ref2 ref1 : path swapTrail
            guard $ (path mapTrail) `extends` swapPth'
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
        ChooseLeft:xs -> dependencies (trail { path = xs })
        ChooseRight:xs -> dependencies (trail { path = xs })
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
