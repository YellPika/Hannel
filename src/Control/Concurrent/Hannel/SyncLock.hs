module Control.Concurrent.Hannel.SyncLock (
    SyncLock (), SyncLockHandle (),
    create,
    acquire, release,
    sync, synced
) where

import Control.Concurrent (yield)
import Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef, atomicModifyIORef)
import Data.Unique (Unique, newUnique)

data Status = Waiting | Claimed | Synced deriving Eq
newtype SyncLockHandle = SyncLockHandle (IORef Status)

-- A special kind of lock which may become 'synced',
-- permanently preventing the lock from being obtained.
newtype SyncLock = SyncLock (Unique, IORef Status) deriving Eq

-- Implementing Ord to assign a global ordering to locks,
-- allowing us to obtain an arbitrary amount of locks without
-- worrying about deadlock.
instance Ord SyncLock where
    compare (SyncLock (x, _)) (SyncLock (y, _)) = compare x y

create :: IO SyncLock
create = do
    unique <- newUnique
    ref <- newIORef Waiting
    return $ SyncLock (unique, ref)

acquire :: SyncLock -> IO (Maybe SyncLockHandle)
acquire lock@(SyncLock (_, ref)) = do
    result <- atomicModifyIORef ref $ \x ->
        (if x == Waiting then Synced else x, x)

    case result of
        Waiting -> return $ Just $ SyncLockHandle ref
        Claimed -> do
            yield
            acquire lock
        Synced -> return Nothing

release :: SyncLockHandle -> IO ()
release (SyncLockHandle ref) = atomicWriteIORef ref Waiting

sync :: SyncLockHandle -> IO ()
sync (SyncLockHandle ref) = atomicWriteIORef ref Synced

synced :: SyncLock -> IO Bool
synced (SyncLock (_, ref)) = fmap (== Synced) $ readIORef ref
