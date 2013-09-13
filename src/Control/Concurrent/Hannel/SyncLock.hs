{-# LANGUAGE DoAndIfThenElse #-}

module Control.Concurrent.Hannel.SyncLock (
    SyncLock (), SyncLockHandle (),
    create,
    acquire, release,
    sync, synced
) where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef)
import Data.Unique (Unique, newUnique)

data SyncLockHandle = SyncLockHandle {
    statusVar :: MVar Bool,
    syncedVar :: IORef Bool
}

-- A special kind of lock which may become 'synced',
-- permanently preventing the lock from being obtained.
data SyncLock = SyncLock {
    uniqueID :: Unique,
    handle :: SyncLockHandle
}

instance Eq SyncLock where
    x == y = uniqueID x == uniqueID y

-- Implementing Ord to assign a global ordering to locks,
-- allowing us to obtain an arbitrary amount of locks without
-- worrying about deadlock.
instance Ord SyncLock where
    compare x y = compare (uniqueID x) (uniqueID y)

create :: IO SyncLock
create = do
    unique <- newUnique
    mVar <- newMVar False
    syncVal <- newIORef False

    return $ SyncLock unique $ SyncLockHandle mVar syncVal

acquire :: SyncLock -> IO (Maybe SyncLockHandle)
acquire lock = do
    synced' <- readIORef $ syncedVar $ handle lock
    if synced' then
        return Nothing
    else do
        result <- takeMVar $ statusVar $ handle lock
        if result then do
            putMVar (statusVar $ handle lock) True
            return Nothing
        else
            return $ Just $ handle lock

release :: SyncLockHandle -> IO ()
release handle' = putMVar (statusVar handle') False

sync :: SyncLockHandle -> IO ()
sync handle' = do
    atomicWriteIORef (syncedVar handle') True
    putMVar (statusVar handle') True

synced :: SyncLock -> IO Bool
synced = readIORef . syncedVar . handle
