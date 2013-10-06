{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE Safe #-}

module Control.Concurrent.Hannel.Event.SyncLock (
    SyncLock (), newSyncLock, identifier, isSynced, syncAll
) where

import Control.Applicative ((<$))
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef)
import Data.List (sort)
import Data.Unique (Unique, newUnique)

-- A SyncLock is a lock-like data structure that may be in one of three states:
-- * Locked - the lock is in use, and will block other attempts to obtain it.
-- * Unlocked - the lock is not in use, and may be obtained by anyone.
-- * Synced - the lock has been permanently locked.
data SyncLock = SyncLock {
    identifier :: !Unique,
    status :: !(MVar Bool),
    synced :: !(IORef Bool)
}

instance Eq SyncLock where
    x == y = identifier x == identifier y

-- SyncLocks are ordered, which allows an arbitrary number of
-- SyncLocks to be obtained simultaneously without risking deadlock.
instance Ord SyncLock where
    compare x y = compare (identifier x) (identifier y)

-- A new SyncLock is unlocked by default.
newSyncLock :: IO SyncLock
newSyncLock = do
    identifier' <- newUnique
    status' <- newMVar False
    synced' <- newIORef False

    return SyncLock {
        identifier = identifier',
        status = status',
        synced = synced'
    }

isSynced :: SyncLock -> IO Bool
isSynced = readIORef . synced

-- Synchronizes a set of locks in a deadlock free manner.
-- Returns false if any of the locks have already been synced.
syncAll :: [SyncLock] -> IO Bool
syncAll = syncAll' [] . sort
  where
    syncAll' obtained [] = True <$ mapM_ sync obtained
    syncAll' obtained (x:xs) = do
        acquired <- acquire x
        if acquired
        then syncAll' (x : obtained) xs
        else False <$ mapM_ release obtained

acquire :: SyncLock -> IO Bool
acquire lock = do
    synced' <- isSynced lock

    if synced' then
        return False
    else do
        synced'' <- takeMVar $ status lock
        when synced'' $ putMVar (status lock) True
        return $ not synced''

release :: SyncLock -> IO ()
release lock = putMVar (status lock) False

sync :: SyncLock -> IO ()
sync lock = do
    atomicWriteIORef (synced lock) True
    putMVar (status lock) True
