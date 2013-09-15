{-# LANGUAGE DoAndIfThenElse #-}

module Control.Concurrent.Hannel.Internal.SyncLock (
    SyncLock (), create, isSynced, withAll
) where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef)
import Data.List (sort)
import Data.Unique (Unique, newUnique)

data SyncLock = SyncLock {
    identifier :: Unique,
    handle :: SyncLockHandle
}

data SyncLockHandle = SyncLockHandle {
    status :: MVar Bool,
    synced :: IORef Bool
}

instance Eq SyncLock where
    x == y = identifier x == identifier y
instance Ord SyncLock where
    compare x y = compare (identifier x) (identifier y)

create :: IO SyncLock
create = do
    identifier' <- newUnique
    status' <- newMVar False
    synced' <- newIORef False

    return SyncLock {
        identifier = identifier',
        handle = SyncLockHandle {
            status = status',
            synced = synced'
        }
    }

isSynced :: SyncLock -> IO Bool
isSynced = readIORef . synced . handle

withAll :: [SyncLock] -> IO a -> IO (Maybe a)
withAll locks action = withAll' [] $ sort locks
  where
    withAll' obtained [] = do
        output <- action
        mapM_ sync obtained
        return $ Just output
    withAll' obtained (x:xs) = do
        acquisition <- acquire x
        case acquisition of
            Just handle' -> withAll' (handle' : obtained) xs
            Nothing -> do
                mapM_ release obtained
                return Nothing

acquire :: SyncLock -> IO (Maybe SyncLockHandle)
acquire lock = do
    let handle' = handle lock

    synced' <- readIORef $ synced handle'
    if synced' then
        return Nothing
    else do
        synced'' <- takeMVar $ status handle'
        if synced'' then do
            putMVar (status handle') True
            return Nothing
        else
            return $ Just handle'

release :: SyncLockHandle -> IO ()
release handle' = putMVar (status  handle') False

sync :: SyncLockHandle -> IO ()
sync handle' = do
    atomicWriteIORef (synced handle') True
    putMVar (status handle') True
