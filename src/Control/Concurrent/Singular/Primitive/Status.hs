{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Control.Concurrent.Singular.Primitive.Status (
    Status (..),
    StatusRef (), newStatusRef,
    readStatusRef, writeStatusRef, casStatusRef
) where

import Control.Monad.Trans (MonadIO, liftIO)
import GHC.IO (IO (..))
import GHC.Prim (RealWorld, MutVar#, newMutVar#, readMutVar#, writeMutVar#, casMutVar#, sameMutVar#)

data Status = Waiting | Claimed | Synced deriving (Eq, Show)

data StatusRef = StatusRef {
    unStatusRef :: MutVar# RealWorld Status
}

instance Eq StatusRef where
    x == y = sameMutVar# (unStatusRef x) (unStatusRef y)

newStatusRef :: MonadIO m => m StatusRef
newStatusRef = liftIO $ IO $ \s ->
    case newMutVar# Waiting s of
        (# s', x #) -> (# s', StatusRef x #)

readStatusRef :: MonadIO m => StatusRef -> m Status
readStatusRef (StatusRef ref) = liftIO $ IO $ \s ->
    case readMutVar# ref s of
        (# s', x #) -> (# s', x #)

writeStatusRef :: MonadIO m => StatusRef -> Status -> m ()
writeStatusRef (StatusRef ref) value = liftIO $ IO $ \s ->
    case writeMutVar# ref value s of
        s' -> (# s', () #)

casStatusRef :: MonadIO m => StatusRef -> Status -> Status -> m Status
casStatusRef (StatusRef ref) comparand value = liftIO $ IO $ \s ->
    case casMutVar# ref comparand value s of
        (# s', _, x #) -> (# s', x #)
