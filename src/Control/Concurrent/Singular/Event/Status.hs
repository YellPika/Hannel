{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Control.Concurrent.Singular.Event.Status (
    Status (..),
    StatusRef (), newStatusRef,
    readStatusRef, writeStatusRef, casStatusRef
) where

import GHC.IO (IO (..))
import GHC.Prim (RealWorld, MutVar#, newMutVar#, readMutVar#, writeMutVar#, casMutVar#, sameMutVar#)

data Status = Waiting | Claimed | Synced deriving (Eq, Show)

data StatusRef = StatusRef {
    unStatusRef :: MutVar# RealWorld Status
}

instance Eq StatusRef where
    x == y = sameMutVar# (unStatusRef x) (unStatusRef y)

newStatusRef :: IO StatusRef
newStatusRef = IO $ \s ->
    case newMutVar# Waiting s of
        (# s', x #) -> (# s', StatusRef x #)

readStatusRef :: StatusRef -> IO Status
readStatusRef (StatusRef ref) = IO $ \s ->
    case readMutVar# ref s of
        (# s', x #) -> (# s', x #)

writeStatusRef :: StatusRef -> Status -> IO ()
writeStatusRef (StatusRef ref) value = IO $ \s ->
    case writeMutVar# ref value s of
        s' -> (# s', () #)

casStatusRef :: StatusRef -> Status -> Status -> IO Status
casStatusRef (StatusRef ref) comparand value = IO $ \s ->
    case casMutVar# ref comparand value s of
        (# s', _, x #) -> (# s', x #)
