{-# LANGUAGE Safe #-}

module Control.Concurrent.Hannel.Var.Mutable (
    MVar (), newMVar, newEmptyMVar, putMVar, takeMVar
) where

import Control.Concurrent.Hannel.Event
import Control.Concurrent.Hannel.EventHandle
import Control.Concurrent.Hannel.Var.Class

import Data.Functor ((<$>), (<$))

-- |An MVar is a concurrent data structure that may either be full or empty.
data MVar a = MVar {
    -- |Writes a value to an MVar. If the MVar is already full,
    -- this event blocks until it is empty.
    putMVar :: !(a -> Event ()),

    -- |Removes a value from an MVar. If the MVar is empty,
    -- this event blocks until it is full.
    takeMVar :: !(Event a)
}

-- |Creates a new MVar that is filled with the specified value.
newMVar :: a -> Event (MVar a)
newMVar = newMVar' . Just

-- |Creates a new empty MVar.
newEmptyMVar :: Event (MVar a)
newEmptyMVar = newMVar' Nothing

newMVar' :: Maybe a -> Event (MVar a)
newMVar' value  = do
    (sendIn, receiveIn) <- swap
    (sendOut, receiveOut) <- swap

    let step Nothing = Just <$> receiveIn ()
        step (Just x) = Nothing <$ sendOut x

    (_, handle) <- forkServer value step
    return MVar {
        putMVar = wrapEvent handle . sendIn,
        takeMVar = wrapEvent handle $ receiveOut ()
    }

instance Var MVar where
    putVar = putMVar
    takeVar = takeMVar
