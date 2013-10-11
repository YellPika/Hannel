{-# LANGUAGE Safe #-}

module Control.Concurrent.Transactional.Var.Immutable (
    IVar (), newIVar, putIVar, takeIVar
) where

import Control.Concurrent.Transactional.Event
import Control.Concurrent.Transactional.EventHandle
import Control.Concurrent.Transactional.Var.Class

import Data.Functor ((<$>), (<$))

-- |An IVar is a concurrent data structure that may either be full or empty.
-- Once the IVar is full, it remains so for the remainder of its lifetime.
-- A full IVar may be read an arbitrary number of times.
data IVar a = IVar {
    -- |Writes a value to an IVar. If the IVar is already full,
    -- then this event blocks indefinitely.
    putIVar :: !(a -> Event ()),

    -- |Reads a value from an IVar. If the IVar is empty,
    -- then this event blocks until it is full.
    takeIVar :: !(Event a)
}

-- |Creates a new IVar that is filled with the specified value.
newIVar :: Event (IVar a)
newIVar = do
    (sendIn, receiveIn) <- swap
    (sendOut, receiveOut) <- swap

    let step Nothing = Just <$> receiveIn ()
        step (Just x) = Just x <$ sendOut x

    (_, handle) <- forkServer Nothing step
    return IVar {
        putIVar = wrapEvent handle . sendIn,
        takeIVar = wrapEvent handle $ receiveOut ()
    }

instance Var IVar where
    putVar = putIVar
    takeVar = takeIVar
