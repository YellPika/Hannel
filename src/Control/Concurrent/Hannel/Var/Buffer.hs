{-# LANGUAGE Safe #-}

module Control.Concurrent.Hannel.Var.Buffer (
    BVar, newBVar, putBVar, takeBVar
) where

import Control.Concurrent.Hannel.Event
import Control.Concurrent.Hannel.EventHandle
import Control.Concurrent.Hannel.Var.Class

import Control.Applicative ((<|>), (<$>), (<$))
import Data.Sequence ((|>), ViewL ((:<)))

import qualified Data.Sequence as Seq

-- |An asynchronous channel. 
data BVar a = BVar {
    -- |Writes a value to a channel.
    putBVar :: !(a -> Event ()),

    -- |Reads a value from a channel.
    takeBVar :: !(Event a)
}

-- |Creates a new buffered channel.
newBVar :: Event (BVar a)
newBVar = do
    (sendIn, receiveIn) <- swap
    (sendOut, receiveOut) <- swap

    let enqueue queue = (queue |>) <$> receiveIn ()
        dequeue value queue = queue <$ sendOut value

    (_, handle) <- forkServer Seq.empty $ \queue ->
        case Seq.viewl queue of
            Seq.EmptyL -> enqueue queue
            x :< xs -> enqueue queue <|> dequeue x xs

    return BVar {
        putBVar = wrapEvent handle . sendIn,
        takeBVar = wrapEvent handle $ receiveOut ()
    }

instance Var BVar where
    putVar = putBVar
    takeVar = takeVar
