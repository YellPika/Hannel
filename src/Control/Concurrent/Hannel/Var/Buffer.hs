{-# LANGUAGE Safe #-}

module Control.Concurrent.Hannel.Var.Buffer (
    BVar, newBVar, putBVar, takeBVar
) where

import Control.Concurrent.Hannel.Channel.Swap
import Control.Concurrent.Hannel.Event
import Control.Concurrent.Hannel.EventHandle
import Control.Concurrent.Hannel.Var.Class

import Control.Applicative ((<|>), (<$>), (<$))
import Data.Sequence ((|>), ViewL ((:<)))

import qualified Data.Sequence as Seq

-- |An asynchronous channel.
data BVar a = BVar {
    -- |Writes a value to a channel.
    putBVar :: a -> Event (),

    -- |Reads a value from a channel.
    takeBVar :: Event a
}

-- |Creates a new buffered channel.
newBVar :: Event (BVar a)
newBVar = do
    inChannel <- newSwapChannel
    outChannel <- newSwapChannel

    let enqueue queue = (queue |>) <$> receiveFront inChannel
        dequeue value queue = queue <$ sendFront outChannel value

    (_, handle) <- forkServer Seq.empty $ \queue ->
        case Seq.viewl queue of
            Seq.EmptyL -> enqueue queue
            x :< xs -> enqueue queue <|> dequeue x xs

    return BVar {
        putBVar = wrapEvent handle . sendBack inChannel,
        takeBVar = wrapEvent handle $ receiveBack outChannel
    }

instance Var BVar where
    putVar = putBVar
    takeVar = takeVar
