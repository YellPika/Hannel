module Control.Concurrent.Hannel.Channel.Buffer (
    BufferChannel, newBufferChannel, putBufferChannel, takeBufferChannel
) where

import Control.Concurrent.Hannel.Channel.Swap
import Control.Concurrent.Hannel.Event
import Control.Concurrent.Hannel.EventHandle

import Control.Applicative ((<|>), (<$>), (<$))
import Data.Sequence ((|>), ViewL ((:<)))

import qualified Data.Sequence as Seq

-- |An asynchronous channel.
data BufferChannel a = BufferChannel {
    -- |Writes a value to a channel.
    putBufferChannel :: a -> Event (),

    -- |Reads a value from a channel.
    takeBufferChannel :: Event a
}

-- |Creates a new buffered channel.
newBufferChannel :: Event (BufferChannel a)
newBufferChannel = do
    inChannel <- newSwapChannel
    outChannel <- newSwapChannel

    let enqueue queue = (queue |>) <$> receiveFront inChannel
        dequeue value queue = queue <$ sendFront outChannel value

    (_, handle) <- forkServer Seq.empty $ \queue ->
        case Seq.viewl queue of
            Seq.EmptyL -> enqueue queue
            x :< xs -> enqueue queue <|> dequeue x xs

    return BufferChannel {
        putBufferChannel = wrapEvent handle . sendBack inChannel,
        takeBufferChannel = wrapEvent handle $ receiveBack outChannel
    }
