module Control.Concurrent.Hannel.Channel.Buffer (
    BufferChannel, newBufferChannel, putBufferChannel, takeBufferChannel
) where

import Control.Applicative ((<|>), (<$>), (<$))
import Control.Concurrent.Hannel.Channel.Swap (SwapChannel, newSwapChannel, sendFront, sendBack, receiveFront, receiveBack)
import Control.Concurrent.Hannel.Event (Event, forkServer, withServerHandle)
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

    return $ BufferChannel {
        putBufferChannel = withServerHandle handle . sendBack inChannel,
        takeBufferChannel = withServerHandle handle $ receiveBack outChannel
    }
