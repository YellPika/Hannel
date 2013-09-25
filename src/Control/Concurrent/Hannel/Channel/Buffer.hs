module Control.Concurrent.Hannel.Channel.Buffer (
    BufferChannel, newBufferChannel, putBufferChannel, takeBufferChannel
) where

import Control.Applicative ((<|>), (<$>), (<$))
import Control.Concurrent.Hannel.Channel.Swap (SwapChannel, newSwapChannel, sendFront, sendBack, receiveFront, receiveBack)
import Control.Concurrent.Hannel.Event (Event, forkServer)
import Control.Monad (void)
import Data.Sequence ((|>), ViewL ((:<)))
import qualified Data.Sequence as Seq

-- |An asynchronous channel.
data BufferChannel a = BufferChannel {
    inChannel :: SwapChannel () a,
    outChannel :: SwapChannel a ()
}

-- |Creates a new buffered channel.
newBufferChannel :: Event (BufferChannel a)
newBufferChannel = do
    inChannel' <- newSwapChannel
    outChannel' <- newSwapChannel

    let enqueue queue = (queue |>) <$> receiveFront inChannel'
        dequeue value queue = queue <$ sendFront outChannel' value

    void $ forkServer Seq.empty $ \queue ->
        case Seq.viewl queue of
            Seq.EmptyL -> enqueue queue
            x :< xs -> enqueue queue <|> dequeue x xs

    return $ BufferChannel inChannel' outChannel'

-- |Writes a value to a channel.
putBufferChannel :: BufferChannel a -> a -> Event ()
putBufferChannel = sendBack . inChannel

-- |Reads a value from a channel.
takeBufferChannel :: BufferChannel a -> Event a
takeBufferChannel = receiveBack . outChannel
