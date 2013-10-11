module Control.Concurrent.Transactional.Channel.Broadcast (
    BroadcastChannel (), newBroadcastChannel, broadcast, enroll,
    BroadcastListener (), listen
) where

import Control.Concurrent.Transactional.Event
import Control.Concurrent.Transactional.EventHandle
import Data.List.Util

import Control.Applicative ((<$>), (<$), (<|>))
import Control.Monad (forM, msum)

data BroadcastChannel i o = Broadcast {
    broadcast :: i -> Event [o],
    enroll :: Event (BroadcastListener i o)
}

data BroadcastListener i o = Listener {
    listen :: o -> Event i
}

data BroadcastConnection i o = Connection {
    sendConnection :: i -> Event o,
    closeConnection :: Event ()
}

newBroadcastChannel :: Event (BroadcastChannel i o)
newBroadcastChannel = do
    (createConnection, requestConnection) <- swap
    (requestBroadcast, receiveBroadcast) <- swap

    let send xs = do
            (input, respond) <- receiveBroadcast ()
            output <- forM xs $ flip sendConnection input
            respond output
            return xs
        add xs = (: xs) <$> requestConnection ()
        close = msum . map (\(x, xs) -> xs <$ closeConnection x) . deletions

    (_, handle) <- forkServer [] $ \xs ->
        send xs <|> add xs <|> close xs

    return Broadcast {
        broadcast = \value -> wrapEvent handle $ do
            (respond, wait) <- swap
            requestBroadcast (value, respond)
            wait (),

        enroll = wrapEvent handle $ do
            (sendListener, receiveListener) <- swap
            (listenerHandle, closeListener) <- newEventHandle

            createConnection Connection {
                sendConnection = sendListener,
                closeConnection = closeListener
            }

            return Listener {
                listen = wrapEvent listenerHandle . wrapEvent handle . receiveListener
            }
    }
