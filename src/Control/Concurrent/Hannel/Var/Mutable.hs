module Control.Concurrent.Hannel.Var.Mutable (
    MVar (), newMVar, newEmptyMVar, putMVar, takeMVar
) where

import Control.Concurrent.Hannel.Channel.Swap (SwapChannel, newSwapChannel, sendFront, receiveFront, sendBack, receiveBack)
import Control.Concurrent.Hannel.Event (Event, forkServer, withServerHandle)
import Control.Concurrent.Hannel.Var.Class (Var, putVar, takeVar)
import Data.Functor ((<$>), (<$))

-- |An MVar is a concurrent data structure that may either be full or empty.
data MVar a = MVar {
    -- |Writes a value to an MVar. If the MVar is already full,
    -- this event blocks until it is empty.
    putMVar :: a -> Event (),

    -- |Removes a value from an MVar. If the MVar is empty,
    -- this event blocks until it is full.
    takeMVar :: Event a
}

-- |Creates a new MVar that is filled with the specified value.
newMVar :: a -> Event (MVar a)
newMVar = newMVar' . Just

-- |Creates a new empty MVar.
newEmptyMVar :: Event (MVar a)
newEmptyMVar = newMVar' Nothing

newMVar' :: Maybe a -> Event (MVar a)
newMVar' value  = do
    inChannel <- newSwapChannel
    outChannel <- newSwapChannel

    let step Nothing = Just <$> receiveFront inChannel
        step (Just x) = Nothing <$ sendFront outChannel x

    (_, handle) <- forkServer value step
    return $ MVar {
        putMVar = withServerHandle handle . sendBack inChannel,
        takeMVar = withServerHandle handle $ receiveBack outChannel
    }

instance Var MVar where
    putVar = putMVar
    takeVar = takeMVar
