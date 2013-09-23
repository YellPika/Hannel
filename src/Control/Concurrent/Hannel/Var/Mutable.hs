module Control.Concurrent.Hannel.Var.Mutable (
    MVar (), newMVar, newEmptyMVar, putMVar, takeMVar
) where

import Control.Concurrent.Hannel.Channel.Swap (SwapChannel, newSwapChannel, swap, signal, swapOther, signalOther)
import Control.Concurrent.Hannel.Event (Event, forkEvent, sync)
import Control.Concurrent.Hannel.Var.Class (Var, putVar, takeVar)
import Control.Monad (mplus, void)

-- |An MVar is a concurrent data structure that may either be full or empty.
data MVar a = MVar {
    inChannel :: SwapChannel () a,
    outChannel :: SwapChannel a ()
}

-- |Creates a new MVar that is filled with the specified value.
newMVar :: a -> Event (MVar a)
newMVar = newMVar' . Just

-- |Creates a new empty MVar.
newEmptyMVar :: Event (MVar a)
newEmptyMVar = newMVar' Nothing

newMVar' :: Maybe a -> Event (MVar a)
newMVar' value  = do
    inChannel' <- newSwapChannel
    outChannel' <- newSwapChannel

    let output = MVar inChannel' outChannel'
        event = maybe (empty output) (full output) value
    void $ forkEvent event $ loop output
    return output

full :: MVar a -> a -> Event (Maybe a)
full var value = output `mplus` done
  where
    output = swap (outChannel var) value >> empty var
    done = return $ Just value

empty :: MVar a -> Event (Maybe a)
empty var = input `mplus` done
  where
    input = signal (inChannel var) >>= full var
    done = return Nothing

loop :: MVar a -> Maybe a -> IO ()
loop var value = sync' value >>= loop var
  where
    sync' = sync . maybe (empty var) (full var)

-- |Writes a value to an MVar. If the MVar is already full,
-- this event blocks until it is empty.
putMVar :: MVar a -> a -> Event ()
putMVar var = swapOther $ inChannel var

-- |Removes a value from an MVar. If the MVar is empty,
-- this event blocks until it is full.
takeMVar :: MVar a -> Event a
takeMVar = signalOther . outChannel

instance Var MVar where
    putVar = putMVar
    takeVar = takeMVar
