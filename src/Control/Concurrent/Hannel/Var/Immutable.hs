module Control.Concurrent.Hannel.Var.Immutable (
    IVar (), newIVar, putIVar, takeIVar
) where

import Control.Concurrent.Hannel.Channel.Swap (SwapChannel, newSwapChannel, swap, signal, swapOther, signalOther)
import Control.Concurrent.Hannel.Event (Event, forkEvent, sync)
import Control.Concurrent.Hannel.Var.Class (Var, putVar, takeVar)
import Control.Monad (mplus, void)

-- |An IVar is a concurrent data structure that may either be full or empty.
-- Once the IVar is full, it remains so for the remainder of its lifetime.
-- A full IVar may be read an arbitrary number of times.
data IVar a = IVar {
    inChannel :: SwapChannel () a,
    outChannel :: SwapChannel a ()
}

-- |Creates a new IVar that is filled with the specified value.
newIVar :: Event (IVar a)
newIVar = do
    inChannel' <- newSwapChannel
    outChannel' <- newSwapChannel

    let output = IVar inChannel' outChannel'
        event = empty output
    void $ forkEvent event $ loop output
    return output

full :: IVar a -> a -> Event (Maybe a)
full var value = output `mplus` done
  where
    output = swap (outChannel var) value >> full var value
    done = return $ Just value

empty :: IVar a -> Event (Maybe a)
empty var = input `mplus` done
  where
    input = signal (inChannel var) >>= full var
    done = return Nothing

loop :: IVar a -> Maybe a -> IO ()
loop var value = sync' value >>= loop var
  where
    sync' = sync . maybe (empty var) (full var)

-- |Writes a value to an IVar. If the IVar is already full,
-- then this event blocks indefinitely.
putIVar :: IVar a -> a -> Event ()
putIVar = swapOther . inChannel

-- |Reads a value from an IVar. If the IVar is empty,
-- then this event blocks until it is full.
takeIVar :: IVar a -> Event a
takeIVar = signalOther . outChannel

instance Var IVar where
    putVar = putIVar
    takeVar = takeIVar
