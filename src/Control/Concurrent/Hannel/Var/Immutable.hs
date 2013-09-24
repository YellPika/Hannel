module Control.Concurrent.Hannel.Var.Immutable (
    IVar (), newIVar, putIVar, takeIVar
) where

import Control.Concurrent.Hannel.Channel.Swap (SwapChannel, newSwapChannel, swap, signal, swapOther, signalOther)
import Control.Concurrent.Hannel.Event (Event, forkServer)
import Control.Concurrent.Hannel.Var.Class (Var, putVar, takeVar)
import Control.Monad (void)
import Data.Functor ((<$>), (<$))

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

    let step Nothing = Just <$> signal inChannel'
        step (Just x) = Just x <$ swap outChannel' x

    void $ forkServer Nothing step
    return $ IVar inChannel' outChannel'

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
