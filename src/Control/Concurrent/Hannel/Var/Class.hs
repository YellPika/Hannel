{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Concurrent.Hannel.Var.Class (
    Var, putVar, takeVar
) where

import Control.Concurrent.Hannel.Event (Event)
--import Control.Concurrent.Hannel.Channel.Class (Channel, swap)

-- |Describes a variable that supports concurrent operations.
class Var v where
    -- |Writes a value to a variable.
    putVar :: v a -> a -> Event ()

    -- |Reads a value from a variable.
    takeVar :: v a -> Event a

-- instance Var v => Channel (v a) a () where
    -- swap = putVar

-- instance Var v => Channel (v a) () a where
    -- swap var () = takeVar var
