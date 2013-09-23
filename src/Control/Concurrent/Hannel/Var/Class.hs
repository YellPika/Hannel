module Control.Concurrent.Hannel.Var.Class (
    Var, putVar, takeVar
) where

import Control.Concurrent.Hannel.Event (Event)

-- |Describes a variable that supports concurrent operations.
class Var v where
    -- |Writes a value to a variable.
    putVar :: v a -> a -> Event ()

    -- |Reads a value from a variable.
    takeVar :: v a -> Event a
