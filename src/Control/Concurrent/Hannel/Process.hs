module Control.Concurrent.Hannel.Process (
    Process, with, withIO
) where

import Control.Exception (bracket)
import Control.Concurrent.Hannel.Internal.Event (Event, sync)

-- |Represents a process that runs in a separate thread.
class Process a where
    -- |An event representing when a process finishes execution.
    finish :: a -> Event ()

-- |Performs an action with a process, and then waits for the process to finish.
with :: Process a => a -> (a -> IO b) -> IO b
with = withIO . return

-- |Performs an action with a process, and then waits for the process to finish.
withIO :: Process a => IO a -> (a -> IO b) -> IO b
withIO process = bracket process (sync . finish)