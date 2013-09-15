module Control.Concurrent.Hannel.Primitive.Condition (

) where

data Condition = Condition {
    lock :: MVar ()
    signaled :: IORef Bool
    listeners :: IORef [Listener]
}

wait :: Condition -> Event ()
wait condition = Event.create $ \trail handler -> do
    takeMVar $ lock condition
    x <- readIORef $ signaled condition

    if x then do
        putMVar (lock condition) ()
        handler () trail
    else do
        modifyIORef (listeners condition) ((trail, handler) :)
        putMVar (lock condition) ()

set :: Condition -> IO ()
set condition = do
    x <- withMVar (lock condition) $ \_ ->
        x <- readIORef $ signaled condition
        writeIORef (signaled condition) True
        return x

    unless x $ do
        xs <- readIORef $ listeners condition
        writeIORef (listeners condition) []

        forM_ xs $ \(trail, handler) ->
            handler () trail
