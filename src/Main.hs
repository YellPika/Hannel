module Main where

import Control.Concurrent
import Control.Concurrent.Hannel
import Control.Monad
import System.IO

times num = forM_ [1 :: Int .. num]

main = do
    (s, r) <- sync swap
    (s', r') <- sync swap

    thrash s r' "a"
    thrash s' r "b"

    forever yield

thrash s r v = void $ forkIO $ forever $ do
    value <- sync $ merge [
            fmap (const Nothing) $ s v,
            fmap Just $ r ()
        ]

    mapM (\x ->
        case x of
            Just x' -> do { putStr x'; hFlush stdout }
            Nothing -> return ()) value
