module Main where

import Control.Concurrent
import Control.Concurrent.Hannel
import Control.Monad
import System.IO

main = test2 >> forever yield

test1 = do
    (s, r) <- sync swap
    (s', r') <- sync swap
    (s'', r'') <- sync swap

    thrash s s' r'' "a" "b"
    thrash s' s'' r "c" "d"
    thrash s'' s r' "e" "f"
  where
    thrash s1 s2 r v1 v2 = forkIO $ forever $ do
        value <- sync $ msum [ fmap (const v1) (s1 v1)
                             , fmap (const v2) (s2 v2)
                             , fmap (const "") (r ())
                             ]

        putStr value
        hFlush stdout

test2 = do
    (s, r) <- sync swap
    (s', r') <- sync swap

    thrash s r' "a"
    thrash s' r "b"
  where
    thrash s r v = void $ forkIO $ forever $ do
        value <- sync $ merge [
                fmap (const Nothing) $ s v,
                fmap Just $ r ()
            ]

        mapM (\x ->
            case x of
                Just x' -> putStr x' >> hFlush stdout
                Nothing -> return ()) value
