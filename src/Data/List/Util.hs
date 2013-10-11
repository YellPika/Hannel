module Data.List.Util (
    deletions, splits, shuffle
) where

import Control.Arrow (second)
import Control.Monad (forM)
import Data.Array.IO.Safe (IOArray, newListArray, readArray, writeArray)
import System.Random (randomRIO)

deletions :: [a] -> [(a, [a])]
deletions [] = []
deletions (x:xs) = (x, xs) : map (second (x :)) (deletions xs)

splits :: [a] -> [([a], a, [a])]
splits [] = []
splits (x:xs) = ([], x, xs) : map (\(l, c, r) -> (x:l, c, r)) (splits xs)

shuffle :: [a] -> IO [a]
shuffle xs = do
    let n = length xs
    ar <- newArray n xs
    forM [1 .. n] $ \i -> do
        j <- randomRIO (i, n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj

newArray :: Int -> [a] -> IO (IOArray Int a)
newArray n = newListArray (1, n)
