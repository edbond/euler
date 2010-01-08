{-
    In the 20×20 grid below, four numbers along a diagonal line have been marked in red.
    (11.grid)
    The product of these numbers is 26 × 63 × 78 × 14 = 1788696.
    What is the greatest product of four adjacent numbers in any direction (up, down, left, right, or diagonally) in the 20×20 grid?
-}

module Main
where

import Data.List

getLineVariants :: Int -> [a] -> [[a]]
getLineVariants len arr = let
        v = map (take len) $ filter (\x -> length(x) >= len) $ tails arr
    in
    v

getGridVariants :: Int -> [Char] -> [[Int]]
getGridVariants len s = let
        mtrx = map words $ lines s
    in
    [[]]

-- read grid file
-- and return matrix
readGrid = do
    text <- readFile "11.grid"
    let
        variants = getGridVariants 4 text
    return []
