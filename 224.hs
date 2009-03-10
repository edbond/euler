-- http://projecteuler.net/index.php?section=problems&id=224
--
-- Let us call an integer sided triangle with sides a ≤ b ≤ c barely obtuse if the sides satisfy
-- a^(2) + b^(2) = c^(2) - 1.
-- How many barely obtuse triangles are there with perimeter ≤ 75,000,000?

-- -O2 -fvia-C -optc-O2

module Main
where

import qualified Debug.Trace
--import Data.IntSet
import Data.Map
import Control.Parallel
import Control.Parallel.Strategies

lim :: Int
-- lim = 75000000
lim = 75000000
lh = lim `div` 3
c = 100

m = Data.Map.fromDistinctAscList [(x*x,x) | x <- [1..h]]
  where
  h :: Int
  h = lim `div` 2

calcC :: Int -> Int -> Maybe Int
calcC a b = let 
    cc = 1+a*a+b*b
  in
  Data.Map.lookup cc m

iterateC :: Int -> Int -> Bool
iterateC a b = let
  c = calcC a b
  in
  case c of
    Just cs -> cs >= b && (a+b+cs <= lim)
    Nothing -> False

iterateBC :: Int -> Int
iterateBC a = let
    high = (lim-a) `div` 2
    bs = [a..high]
    solution = any (iterateC a) bs
  in
  case solution of
    True -> 1
    False -> 0 -- [(a,head cs)]

main :: IO ()
main = do
  --putStrLn $ show $ foldr (\n acc -> acc+iterateBC(n)) 0 [1..lh]
  putStrLn $ show $ sum( parMap rnf iterateBC [1..lh] )
  --putStrLn $ show $ sum( map iterateBC [1..lh] `using` parListChunk c rnf )
