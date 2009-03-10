-- http://projecteuler.net/index.php?section=problems&id=224
--
-- Let us call an integer sided triangle with sides a ≤ b ≤ c barely obtuse if the sides satisfy
-- a^(2) + b^(2) = c^(2) - 1.
-- How many barely obtuse triangles are there with perimeter ≤ 75,000,000?

module Main
where

import qualified Debug.Trace
import Control.Parallel
import Control.Parallel.Strategies

lim :: Int
-- lim = 75000000
lim = 7500
lh = lim `div` 3
c = 100

calcC :: Int -> Int -> Float
calcC a b = let 
    aa = a*a
    bb = b*b
    cs :: Float
    cs = (fromIntegral $ 1+aa+bb)::Float
    c = sqrt cs
  in
  c
  --Debug.Trace.traceShow (a,b,cs) c

isInt :: Float -> Bool
isInt x = x == fromIntegral (truncate x)

iterateC :: Int -> Int -> Bool
iterateC a b = let
  c = calcC a b
  ct = truncate c
  i = isInt c
  --r = i `par` c `pseq` ct `pseq` ct >= b && i && (a+b+ct <= lim)
  r = ct >= b && i && (a+b+ct <= lim)
  in
  r
  --case r of
    --True -> Debug.Trace.traceShow (a,b,ct) True
    --False -> False

iterateBC :: Int -> Int
iterateBC a = let
    high = lim-a+1
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
