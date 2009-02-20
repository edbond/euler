module FastSQRT
where

import Text.Printf
import Debug.Trace
import Test.BenchPress

int2sp o = let
  s :: String
  s = printf "%i" o
  l = length s
  in
  if odd l then
    '0':s
  else
    s

topairs x = map (take 2) . takeWhile (not . null) . iterate (drop 2) $ x

-- returns remainder and sqrt
solve r [] p = (r,p)
solve rem pairs p = let
  c :: Int
  c = rem*100 + (read $ head pairs)
  x = last $ takeWhile (\z -> (20*p+z)*z <= c) [0..]
  y = (20*p+x)*x
  l = length pairs
  newp = p*10+x
  newrem = c-y
  in
  solve newrem (tail pairs) newp
  --traceShow (x, y, p, newp, c, newrem) solve newrem (tail pairs) newp

fsqrt :: Int -> (Int,Int)
fsqrt i = let
  pairs = topairs $ int2sp i
  in
  solve 0 pairs 0

std :: IO ()
std =
  let
    x = map sqrt [1..10000000000]
  in
  return ()

my :: IO ()
my =
  let
    x = map fsqrt [1..10000000000]
  in
  return ()

main = benchMany 1000000 $ [("standard", std), ("my", my)]
