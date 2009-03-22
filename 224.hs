-- http://projecteuler.net/index.php?section=problems&id=224
--
-- Let us call an integer sided triangle with sides a ≤ b ≤ c barely obtuse if the sides satisfy
-- a^(2) + b^(2) = c^(2) - 1.
-- How many barely obtuse triangles are there with perimeter ≤ 75,000,000?

-- -O2 -fvia-C -optc-O2

module Main
where

import Data.List
import Debug.Trace
--import Data.IntSet
--import Data.Map

import Control.Parallel
import Control.Parallel.Strategies

lim :: Int
-- lim = 75000000
lim = 75000000

lh :: Int
lh = (lim `div` 3)

chunk :: Int
chunk = 100

eq :: Int -> Int -> Int -> Bool
eq a c b = let
  s = (a*a) + (b*b) == (c*c)-1
  in
  s
  --case s of
    --True -> traceShow ("a", a, "b", b, "c",c, "perim",a+b+c) True
    --False -> False

iSqrt :: Int -> Int
iSqrt = truncate . fSqrt

fSqrt :: Int -> Float
fSqrt = sqrt . fromIntegral

isSqr' :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
isSqr' _ 0 _ x _ _ = isSqr x
isSqr' 1 _ x _ _ _ = x `mod` 4 == 0
isSqr' 4 _ _ _ x _ = even x
isSqr' 9 _ x _ _ _ = x `mod` 4 == 0
isSqr' 6 _ _ _ x _ = odd x
isSqr' _ 25 _ _ a b = (a==0) || (a==2) || (b==06) || (b==56) || False
isSqr' _ _ _ _ _ _ = False

isSqr :: Int -> Bool
isSqr 1 = True
isSqr n | n <= 0 = False

isSqr n = let
  last = n `mod` 10 -- aaax
  last2 = n `mod` 100 -- aaxx
  before = n `div` 10 -- xxxa
  before2 = n `div` 100 -- xxaa
  prev = (n `mod` 100) `div` 10 -- aaxa
  prev2 = (n `mod` 1000) `div` 10 -- axxa
  t1 = isSqr' last last2 before before2 prev prev2
  in
  t1

iterateB :: Int -> Int -> Bool
iterateB a c = let
  b2 = (c*c)-(a*a)-1
  s = (b2 <= (c*c)) && ((a*a) <= b2) && isSqr b2

  b :: Int
  b = iSqrt b2
  perim = a+b+c
  t2 :: Bool
  t2 = ((fromIntegral . iSqrt) b2) == (fSqrt b2)
  in
  case s of
    True -> (perim <= lim) && t2
    False -> False
  --b2 <= (c*c) && (a*a) <= b2

iterateBC :: Int -> Int
iterateBC a = let
    high = lim-(a*2)
    cs = [a+1,a+3..high]
    solution = any (iterateB a) cs
  in
  case solution of
    True -> 1
    False -> 0 -- [(a,head cs)]

main :: IO ()
main = do
  --putStrLn $ show $ foldr (\n acc -> acc+iterateBC(n)) 0 [2,4..lh]
  --putStrLn $ show $ sum( parMap rnf iterateBC [2,4..lh] )
  putStrLn $ show $ sum( map iterateBC [2,4..lh] `using` parListChunk chunk rnf )
