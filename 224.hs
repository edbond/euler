-- http://projecteuler.net/index.php?section=problems&id=224
--
-- Let us call an integer sided triangle with sides a ≤ b ≤ c barely obtuse if the sides satisfy
-- a^(2) + b^(2) = c^(2) - 1.
-- How many barely obtuse triangles are there with perimeter ≤ 75,000,000?

module Main
where

import qualified Data.Set as Set
import Bits (shiftL, shiftR)

lim :: Integer
-- lim = 75000000
lim = 7500

squares = Set.fromList [a*a | a <- [1..lim]]
--squares = [a*a | a <- [1..lim]]

isSquare :: Integer -> Bool
isSquare a = Set.member a squares
--isSquare a = elem a squares

--perim :: (Num a, Ord a) => a -> a -> a -> Bool
perim :: Integer -> Integer -> Integer -> Bool
perim a b c = let
  s :: Integer
  s = (a + b + c)
  in
  s <= lim

csqr :: Integer -> Integer -> Integer
csqr a b = 1+a*a+b*b

--sqrec :: Float -> Float -> Float
--sqrec r x = if abs (r * r - x) < 0.01 then r else sqrec i x
    --where i = (x / r + r) / 2

--fsqrt :: Float -> Float
--fsqrt x = sqrec 1.0 x

intsqr = floor . sqrt . fromIntegral

intSqrt :: Integer -> Integer
intSqrt 0 = 0
intSqrt n = newtonianIteration n (findx0 n 1)
	where
		-- find x0 == 2^(a+1), such that 4^a <= n < 4^(a+1).
		findx0 a b = if a == 0 then b else findx0 (a `shiftR` 2) (b `shiftL` 1)
		newtonianIteration n x =
			let x' = (x + n `div` x) `div` 2
			in if x' < x then newtonianIteration n x' else x

-- there is only one solution for a b
-- iterateC :: (Integral a) => a -> a -> [a]
iterateC a b = let
  --h = maximum [a,b]
  c = csqr a b
  cs = intsqr c -- intSqrt c
  p = perim a b cs
  in
  c==cs*cs && p

iterateBC a = let
  high = lim - a
  bs = [a..high]
  solution = any (iterateC a) bs
  in
  case solution of
    True -> 1
    False -> 0 -- [(a,head cs)]

main = do
  putStrLn $ show $ foldr (\n acc -> acc+iterateBC(n)) 0 [1..lim]
