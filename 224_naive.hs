-- http://projecteuler.net/index.php?section=problems&id=224
--
-- Let us call an integer sided triangle with sides a ≤ b ≤ c barely obtuse if the sides satisfy
-- a^(2) + b^(2) = c^(2) - 1.
-- How many barely obtuse triangles are there with perimeter ≤ 75,000,000?


{-# OPTIONS -funbox-strict-fields #-}

module Main
where

import Debug.Trace
import FastSQRT

lim :: Integer
lim = 7500

iterAB :: Integer -> Integer -> Bool
iterAB a b = let
	l = (a*a) + (b*b) + 1
	s = FastSQRT.hasIntSqrt $ fromIntegral l
	r = sqrt $ fromIntegral l
	rt = truncate r
	p = a+b+rt
	in
	case s of
		False -> False
		True -> (p < lim) && (fromIntegral rt) == r
		{- True -> traceShow (a,b,r,p) (p < lim) && (fromIntegral rt) == r -}

iterA a = let
  l = fromIntegral a
  h = ((lim-a) `div` 2)
  b = any (iterAB a) [l..h]
  in
  case b of
    True -> 1
    False -> 0

main = putStrLn $ show $ sum $ map iterA [1..lim]
