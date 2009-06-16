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

lim = 7500

{- iterAB :: (Integral a) => a -> a -> Bool -}
iterAB a b = let
	l = ((a*a) + (b*b) + 1) :: Integer
	in
	FastSQRT.hasIntSqrt $ fromIntegral l

iterA a = let
  l = a :: Integer
  h = ((lim-a) `div` 2) :: Integer
  b = any (iterAB a) [l..h]
  in
  case b of
    True -> 1
    False -> 0

main = putStrLn $ show $ sum $ map iterA [(1::Integer)..lim]
