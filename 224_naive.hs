-- http://projecteuler.net/index.php?section=problems&id=224
--
-- Let us call an integer sided triangle with sides a ≤ b ≤ c barely obtuse if the sides satisfy
-- a^(2) + b^(2) = c^(2) - 1.
-- How many barely obtuse triangles are there with perimeter ≤ 75,000,000?


{-# OPTIONS -funbox-strict-fields #-}

module Main
where

import Debug.Trace
import Control.Parallel.Strategies
import FastSQRT

lim = 7500

eq a b c = a*a+b*b == c*c-1
{-# INLINE eq #-}

iterAB a b = let
  l = max a b
  ch = truncate $ sqrt $ fromIntegral (a*a+b*b+1)
  h = minimum [a+b, lim-a-b, ch]

  cc :: Bool
  cc = any (eq a b) [l..h]
  --cc = elem True (parMap rnf (eq a b) [l..h])
  in
  cc
  --traceShow ("a",a, "b",b, "l",l, "h",h, "cc",cc) cc

iterA a = let
  l = a
  h = (lim-a) `div` 2
  b = any (iterAB a) [l..h]
  in
  case b of
    True -> 1
    False -> 0

main = putStrLn $ show $ sum $ map iterA [1..lim]
