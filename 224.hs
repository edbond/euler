{-# OPTIONS_GHC -O -fglasgow-exts -ddump-simpl-stats -optc-O3 -optc-ffast-math -fexcess-precision #-}
-- http://projecteuler.net/index.php?section=problems&id=224
--
-- Let us call an integer sided triangle with sides a ≤ b ≤ c barely obtuse if the sides satisfy
-- a^(2) + b^(2) = c^(2) - 1.
-- How many barely obtuse triangles are there with perimeter ≤ 75,000,000?

module Main
where

import qualified Debug.Trace
import Data.List

lim :: Integer
-- lim = 75000000
lim = 7500

{-# INLINE eq #-}
eq a b c = let
  l=a*a+b*b
  r=c*c-1
  in
  --Debug.Trace.traceShow (a,b,c, l==r) (l==r)
  l==r

calcC :: Integer -> Integer -> Float
calcC a b = let 
    aa = fromIntegral (a*a)
    bb = fromIntegral (b*b)
  in
  sqrt(1.0+aa+bb)

{-# INLINE isInt #-}
isInt :: Float -> Bool
isInt x = let
  r=(x == (fromIntegral . floor) x)
  in
  --Debug.Trace.traceShow (x,r) r
  r

{-# INLINE perim #-}
perim :: Integer -> Integer -> Integer -> Bool
perim a b c = let
  p=(a+b+c)
  pp = p <= lim
  in
  --Debug.Trace.traceShow (p,pp) pp
  pp

-- there is only one solution for a b
-- iterateC :: (Integral a) => a -> a -> [a]
iterateC a b = let
    --h = minimum [a+b-1, lim]
    --cs = [b..h]
    c = calcC a b
    ci = fromIntegral $ floor c
    p = perim a b ci
    i = isInt(c)
  in
  i && p
  --any (eq a b) cs

iterateBC a = let
    high = lim - a + 1
    bs = [a..high]
    solution = any (iterateC a) bs
  in
  case solution of
    True -> 1
    False -> 0 -- [(a,head cs)]

main = do
  putStrLn $ show $ foldr (\n acc -> acc+iterateBC(n)) 0 [1..lim]
