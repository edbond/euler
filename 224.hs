{-# OPTIONS_GHC -O -fglasgow-exts -optc-O3 -optc-ffast-math -fvia-C #-}
-- http://projecteuler.net/index.php?section=problems&id=224
--
-- Let us call an integer sided triangle with sides a ≤ b ≤ c barely obtuse if the sides satisfy
-- a^(2) + b^(2) = c^(2) - 1.
-- How many barely obtuse triangles are there with perimeter ≤ 75,000,000?

module Main
where

import qualified Debug.Trace
import Data.List
import FastSQRT

lim :: Int
-- lim = 75000000
lim = 7500

eq :: (Num a) => a -> a -> a -> Bool
eq a b c = let
  l=a*a+b*b
  r=c*c-1
  in
  --Debug.Trace.traceShow (a,b,c, l==r) (l==r)
  l==r

calcC :: Int -> Int -> (Int, Int)
calcC a b = let 
    aa = a*a
    bb = b*b
    cs = 1+aa+bb
    c = fsqrt cs
  in
  --Debug.Trace.traceShow (a,b,cs) c
  c

{-# INLINE isInt #-}
isInt :: Float -> Bool
isInt x = let
  d = snd $ properFraction x -- x == (fromIntegral . round) x
  in d == 0

perim :: Int -> Int -> Int -> Bool
perim a b c = let
  p=(a+b+c)
  pp = p <= lim
  in
  --Debug.Trace.traceShow (p,pp) pp
  pp

-- there is only one solution for a b
iterateC :: Int -> Int -> Bool
iterateC a b = let
    --h = minimum [a+b-1, lim]
    --cs = [b..h]
    c = calcC a b
    ci = snd c
    p = perim a b ci
    i = fst c == 0
  in
  p && i
  --any (eq a b) cs

iterateBC :: forall t. (Num t) => Int -> t
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
  putStrLn $ show $ foldr (\n acc -> acc+iterateBC(n)) 0 [1..lim]
