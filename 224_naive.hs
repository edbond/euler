module Main
where

import GHC.Base
import GHC.Float

lim = 7500

eq a b c = a*a+b*b == c*c-1

--perim :: Int# -> Int# -> Int# -> Bool
perim a b c = a+b+c <= lim
{-# INLINE perim #-}

iterAB a b = let
  l = max a b
  h = min (a+b) (lim-a-b)
  cc = any (\c -> (perim a b c) && (eq a b c)) [l..h]
  in
  cc

iterA a = let
  l = a
  h = lim-a
  b = any (iterAB a) [l..h]
  in
  case b of
    True -> 1
    False -> 0

main = putStrLn $ show $ sum $ map iterA [1..lim]
