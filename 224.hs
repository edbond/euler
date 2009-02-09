-- http://projecteuler.net/index.php?section=problems&id=224
--
-- Let us call an integer sided triangle with sides a ≤ b ≤ c barely obtuse if the sides satisfy
-- a^(2) + b^(2) = c^(2) - 1.
-- How many barely obtuse triangles are there with perimeter ≤ 75,000,000?

module Main
where

import qualified Data.Set as Set

lim :: Integer
-- lim = 75000000
lim = 75000

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

csqr a b = 1+a*a+b*b

-- there is only one solution for a b
-- iterateC :: (Integral a) => a -> a -> [a]
iterateC a b = let
  h = maximum [a,b]
  c = csqr a b
  p = perim a b c
  in
  case isSquare(c) && p of
    True -> [c] -- [(b, truncate $ sqrt $ fromIntegral c)]
    _ -> []

iterateBC a = let
  high = lim - a
  bs = [a..high]
  all = map (iterateC a) bs
  cs = dropWhile null all
  in
  case null cs of
    True -> 0
    False -> 1 -- [(a,head cs)]

main = do
  putStrLn $ show $ foldr (\n acc -> acc+iterateBC(n)) 0 [1..lim]
