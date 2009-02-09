-- http://projecteuler.net/index.php?section=problems&id=224
--
-- Let us call an integer sided triangle with sides a ≤ b ≤ c barely obtuse if the sides satisfy
-- a^(2) + b^(2) = c^(2) - 1.
-- How many barely obtuse triangles are there with perimeter ≤ 75,000,000?

module Main
where

import qualified Data.Set as Set

lim :: Integer
lim = 75000000

squares :: [Integer]
squares = takeWhile (<=lim) [a*a | a <- [1..]]

isSquare :: Integer -> Bool
isSquare a = let
  s = Set.fromList squares
  in
  Set.member a s

perim :: (Integral a) => a -> a -> a -> Bool
perim a b c = let
  s = (a + b + c)
  in
  s <= 75000000

csqr a b = 1+a*a+b*b

-- there is only one solution for a b
-- walk from max(a,b) to lim and check for square
-- iterateC :: (Integral a) => a -> a -> [a]
iterateC a b = let
  h = maximum [a,b]
  c = csqr a b
  p = perim a b c
  in
  case isSquare(c) && p of
    True -> [truncate $ sqrt $ fromIntegral c]
    _ -> []

iterateBC a = let
  high = lim - a
  bs = [a..high]
  all = map (iterateC a) bs
  cs = filter (not . null) all
  in
  case null cs of
    True -> []
    False -> head cs

main = do
  putStrLn $ show $ concat $ map iterateBC [1..lim]
