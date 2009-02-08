-- http://projecteuler.net/index.php?section=problems&id=224
--
-- Let us call an integer sided triangle with sides a ≤ b ≤ c barely obtuse if the sides satisfy
-- a^(2) + b^(2) = c^(2) - 1.
-- How many barely obtuse triangles are there with perimeter ≤ 75,000,000?

module Main
where

--lim :: Integer
lim = 75000000::Integer

eq :: (Integral a) => a -> a -> a -> Bool
eq a b c = let
  l = a*a + b*b
  r = c*c - 1
  in l == r

perim :: (Integral a) => a -> a -> a -> Bool
perim a b c = let
  s = (a + b + c)
  in
  s <= 75000000

iterateC a b = let
  maxab = maximum [a,b]
  h = lim - a - b
  cs = [maxab..h]
  css = takeWhile (perim a b) cs
  in
  filter (eq a b) css

--iterateB :: (Integral a) => a -> [a]
iterateBC a = let
  high = lim - a
  bs = [a..high]
  cs = map (iterateC a) bs
  in 
  cs -- takeWhile perim 

main = putStrLn $ show $ iterateBC 1
