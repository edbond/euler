-- http://projecteuler.net/index.php?section=problems&id=224
--
-- Let us call an integer sided triangle with sides a ≤ b ≤ c barely obtuse if the sides satisfy
-- a^(2) + b^(2) = c^(2) - 1.
-- How many barely obtuse triangles are there with perimeter ≤ 75,000,000?

module Main
where

isInt :: (RealFloat a) => a -> Bool
isInt c = let
  l = c
  r = fromIntegral $ truncate c
  in
  l == r

calcC :: (Integral a, Floating b) => a -> a -> b
calcC a b = let
  af = fromIntegral a
  bf = fromIntegral b
  in
  sqrt $ 1 + af*af + bf*bf

lim = 75000000::Integer

--iterateB :: (Integral a) => a -> [a]
iterateB a = let
  high :: Integer
  high = lim - a
  bs = [a..high]
  cs = map ((calcC a) . fromIntegral) bs
  in 
  filter isInt cs -- map isInt cs

main = putStrLn "224"
