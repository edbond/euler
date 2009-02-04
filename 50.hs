-- http://projecteuler.net/index.php?section=problems&id=50
--
-- The prime 41, can be written as the sum of six consecutive primes:
-- 41 = 2 + 3 + 5 + 7 + 11 + 13
--
-- This is the longest sum of consecutive primes that adds to a prime below one-hundred.
-- The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
-- Which prime, below one-million, can be written as the sum of the most consecutive primes?

module Main
where

isPrime :: (Integral a) => a -> Bool
isPrime x = let
  u = ceiling . sqrt $ fromIntegral x
  in
  all (\f -> x `rem` f /= 0) [2..u]

--main = putStrLn "50"
