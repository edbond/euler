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

import Data.List

divides :: (Integral a) => a -> a -> Bool
a `divides` b = a `rem` b == 0

isPrime :: (Integral a) => a -> Bool
isPrime 0 = False
isPrime 1 = True
isPrime 2 = True
isPrime x = let
  up = ceiling . sqrt $ fromIntegral x
  in
  all (not . divides x) [2..up]

primesBelow :: (Integral a) => a -> [a]
primesBelow x = filter isPrime [2..x]

sumIsPrime :: (Integral a) => [a] -> Bool
sumIsPrime = isPrime . sum

sumIsPrimeAndBelow :: (Integral a) => a -> [a] -> Bool
sumIsPrimeAndBelow n a = let s = sum(a) in
  s < n && isPrime s

sortByLength :: (Integral a) => [a] -> [a] -> Ordering
sortByLength a b | length a > length b = GT
sortByLength a b | length a == length b = EQ
sortByLength a b | length a < length b = LT

findLongestRun :: (Integral a) => [a] -> a -> [a]
findLongestRun ps n = let
    lss = map inits (tails ps)
    f = filter (sumIsPrimeAndBelow n) (concat lss)
  in
  last $ sortBy sortByLength f

main = putStrLn $ show $ sum $ findLongestRun (primesBelow(10000)) 1000000
