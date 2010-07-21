-- http://projecteuler.net/index.php?section=problems&id=7
--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6^(th) prime is 13.
-- What is the 10001^(st) prime number?


module Main
where

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

primes = filter isPrime [2..]

main = putStrLn $ show $ primes !! (10001-1)
