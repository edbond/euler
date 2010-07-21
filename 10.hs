{-
    The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
    Find the sum of all the primes below two million.
-}


module Main
where

isPrime :: (Integral a) => a -> Bool
isPrime x = let
  c = ceiling . sqrt $ fromIntegral x
  in
    all (\f -> x `rem` f /= 0) [2..c]

primesUntil :: (Integral a) => a -> [a]
primesUntil a = 2 : filter isPrime [2..a]
