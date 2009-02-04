-- naive
-- ./3 600851475  2.48s user 0.04s system 96% cpu 2.605 total

module Main
where

import System

-- решето Эратосфена

-- удаляет из списка числа, которые делятся на x
removeMults :: (Integral a) => a -> [a] -> [a]
removeMults x = filter (\f -> f `rem` x /= 0)

sieve :: (Integral a) => a -> [a] -> [a] -> [a]
sieve _ x [] = x
sieve h e (b:bs) | b*b > h = e ++ bs
sieve h e (b:bs) = let
  r = removeMults b bs in
  sieve h (e ++ [b]) r

sieveUntil :: (Integral a) => a -> [a]
sieveUntil a = sieve a [] [2..a]

-- The prime factors of 13195 are 5, 7, 13 and 29.
primeFactors :: (Integral a) => a -> [a]
primeFactors x = filter (\f -> x `rem` f == 0) primes
  where primes = sieveUntil(x)

largestPrimeFactor :: (Integral a) => (a -> a)
largestPrimeFactor = last . primeFactors

-- 600851475143
main = do
  argv <- getArgs
  let
    num = head argv
    result = largestPrimeFactor $ (read num)::Integer in
    putStrLn $ show result
