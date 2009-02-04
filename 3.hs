--- http://projecteuler.net/index.php?section=problems&id=3
---
--- The prime factors of 13195 are 5, 7, 13 and 29.
--- What is the largest prime factor of the number 600851475143 ?
---
--- naive:
--- ./3 600851475  2.48s user 0.04s system 96% cpu 2.605 total

module Main
where

import System

isPrime :: (Integral a) => a -> Bool
isPrime x = all (\f -> x `rem` f /= 0) [2..x-1]

primesUntil :: (Integral a) => a -> [a]
primesUntil a = filter isPrime [2..a]

primeFactors :: (Integral a) => a -> [a]
primeFactors a =
  let
    s = ceiling . sqrt $ fromIntegral a
    p = primesUntil s
  in
    filter (\f -> a `rem` f == 0) p

largestPrimeFactor :: (Integral a) => (a -> a)
largestPrimeFactor = head . reverse . primeFactors

main = do
  argv <- getArgs
  let
    num = head argv
    result = largestPrimeFactor $ (read num)::Integer in
    putStrLn $ show result

test :: IO ()
test =
  case largestPrimeFactor 13195 of
    29 -> putStrLn "OK"
    otherwise -> putStrLn "FAIL!"
