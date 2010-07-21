-- http://projecteuler.net/index.php?section=problems&id=6
--
-- The sum of the squares of the first ten natural numbers is,
-- 1^(2) + 2^(2) + ... + 10^(2) = 385

-- The square of the sum of the first ten natural numbers is,
-- (1 + 2 + ... + 10)^(2) = 55^(2) = 3025

-- Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

module Main
where

sumsq :: (Integral a) => a -> a
sumsq n = sum $ map (^2) [1..n]

sqsum :: (Integral a) => a -> a
sqsum n = (sum [1..n])^2

--main = putStrLn $ show (sqsum(10) - sumsq(10))
main = putStrLn $ show (sqsum(100) - sumsq(100))
