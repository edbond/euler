-- http://projecteuler.net/index.php?section=problems&id=5
--
-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?

module Main
where

import Data.List
import System

dividesAt :: (Integral a) => a -> a -> Bool
x `dividesAt` n = x `rem` n == 0

dividesAll :: (Integral a) => a -> [a]  -> Bool
dividesAll x y = all (x `dividesAt`) y

smallestDivisibleBy :: (Integral a) => a -> Maybe a
smallestDivisibleBy n = find (\x -> x `dividesAll` [1..n]) [n..]

main = do
  argv <- getArgs
  let
    max = read $ head argv
    result = smallestDivisibleBy (max::Integer)
    in
    case result of
      Just x -> putStrLn $ show x
      _ -> putStrLn "fail"
