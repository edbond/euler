module Main
where

import FastSQRT
import Test.BenchPress

std :: IO ()
std =
  let
    x = map sqrt [1..75000000]
  in
  return ()

my :: IO ()
my =
  let
    x = map fsqrt [1..75]
  in
  return ()

main = benchMany 100000 $ [("standard", std), ("my", my)]
