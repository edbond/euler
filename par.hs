module Main
where

import Control.Parallel.Strategies
import Data.Time.Clock

h = 10^8

par = let
    x = map (*2) [1..h]
  in
  return $ last x

strait = let
    x = map (*2) [1..h]
  in
  return $ last x

main = -- benchMany 10000 [("strait", strait), ("par", par)]
  do
    s1 <- getCurrentTime
    a <- strait
    s2 <- getCurrentTime

    d1 <- getCurrentTime
    b <- par
    d2 <- getCurrentTime

    putStrLn $ show a
    putStrLn $ show $ (utctDayTime s2) - (utctDayTime s1)
    putStrLn $ show b
    putStrLn $ show $ (utctDayTime d2) - (utctDayTime d1)
