module Main
where

fib :: Num a => a -> a
fib 1 = 1
fib 2 = 2
fib n = fib(n-1) + fib(n-2)

lessThan :: Ord a => a -> [a] -> [a]
lessThan x = takeWhile (\n -> n < x)

main = let arr = lessThan 4000000 (map fib [1..]) in
    putStrLn $ show $ sum( filter (\a -> a `rem` 2 == 0) arr)
