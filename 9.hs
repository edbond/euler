{-
    A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
    a^(2) + b^(2) = c^(2)

    For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).

    There exists exactly one Pythagorean triplet for which a + b + c = 1000.
    Find the product abc.
-}

module Main
where

s = [(a,b,c) | a <- [1..1000], b <- [a..1000], let c=1000-b-a, a+b+c==1000, (a*a)+(b*b)==(c*c)]

main = do
    putStrLn $ "Hw"
