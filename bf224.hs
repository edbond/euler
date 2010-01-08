-- http://projecteuler.net/index.php?section=problems&id=224
--
-- Let us call an integer sided triangle with sides a ≤ b ≤ c barely obtuse if the sides satisfy
-- a^(2) + b^(2) = c^(2) - 1.
-- How many barely obtuse triangles are there with perimeter ≤ 75,000,000?

-- -O2 -fvia-C -optc-O2

module Main
where

lim :: Int
lim = 7500

cnt :: Int
cnt = sum [ 1 | a <- [2,4..lim], b <- [a,(a+2)..lim], c <- [(b+1),(b+3)..lim], a<=b, b<=c, (a+b+c) <= lim, (a*a)+(b*b)==(c*c)-1 ]

main :: IO ()
main = do
  putStrLn $ show $ cnt
