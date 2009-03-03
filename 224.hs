-- http://projecteuler.net/index.php?section=problems&id=224
--
-- Let us call an integer sided triangle with sides a ≤ b ≤ c barely obtuse if the sides satisfy
-- a^(2) + b^(2) = c^(2) - 1.
-- How many barely obtuse triangles are there with perimeter ≤ 75,000,000?

module Main
where

--import qualified Debug.Trace

lim :: Int
-- lim = 75000000
lim = 7500

calcC :: Int -> Int -> Float
calcC a b = let 
    aa = a*a
    bb = b*b
    cs = 1+aa+bb
    c = sqrt cs
  in
  c::Float
  --Debug.Trace.traceShow (a,b,cs) c

isInt :: Float -> Bool
isInt x = (fromIntegral x) == (truncate x)

iterateBC :: Int -> Int
iterateBC a = let
    high = lim-a+1
    bs = [a..high]
    solution = any (\b -> isInt $ calcC a b) bs
  in
  case solution of
    True -> 1
    False -> 0 -- [(a,head cs)]

main :: IO ()
main = do
  --putStrLn $ show $ foldr (\n acc -> acc+iterateBC(n)) 0 [1..lim]
  putStrLn $ show $ sum $ map iterateBC [1..lim]
