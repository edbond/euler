-- http://projecteuler.net/index.php?section=problems&id=4
--
-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.
--
-- Usage: ./4 [-t] <num>
-- -t - run tests before run
-- num - number of digits

module Main
where

import Test.HUnit
import System
import System.Console.GetOpt

isPalindrome :: (Integral a) => a -> Bool
isPalindrome a = let s = show a in
  s == reverse s

allPalindromes n =
  let
    xs = reverse [10^(n-1)..10^n]
    ys = xs
  in
  filter (not . null . snd) $ map (\x -> (x, filter (isPalindrome . (*x)) ys) ) xs

calcLargestPalindrome n =
  let m = map (\x -> (fst x, head $ snd x)) (allPalindromes n) in
  maximum $ map (\x -> fst x * snd x) m

-- n is ^
--run :: (Integral n) => n -> IO ()
run n =
  let x = calcLargestPalindrome n in
  putStrLn $ show x

main = run 3

{--
--
--

data Flag = Test deriving (Show)

options :: [OptDescr Flag]
options =
  [
    Option ['t']     ["test"] (NoArg Test)       "test before run"
  ]

testPalindrome = TestCase $ assertEqual
  "Should return True if number is palindrome"
  True
  (isPalindrome 9009)

tests = TestList [testPalindrome]

--runTest :: IO Bool
runTest = do
  result <- runTestTT tests
  return $ case failures result + errors result of
    0 -> True
    _ -> False

testAndRun n = do
  r <- runTest
  return $ case r of
    True ->
      putStrLn "test ok!" >> (run n)
    _ ->
      putStrLn "test failed!"

-- getOpt returns a triple consisting of the option arguments, a list of non-options, and a list of error messages. 
main = do
  argv <- getArgs
  case getOpt Permute options argv of
    ([Test], [n], []) -> testAndRun $ read n
    ([], [n], []) -> return $ run $ read n
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: 4 [OPTION...] number"

--}
