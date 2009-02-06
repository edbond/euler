-- http://projecteuler.net/index.php?section=problems&id=4
--
-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

module Main
where

import Test.HUnit
import System
import System.Console.GetOpt

isPalindrome :: (Integral a) => a -> Bool
isPalindrome a = let s = show a in
  s == reverse s

run :: n -> IO ()
run n = putStrLn "running..."

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
runTest = do
  result <- runTestTT tests
  case result.errors of
    0 -> True
    _ -> False

testAndRun n = case runTest of
  True ->
    putStrLn "test ok!" >> run n
  _ ->
    putStrLn "test failed!"

-- getOpt returns a triple consisting of the option arguments, a list of non-options, and a list of error messages. 
main = do
  argv <- getArgs
  case getOpt Permute options argv of
    ([Test], [n], []) -> testAndRun(n)
    ([], [n], []) -> run(n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: 4 [OPTION...] number"
