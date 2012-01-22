(ns problem23-test
  (:use
   problem23
   [lazytest.describe :only (describe it)]))

(describe "Problem 23"
  (it "can find proper divisors"
    (= [1 2 4 7 14] (divisors 28)))
  
  (it "knows that 12 is the smallest abundant number"
    (= true (some #(= 12 %) abundant-numbers)))
  
  (it "knows that 24 can be written as sum of two abundant numbers"
    (= true (sum-of-two-abundant-number? 24))))