;; The following iterative sequence is defined for the set of positive integers:
;; n → n/2 (n is even)
;; n → 3n + 1 (n is odd)

;; Using the rule above and starting with 13, we generate the following sequence:
;; 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

;; It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
;; Which starting number, under one million, produces the longest chain?
;; NOTE: Once the chain starts the terms are allowed to go above one million.

(ns problem14
  (:use clojure.test))

(set! *warn-on-reflection* true)

(defn next-n
  [n]
  (if (odd? n)
    (inc (* 3 n))
    (/ n 2)))

(def memo-next-n (memoize next-n))

(defn chain
  ([n] (chain n ()))
  ([n l]
     (if (= n 1)
       (reverse (cons n l))
       (chain (next-n n) (cons n l)))))

(def limit 1000000)

(defn solve
  []
  (let [coll (pmap (fn [x] {:count (count (chain x)) :n x}) (range limit 1 -1))]
    (reduce (fn [x y] (max-key :count x y)) coll)))

;; test
(defn test-problem14
  []
  (is (= (chain 13) '(13 40 20 10 5 16 8 4 2 1)))
  (is (= (chain 1) '(1))))

(test-problem14)
(time (println (solve)))
