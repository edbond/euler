(ns problem23)

(def upper-limit 28124)

(defn divisors
  [n]
  (filter #(zero? (mod n %)) (range 1 n)))

(defn abundant?
  [n]
  (> (reduce + (divisors n))
     n))

(def abundant-numbers
  (filter abundant? (range 1 upper-limit)))

(def sums-of-two-abundant-numbers
  (set (for [a abundant-numbers
             b abundant-numbers
             :when (>= b a)] (+ a b))))

(defn solve
  []
  (let [numbers (set (range 1 upper-limit))]
    (reduce +
            (disj numbers sums-of-two-abundant-numbers))))