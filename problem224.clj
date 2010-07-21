;; Let us call an integer sided triangle with sides a ≤ b ≤ c
;; barely obtuse if the sides satisfy
;; a^(2) + b^(2) = c^(2) - 1.

;; How many barely obtuse triangles are there with perimeter ≤ 75,000,000?

(ns problem224)

(set! *warn-on-reflection* true)

(def *limit* 75000000)

(def *sqrt*
     (reduce #(into %1 {(* %2 %2) %2}) {} (range 1 (int (/ *limit* 3)))))

(defn solve
  [a b]
  (let [k (+ 1 (* a a) (* b b))
        c (*sqrt* k)]
    (if (and c (> c b) (< (+ a b c) *limit*))
      [a b c]
      nil)))

(defn walkb
  [a]
  (pmap #(solve a %) (range a *limit* 2)))

(defn solutions
  []
  (filter #(not (nil? %)) (doall (pmap walkb (range 2 *limit* 2)))))

(time (println (count (solutions))))

  ;; (for
  ;;     [a (range 2 *limit* 2)
  ;;      b (range a *limit* 2)
  ;;      :let [key (+ 1 (* a a) (* b b))]
  ;;      :when (and (contains? *sqrt* key) (> (*sqrt* key) b))]
  ;;   [a b]))



; (time (println (solutions)))
