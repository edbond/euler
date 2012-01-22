;; If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

;; If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

;; NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

(ns problem17)

;; copied from cl-format.clj
(def ^{:private true}
     english-cardinal-units 
     ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
      "ten" "eleven" "twelve" "thirteen" "fourteen"
      "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"])

(def ^{:private true}
     english-cardinal-tens
     ["" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])

(defn- format-simple-cardinal
  "Convert a number less than 1000 to a cardinal english string"
  [num]
  (let [hundreds (quot num 100)
        tens (rem num 100)]
    (str
     (if (pos? hundreds) (str (nth english-cardinal-units hundreds) " hundred"))
     (if (and (pos? hundreds) (pos? tens)) " and ")
     (if (pos? tens) 
       (if (< tens 20) 
         (nth english-cardinal-units tens)
         (let [ten-digit (quot tens 10)
               unit-digit (rem tens 10)]
           (str
            (if (pos? ten-digit) (nth english-cardinal-tens ten-digit))
            (if (and (pos? ten-digit) (pos? unit-digit)) "-")
            (if (pos? unit-digit) (nth english-cardinal-units unit-digit)))))))))


(defn solve
  []
  (->> (range 1 1000)
       (map format-simple-cardinal)
       (interpose " ")
       (cons " one thousand ")
       (apply str)
       (re-seq #"\w")
       (count)))
