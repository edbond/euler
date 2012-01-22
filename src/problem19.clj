;; You are given the following information, but you may prefer to do some research for yourself.

;;     * 1 Jan 1900 was a Monday.
;;     * Thirty days has September,
;;       April, June and November.
;;       All the rest have thirty-one,
;;       Saving February alone,
;;       Which has twenty-eight, rain or shine.
;;       And on leap years, twenty-nine.
;;     * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

;; How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

(ns problem19)

(defn leap-year?
  [y]
  (if (zero? (rem y 100))
    (zero? (rem y 400))
    (zero? (rem y 4))))

(defn month-days
  [year month]
  (get [31
        (if (leap-year? year) 29 28)
        31
        30
        31
        30
        31
        31
        30
        31
        30
        31]
       month))

(defn dow
  ([year month]
     (let [num-days (dow year month 0)]
       (rem num-days 7)))
  ([year month days]
     ;; (println year month days)
     (if (and (= year 1900) (= month 0)) (inc days) ;; 1st Jan 1900 was Monday
         (let [prev-month (if (zero? month) 11 (dec month))
               prev-year (if (zero? month) (dec year) year)
               more-days (month-days prev-year prev-month)]
           ;; (println more-days days)
           (recur prev-year prev-month (+ days more-days))))))

(def m-dow (memoize dow))

(defn solve
  []
  (for [year (range 1901 2001) month (range 0 12) :when (= 0 (m-dow year month))] [year month]))
