(ns provisdom.math.format
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [clojure.string :as str]
    [taoensso.truss :as truss :refer [have have! have?]]
    [provisdom.utility-belt.format :as u]
    [provisdom.math.core :as m]))

(defn trim-number
  "Trims number of any unnecessary characters e.g. -0.3 and 0.30"
  [s]
  (let [s (cond (u/starts-with? s "-0") (trim-number (str "-" (u/trim-start s "-0")))
                (u/substring? "." s) (u/trim-end s "0")
                :else s)]
    (u/trim-start s "0")))

(def by-letter {"T" 1000000000000
                "B" 1000000000
                "M" 1000000
                "K" 1000})

(defn format-float
  "Formats a number with a non-negative number of decimal places"
  [^double n ^long decimal-places]
  {:pre [(have? m/non-? decimal-places)]}
  (format (str "%." decimal-places "f") n))

(defn format-exponential
  "Formats a number into exponential form with a number of digits"
  [n & [digits]]
  {:pre [(have? [:or nil? m/non-?] digits)]}
  (if digits
    (u/replace-string
      (u/replace-string
        (format (str "%." (long (dec digits)) "E")
                (double n))
        "E+0" "E+")
      "E-0" "E-")
    (let [s (u/replace-string
              (u/replace-string
                (format (str "%." 15 "E") (double n))
                "E+0" "E+")
              "E-0" "E-")]
      (loop [s1 s]
        (if (u/substring? "0E" s1)
          (recur (u/replace-string s1 "0E" "E"))
          s1)))))

(defn format-number
  "Formats a number into its best form."
  [n max-length & {:keys [max-decimal-places max-digits]}]
  {:pre [(have? m/non-? max-length)]}
  (let [n (double (if max-decimal-places
                    (read-string (format-float n max-decimal-places))
                    n))
        stn (if max-digits
              (format-exponential n max-digits)
              (str n))
        n (if max-digits
            (read-string stn)
            n)
        ml (min max-length (count stn))
        sd (count (str (m/round n :toward-zero)))
        g? (or (and (> sd ml)
                    (> sd (+ 5.5 (* -0.5 (m/sgn n)))))
               (< (m/abs n) 0.0001))]
    (loop [i (max ml 1)]
      (let [s (if g?
                (format-exponential n i)
                (format-float n (dec i)))]
        (if (or (m/one? i) (<= (count s) ml))
          s
          (recur (dec i)))))))

;;;SHORTHAND
(defn unparse-shorthand
  "Converts a number into shorthand."
  [n max-length & {:keys [max-decimal-places max-digits money?]}]
  (cond (m/nan? n) "NaN"
        (m/inf+? n) "Inf"
        (m/inf-? n) "-Inf"
        :else (let [n (double (if max-decimal-places
                                (read-string
                                  (format-float n max-decimal-places))
                                n))
                    n (if max-digits
                        (read-string
                          (format-exponential n max-digits))
                        n)
                    ab (m/abs n)
                    f (fn [x l]
                        (let [stn (str (format-number (/ x (by-letter l))
                                                      (dec max-length)))
                              stn (if (u/ends-with? stn ".0")
                                    (u/butlast-string
                                      (u/butlast-string stn))
                                    stn)]
                          (str stn l)))
                    s (cond (>= ab 1e15) (format-number n max-length)
                            (>= ab (by-letter "T")) (f n "T")
                            (>= ab (by-letter "B")) (f n "B")
                            (>= ab (by-letter "M")) (f n "M")
                            (>= ab (by-letter "K")) (f n "K")
                            :else (format-number n max-length))]
                (if money?
                  (if (neg? n)
                    (str "-$" (u/rest-string s))
                    (str "$" s))
                  s))))

(defn parse-shorthand
  "Converts a shorthand string into a number"
  [s]
  (let [s (if (u/starts-with? s "$")
            (u/rest-string s)
            s)
        s (if (u/starts-with? s "-$")
            (str "-" (u/trim-start s "-$"))
            s)]
    (case s
      "NaN" m/nan
      "Inf" m/inf+
      "-Inf" m/inf-
      (let [f (fn [x l]
                (let [n (read-string (u/butlast-string x))]
                  (when (number? n)
                    (* (by-letter l) n))))]
        (cond (u/ends-with? s "T") (f s "T")
              (u/ends-with? s "B") (f s "B")
              (u/ends-with? s "M") (f s "M")
              (u/ends-with? s "K") (f s "K")
              :else (let [n (read-string s)]
                      (when (number? n)
                        n)))))))

