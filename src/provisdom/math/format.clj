(ns provisdom.math.format
  (require [clojure.string :as str]
           [provisdom.utility-belt.format :refer :all]
           [provisdom.math.core :as m]
           [taoensso.truss :as truss :refer (have have! have?)]))

(set! *warn-on-reflection* true)

(defn trim-number 
  "Trims number of any unnecessary characters, e.g. -0.3 and 0.30"
  [s]
  (let [s (cond (starts-with? s "-0") (trim-number 
                                        (str "-" (trim-start s "-0"))),
                (substring? "." s) (trim-end s "0")
                :else s)]
    (trim-start s "0")))

(defn- by-letter [s]
  (case s 
    "T" 1000000000000
    "B" 1000000000
    "M" 1000000
    "K" 1000))
    
(defn format-float 
  "Formats a number with a non-negative number of decimal places"
  [^double n ^long decimal-places]
  {:pre [(have? m/non-? decimal-places)]}
  (format (str "%." decimal-places "f") n))

(defn format-exponential 
  "Formats a number into exponential form with a number of digits"
  [n & [digits]]
  {:pre [(have? [:or nil? neg?] digits)]}
  (if digits
    (replace-string
      (replace-string
        (format (str "%." (long (dec digits)) "E") (double n))
        "E+0" "E+")
      "E-0" "E-")
    (let [s (replace-string
              (replace-string 
                (format (str "%." 15 "E") (double n)) 
                "E+0" "E+")
              "E-0" "E-")]
      (loop [s1 s]
        (if (substring? "0E" s1) (recur (replace-string s1 "0E" "E"))
          s1)))))

(defn format-number
  "Formats a number into its best form."
  [n max-length & {:keys [max-decimal-places max-digits]}]
  {:pre [(have? m/non-? max-length)]}
  (let [n (if-not max-decimal-places (double n)
            (double (read-string (format-float n max-decimal-places)))),
        stn (if-not max-digits (str n) (format-exponential n max-digits)),
        n (if-not max-digits n (read-string stn)),
        ml (min max-length (count stn)),
        sd (count (str (m/round n :type :toward))),
        g? (or (and (> sd ml) (> sd (+ 5.5 (* -0.5 (m/sgn n))))) 
               (< (m/abs n) 0.0001))]
    (loop [i (max ml 1)]
      (let [s (if g? (format-exponential n i) (format-float n (dec i)))]
        (if (or (m/one? i) (<= (count s) ml)) s
          (recur (dec i)))))))

;;;SHORTHAND
(defn unparse-shorthand 
  "Converts a number into shorthand."
  [n max-length & {:keys [max-decimal-places max-digits money?]}]
  (cond (m/nan? n) "NaN"
        (m/inf+? n) "Inf"
        (m/inf-? n) "-Inf"
        :else (let [n (if-not max-decimal-places (double n)
                        (double 
                          (read-string 
                            (format-float n max-decimal-places)))),
                    n (if-not max-digits n 
                        (read-string 
                          (format-exponential n max-digits))),
                    ab (m/abs n),
                    f (fn [x l]
                        (let [stn (str (format-number (/ x (by-letter l)) 
                                                      (dec max-length))),
                              stn (if (ends-with? stn ".0") 
                                    (butlast-string (butlast-string stn)) 
                                    stn)]
                          (str stn l))),
                    s (cond (>= ab 1e15) (format-number n max-length)
                            (>= ab (by-letter "T")) (f n "T")
                            (>= ab (by-letter "B")) (f n "B")
                            (>= ab (by-letter "M")) (f n "M")
                            (>= ab (by-letter "K")) (f n "K")
                            :else (format-number n max-length))]
                (if money? 
                  (if (neg? n) (str "-$" (rest-string s)) (str "$" s))
                  s))))

(defn parse-shorthand 
  "Converts a shorthand string into a number"
  [s]
  (let [s (if (starts-with? s "$") (rest-string s) s),
        s (if (starts-with? s "-$") (str "-" (trim-start s "-$")) s)]
    (case s 
      "NaN" m/nan
      "Inf" m/inf+
      "-Inf" m/inf-
      (let [f (fn [x l]
                (let [n (read-string (butlast-string x))]
                  (when (number? n) (* (by-letter l) n))))]
        (cond (ends-with? s "T") (f s "T")
              (ends-with? s "B") (f s "B")
              (ends-with? s "M") (f s "M")
              (ends-with? s "K") (f s "K")
              :else (let [n (read-string s)] (when (number? n) n)))))))

