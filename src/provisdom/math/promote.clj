(ns provisdom.math.promote
  (:require [provisdom.math [core :as m]
             [special-functions :as mf]]
            [provisdom.utility-belt.core :as co]
            [clojure.math.numeric-tower :as nt]))

(set! *warn-on-reflection* true)

;;;for max precision, use rationals

(declare gen-continued-fraction')


;;;AUTO-PROMOTE ARITHMETIC 
(defn abs' [x] (nt/abs x))

(defn round' [x] (nt/round x)) ;;doesn't work on big numbers

(defn floor' [x] (nt/floor x))

(defn ceil' [x] (nt/ceil x))

(defn pow' [x y] (nt/expt x y))

(defn sqrt' [x] (nt/sqrt x))

(defn roughly-floor' [x accu] (floor' (+ x accu)))

(defn roughly-ceil' [x accu] (ceil' (- x accu)))

;;;MATH CONSTANTS
(def ^:const ^BigDecimal big-PI 
  "the value of PI to 100 digits" 
  3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679M)

(defn accu-PI 
  "the value of PI with p precision" 
  ^BigDecimal [p] 
  (when (>= p 101) (co/exc "You don't need more than 100 digits of PI!"
                           (var accu-PI))) 
  (round' big-PI p BigDecimal/ROUND_HALF_EVEN))

;;;GENERAL VECTOR MATH
(defn dot-product' 
  "Returns the dot product over of any number of collections"
  [& coll]
  (apply +' (apply map *' coll)))

;;;COUNTING
(def lazy-factorial 
  "lazy sequence of factorials"
  (reductions *' (iterate inc' 1)))

(defn factorial' 
  "Returns the factorial of x"
  [x]
  (cond 
    (zero? x) 1
    (neg? x) (m/exc- x 'factorial')
    (integer? x) (loop [x x f 1]
                   (if (m/one? x)
                     f
                     (recur (dec x) (long (*' f x)))))
    :else (mf/gamma (inc x))))

(defn choose-k-from-n' 
  "Returns the number of ways to choose k items out of n items"
  [k n]
  (letfn [(ms [s e] (reduce *' (range s e)))]
    (let [k (if (> k (* m/half n)) (- n k) k)] 
      (cond 
        (neg? k) (m/exc- k 'choose-k-from-n')
        (neg? n) (m/exc- n 'choose-k-from-n')
        (> k n) 0 
        (or (zero? k) (== k n)) 1
        (or (m/one? k) (== k (dec n))) n 
        :else (/ (ms (inc (- n k)) (inc' n)) (ms 2 (inc' k)))))))

(defn binomial-series-factor' 
  "Returns the k-th out of n binomial series factor"
  ([k n prob prob-factor reverse-prob-factor]
    (cond
      (neg? n) (m/exc- n 'binomial-series-factor')
      (< n k) (m/exc-out-of-range n 'binomial-series-factor')
      (neg? prob) (m/exc- prob 'binomial-series-factor')
      (> prob 1) (m/exc-out-of-range prob 'binomial-series-factor')
      :else (*' (choose-k-from-n' k n) (pow' prob prob-factor) 
                (pow' (m/rev prob) reverse-prob-factor))))      
  ([k n prob] (binomial-series-factor' k n prob k (- n k))))

;;;SERIES SOLVERS
(defn power-series' 
  "Returns the power series of a value x using a constant value c and a 
   term series: (a_n * (x - c)^n)"
  [term-series c x]
  (if (== x c) (first term-series) (map-indexed #(*' %2 (pow' (-' x c) %)) 
                                                term-series)))

(defn power-series-derivative' 
  "Returns the derivative of a power series"
  [term-series c x]
  (if (== x c) 0 
    (map-indexed #(*' %2 % (pow' (-' x c) (dec' %))) term-series)))

(defn power-series-integral' 
  "Returns the integral of a power series"
  [term-series c x integral-constant]
  (if (== x c) (+' integral-constant (*' (first term-series) x) 
                   (map-indexed #(*' %2 (/ (inc' %)) (pow' (-' x c) (inc' %))) 
                                term-series))))

(defn continued-fraction' 
  "Returns the continued-fraction series for a term series: 
   a0 + (1 / (a1 + 1 / (a2 + 1 / (a3 + ..."
  [[h & t]]
  (cons h
        (letfn [(f [[ch & ct :as c] kn2 kn1 m]
                   (if-let [an ch]
                     (let [kn (+' kn2 (*' an kn1)), v (/ m (*' kn1 kn))]
                       (lazy-seq (cons v (f ct kn1 kn (-' m)))))
                     c))]
          (f t 1 h -1))))

(defn gen-continued-fraction' 
  "Returns the generalized-continued-fraction series: 
   a0 + (b0 / (a1 + b1 / (a2 + b2 / (a3 + ..."
  [[h & t] b-term-series]
  (cons h
        (letfn [(f [[cbh & cbt] [cah & cat] b2 a2 b1 a1]
                   (if (or (nil? cbh) (nil? cah))
                     '()
                     (let [b0 (+' (*' cah b1) (*' cbh b2)), 
                           a0 (+' (*' cah a1) (*' cbh a2))]
                       (lazy-seq (cons (-' (/ b0 a0) (/ b1 a1)) 
                                       (f cbt cat b1 a1 b0 a0))))))]
          (f b-term-series t 1 0 h 1))))

;;;NUMERICAL DERIVATIVES AND INTEGRATION
(defn derivative-fn' 
  "Returns a numerical derivative function"
  ([f] (derivative-fn' f m/*sgl-close*))
  ([f dx] #(/ (-' (f (+' % dx)) (f %)) dx)))

(defn integrate'
  "Returns the integral of a function f over a range from a to b"
  ([f a b] 
    (integrate' f a b (pow' 10 m/*sgl-digits*)))
  ([f a b n]
    (let [dx (/ (-' b a) n)]
      (*' dx (reduce #(+' % (f (*' dx %2))) 0 (range (/ a dx) (/ b dx)))))))

;;;GAMMA AND BETA FUNCTIONS
(defn gamma' 
  "Returns the gamma of x: integral[0, inf] (t^(x-1) * e^-t * dt)"
  [x]
  (when (zero? x) (m/exc-out-of-range x (var gamma')))
  (if (m/roughly-round? x m/*dbl-close*) 
    (factorial' (int (dec x)))
    (mf/gamma x)))

(defn beta' 
  "Returns the beta of x and y: integral[0, 1] (t^(x-1) * (1-t)^(y-1) * dt"
  [x y]
  (when (zero? x) (m/exc-out-of-range x (var beta')))
  (when (zero? y) (m/exc-out-of-range y (var beta')))
  (* (gamma' x) (/ (gamma' y) (gamma' (+ x y)))))

