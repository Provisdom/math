(ns provisdom.math.combinatorics
  (:require [provisdom.utility-belt.core :as co]
            [provisdom.math [core :as m]
             [special-functions :as mf]
             [matrix :as mx]]
            [clojure.math.combinatorics :as cmc]
            [clojure.core.reducers :as ccr]
            [taoensso.truss :as truss :refer (have have! have?)])
  (:import [cern.jet.math.tdouble DoubleArithmetic]))

(set! *warn-on-reflection* true)

;;;CONSTANTS
(def ^:private ^:const subfactorials 
  "also called 'recontres numbers' or 'derangements'"
  [1, 0, 1, 2, 9, 44, 265, 1854, 14833, 133496, 1334961, 14684570, 176214841, 
   2290792932, 32071101049, 481066515734, 7697064251745, 130850092279664, 
   2355301661033953, 44750731559645106, 895014631192902121, 
   18795307255050944540])

(def ^:private ^:const bell-numbers
  [1, 1, 2, 5, 15, 52, 203, 877, 4140, 21147, 115975, 678570, 4213597, 
   27644437, 190899322, 1382958545, 10480142147, 82864869804, 682076806159, 
   5832742205057, 51724158235372, 474869816156751, 4506715738447323, 
   44152005855084346, 445958869294805289, 4638590332229999353, 
   49631246523618756274])

(defn factorial
  "Returns the factorial of x.
Returns long if possible."
  [^double x]
  {:pre [(have? m/non-? x)]}
  (m/maybe-long-able (mf/gamma (inc x))))

(defn log-factorial
  "Returns the log-factorial of x"
  ^double [^double x]
  {:pre [(have? m/non-? x)]}
  (mf/log-gamma (inc x)))

(defn subfactorial
  "Returns the subfactorial of x.
The number of ways that n objects can be arranged where no object appears in 
   its natural position (known as 'derangements.')"
  [^double x]
  {:pre [(have? m/non-? x)]}
  (if (and (m/long-able? x) (< x 22)) (subfactorials (m/round x))
    (m/round (/ (factorial x) m/E))))

(defn choose-k-from-n
  "Returns the number of ways to choose k items out of n items. 
n!/(k!(n - k)!).
Returns long if possible.
k must be able to be a long.
Otherwise use log-choose-k-from-n."
  [k ^double n]
  {:pre [(have? m/roughly-round? k)]}
  (m/maybe-long-able (DoubleArithmetic/binomial n (long k))))

(defn log-choose-k-from-n
  "Returns the log of the number of ways to choose k items out of n items.
n must be >= k, and n and k must be non-negative.  
Otherwise, use choose-k-from-n."
  [^double k ^double n]
  {:pre [(have? m/non-? k) (have? m/non-? n) (have? #(>= %2 %1) k n)]}
  (- (log-factorial n) (log-factorial k) (log-factorial (- n k))))

(defn stirling-number-of-the-second-kind
  "Returns the number of ways to partition a set of n items into k subsets.
Returns long if possible."
  [n k]
  {:pre [(have? m/roughly-round-non-? k 0.0)
         (have? m/roughly-round-non-? n 0.0)]}
  (m/maybe-long-able 
    (* (/ (factorial k)) 
       (ccr/fold 
         + (fn [tot e] 
             (+ tot 
                (* (m/pow (- 1) e) (choose-k-from-n e k) (m/pow (- k e) n)))) 
         (range (inc k))))))

(defn bell-number
  "Returns the number of partitions of a set of size n."
  [n]
  {:pre [(have? m/roughly-round-non-? n 0.0)]}
  (if (< n 27) (bell-numbers (long n)) 
    (ccr/fold + (fn [tot e] (+ tot (stirling-number-of-the-second-kind n e))) 
              (range (inc n)))))

(defn binomial-probability
  "Likelihood of seeing 'successes' out of 'trials' with success-prob.  
Successes must be able to be a long, otherwise use 'log-binomial-probability'"
  ^double [successes ^double trials ^double success-prob]
  {:pre [(have? m/prob? success-prob)
         (have? m/long-able? successes)
         (have? m/non-? trials)
         (have? #(>= %2 %1) successes trials)]}
  (* (choose-k-from-n successes trials) (m/pow success-prob successes) 
     (m/pow (m/rev success-prob) (- trials successes))))

(defn log-binomial-probability
  "Log-Likelihood of seeing 'successes' out of 'trials' with success-prob"
  ^double [^double successes ^double trials ^double success-prob]
  {:pre [(have? m/prob? success-prob)
         (have? m/roughly-round? successes 0.0)
         (have? m/non-? trials)
         (have? #(>= %2 %1) successes trials)]}
  (+ (log-choose-k-from-n successes trials) (* successes (m/log success-prob)) 
     (* (- trials successes) (m/log (m/rev success-prob)))))

;;;HYPERGEOMETRIC FUNCTION
(defn generalized-hypergeometric
  "p and q should be arrays"
  [p q z] (throw (ex-info "Not Implemented" {:fn (var generalized-hypergeometric)})))

;redundancy taken from cmc
(defn selections 
  "All the ways of taking n (possibly the same) elements from the sequence of 
  items" 
  [items ^long n]
  {:pre [(have? m/non-? n)]}
  (cmc/selections items n))

(defn combinations 
  "All the unique ways of taking n different elements from items" 
  [items ^long n]
  {:pre [(have? m/non-? n)]}
  (cmc/combinations items n))

(defn subsets 
  "All the subsets of items" 
  [items] (cmc/subsets items))

(defn permutations 
  "All the permutations of items, lexicographic by index" 
  [items] (cmc/permutations items))

(defn lex-permutations 
  "Fast lexicographic permutation generator for a sequence of numbers" 
  [items] (cmc/lex-permutations items))

(defn cartesian-product 
  "All the ways to take one item from each sequence" 
  [& seqs] (apply cmc/cartesian-product seqs))

;;;OTHER COMBOS
(defn subsets-with-opposites
  [items]
  (let [s (cmc/subsets items), r (reverse s)] (partition 2 (interleave s r))))

(defn combinations-with-opposites
  [items ^long n]
  {:pre [(have? m/non-? n)]}
  (let [s (cmc/combinations items n), 
        r (reverse (cmc/combinations items (- (count items) n)))] 
    (partition 2 (interleave s r))))

(defn combinations-using-all
  "Combinations that use all of the items by grouping into the breakdown 
   pattern, where breakdown is a vector of longs that sum to the number of 
   items."
  [items breakdown]
  {:pre [(have? #(= (mx/esum %2) (count %1)) items breakdown)]}
  (if-not (next breakdown) (list (list items))
    (let [cwos (combinations-with-opposites items (first breakdown))]
      (mapcat (fn [cua] (map (fn [dl] (apply list (first cua) dl)) 
                             (combinations-using-all 
                               (second cua) (rest breakdown)))) cwos))))

(defn unique-unordered-subsets-with-replacement
  "All unique unordered subsets of the items with up to 'n' items in a subset"
  [items ^long n]
  {:pre [(have? m/non-? n)]}
  (filter #(<= (count %) n) 
          (distinct (map sort (subsets (apply concat (repeat n items)))))))

(defn unique-unordered-combinations-using-all
  "Unique unordered combinations that use all of the items by grouping into 
   partitions of count n"
  [items ^long n]
  {:pre [(have? m/non-? n)]}
  (let [k (have #(or (zero? n) (not (zero? (rem % n)))) (count items))]
    (cond (= k n) items
          :else (map #(map (fn [g] (map second g)) %) 
                     (filter #(apply distinct? (mapcat 
                                                 (fn [g] (map first g)) %)) 
                             (cmc/combinations 
                               (cmc/combinations 
                                 (map-indexed (fn [idx ele] [idx ele]) items) 
                                 n) 
                               (quot k n)))))))


