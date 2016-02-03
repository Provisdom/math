(ns provisdom.math.combinatorics
  (:require [provisdom.math [core :as m]
             [special-functions :as mf]
             [matrix :as mx]]
            [clojure.core.reducers :as ccr]
            [taoensso.truss :refer (have have! have?)])
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
  {:pre [(have? #(m/roughly-round? % 0.0) k)]}
  (m/maybe-long-able (DoubleArithmetic/binomial n (long k))))

(defn log-choose-k-from-n
  "Returns the log of the number of ways to choose k items out of n items.
n must be >= k, and n and k must be non-negative.  
Otherwise, use choose-k-from-n."
  [^double k ^double n]
  {:pre [(have? m/non-? k n) (have? #(>= n %) k)]}
  (- (log-factorial n) (log-factorial k) (log-factorial (- n k))))

(defn stirling-number-of-the-second-kind
  "Returns the number of ways to partition a set of n items into k subsets.
Returns long if possible."
  [n k]
  {:pre [(have? #(m/roughly-round-non-? % 0.0) k n)]}
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
  {:pre [(have? #(m/roughly-round-non-? % 0.0) n)]}
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
         (have? (fn [[trials successes]] (>= trials successes)) [trials successes])]}
  (* (choose-k-from-n successes trials) (m/pow success-prob successes)
     (m/pow (m/rev success-prob) (- trials successes))))

(defn log-binomial-probability
  "Log-Likelihood of seeing 'successes' out of 'trials' with success-prob"
  ^double [^double successes ^double trials ^double success-prob]
  {:pre [(have? m/prob? success-prob)
         (have? #(m/roughly-round? % 0.0) successes)
         (have? m/non-? trials)
         (have? (fn [[trials successes]] (>= trials successes)) [trials successes])]}
  (+ (log-choose-k-from-n successes trials) (* successes (m/log success-prob))
     (* (- trials successes) (m/log (m/rev success-prob)))))

;;;HYPERGEOMETRIC FUNCTION
(defn generalized-hypergeometric
  "p and q should be arrays"
  [p q z] (throw (ex-info "Not Implemented" {:fn (var generalized-hypergeometric)})))

;redundancy taken from an old version of clojure.math.combinatorics
(defn- unchunk
  "Given a sequence that may have chunks, return a sequence that is 1-at-a-time
lazy with no chunks. Chunks are good for efficiency when the data items are
small, but when being processed via map, for example, a reference is kept to
every function result in the chunk until the entire chunk has been processed,
which increases the amount of memory in use that cannot be garbage
collected."
  [s]
  (lazy-seq
    (when (seq s)
      (cons (first s) (unchunk (rest s))))))

(defn- index-combinations
  [n cnt]
  (lazy-seq
    (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
          iter-comb
          (fn iter-comb [c j]
            (if (> j n) nil
                        (let [c (assoc c j (dec (c j)))]
                          (if (< (c j) j) [c (inc j)]
                                          (loop [c c, j j]
                                            (if (= j 1) [c j]
                                                        (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
          step
          (fn step [c j]
            (cons (rseq (subvec c 1 (inc n)))
                  (lazy-seq (let [next-step (iter-comb c j)]
                              (when next-step (step (next-step 0) (next-step 1)))))))]
      (step c 1))))

(defn combinations
  "All the unique ways of taking n different elements from items"
  [items ^long n]
  {:pre [(have? m/non-? n)]}
  (let [v-items (vec (reverse items))]
    (if (zero? n)
      (list ())
      (let [cnt (count items)]
        (cond (> n cnt) nil
              (= n cnt) (list (seq items))
              :else
              (map #(map v-items %) (index-combinations n cnt)))))))

(defn subseqs
  "All the subseqs of items"
  [items]
  (mapcat (fn [n] (combinations items n)) (unchunk (range (inc (count items))))))

(defn- permute
  "All the permutations of items"
  [items prefix]
  (if (empty? items)
    [prefix]
    (mapcat (fn [i]
              (permute (concat (take i items) (drop (inc i) items)) (conj prefix (nth items i))))
            (range (count items)))))

(defn permutations
  "All the permutations of items"
  [items]
  (permute items (if (vector? items) [] '())))

(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& colls]
  (let [v (vec colls)]
    (if (empty? v)
      '(())
      (for [x (first v)
            more (apply cartesian-product (rest v))]
        (cons x more)))))

(defn selections
  "All the ways of taking n (possibly the same) elements from the sequence of items"
  [items ^long n]
  {:pre [(have? m/non-? n)]}
  (apply cartesian-product (take n (repeat items))))

;;;OTHER COMBOS
(defn subseqs-with-complements
  [items]
  (let [s (subseqs items), r (reverse s)] (partition 2 (interleave s r))))

(defn combinations-with-complements
  "All combinations of size 'n' with complements"
  [items ^long n]
  {:pre [(have? m/non-? n)]}
  (let [s (combinations items n)
        r (reverse (combinations items (- (count items) n)))]
    (partition 2 (interleave s r))))

(defn combinations-using-all
  "Combinations that use all of the items by grouping into the breakdown 
   pattern, where breakdown is a vector of longs that sum to the number of items."
  [items breakdown]
  {:pre [(have? (fn [[items breakdown]] (= (mx/esum breakdown) (count items))) [items breakdown])]}
  (if-not (next breakdown) (list (list items))
                           (let [cwos (combinations-with-complements items (first breakdown))]
                             (mapcat (fn [cua] (map (fn [dl] (apply list (first cua) dl))
                                                    (combinations-using-all
                                                      (second cua) (rest breakdown)))) cwos))))

(defn subsets-with-replacement
  "All subsets of the items with up to 'n' items in a subset"
  [items ^long n]
  {:pre [(have? m/non-? n)]}
  (filter #(<= (count %) n) (distinct (map sort (subseqs (apply concat (repeat n items)))))))

(defn unique-unordered-combinations-using-all
  "Unique unordered combinations that use all of the items by grouping into partitions of count n"
  [items ^long n]
  {:pre [(have? m/non-? n)]}
  (let [k (have #(and (not (zero? n)) (zero? (rem % n))) (count items))]
    (cond (= k n) items
          :else (map #(map (fn [g] (map second g)) %)
                     (filter #(apply distinct? (mapcat (fn [g] (map first g)) %))
                             (combinations
                               (combinations
                                 (map-indexed (fn [idx ele] [idx ele]) items)
                                 n)
                               (quot k n)))))))


