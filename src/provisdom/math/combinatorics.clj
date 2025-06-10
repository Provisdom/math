(ns provisdom.math.combinatorics
  "Combinatorial functions and sequence generation.
  
  Provides efficient algorithms for:
  - Factorials and subfactorials (derangements)
  - Combinations and permutations
  - Stirling numbers and Bell numbers  
  - Binomial probability calculations
  - Lazy sequence generators for combinatorial objects
  
  Includes both exact computations for small values and log-space calculations
  for larger values to avoid overflow."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.core.reducers :as ccr]
    [provisdom.math.core :as m]
    [provisdom.math.special-functions :as special-fns]))

(declare log-choose-k-from-n)

;;;SPECS
(def mdl 3)                                                 ;max-dim-length for generators

(s/def ::items
  (s/with-gen
    (s/coll-of any?)
    #(gen/vector (s/gen any?) 0 mdl)))

(s/def ::groups-of-items
  (s/with-gen
    (s/coll-of ::items)
    #(gen/vector (s/gen ::items) 0 mdl)))

(s/def ::replacement-count
  (s/with-gen
    ::m/int-non-
    #(gen/large-integer* {:min 0 :max mdl})))


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

;;;FACTORIALS
(defn factorial
  "Computes the factorial of a number.
  
  Returns x! = x × (x-1) × ... × 2 × 1. Uses the gamma function identity:
  x! = Γ(x+1) for accurate computation of non-integer factorials.
  
  Examples:
    (factorial 5)    ;=> 120.0
    (factorial 0)    ;=> 1.0  
    (factorial 3.5)  ;=> 11.631728396567448 (Γ(4.5))
    (factorial -1)   ;=> ##Inf (undefined for negative integers)"
  [x]
  (special-fns/gamma (inc (double x))))

(s/fdef factorial
  :args (s/cat :x ::m/non-)
  :ret ::m/num)

(defn factorial'
  "Computes the factorial of a number, returning a long when possible.
  
  Like [[factorial]] but returns a long if the result fits in long range,
  otherwise returns a double. Useful for exact integer arithmetic.
  
  Examples:
    (factorial' 5)   ;=> 120 (long)
    (factorial' 20)  ;=> 2432902008176640000 (long)
    (factorial' 21)  ;=> 5.109094217170944E19 (double, too large for long)"
  [x]
  (m/maybe-long-able (factorial x)))

(s/fdef factorial'
  :args (s/cat :x ::m/non-)
  :ret ::m/num)

(defn log-factorial
  "Computes the natural logarithm of the factorial.
  
  Returns ln(x!). More numerically stable than computing factorial and then
  taking the log, especially for large x where x! would overflow.
  
  Examples:
    (log-factorial 5)   ;=> 4.787491742782046 (ln(120))
    (log-factorial 100) ;=> 363.7393755555635 (ln(100!) without overflow)
    (log-factorial 0)   ;=> 0.0 (ln(1))"
  [x]
  (special-fns/log-gamma-inc (double x)))

(s/fdef log-factorial
  :args (s/cat :x ::m/non-)
  :ret ::m/num)

(defn subfactorial
  "Computes the subfactorial (number of derangements).
  
  Returns !x, the number of permutations of x objects where no object appears
  in its original position. Also known as derangements or discordant permutations.
  
  Uses precomputed values for x < 22, otherwise approximates using !x ≈ x!/e.
  
  Examples:
    (subfactorial 3)  ;=> 2 (permutations: [2,3,1] and [3,1,2])
    (subfactorial 4)  ;=> 9 
    (subfactorial 0)  ;=> 1 (by convention)
    (subfactorial 1)  ;=> 0 (impossible to derange one item)"
  [x]
  (if (and (m/long-able? x) (< x 22))
    (subfactorials (m/round x :up))
    (m/round (/ (factorial x) m/E) :up)))

(s/fdef subfactorial
  :args (s/cat :x ::m/non-)
  :ret ::m/num)

;;;CHOOSING
(defn choose-k-from-n
  "Calculates binomial coefficient C(n,k) = n! / (k! × (n-k)!).
  
  Returns the number of ways to choose `k` items from `n` items without regard 
  to order. Uses optimized algorithm that avoids computing large factorials
  directly. For very large values, consider using [[log-choose-k-from-n]].
  
  Constraints: k ≤ n, both non-negative integers
  
  Examples:
    (choose-k-from-n 2 5)   ;=> 10.0 (choosing 2 from 5: {1,2},{1,3},{1,4},{1,5},{2,3},{2,4},{2,5},{3,4},{3,5},{4,5})
    (choose-k-from-n 0 5)   ;=> 1.0  (only one way to choose nothing)
    (choose-k-from-n 5 5)   ;=> 1.0  (only one way to choose everything)"
  [k n]
  (let [diff (- n k)]
    (cond (or (zero? diff) (zero? k)) 1.0
          (or (m/one? k) (m/one? diff)) (double n)
          (> k (/ n 2)) (choose-k-from-n diff n)
          :else (reduce (fn [acc i]
                          (let [acc (* acc (/ (+ diff i) i))]
                            (if (m/inf+? acc)
                              (reduced acc)
                              acc)))
                  1.0
                  (range 1 (inc k))))))

(defn n-no-less-than-k?
  [{:keys [k n]}]
  (>= n k))

(s/fdef choose-k-from-n
  :args (s/with-gen
          (s/and (s/cat :k ::m/int-non-
                   :n ::m/int-non-)
            n-no-less-than-k?)
          #(gen/bind
             (s/gen ::m/int-non-)
             (fn [k]
               (gen/fmap
                 (fn [n] [k n])
                 (s/gen (s/int-in k m/max-int))))))
  :ret ::m/num)

(defn choose-k-from-n'
  "Returns the number of ways to choose `k` items out of `n` items.
  `n`! / (`k`! × (`n` - `k`)!). Returns a long if possible. `k` and `n` must be
  int, otherwise use [[log-choose-k-from-n]]."
  [k n]
  (m/maybe-long-able (choose-k-from-n k n)))

(s/fdef choose-k-from-n'
  :args (s/and (s/cat :k ::m/int-non-
                 :n ::m/int-non-)
          (fn [{:keys [k n]}]
            (>= n k)))
  :ret ::m/num)

(defn log-choose-k-from-n
  "Computes the natural logarithm of the binomial coefficient.
  
  Returns ln(C(n,k)) = ln(n!/(k!(n-k)!)). More numerically stable than 
  computing the binomial coefficient directly for large values.
  
  Constraints: n ≥ k ≥ 0
  
  Examples:
    (log-choose-k-from-n 2 5)   ;=> 2.3025850929940455 (ln(10))
    (log-choose-k-from-n 50 100) ;=> 66.7838... (ln of very large number)
    (log-choose-k-from-n 0 5)   ;=> 0.0 (ln(1))"
  [k n]
  (let [lfn (log-factorial n)]
    (if (m/inf+? lfn)
      m/inf+
      (- (log-factorial n)
        (log-factorial k)
        (log-factorial (- n k))))))

(s/fdef log-choose-k-from-n
  :args (s/and (s/cat :k ::m/non-
                 :n ::m/non-)
          (fn [{:keys [k n]}]
            (>= n k)))
  :ret ::m/num)

(defn stirling-number-of-the-second-kind
  "Returns the number of ways to partition a set of `n` items into `k` subsets."
  [k n]
  (if (> k 170)
    m/nan
    (* (/ (factorial k))
      (ccr/fold
        + (fn [tot e]
            (+ tot
              (* (m/pow (- 1) e)
                (choose-k-from-n e k)
                (m/pow (- k e) n))))
        (range (inc k))))))

(s/fdef stirling-number-of-the-second-kind
  :args (s/and (s/cat :k ::m/long-non-
                 :n ::m/long)
          (fn [{:keys [k n]}]
            (>= n k)))
  :ret ::m/number)

(defn stirling-number-of-the-second-kind'
  "Returns the number of ways to partition a set of `n` items into `k` subsets.
  Returns long if possible."
  [k n]
  (m/maybe-long-able (stirling-number-of-the-second-kind k n)))

(s/fdef stirling-number-of-the-second-kind'
  :args (s/and (s/cat :k ::m/long-non- :n ::m/long)
          (fn [{:keys [k n]}]
            (>= n k)))
  :ret ::m/number)

(defn bell-number
  "Returns the number of partitions of a set of size `n`."
  [n]
  (cond (> n 170) m/nan
        (and (m/non-? n) (< n 27)) (bell-numbers (long n))
        :else (ccr/fold + (fn [tot e]
                            (+ tot (stirling-number-of-the-second-kind e n)))
                (range (inc n)))))

(s/fdef bell-number
  :args (s/cat :n ::m/long)
  :ret ::m/number)

(defn binomial-probability
  "Computes the binomial probability mass function.
  
  Returns P(X = k) for a binomial distribution: the probability of exactly
  `successes` successes in `trials` independent trials, each with probability
  `success-prob` of success.
  
  Formula: P(X=k) = C(n,k) × p^k × (1-p)^(n-k)
  
  For large `trials` (>1000), consider using [[log-binomial-probability]]
  to avoid numerical overflow.
  
  Examples:
    (binomial-probability 2 5 0.3)  ;=> 0.30869 (2 successes in 5 trials)
    (binomial-probability 0 3 0.5)  ;=> 0.125 (no successes in 3 trials)
    (binomial-probability 5 5 0.9)  ;=> 0.59049 (all successes)"
  [successes trials success-prob]
  (* (choose-k-from-n successes trials)
    (m/pow success-prob successes)
    (m/pow (m/one- success-prob) (- trials successes))))

(s/fdef binomial-probability
  :args (s/and (s/cat :successes ::m/int-non-
                 :trials ::m/int-non-
                 :success-prob ::m/open-prob)
          (fn [{:keys [trials successes]}]
            (>= trials successes)))
  :ret ::m/number)

(defn log-binomial-probability
  "Log-Likelihood of seeing `successes` out of `trials` with `success-prob`."
  [successes trials success-prob]
  (+ (log-choose-k-from-n successes trials)
    (* successes (m/log success-prob))
    (if (== trials successes)
      0.0
      (* (- trials successes)
        (m/log (m/one- success-prob))))))

(s/fdef log-binomial-probability
  :args (s/and (s/cat :successes ::m/long-non-
                 :trials ::m/finite-non-
                 :success-prob ::m/open-prob)
          (fn [{:keys [trials successes]}]
            (>= trials successes)))
  :ret ::m/num)

;;;UNORDERED COMBINATIONS

;taken from an old version of clojure.math.combinatorics
(defn- unchunk
  "Given a sequence that may have chunks, return a sequence that is 1-at-a-time
  lazy with no chunks. Chunks are good for efficiency when the data items are
  small, but when being processed via map, for example, a reference is kept to
  every function result in the chunk until the entire chunk has been processed,
  which increases the amount of memory in use that cannot be garbage collected."
  [s]
  (lazy-seq
    (when (seq s)
      (cons (first s) (unchunk (rest s))))))

(defn- index-combinations
  [n cnt]
  (lazy-seq
    (let [c (vec (cons nil
                   (for [j (range 1 (inc n))]
                     (+ j cnt (- (inc n))))))
          iter-c (fn iter-c [c j]
                   (when-not (> j n)
                     (let [c (assoc c j (dec (c j)))]
                       (if (< (c j) j)
                         [c (inc j)]
                         (loop [c c
                                j j]
                           (if (= j 1)
                             [c j]
                             (recur (assoc c (dec j) (dec (c j))) (dec j))))))))
          step (fn step [c j]
                 (cons (rseq (subvec c 1 (inc n)))
                   (lazy-seq
                     (let [next-step (iter-c c j)]
                       (when next-step
                         (step (next-step 0) (next-step 1)))))))]
      (step c 1))))

(defn combinations
  "Generates all combinations of items.
  
  With one argument, returns all possible combinations of all sizes (power set).
  With two arguments, returns all combinations of exactly `n` items.
  
  Combinations are unordered selections - [1 2] and [2 1] are the same combination.
  Returns lazy sequences for memory efficiency.
  
  Examples:
    (combinations [1 2 3] 2)     ;=> ((1 2) (1 3) (2 3))
    (combinations [:a :b] 1)     ;=> ((:a) (:b))
    (combinations [1 2])         ;=> (() (1) (2) (1 2)) [all sizes]
    (combinations [] 1)          ;=> nil (impossible)"
  ([items]
   (let [c (count items)
         c (if (= c m/max-long) (double c) c)]
     (mapcat (fn [n]
               (combinations items n))
       (unchunk (range (inc c))))))
  ([items n]
   (let [v-items (vec (reverse items))]
     (if (zero? n)
       (list ())
       (let [cnt (count items)]
         (cond (> n cnt) nil
               (= n cnt) (list (seq items))
               :else (map #(map v-items %)
                       (index-combinations n cnt))))))))

(s/fdef combinations
  :args (s/cat :items ::items
          :n (s/? ::m/long-non-))
  :ret (s/nilable ::groups-of-items))

(defn combinations-with-complements
  "All combinations of size `n` with complements, or all combinations with
  complements."
  ([items]
   (let [s (combinations items)
         r (reverse s)]
     (partition 2 (interleave s r))))
  ([items n]
   (when-let [s (combinations items n)]
     (partition 2
       (interleave s
         (reverse
           (combinations items (- (count items) n))))))))

(s/fdef combinations-with-complements
  :args (s/cat :items ::items
          :n (s/? ::m/long-non-))
  :ret (s/nilable ::groups-of-items))

(defn combinations-using-all
  "Combinations that use all the `items` by grouping into the `breakdown`
   pattern, where `breakdown` is a collection of positive longs that sum to the
   number of items."
  [items breakdown]
  (if-not (next breakdown)
    (list (list items))
    (let [combos (combinations-with-complements items (first breakdown))]
      (mapcat (fn [cua]
                (map (fn [dl]
                       (apply list (first cua) dl))
                  (combinations-using-all (second cua) (rest breakdown))))
        combos))))

(s/fdef combinations-using-all
  :args (s/with-gen
          (s/and (s/cat :items ::items
                   :breakdown (s/coll-of ::m/long+))
            (fn [{:keys [items breakdown]}]
              (== (apply + 0.0 breakdown) (count items))))
          #(gen/one-of
             (map gen/return
               (list [[1 2 3] [2 1]] [[] []] [[[] nil [12 34]] [1]]))))
  :ret ::groups-of-items)

(defn distinct-combinations-with-replacement
  "All distinct combinations of the `items` with replacement of up to `n`
  items."
  [items n]
  (filter #(<= (count %) n)
    (distinct (combinations (apply interleave (repeat n items))))))

(s/fdef distinct-combinations-with-replacement
  :args (s/cat :items ::items
          :n ::replacement-count)
  :ret ::groups-of-items)

;;;ORDERED COMBINATIONS
(defn- permute
  "All the permutations of `items`."
  [items prefix]
  (if (empty? items)
    [prefix]
    (mapcat (fn [i]
              (permute (concat (take i items) (drop (inc i) items))
                (conj prefix (nth items i))))
      (range (count items)))))

(defn permutations
  "Generates all permutations of items.
  
  Returns all possible arrangements where order matters. Unlike combinations,
  [1 2] and [2 1] are different permutations.
  
  Preserves the input collection type (vector input → vector output).
  
  Examples:
    (permutations [1 2 3])  ;=> ([1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1])
    (permutations '(a b))   ;=> ((a b) (b a))
    (permutations [])       ;=> [[]]
    
  Warning: Number of permutations is n! which grows very quickly."
  [items]
  (let [p (permute (into [] items)
            (if (vector? items) [] '()))]
    (if (vector? items)
      p
      (map #(into '() %)
        p))))

(s/fdef permutations
  :args (s/cat :items ::items)
  :ret ::groups-of-items)

(defn cartesian-product
  "All the ways to take one item from each sequence in `sequences-of-items`."
  [& sequences-of-items]
  (let [v (vec sequences-of-items)]
    (if (empty? v)
      '(())
      (for [x (first v)
            more (apply cartesian-product (rest v))]
        (cons x more)))))

(s/fdef cartesian-product
  :args (s/cat :sequences-of-items (s/* ::items))
  :ret ::groups-of-items)

(defn selections
  "All the ways of taking `n` (possibly the same) elements from the sequence of
  items."
  [items n]
  (apply cartesian-product
    (take n (repeat items))))

(s/fdef selections
  :args (s/cat :items ::items
          :n ::replacement-count)
  :ret ::groups-of-items)

