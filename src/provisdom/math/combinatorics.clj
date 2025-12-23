(ns provisdom.math.combinatorics
  "Combinatorial functions and sequence generation.

  Provides efficient algorithms for:
  - Factorials: factorial, subfactorial, double factorial
  - Pochhammer symbols: rising and falling factorials
  - Binomial coefficients: choose-k-from-n, multinomial
  - Special numbers: Stirling (1st/2nd kind), Bell, Catalan
  - Binomial probability calculations
  - Integer partitions (different from set partitions)
  - Combinations and permutations (lazy sequence generators)
  - K-permutations (partial permutations)
  - Direct access: nth-combination, nth-permutation for random sampling
  - Counting functions for combinatorial enumeration

  Includes both exact computations for small values and log-space calculations
  for larger values to avoid overflow."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.core.reducers :as ccr]
    [provisdom.math.core :as m]
    [provisdom.math.special-functions :as special-fns]))

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
    (subfactorials (m/round' x :up))
    (m/round' (/ (factorial x) m/E) :up)))

(s/fdef subfactorial
  :args (s/cat :x ::m/non-)
  :ret ::m/num)

(defn double-factorial
  "Computes the double factorial n!!.

  Returns n!! = n × (n-2) × (n-4) × ... × 3 × 1 (for odd n)
  or n!! = n × (n-2) × (n-4) × ... × 4 × 2 (for even n).

  By convention: 0!! = 1, (-1)!! = 1.

  Examples:
    (double-factorial 7)  ;=> 105.0 (7 × 5 × 3 × 1)
    (double-factorial 6)  ;=> 48.0 (6 × 4 × 2)
    (double-factorial 0)  ;=> 1.0
    (double-factorial 1)  ;=> 1.0"
  [n]
  (cond
    (<= n 0) 1.0
    (== n 1) 1.0
    :else (reduce * 1.0 (range n 0 -2))))

(s/fdef double-factorial
  :args (s/cat :n ::m/int)
  :ret ::m/num)

(defn double-factorial'
  "Like [[double-factorial]] but returns a long when possible."
  [n]
  (m/maybe-long-able (double-factorial n)))

(s/fdef double-factorial'
  :args (s/cat :n ::m/int)
  :ret ::m/num)

(defn rising-factorial
  "Computes the rising factorial (Pochhammer symbol).

  Returns x^(n) = x × (x+1) × (x+2) × ... × (x+n-1).
  Also known as the Pochhammer symbol (x)_n in some notations.

  Used extensively in hypergeometric functions and series expansions.

  Examples:
    (rising-factorial 3 4)  ;=> 360.0 (3 × 4 × 5 × 6)
    (rising-factorial 1 5)  ;=> 120.0 (same as 5!)
    (rising-factorial 5 0)  ;=> 1.0 (empty product)
    (rising-factorial 0.5 3) ;=> 1.875 (0.5 × 1.5 × 2.5)"
  [x n]
  (if (zero? n)
    1.0
    (reduce * 1.0 (map #(+ (double x) %) (range n)))))

(s/fdef rising-factorial
  :args (s/with-gen
          (s/cat :x ::m/num :n ::m/int-non-)
          #(gen/tuple (s/gen ::m/num) (gen/large-integer* {:min 0 :max 20})))
  :ret ::m/number)

(def pochhammer-symbol
  "Alias for [[rising-factorial]]. The Pochhammer symbol (x)_n."
  rising-factorial)

(defn falling-factorial
  "Computes the falling factorial.

  Returns (x)_n = x × (x-1) × (x-2) × ... × (x-n+1).
  Also known as the descending factorial or falling Pochhammer symbol.

  Related to permutations: P(n,k) = (n)_k = n!/(n-k)!

  Examples:
    (falling-factorial 5 3)  ;=> 60.0 (5 × 4 × 3)
    (falling-factorial 5 5)  ;=> 120.0 (same as 5!)
    (falling-factorial 5 0)  ;=> 1.0 (empty product)
    (falling-factorial 3.5 2) ;=> 8.75 (3.5 × 2.5)"
  [x n]
  (if (zero? n)
    1.0
    (reduce * 1.0 (map #(- (double x) %) (range n)))))

(s/fdef falling-factorial
  :args (s/with-gen
          (s/cat :x ::m/num :n ::m/int-non-)
          #(gen/tuple (s/gen ::m/num) (gen/large-integer* {:min 0 :max 20})))
  :ret ::m/num)

(def falling-pochhammer-symbol
  "Alias for [[falling-factorial]]. The falling Pochhammer symbol."
  falling-factorial)

;;;CHOOSING
(defn choose-k-from-n
  "Calculates binomial coefficient C(n,k) = n! / (k! × (n-k)!).
  
  Returns the number of ways to choose `k` items from `n` items without regard 
  to order. Uses optimized algorithm that avoids computing large factorials
  directly. For very large values, consider using [[log-choose-k-from-n]].
  
  Constraints: k ≤ n, both non-negative integers
  
  Examples:
    (choose-k-from-n 2 5)   ;=> 10.0 (choosing 2 from 5:
                                        {1,2},{1,3},{1,4},{1,5},{2,3},{2,4},{2,5},{3,4},{3,5},{4,5})
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
      (- lfn
        (log-factorial k)
        (log-factorial (- n k))))))

(s/fdef log-choose-k-from-n
  :args (s/and (s/cat :k ::m/non-
                 :n ::m/non-)
          (fn [{:keys [k n]}]
            (>= n k)))
  :ret ::m/num)

(defn multinomial-coefficient
  "Computes the multinomial coefficient.

  Returns n! / (k₁! × k₂! × ... × kₘ!) where n = sum of ks.
  This is the number of ways to partition n items into groups of sizes k₁, k₂, ..., kₘ.

  The multinomial coefficient generalizes the binomial coefficient:
  (multinomial-coefficient [k (- n k)]) = (choose-k-from-n k n)

  Examples:
    (multinomial-coefficient [2 3 1])  ;=> 60.0 (ways to arrange AABBBC)
    (multinomial-coefficient [2 2])    ;=> 6.0 (same as C(4,2))
    (multinomial-coefficient [3])      ;=> 1.0 (only one way)
    (multinomial-coefficient [1 1 1])  ;=> 6.0 (3! permutations)"
  [ks]
  (let [n (reduce + 0 ks)]
    (/ (factorial n)
       (reduce * 1.0 (map factorial ks)))))

(s/fdef multinomial-coefficient
  :args (s/cat :ks (s/coll-of ::m/int-non-))
  :ret ::m/number)

(defn log-multinomial-coefficient
  "Computes the natural logarithm of the multinomial coefficient.

  More numerically stable than computing the coefficient directly for large values.

  Examples:
    (log-multinomial-coefficient [2 3 1])  ;=> 4.0943... (ln(60))
    (log-multinomial-coefficient [50 50])  ;=> 66.7838... (ln(C(100,50)))"
  [ks]
  (let [n (reduce + 0 ks)]
    (- (log-factorial n)
       (reduce + 0.0 (map log-factorial ks)))))

(s/fdef log-multinomial-coefficient
  :args (s/cat :ks (s/coll-of ::m/non-))
  :ret ::m/number)

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

(defn log-stirling-number-of-the-second-kind
  "Computes the natural logarithm of the Stirling number of the second kind.

  More numerically stable for large values where the direct computation
  would overflow (k > 170).

  Examples:
    (log-stirling-number-of-the-second-kind 3 5)  ;=> 3.2188... (ln(25))
    (log-stirling-number-of-the-second-kind 100 200) ;=> handles large values"
  [k n]
  (cond
    (zero? k) (if (zero? n) 0.0 m/inf-)
    (== k n) 0.0
    (> k n) m/inf-
    (<= k 170)
    ;; For smaller values, compute directly and take log
    (let [result (stirling-number-of-the-second-kind k n)]
      (if (or (m/nan? result) (<= result 0))
        m/inf-
        (m/log result)))
    :else
    ;; For very large k, use log-space approximation
    ;; This is approximate but avoids overflow
    (let [;; Use Temme's asymptotic approximation for large k
          ;; S(k,n) ~ k^n / k! for n >> k
          ;; log(S(k,n)) ~ n*log(k) - log(k!)
          approx (- (* n (m/log k)) (log-factorial k))]
      approx)))

(s/fdef log-stirling-number-of-the-second-kind
  :args (s/and (s/cat :k ::m/long-non- :n ::m/long-non-)
          (fn [{:keys [k n]}]
            (>= n k)))
  :ret ::m/num)

(defn stirling-number-of-the-first-kind
  "Computes the unsigned Stirling number of the first kind.

  Returns s(n,k), the number of permutations of n elements with exactly k cycles.
  Also counts the number of ways to arrange n objects into k non-empty cycles.

  Uses the recurrence relation: s(n,k) = (n-1) × s(n-1,k) + s(n-1,k-1)

  Examples:
    (stirling-number-of-the-first-kind 1 4)  ;=> 6.0 (permutations of 4 with 1 cycle)
    (stirling-number-of-the-first-kind 4 4)  ;=> 1.0 (identity permutation)
    (stirling-number-of-the-first-kind 2 4)  ;=> 11.0
    (stirling-number-of-the-first-kind 0 0)  ;=> 1.0 (by convention)"
  [k n]
  (cond
    (and (zero? n) (zero? k)) 1.0
    (or (zero? n) (zero? k)) 0.0
    (== k n) 1.0
    (> k n) 0.0
    :else
    ;; Use dynamic programming to compute
    (let [table (reduce
                  (fn [prev-row curr-n]
                    (reduce
                      (fn [row curr-k]
                        (assoc row curr-k
                          (+ (* (dec curr-n) (get prev-row curr-k 0.0))
                             (get prev-row (dec curr-k) 0.0))))
                      {}
                      (range 1 (inc (min curr-n k)))))
                  {0 1.0}
                  (range 1 (inc n)))]
      (get table k 0.0))))

(s/fdef stirling-number-of-the-first-kind
  :args (s/with-gen
          (s/and (s/cat :k ::m/long-non- :n ::m/long-non-)
            (fn [{:keys [k n]}]
              (>= n k)))
          #(gen/bind
             (gen/large-integer* {:min 0 :max 15})
             (fn [k]
               (gen/fmap (fn [n] [k n])
                 (gen/large-integer* {:min k :max 15})))))
  :ret ::m/num)

(defn stirling-number-of-the-first-kind'
  "Like [[stirling-number-of-the-first-kind]] but returns a long when possible."
  [k n]
  (m/maybe-long-able (stirling-number-of-the-first-kind k n)))

(s/fdef stirling-number-of-the-first-kind'
  :args (s/with-gen
          (s/and (s/cat :k ::m/long-non- :n ::m/long-non-)
            (fn [{:keys [k n]}]
              (>= n k)))
          #(gen/bind
             (gen/large-integer* {:min 0 :max 15})
             (fn [k]
               (gen/fmap (fn [n] [k n])
                 (gen/large-integer* {:min k :max 15})))))
  :ret ::m/num)

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

(defn catalan-number
  "Computes the nth Catalan number.

  Catalan numbers C_n = C(2n,n)/(n+1) count many combinatorial structures:
  - Number of valid arrangements of n pairs of parentheses
  - Number of different binary search trees with n nodes
  - Number of paths from (0,0) to (n,n) staying below diagonal
  - Number of triangulations of a convex polygon with n+2 sides
  - Number of full binary trees with n+1 leaves

  The sequence begins: 1, 1, 2, 5, 14, 42, 132, 429, 1430, ...

  Examples:
    (catalan-number 0)  ;=> 1.0
    (catalan-number 3)  ;=> 5.0
    (catalan-number 5)  ;=> 42.0
    (catalan-number 10) ;=> 16796.0"
  [n]
  (/ (choose-k-from-n n (* 2 n)) (inc n)))

(s/fdef catalan-number
  :args (s/cat :n ::m/int-non-)
  :ret ::m/num)

(defn catalan-number'
  "Like [[catalan-number]] but returns a long when possible."
  [n]
  (let [result (catalan-number n)]
    (if (m/roughly-round? result 1e-12)
      (m/maybe-long-able (m/round' result :away-from-zero))
      result)))

(s/fdef catalan-number'
  :args (s/cat :n ::m/int-non-)
  :ret ::m/num)

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

;;;COUNTING
(defn count-combinations
  "Returns the count of combinations of k items from n items.

  This is equivalent to [[choose-k-from-n]] but with clearer naming
  for use when counting rather than computing binomial coefficients.

  Examples:
    (count-combinations 2 5)  ;=> 10.0 (C(5,2))
    (count-combinations 0 5)  ;=> 1.0"
  [k n]
  (choose-k-from-n k n))

(s/fdef count-combinations
  :args (s/and (s/cat :k ::m/int-non- :n ::m/int-non-)
          n-no-less-than-k?)
  :ret ::m/num)

(defn count-permutations
  "Returns the count of k-permutations from n items (or all permutations if k not given).

  P(n,k) = n!/(n-k)! = n × (n-1) × ... × (n-k+1)

  With one argument, returns n! (all permutations of n items).
  With two arguments, returns P(n,k) (k-permutations from n items).

  Examples:
    (count-permutations 5)    ;=> 120.0 (5!)
    (count-permutations 2 5)  ;=> 20.0 (5 × 4)
    (count-permutations 5 5)  ;=> 120.0 (5!)"
  ([n]
   (factorial n))
  ([k n]
   (falling-factorial n k)))

(s/fdef count-permutations
  :args (s/with-gen
          (s/or :one (s/cat :n ::m/int-non-)
            :two (s/and (s/cat :k ::m/int-non- :n ::m/int-non-)
                   n-no-less-than-k?))
          #(gen/one-of
             [(gen/fmap vector (gen/large-integer* {:min 0 :max 20}))
              (gen/bind
                (gen/large-integer* {:min 0 :max 20})
                (fn [k]
                  (gen/fmap (fn [n] [k n])
                    (gen/large-integer* {:min k :max 20}))))]))
  :ret ::m/num)

;;;INTEGER PARTITIONS
(defn- integer-partitions-helper
  "Helper to generate partitions of n with maximum part max-part."
  [n max-part]
  (cond
    (zero? n) '(())
    (neg? n) '()
    (zero? max-part) '()
    :else
    (lazy-cat
      (map #(cons max-part %)
        (integer-partitions-helper (- n max-part) max-part))
      (integer-partitions-helper n (dec max-part)))))

(defn integer-partitions
  "Generates all partitions of integer n as sums of positive integers.

  An integer partition of n is a way of writing n as a sum of positive integers,
  where order doesn't matter. Each partition is returned as a sequence of parts
  in descending order.

  Different from set partitions (Bell numbers) - here we partition a number, not a set.

  Examples:
    (integer-partitions 4)  ;=> ((4) (3 1) (2 2) (2 1 1) (1 1 1 1))
    (integer-partitions 3)  ;=> ((3) (2 1) (1 1 1))
    (integer-partitions 1)  ;=> ((1))
    (integer-partitions 0)  ;=> (())

  Optional second argument limits the maximum part size:
    (integer-partitions 5 3) ;=> ((3 2) (3 1 1) (2 2 1) (2 1 1 1) (1 1 1 1 1))"
  ([n]
   (integer-partitions n n))
  ([n max-part]
   (integer-partitions-helper n (min n max-part))))

(s/fdef integer-partitions
  :args (s/with-gen
          (s/cat :n ::m/int-non- :max-part (s/? ::m/int-non-))
          #(gen/one-of
             [(gen/fmap vector (gen/large-integer* {:min 0 :max 15}))
              (gen/fmap (fn [[n mp]] [n mp])
                (gen/tuple
                  (gen/large-integer* {:min 0 :max 15})
                  (gen/large-integer* {:min 0 :max 15})))]))
  :ret (s/coll-of (s/coll-of ::m/int-non-)))

(defn count-integer-partitions
  "Returns the number of integer partitions of n.

  Uses dynamic programming for efficiency. This is the partition function p(n).

  Examples:
    (count-integer-partitions 5)   ;=> 7
    (count-integer-partitions 10)  ;=> 42
    (count-integer-partitions 100) ;=> 190569292"
  [n]
  (if (neg? n)
    0
    (let [table (long-array (inc n))]
      (aset table 0 1)
      (doseq [i (range 1 (inc n))]
        (doseq [j (range i (inc n))]
          (aset table j (+ (aget table j) (aget table (- j i))))))
      (aget table n))))

(s/fdef count-integer-partitions
  :args (s/cat :n ::m/int)
  :ret ::m/int-non-)

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

(defn k-permutations
  "Generates all k-permutations of items (ordered selections of k items).

  Returns all possible ways to select and arrange k items from the collection.
  Unlike `permutations` which returns all n! permutations, this returns P(n,k)
  arrangements.

  When k equals the number of items, equivalent to [[permutations]].

  Examples:
    (k-permutations [1 2 3] 2)  ;=> ([1 2] [1 3] [2 1] [2 3] [3 1] [3 2])
    (k-permutations [:a :b :c] 1)  ;=> ([:a] [:b] [:c])
    (k-permutations [1 2] 2)  ;=> ([1 2] [2 1]) (same as permutations)
    (k-permutations [1 2 3] 0)  ;=> ([]) (one empty arrangement)"
  [items k]
  (let [n (count items)]
    (cond
      (zero? k) (list (if (vector? items) [] '()))
      (> k n) nil
      (== k n) (permutations items)
      :else
      (mapcat (fn [combo]
                (permutations (if (vector? items)
                                (vec combo)
                                combo)))
        (combinations items k)))))

(s/fdef k-permutations
  :args (s/cat :items ::items :k ::m/int-non-)
  :ret (s/nilable ::groups-of-items))

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
  :args (s/with-gen
          (s/cat :sequences-of-items (s/* ::items))
          #(gen/vector (s/gen ::items) 0 mdl))
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

;;;DIRECT ACCESS
(defn nth-combination
  "Returns the nth combination (0-indexed) of k items from the collection.

  Computes the combination directly without generating all previous combinations.
  Useful for random sampling from large combinatorial spaces.

  Uses combinatorial number system (combinadics) to decode the index.

  Examples:
    (nth-combination [1 2 3 4 5] 2 0)  ;=> (1 2) first combination
    (nth-combination [1 2 3 4 5] 2 9)  ;=> (4 5) last combination
    (nth-combination [:a :b :c :d] 2 3) ;=> (:b :c)"
  [items k idx]
  (let [v-items (vec items)
        n (count v-items)
        total (choose-k-from-n' k n)]
    (when (and (<= k n) (>= idx 0) (< idx total))
      (loop [result []
             remaining-k k
             remaining-n n
             remaining-idx idx
             offset 0]
        (if (zero? remaining-k)
          (map v-items result)
          ;; For each position, find which element goes there
          (let [;; Count combinations if we skip elements
                [chosen skip-count]
                (loop [skip 0
                       acc 0]
                  (let [combos-without (choose-k-from-n' (dec remaining-k) (- remaining-n skip 1))]
                    (if (< remaining-idx (+ acc combos-without))
                      [(+ offset skip) acc]
                      (recur (inc skip) (+ acc combos-without)))))]
            (recur (conj result chosen)
              (dec remaining-k)
              (- remaining-n (- chosen offset) 1)
              (- remaining-idx skip-count)
              (inc chosen))))))))

(s/fdef nth-combination
  :args (s/cat :items ::items :k ::m/int-non- :idx ::m/int-non-)
  :ret (s/nilable ::items))

(defn nth-permutation
  "Returns the nth permutation (0-indexed) of the collection.

  Computes the permutation directly without generating all previous permutations.
  Useful for random sampling from large permutation spaces.

  Uses factorial number system (factoradic) to decode the index.

  Examples:
    (nth-permutation [1 2 3] 0)  ;=> (1 2 3) first permutation
    (nth-permutation [1 2 3] 5)  ;=> (3 2 1) last permutation
    (nth-permutation [:a :b :c] 2) ;=> (:b :a :c)"
  [items idx]
  (let [v-items (vec items)
        n (count v-items)]
    (when (and (>= idx 0) (< idx (factorial n)))
      (loop [result []
             remaining (vec (range n))
             remaining-idx idx
             divisor (factorial' (dec n))]
        (if (empty? remaining)
          (map v-items result)
          (let [quot-val (long (quot remaining-idx divisor))
                chosen-pos (nth remaining quot-val)
                new-remaining (vec (concat (subvec remaining 0 quot-val)
                                     (subvec remaining (inc quot-val))))]
            (recur (conj result chosen-pos)
              new-remaining
              (rem remaining-idx divisor)
              (if (> (count new-remaining) 0)
                (/ divisor (count new-remaining))
                1))))))))

(s/fdef nth-permutation
  :args (s/cat :items ::items :idx ::m/int-non-)
  :ret (s/nilable ::items))

(defn nth-k-permutation
  "Returns the nth k-permutation (0-indexed) of the collection.

  Computes the k-permutation directly without generating all previous arrangements.
  Useful for random sampling from large P(n,k) spaces.

  Examples:
    (nth-k-permutation [1 2 3 4] 2 0)  ;=> (1 2) first 2-permutation
    (nth-k-permutation [1 2 3 4] 2 11) ;=> (4 3) last 2-permutation
    (nth-k-permutation [:a :b :c] 2 3) ;=> (:b :a)"
  [items k idx]
  (let [v-items (vec items)
        n (count v-items)
        total-count (falling-factorial n k)]
    (when (and (<= k n) (>= idx 0) (< idx total-count))
      (loop [result []
             remaining (vec (range n))
             remaining-k k
             remaining-idx idx]
        (if (zero? remaining-k)
          (map v-items result)
          (let [perms-per-choice (falling-factorial (dec (count remaining)) (dec remaining-k))
                quot-val (long (quot remaining-idx perms-per-choice))
                chosen-pos (nth remaining quot-val)
                new-remaining (vec (concat (subvec remaining 0 quot-val)
                                     (subvec remaining (inc quot-val))))]
            (recur (conj result chosen-pos)
              new-remaining
              (dec remaining-k)
              (rem remaining-idx perms-per-choice))))))))

(s/fdef nth-k-permutation
  :args (s/cat :items ::items :k ::m/int-non- :idx ::m/int-non-)
  :ret (s/nilable ::items))

