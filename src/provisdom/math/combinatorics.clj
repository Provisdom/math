(ns provisdom.math.combinatorics
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.math.core :as m]
    [provisdom.math.special-functions :as special-fns]
    [clojure.core.reducers :as ccr])
  (:import
    [cern.jet.math.tdouble DoubleArithmetic]))

(declare log-choose-k-from-n)
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
  "Returns the factorial of `x`."
  [x]
  (special-fns/gamma (inc (double x))))

(s/fdef factorial
        :args (s/cat :x ::m/non-)
        :ret ::m/num)

(defn factorial'
  "Returns the factorial of `x`. Returns long if possible."
  [x]
  (m/maybe-long-able (factorial x)))

(s/fdef factorial'
        :args (s/cat :x ::m/non-)
        :ret ::m/num)

(defn log-factorial
  "Returns the log-factorial of `x`"
  [x]
  (special-fns/log-gamma (inc (double x))))

(s/fdef log-factorial
        :args (s/cat :x ::m/non-)
        :ret ::m/num)

(defn subfactorial
  "Returns the subfactorial of `x`. The number of ways that n objects can be
  arranged where no object appears in its natural position (known as
  'derangements.')"
  [x]
  (if (and (m/long-able? x) (< x 22))
    (subfactorials (m/round x :up))
    (m/round (/ (factorial x) m/E) :up)))

(s/fdef subfactorial
        :args (s/cat :x ::m/non-)
        :ret ::m/num)

;;;CHOOSING
(defn choose-k-from-n
  "Returns the number of ways to choose `k` items out of `n` items.
  `n`! / (`k`! × (`n` - `k`)!). `k` must be able to be a long and less than 1e7,
  otherwise use [[log-choose-k-from-n]]."
  [k n]
  (let [b (DoubleArithmetic/binomial (double n) (long k))]
    (if (m/nan? b)
      (m/exp (log-choose-k-from-n k n))
      b)))

(s/fdef choose-k-from-n
        :args (s/and (s/cat :k (s/and ::m/long-non- #(< % 1e7)) ;too slow otherwise
                            :n ::m/non-)
                     (fn [{:keys [k n]}]
                       (>= n k)))
        :ret ::m/num)

(defn choose-k-from-n'
  "Returns the number of ways to choose `k` items out of `n` items.
  `n`! / (`k`! × (`n` - `k`)!). Returns a long if possible. `k` must be able to
  be a long and less than 1e7, otherwise use [[log-choose-k-from-n]]."
  [k n]
  (m/maybe-long-able (choose-k-from-n k n)))

(s/fdef choose-k-from-n'
        :args (s/and (s/cat :k (s/and ::m/long-non- #(< % 1e7)) ;too slow otherwise
                            :n ::m/non-)
                     (fn [{:keys [k n]}]
                       (>= n k)))
        :ret ::m/num)

(defn log-choose-k-from-n
  "Returns the log of the number of ways to choose `k` items out of `n` items.
  `n` must be >= `k`, and `n` and `k` must be non-negative. Otherwise, use
  [[choose-k-from-n]]."
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
  "Likelihood of seeing `successes` out of `trials` with `success-prob`.
  `Successes` must be a long and less than 1e7, otherwise use
  [[log-binomial-probability]]. For general use, if `trials` is greater than
  1000, use [[log-binomial-probability]]."
  [successes trials success-prob]
  (* (choose-k-from-n successes trials)
     (m/pow success-prob successes)
     (m/pow (m/one- success-prob) (- trials successes))))

(s/fdef binomial-probability
        :args (s/and (s/cat :successes (s/and ::m/long-non- #(< % 1e7))
                            :trials ::m/finite-non-
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

(comment
  ;;;HYPERGEOMETRIC FUNCTION
  (defn generalized-hypergeometric
    "`p` and `q` should be arrays."
    [p q z]
    (throw (ex-info "Not Implemented" {:fn (var generalized-hypergeometric)}))))

;;;UNORDERED COMBINATIONS
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
          iter-comb (fn iter-comb [c j]
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
                         (let [next-step (iter-comb c j)]
                           (when next-step
                             (step (next-step 0) (next-step 1)))))))]
      (step c 1))))

(defn combinations
  "All the unique ways of taking `n` different elements from `items`, or all the
  unique ways of taking different elements from `items`."
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
  "Combinations that use all of the `items` by grouping into the `breakdown`
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
  "All the permutations of `items`."
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

