(ns provisdom.math.random
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.math.core :as m]
    [provisdom.math.intervals :as intervals]
    [provisdom.math.special-functions :as mf]
    [provisdom.math.internal-splittable-random :as split]
    [clojure.core.reducers :as reducers])
  (:import
    [org.apache.commons.math3.random MersenneTwister
                                     ISAACRandom
                                     SobolSequenceGenerator]))

(def mdl 6)

(s/def ::rng (s/with-gen
               (partial satisfies? split/IRandom)
               #(gen/return (split/next-rng))))

(s/def ::seed ::m/long)
(s/def ::rnd ::m/prob)
(s/def ::rnd-lazy (s/every ::rnd))
(s/def ::apache-rng
  #(or (instance? MersenneTwister %)
       (instance? SobolSequenceGenerator %)
       (instance? ISAACRandom %)))

(s/def ::rnd-vector
  (s/with-gen
    (s/coll-of ::rnd :kind clojure.core/vector? :into [])
    #(gen/vector (s/gen ::rnd) 0 mdl)))

(def ^:dynamic *rng-gen* nil)

;;;HELPERS
(defn random-long
  "Returns a random long within the given interval (or any long by default)."
  ([rnd] (random-long rnd [m/min-long m/max-long]))
  ([rnd [lower upper]] (m/floor' (+ lower (* (- (inc (double upper)) lower) rnd)))))

(s/fdef random-long
        :args (s/cat :rnd ::rnd
                     :interval (s/? ::intervals/long-interval))
        :ret ::m/long)

(defn random-bool
  "Returns a random boolean."
  [rnd]
  (<= rnd 0.5))

(s/fdef random-bool
        :args (s/cat :rnd ::rnd)
        :ret boolean?)

(defn random-normal
  "Returns a value drawn from a standard Normal distribution."
  [rnd]
  (mf/inv-cdf-standard-normal rnd))

(s/fdef random-normal
        :args (s/cat :rnd ::rnd)
        :ret ::m/num)

;;;IMMUTABLE RNG
(defn rng
  "Returns a new RNG."
  [seed]
  (split/make-java-util-splittable-random seed))

(s/fdef rng
        :args (s/cat :seed ::seed)
        :ret ::rng)

(defn rnd
  "Returns a random finite within the given interval (or [0, 1] by default)."
  ([rng] (split/rand-double rng))
  ([rng [lower upper]] (+ lower (* (- upper lower) (split/rand-double rng)))))

(s/fdef rnd
        :args (s/cat :rng ::rng
                     :interval (s/? ::intervals/finite-interval))
        :ret ::m/finite)

(defn rnd-long
  "Returns a random long within the given interval (or any long by default)."
  ([rng] (split/rand-long rng))
  ([rng [lower upper]] (m/floor' (+ lower (* (- (inc (double upper)) lower) (rnd rng))))))

(s/fdef rnd-long
        :args (s/cat :rng ::rng
                     :interval (s/? ::intervals/long-interval))
        :ret ::m/long)

(defn rnd-bool
  "Returns a random boolean."
  [rng]
  (random-bool (rnd rng)))

(s/fdef rnd-bool
        :args (s/cat :rng ::rng)
        :ret boolean?)

(defn rnd-normal
  "Returns a value randomly drawn from a standard Normal distribution."
  [rng]
  (random-normal (rnd rng)))

(s/fdef rnd-normal
        :args (s/cat :rng ::rng)
        :ret ::m/num)

(defn rng-lazy
  "Returns a lazy sequence of RNG where each iteration is split from the previous."
  [rng]
  (iterate (comp first split/split) rng))

(s/fdef rng-lazy
        :args (s/cat :rng ::rng)
        :ret (s/every ::rng))

(defn rnd-lazy
  "Returns a lazy seq of random rnds."
  [rng]
  (map split/rand-double (rng-lazy rng)))

(s/fdef rnd-lazy
        :args (s/cat :rng ::rng)
        :ret ::rnd-lazy)

(defn rnd-long-lazy
  "Returns a lazy seq of random longs."
  [rng]
  (map split/rand-long (rng-lazy rng)))

(s/fdef rnd-long-lazy
        :args (s/cat :rng ::rng)
        :ret (s/every ::m/long))

;;;BOUND RNG
(defn rng!
  "Returns the bound RNG."
  []
  (*rng-gen*))

(s/fdef rng!
        :args (s/cat)
        :ret ::rng)

(defn rng-gen
  "Returns a function that will generate random numbers from the bound or other static RNG."
  [rng]
  (let [gens (atom (rng-lazy rng))]
    (fn [] volatile!
      (let [rng (first @gens)]
        (swap! gens rest)
        rng))))

(defn set-seed!
  "Sets the RNG generator to `seed`."
  [seed]
  (alter-var-root (var *rng-gen*) (constantly (rng-gen (rng seed)))))

(defn rnd!
  "Returns a random finite within the given interval (or [0, 1] by default)."
  ([] (rnd (rng!)))
  ([[lower upper]] (rnd (rng!) [lower upper])))

(s/fdef rnd!
        :args (s/cat :interval (s/? ::intervals/finite-interval))
        :ret ::m/finite)

(defn rnd-long!
  "Returns a random long within the given interval (or any long by default)."
  ([] (rnd-long (rng!)))
  ([[lower upper]] (rnd-long (rng!) [lower upper])))

(s/fdef rnd-long!
        :args (s/cat :interval (s/? ::intervals/long-interval))
        :ret ::m/long)

(defn rnd-bool!
  "Returns a random boolean."
  []
  (rnd-bool (rng!)))

(s/fdef rnd-bool!
        :args (s/cat)
        :ret boolean?)

(defn rnd-normal!
  "Returns a value randomly drawn from a standard Normal distribution."
  []
  (rnd-normal (rng!)))

(s/fdef rnd-normal!
        :args (s/cat)
        :ret ::m/num)

(defn rng-lazy!
  "Returns a lazy sequence of RNG where each iteration is split from the previous."
  []
  (rng-lazy (rng!)))

(s/fdef rng-lazy!
        :args (s/cat)
        :ret (s/every ::rng))

(defn rnd-lazy!
  "Returns a lazy seq of random doubles."
  []
  (rnd-lazy (rng!)))

(s/fdef rnd-lazy!
        :args (s/cat)
        :ret ::rnd-lazy)

(defn rnd-long-lazy!
  "Returns a lazy seq of random longs."
  []
  (rnd-long-lazy (rng!)))

(s/fdef rnd-long-lazy!
        :args (s/cat)
        :ret (s/every ::m/long))

;;;USE CLOCK
(defn rng$
  "Makes a new RNG from the current clock time."
  []
  (split/next-rng))

(s/fdef rng$
        :args (s/cat)
        :ret ::rng)

(defn seed$
  "Returns a new seed from the current clock time."
  []
  (rnd-long (rng$)))

(s/fdef seed$
        :args (s/cat)
        :ret ::seed)

(defn set-seed!$
  "Sets the RNG generator to the current clock time."
  []
  (set-seed! (System/currentTimeMillis)))

;; TODO: Is there a better way to set the default value for *rng-gen*?
;; This is needed due to a circular dependency on functions when initially compiled.
;; You will get an "Attempting to call unbound fn" error if you try to directly set
;; *rng-gen* to `(rng-gen)`.
(set-seed!$)

;;;MACROS
(defmacro bind-seed
  "Sets the seed for the RNGs to `seed` for the code in `body`. If used with a lazy sequence,
  ensure the seq is realized within the scope of the binding otherwise you will get inconsistent
  results."
  [seed & body]
  `(binding [*rng-gen* (rng-gen (rng ~seed))]
     ~@body))

(defmacro do-set-seed!
  "Runs [[set-seed!]] with `seed` and then executes `body`."
  [seed & body]
  `(do
     (set-seed! ~seed)
     ~@body))

;;;APACHE RANDOM NUMBER GENERATORS
(defn quasi-rng
  "Creates an Apache RNG with better coverage but more predictable through
  a lazy sequence of vectors of size `dimensions`.
  Because of predictability, can be better for a single use simulation."
  [dimensions]
  (SobolSequenceGenerator. ^long dimensions))

(s/fdef quasi-rng
        :args (s/cat :dimensions (s/int-in 1 1000))
        :ret ::apache-rng)

(defn quasi-rnd-vector-lazy
  "Better coverage but more predictable through
  a lazy sequence of vectors of size `dimensions`.
  Because of predictability, can be better for a single use simulation."
  [dimensions]
  (let [qr (quasi-rng dimensions)]
    (repeatedly #(vec (.nextVector ^SobolSequenceGenerator qr)))))

(s/fdef quasi-rnd-vector-lazy
        :args (s/cat :dimensions (s/int-in 1 1000))
        :ret (s/every ::rnd-vector))

(defn secure-rng
  "Creates an Apache RNG that is less predictable but slower RNG than Mersenne Twister."
  [seed]
  (ISAACRandom. ^long seed))

(s/fdef secure-rng
        :args (s/cat :seed ::seed)
        :ret ::apache-rng)

(defn secure-rnd-lazy
  "A less predictable but slower rnd-lazy than Mersenne Twister."
  [seed]
  (repeatedly #(.nextDouble ^ISAACRandom (secure-rng seed))))

(s/fdef secure-rnd-lazy
        :args (s/cat :seed ::seed)
        :ret ::rnd-lazy)

(defn mersenne-rng
  "Creates an Apache RNG using `seed`."
  [seed]
  (MersenneTwister. ^long seed))

(s/fdef mersenne-rng
        :args (s/cat :seed ::seed)
        :ret ::apache-rng)

(defn mersenne-rnd-lazy
  "Returns rnd-lazy using `seed`."
  [seed]
  (repeatedly #(.nextDouble ^MersenneTwister (mersenne-rng seed))))

(s/fdef mersenne-rnd-lazy
        :args (s/cat :seed ::seed)
        :ret ::rnd-lazy)

(comment "Not sure if any of the following will be useful in the future..."

         (defn split-random-lazy
           "Returns tuple of a lazy-seq of `rnd-lazy` and a `rnd-lazy`.
           Useful for parallelization.
           There is an extremely tiny chance (2^-64 perhaps) of non-randomness per split."
           [rnd-lazy]
           [(map #(mersenne-rnd-lazy (rnd-long %)) (rest rnd-lazy))
            (mersenne-rnd-lazy (rnd-long (first rnd-lazy)))])

         (defn split-random
           "Returns tuple of rnd-lazy.
           Useful for parallelization.
           There is an extremely tiny chance (2^-64 perhaps) of non-randomness per split."
           [rnd-lazy]
           [(rest rnd-lazy) (mersenne-rnd-lazy (rnd-long (first rnd-lazy)))])

         (defn multi-sample
           "Returns tuple of [sampled-values rnd-lazy].
         Use :r meta-tag on samplef for inputting :rnd or :rnd-lazy (default)."
           [samplef rnd-lazy ^long ntake]
           (if-not (= :rnd (:r (meta samplef)))
             (let [s (take ntake (iterate #(samplef (second %)) (samplef rnd-lazy)))]
               [(map first s) (second (last s))])
             [(map samplef (take ntake rnd-lazy)), (drop ntake rnd-lazy)]))

         (defn multi-sample-indexed
           "Returns tuple of [sampled-values rnd-lazy].
           `samplef` should be function of index and either rnd or rnd-lazy.
           Use :r meta-tag on samplef for inputting :rnd or :rnd-lazy (default)"
           [samplef rnd-lazy ^long ntake]
           (if-not (= :rnd (:r (meta samplef)))
             (loop [coll [], r rnd-lazy, i 0]
               (if (>= i ntake) [coll r]
                                (let [[s laz] (samplef i r)] (recur (conj coll s) laz (inc i)))))
             [(map-indexed samplef (take ntake rnd-lazy)), (drop ntake rnd-lazy)]))

         (defn fold-random
           "Returns tuple of value and rnd-lazy.
           This fn is an extension of 'fold' in core.reducers for folding rnd-lazy or rnd.
           Reduces a collection using a (potentially parallel) reduce-combine strategy.
           The collection is partitioned into groups of approximately chunk-size (default 512), each of which is reduced
           with reducef (with a seed value obtained by calling (combinef) with no arguments).
           For rnd-lazy, the reducef should take the result and first part of the tuple from the samplef.
           The results of these reductions are then reduced with combinef (default reducef).
           combinef must be associative, and, when called with no arguments, (combinef) must produce its identity element.
           These operations may be performed in parallel, but the results will preserve order.
           Use :r meta-tag on samplef for inputting :rnd or :rnd-lazy (default)"
           ([^long min-runs reducef samplef rnd-lazy]
            (fold-random min-runs reducef reducef samplef rnd-lazy))
           ([min-runs combinef reducef samplef rnd-lazy]
            (let [chunk-size 512]
              (fold-random chunk-size (m/ceil (/ min-runs chunk-size)) combinef reducef samplef rnd-lazy)))
           ([chunk-size chunks combinef reducef samplef rnd-lazy]
            (let [runs (* chunk-size chunks)]
              (if (= :rnd (:r (meta samplef)))
                [(reducers/fold chunk-size combinef #(reducef %1 (samplef %2)) (take runs rnd-lazy))
                 (drop runs rnd-lazy)]
                (let [[lazies laz] (split-random-lazy rnd-lazy)]
                  [(reducers/fold chunk-size combinef #(reducef %1 (first (samplef %2))) (take runs lazies))
                   laz]))))))