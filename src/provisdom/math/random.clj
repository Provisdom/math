(ns provisdom.math.random
  "Random number generation and probability distributions.
  
  Provides high-quality random number generators and implementations of 
  common probability distributions including:
  - Uniform, normal (Gaussian), exponential, gamma distributions
  - Beta, chi-squared, Student's t, F distributions  
  - Discrete distributions (binomial, Poisson, etc.)
  - Multivariate distributions
  
  Uses splittable random number generators for reproducible parallel computation.
  All functions support both seeded and unseeded generation."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.core.reducers :as reducers]
    [provisdom.math.core :as m]
    [provisdom.math.internal-splittable-random :as split]
    [provisdom.math.intervals :as intervals]
    [provisdom.math.special-functions :as special-fns]))

(def mdl 6)

(s/def ::rng
  (s/with-gen
    (partial satisfies? split/IRandom)
    #(gen/return (split/next-rng))))

(s/def ::seed ::m/long)
(s/def ::rnd ::m/prob)
(s/def ::rnd-lazy (s/every ::rnd))

(s/def ::rnd-vector
  (s/with-gen
    (s/coll-of ::rnd :kind clojure.core/vector? :into [])
    #(gen/vector (s/gen ::rnd) 0 mdl)))

(def ^:dynamic *rng-gen* nil)

;;;HELPERS
(defn random-long
  "Converts random double `rnd` [0,1) to a long within the specified `interval`.
  
  Parameters:
    `rnd` - Random double in [0,1)
    `interval` - Optional [lower upper] bounds (default: [min-long max-long])
  
  Returns a long uniformly distributed within the interval."
  ([rnd] (random-long rnd [m/min-long m/max-long]))
  ([rnd [lower upper]]
   (m/floor' (+ lower (* (- (inc (double upper)) lower) rnd)))))

(s/fdef random-long
        :args (s/cat :rnd ::rnd
                     :interval (s/? ::intervals/long-interval))
        :ret ::m/long)

(defn random-bool
  "Converts random double `rnd` to a boolean with 50% probability.
  
  Parameters:
    `rnd` - Random double in [0,1)
  
  Returns true if `rnd` ≤ 0.5, false otherwise."
  [rnd]
  (<= rnd 0.5))

(s/fdef random-bool
        :args (s/cat :rnd ::rnd)
        :ret boolean?)

(defn random-normal
  "Converts uniform random value `rnd` to a standard normal distribution.
  
  Uses the inverse CDF transformation method to generate normally
  distributed values with mean=0 and standard deviation=1.
  
  Parameters:
    `rnd` - Random double in [0,1)
  
  Returns a normally distributed value."
  [rnd]
  (special-fns/inv-cdf-standard-normal rnd))

(s/fdef random-normal
        :args (s/cat :rnd ::rnd)
        :ret ::m/num)

;;;IMMUTABLE RNG
(defn rng
  "Creates a new splittable random number generator from `seed`.
  
  Parameters:
    `seed` - Long value to seed the generator
  
  Returns a new RNG instance that implements IRandom."
  [seed]
  (split/make-java-util-splittable-random seed))

(s/fdef rng
        :args (s/cat :seed ::seed)
        :ret ::rng)

(defn rnd
  "Generates a random double from `rng` within the specified `interval`.
  
  Parameters:
    `rng` - Random number generator
    `interval` - Optional [lower upper] bounds (default: [0, 1])
  
  Returns a uniformly distributed double within the interval.
  Special case: if interval difference is infinite, uses scaled generation."
  ([rng] (split/rand-double rng))
  ([rng [lower upper]]
   (let [diff (- (double upper) lower)]
     (if (m/inf+? diff)
       (* 2.0 (rnd rng [(* 0.5 lower) (* 0.5 upper)]))
       (+ lower (* diff (split/rand-double rng)))))))

(s/fdef rnd
        :args (s/cat :rng ::rng
                     :interval (s/? ::intervals/finite-interval))
        :ret ::m/finite)

(defn rnd-long
  "Generates a random long from `rng` within the specified `interval`.
  
  Parameters:
    `rng` - Random number generator  
    `interval` - Optional [lower upper] bounds (default: all possible longs)
  
  Returns a uniformly distributed long within the interval."
  ([rng] (split/rand-long rng))
  ([rng [lower upper]]
   (m/floor' (+ lower
                (* (- (inc (double upper)) lower)
                   (rnd rng))))))

(s/fdef rnd-long
        :args (s/cat :rng ::rng
                     :interval (s/? ::intervals/long-interval))
        :ret ::m/long)

(defn rnd-bool
  "Generates a random boolean from `rng`.
  
  Parameters:
    `rng` - Random number generator
  
  Returns true or false with equal probability."
  [rng]
  (random-bool (rnd rng)))

(s/fdef rnd-bool
        :args (s/cat :rng ::rng)
        :ret boolean?)

(defn rnd-normal
  "Generates a normally distributed value from `rng`.
  
  Parameters:
    `rng` - Random number generator
  
  Returns a value from standard normal distribution (mean=0, std=1)."
  [rng]
  (random-normal (rnd rng)))

(s/fdef rnd-normal
        :args (s/cat :rng ::rng)
        :ret ::m/num)

(defn rng-lazy
  "Creates a lazy sequence of independent RNG instances.
  
  Each RNG in the sequence is split from the previous one, ensuring
  statistical independence for parallel computation.
  
  Parameters:
    `rng` - Initial random number generator
  
  Returns a lazy sequence of RNG instances."
  [rng]
  (iterate (comp first split/split) rng))

(s/fdef rng-lazy
        :args (s/cat :rng ::rng)
        :ret (s/every ::rng))

(defn rnd-lazy
  "Creates a lazy sequence of random doubles from independent RNGs.
  
  Parameters:
    `rng` - Initial random number generator
  
  Returns a lazy sequence of doubles in [0, 1)."
  [rng]
  (map split/rand-double (rng-lazy rng)))

(s/fdef rnd-lazy
        :args (s/cat :rng ::rng)
        :ret ::rnd-lazy)

(defn rnd-long-lazy
  "Creates a lazy sequence of random longs from independent RNGs.
  
  Parameters:
    rng - Initial random number generator
  
  Returns a lazy sequence of random longs."
  [rng]
  (map split/rand-long (rng-lazy rng)))

(s/fdef rnd-long-lazy
        :args (s/cat :rng ::rng)
        :ret (s/every ::m/long))

;;;BOUND RNG
(defn rng!
  "Retrieves the current bound RNG from the dynamic context.
  
  Returns the RNG instance bound to *rng-gen*.
  Must be called within a bind-seed context or after set-seed!."
  []
  (*rng-gen*))

(s/fdef rng!
        :args (s/cat)
        :ret ::rng)

(defn rng-gen
  "Creates a stateful RNG generator function from an initial RNG.
  
  The returned function maintains an internal sequence of split RNGs
  to ensure each call produces an independent generator.
  
  Parameters:
    rng - Initial random number generator
  
  Returns a function that produces fresh RNG instances on each call."
  [rng]
  (let [gens (atom (rng-lazy rng))]
    (fn [] volatile!
      (let [rng (first @gens)]
        (swap! gens rest)
        rng))))

(defn set-seed!
  "Sets the global RNG generator to use the specified seed.
  
  This mutates the global *rng-gen* var, affecting all subsequent
  calls to bound RNG functions (rnd!, rnd-long!, etc.).
  
  Parameters:
    seed - Long value to seed the generator
  
  Side effects: Modifies global *rng-gen* state."
  [seed]
  (alter-var-root (var *rng-gen*) (constantly (rng-gen (rng seed)))))

(defn rnd!
  "Generates a random double using the bound RNG.
  
  Parameters:
    interval - Optional [lower upper] bounds (default: [0, 1])
  
  Returns a uniformly distributed double within the interval.
  Requires bound RNG context (bind-seed or set-seed!)."
  ([] (rnd (rng!)))
  ([[lower upper]] (rnd (rng!) [lower upper])))

(s/fdef rnd!
        :args (s/cat :interval (s/? ::intervals/finite-interval))
        :ret ::m/finite)

(defn rnd-long!
  "Generates a random long using the bound RNG.
  
  Parameters:
    interval - Optional [lower upper] bounds (default: all possible longs)
  
  Returns a uniformly distributed long within the interval.
  Requires bound RNG context (bind-seed or set-seed!)."
  ([] (rnd-long (rng!)))
  ([[lower upper]] (rnd-long (rng!) [lower upper])))

(s/fdef rnd-long!
        :args (s/cat :interval (s/? ::intervals/long-interval))
        :ret ::m/long)

(defn rnd-bool!
  "Generates a random boolean using the bound RNG.
  
  Returns true or false with equal probability.
  Requires bound RNG context (bind-seed or set-seed!)."
  []
  (rnd-bool (rng!)))

(s/fdef rnd-bool!
        :args (s/cat)
        :ret boolean?)

(defn rnd-normal!
  "Generates a normally distributed value using the bound RNG.
  
  Returns a value from standard normal distribution (mean=0, std=1).
  Requires bound RNG context (bind-seed or set-seed!)."
  []
  (rnd-normal (rng!)))

(s/fdef rnd-normal!
        :args (s/cat)
        :ret ::m/num)

(defn rng-lazy!
  "Creates a lazy sequence of independent RNGs using the bound RNG.
  
  Returns a lazy sequence of RNG instances, each split from the previous.
  Requires bound RNG context (bind-seed or set-seed!)."
  []
  (rng-lazy (rng!)))

(s/fdef rng-lazy!
        :args (s/cat)
        :ret (s/every ::rng))

(defn rnd-lazy!
  "Creates a lazy sequence of random doubles using the bound RNG.
  
  Returns a lazy sequence of doubles in [0, 1).
  Requires bound RNG context (bind-seed or set-seed!)."
  []
  (rnd-lazy (rng!)))

(s/fdef rnd-lazy!
        :args (s/cat)
        :ret ::rnd-lazy)

(defn rnd-long-lazy!
  "Creates a lazy sequence of random longs using the bound RNG.
  
  Returns a lazy sequence of random longs.
  Requires bound RNG context (bind-seed or set-seed!)."
  []
  (rnd-long-lazy (rng!)))

(s/fdef rnd-long-lazy!
        :args (s/cat)
        :ret (s/every ::m/long))

;;;USE CLOCK
(defn rng$
  "Creates a new RNG seeded from the current system time.
  
  Uses the current clock time to generate a unique RNG instance.
  Useful for non-reproducible random generation.
  
  Returns a new RNG instance."
  []
  (split/next-rng))

(s/fdef rng$
        :args (s/cat)
        :ret ::rng)

(defn seed$
  "Generates a random seed from the current system time.
  
  Creates a time-based RNG and extracts a random long value
  suitable for seeding other generators.
  
  Returns a random long seed value."
  []
  (rnd-long (rng$)))

(s/fdef seed$
        :args (s/cat)
        :ret ::seed)

(defn set-seed!$
  "Sets the global RNG generator using the current system time.
  
  Equivalent to calling set-seed! with a time-based seed.
  Provides non-reproducible random behavior.
  
  Side effects: Modifies global *rng-gen* state."
  []
  (set-seed! (System/currentTimeMillis)))

;; TODO: Is there a better way to set the default value for *rng-gen*?
;; This is needed due to a circular dependency on functions when initially
;; compiled. You will get an "Attempting to call unbound fn" error if you try to
;; directly set *rng-gen* to `(rng-gen)`.
(set-seed!$)

;;;MACROS
(defmacro bind-seed
  "Sets the seed for the RNGs to `seed` for the code in `body`. If used with a
  lazy sequence, ensure the seq is realized within the scope of the binding
  otherwise you will get inconsistent results."
  [seed & body]
  `(binding [*rng-gen* (rng-gen (rng ~seed))]
     ~@body))

(defmacro do-set-seed!
  "Runs [[set-seed!]] with `seed` and then executes `body`."
  [seed & body]
  `(do
     (set-seed! ~seed)
     ~@body))

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
             [(map samplef (take ntake rnd-lazy))
              (drop ntake rnd-lazy)]))

         (defn multi-sample-indexed
           "Returns tuple of [sampled-values rnd-lazy].
           `samplef` should be function of index and either rnd or rnd-lazy.
           Use :r meta-tag on samplef for inputting :rnd or :rnd-lazy (default)"
           [samplef rnd-lazy ^long ntake]
           (if-not (= :rnd (:r (meta samplef)))
             (loop [coll []
                    r rnd-lazy
                    i 0]
               (if (>= i ntake)
                 [coll r]
                 (let [[s laz] (samplef i r)]
                   (recur (conj coll s) laz (inc i)))))
             [(map-indexed samplef (take ntake rnd-lazy))
              (drop ntake rnd-lazy)]))

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
