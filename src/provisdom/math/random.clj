(ns provisdom.math.random
  "Random number generation with splittable generators.

  Provides high-quality random number generators with support for:
  - Uniform random doubles and longs
  - Normal (Gaussian) distribution via inverse CDF
  - UUID generation (RFC 4122 version 4)
  - Collection utilities (shuffle, sample, weighted choice)
  - Parallel random sampling with split RNGs

  Uses splittable random number generators for reproducible parallel computation.
  All functions support both seeded and unseeded generation.

  Algorithm options (use with `rng` function):
  - :default - L64X128MixRandom (best balance of speed/quality, Oracle recommended)
  - :fast - L32X64MixRandom (fastest, smaller state)
  - :quality - L128X256MixRandom (higher quality, 4-dim equidistribution)
  - :max-quality - L128X1024MixRandom (highest quality, 16-dim equidistribution)
  - :legacy - SplittableRandom (Java 8 algorithm)
  - :secure - SecureRandom (cryptographically secure, NOT splittable)"
  (:refer-clojure :exclude [random-uuid])
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.math.core :as m]
    [provisdom.math.intervals :as intervals]
    [provisdom.math.special-functions :as special-fns])
  (:import
    [java.util UUID]
    [java.util.random RandomGenerator$SplittableGenerator RandomGeneratorFactory]
    [java.security SecureRandom]))

(declare ensure-rng-gen!)

(def mdl 6)

;;;PROTOCOL AND IMPLEMENTATIONS
(defprotocol IRandom
  "Protocol for splittable random number generators."
  (rand-long [rng]
    "Returns a random long based on the given immutable RNG.
    Note: to maintain independence you should not call more than one
    function in the IRandom protocol with the same argument")
  (rand-double [rng]
    "Returns a random double between 0.0 (inclusive) and 1.0 (exclusive)
    based on the given immutable RNG.
    Note: to maintain independence you should not call more than one
    function in the IRandom protocol with the same argument")
  (split [rng]
    "Returns two new RNGs [rng1 rng2], which should generate
    sufficiently independent random data.
    Note: to maintain independence you should not call more than one
    function in the IRandom protocol with the same argument")
  (split-n [rng n]
    "Returns a collection of `n` RNGs, which should generate
    sufficiently independent random data.
    Note: to maintain independence you should not call more than one
    function in the IRandom protocol with the same argument"))

(def ^:const ^:private default-algorithm "L64X128MixRandom")

(def algorithms
  "Available splittable random algorithms.
   :default - L64X128MixRandom (best balance, Oracle recommended)
   :fast - L32X64MixRandom (fastest, 32-bit)
   :quality - L128X256MixRandom (better quality, 4-dim equidistribution)
   :max-quality - L128X1024MixRandom (best quality, 16-dim equidistribution)
   :legacy - SplittableRandom (Java 8 algorithm)
   :secure - SecureRandom (cryptographic, NOT splittable)"
  {:default     "L64X128MixRandom"
   :fast        "L32X64MixRandom"
   :quality     "L128X256MixRandom"
   :max-quality "L128X1024MixRandom"
   :legacy      "SplittableRandom"
   :secure      :secure})

(defn- ^RandomGenerator$SplittableGenerator create-gen
  "Creates a mutable Java generator from algorithm name and seed."
  [^String algo ^long seed]
  (.create (RandomGeneratorFactory/of algo) seed))

(set! *unchecked-math* :warn-on-boxed)

(deftype Java17SplittableRandom [^String algorithm ^long seed]
  IRandom
  (rand-long [_]
    (.nextLong (create-gen algorithm seed)))
  (rand-double [_]
    (.nextDouble (create-gen algorithm seed)))
  (split [_]
    (let [gen (create-gen algorithm seed)
          child (.split gen)]
      [(Java17SplittableRandom. algorithm (.nextLong gen))
       (Java17SplittableRandom. algorithm (.nextLong child))]))
  (split-n [this n]
    (let [n (long n)]
      (case n
        0 []
        1 [this]
        (let [gen (create-gen algorithm seed)]
          (loop [i (dec n)
                 ret (transient [])]
            (if (zero? i)
              (-> ret
                (conj! (Java17SplittableRandom. algorithm (.nextLong gen)))
                persistent!)
              (let [child (.split gen)]
                (recur (dec i)
                  (conj! ret (Java17SplittableRandom.
                               algorithm
                               (.nextLong child))))))))))))

(set! *unchecked-math* false)

(deftype SecureRandomWrapper [^SecureRandom secure-rng]
  IRandom
  (rand-long [_]
    (.nextLong secure-rng))
  (rand-double [_]
    (.nextDouble secure-rng))
  (split [_]
    ;; Not truly splittable - creates independent secure instances
    [(SecureRandomWrapper. (SecureRandom.))
     (SecureRandomWrapper. (SecureRandom.))])
  (split-n [_ n]
    (repeatedly n #(SecureRandomWrapper. (SecureRandom.)))))

(defn- make-rng
  "Internal factory for creating RNG instances."
  ([^long seed]
   (Java17SplittableRandom. default-algorithm seed))
  ([^long seed algorithm]
   (if (= algorithm :secure)
     (SecureRandomWrapper. (SecureRandom.))
     (let [algo (if (keyword? algorithm)
                  (get algorithms algorithm default-algorithm)
                  (str algorithm))]
       (Java17SplittableRandom. algo seed)))))

(def next-rng$
  "Returns a random-number generator. Successive calls should return independent results."
  (let [a (atom (make-rng (System/currentTimeMillis)))
        thread-local
        (proxy [ThreadLocal] []
          (initialValue []
            (first (split (swap! a #(second (split %)))))))]
    (fn []
      (let [rng (.get ^ThreadLocal thread-local)
            [rng1 rng2] (split rng)]
        (.set ^ThreadLocal thread-local rng2)
        rng1))))

;;;SPECS
(s/def ::algorithm (into #{} (keys algorithms)))

(s/def ::rng
  (s/with-gen
    (partial satisfies? IRandom)
    #(gen/return (next-rng$))))

(s/fdef next-rng$
  :args (s/cat)
  :ret ::rng)

(s/def ::seed ::m/long)
(s/def ::rnd ::m/prob)
(s/def ::rnd-lazy (s/every ::rnd))

(s/def ::rnd-vector
  (s/with-gen
    (s/coll-of ::rnd :kind clojure.core/vector? :into [])
    #(gen/vector (s/gen ::rnd) 0 mdl)))

(s/def ::n
  (s/with-gen
    ::m/int-non-
    #(gen/choose 0 mdl)))

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

(defn random-uuid
  "Converts two random doubles to a UUID (version 4).

  Creates a random UUID following RFC 4122 version 4 format.
  Requires two random values since a UUID needs 128 bits of randomness.

  Parameters:
    `rnd1` - Random double in [0,1) for most significant bits
    `rnd2` - Random double in [0,1) for least significant bits

  Returns a random java.util.UUID."
  [rnd1 rnd2]
  (let [msb (-> (random-long rnd1)
              (bit-and (unchecked-long 0xffffffffffff0fff))
              (bit-or (unchecked-long 0x0000000000004000)))
        lsb (-> (random-long rnd2)
              (bit-and (unchecked-long 0x3fffffffffffffff))
              (bit-or (unchecked-long 0x8000000000000000)))]
    (UUID. msb lsb)))

(s/fdef random-uuid
  :args (s/cat :rnd1 ::rnd :rnd2 ::rnd)
  :ret uuid?)

;;;IMMUTABLE RNG
(defn rng
  "Creates a new splittable random number generator from `seed`.

  Parameters:
    `seed` - Long value to seed the generator
    `algorithm` - Optional algorithm keyword or string name (default: :default)
                  Options: :default, :fast, :quality, :max-quality, :legacy, :secure

  Returns a new RNG instance that implements IRandom."
  ([seed]
   (make-rng seed))
  ([seed algorithm]
   (make-rng seed algorithm)))

(s/fdef rng
  :args (s/cat :seed ::seed
          :algorithm (s/? ::algorithm))
  :ret ::rng)

(defn rnd
  "Generates a random double from `rng` within the specified `interval`.
  
  Parameters:
    `rng` - Random number generator
    `interval` - Optional [lower upper] bounds (default: [0, 1])
  
  Returns a uniformly distributed double within the interval.
  Special case: if interval difference is infinite, uses scaled generation."
  ([rng] (rand-double rng))
  ([rng [lower upper]]
   (let [diff (- (double upper) lower)]
     (if (m/inf+? diff)
       (* 2.0 (rnd rng [(* 0.5 lower) (* 0.5 upper)]))
       (+ lower (* diff (rand-double rng)))))))

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
  ([rng] (rand-long rng))
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

(defn rnd-uuid
  "Generates a random UUID (version 4) from `rng`.

  Creates a random UUID following RFC 4122 version 4 format,
  using two independent random doubles from split RNGs.

  Parameters:
    `rng` - Random number generator

  Returns a random java.util.UUID."
  [rng]
  (let [[rng1 rng2] (split rng)]
    (random-uuid (rnd rng1) (rnd rng2))))

(s/fdef rnd-uuid
  :args (s/cat :rng ::rng)
  :ret uuid?)

(defn rng-lazy
  "Creates a lazy sequence of independent RNG instances.
  
  Each RNG in the sequence is split from the previous one, ensuring
  statistical independence for parallel computation.
  
  Parameters:
    `rng` - Initial random number generator
  
  Returns a lazy sequence of RNG instances."
  [rng]
  (iterate (comp first split) rng))

(s/fdef rng-lazy
  :args (s/cat :rng ::rng)
  :ret (s/every ::rng))

(defn rnd-lazy
  "Creates a lazy sequence of random doubles from independent RNGs.
  
  Parameters:
    `rng` - Initial random number generator
  
  Returns a lazy sequence of doubles in [0, 1)."
  [rng]
  (map rand-double (rng-lazy rng)))

(s/fdef rnd-lazy
  :args (s/cat :rng ::rng)
  :ret ::rnd-lazy)

(defn rnd-long-lazy
  "Creates a lazy sequence of random longs from independent RNGs.
  
  Parameters:
    rng - Initial random number generator
  
  Returns a lazy sequence of random longs."
  [rng]
  (map rand-long (rng-lazy rng)))

(s/fdef rnd-long-lazy
  :args (s/cat :rng ::rng)
  :ret (s/every ::m/long))

;;;BOUND RNG
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

;; Use fn? instead of fspec - Orchestra would call the function to validate,
;; which consumes state from the lazy RNG sequence
(s/def ::rng-gen-fn
  (s/with-gen fn?
    #(gen/return (fn [] (rng 42)))))

(s/fdef rng-gen
  :args (s/cat :rng ::rng)
  :ret ::rng-gen-fn)

(defn- ensure-rng-gen!
  "Lazily initializes *rng-gen* on first use if nil.
  Uses system time for non-reproducible default behavior."
  []
  (when (nil? *rng-gen*)
    (alter-var-root (var *rng-gen*)
      (fn [current]
        (if (nil? current)
          (rng-gen (rng (System/currentTimeMillis)))
          current)))))

(defn rng!
  "Retrieves the current bound RNG from the dynamic context.

  Returns the RNG instance bound to *rng-gen*.
  Lazily initializes with system time if not already set."
  []
  (ensure-rng-gen!)
  (*rng-gen*))

(s/fdef rng!
  :args (s/cat)
  :ret ::rng)

(defn set-seed!
  "Sets the global RNG generator to use the specified seed.

  This mutates the global *rng-gen* var, affecting all subsequent calls to bound RNG
  functions (rnd!, rnd-long!, etc.).

  Parameters:
    seed - Long value to seed the generator

  Side effects: Modifies global *rng-gen* state."
  [seed]
  (alter-var-root (var *rng-gen*) (constantly (rng-gen (rng seed)))))

(s/fdef set-seed!
  :args (s/cat :seed ::seed)
  :ret ::rng-gen-fn)

(defn rnd!
  "Generates a random double using the bound RNG.
  
  Parameters:
    interval - Optional [lower upper] bounds (default: [0, 1])
  
  Returns a uniformly distributed double within the interval.
  Requires the bound RNG context (bind-seed or set-seed!)."
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
  Requires the bound RNG context (bind-seed or set-seed!)."
  ([] (rnd-long (rng!)))
  ([[lower upper]] (rnd-long (rng!) [lower upper])))

(s/fdef rnd-long!
  :args (s/cat :interval (s/? ::intervals/long-interval))
  :ret ::m/long)

(defn rnd-bool!
  "Generates a random boolean using the bound RNG.
  
  Returns true or false with equal probability.
  Requires the bound RNG context (bind-seed or set-seed!)."
  []
  (rnd-bool (rng!)))

(s/fdef rnd-bool!
  :args (s/cat)
  :ret boolean?)

(defn rnd-normal!
  "Generates a normally distributed value using the bound RNG.
  
  Returns a value from standard normal distribution (mean=0, std=1).
  Requires the bound RNG context (bind-seed or set-seed!)."
  []
  (rnd-normal (rng!)))

(s/fdef rnd-normal!
  :args (s/cat)
  :ret ::m/num)

(defn rnd-uuid!
  "Generates a random UUID (version 4) using the bound RNG.

  Returns a random java.util.UUID.
  Requires the bound RNG context (bind-seed or set-seed!)."
  []
  (rnd-uuid (rng!)))

(s/fdef rnd-uuid!
  :args (s/cat)
  :ret uuid?)

(defn rng-lazy!
  "Creates a lazy sequence of independent RNGs using the bound RNG.

  Returns a lazy sequence of RNG instances, each split from the previous.
  Requires the bound RNG context (bind-seed or set-seed!)."
  []
  (rng-lazy (rng!)))

(s/fdef rng-lazy!
  :args (s/cat)
  :ret (s/every ::rng))

(defn rnd-lazy!
  "Creates a lazy sequence of random doubles using the bound RNG.
  
  Returns a lazy sequence of doubles in [0, 1).
  Requires the bound RNG context (bind-seed or set-seed!)."
  []
  (rnd-lazy (rng!)))

(s/fdef rnd-lazy!
  :args (s/cat)
  :ret ::rnd-lazy)

(defn rnd-long-lazy!
  "Creates a lazy sequence of random longs using the bound RNG.
  
  Returns a lazy sequence of random longs.
  Requires the bound RNG context (bind-seed or set-seed!)."
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
  (next-rng$))

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

(s/fdef set-seed!$
  :args (s/cat)
  :ret ::rng-gen-fn)

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

;;;INTROSPECTION
(defn rng-algorithm
  "Returns the algorithm name for the given RNG.

  Parameters:
    `rng` - Random number generator

  Returns the algorithm string (e.g., \"L64X128MixRandom\") or :secure for SecureRandom."
  [rng]
  (cond
    (instance? Java17SplittableRandom rng) (.-algorithm ^Java17SplittableRandom rng)
    (instance? SecureRandomWrapper rng) :secure
    :else (throw (ex-info "Unknown RNG type" {:rng rng}))))

(s/fdef rng-algorithm
  :args (s/cat :rng ::rng)
  :ret (s/or :algorithm string? :secure #{:secure}))

(defn rng-seed
  "Returns the original seed for the given RNG.

  Parameters:
    `rng` - Random number generator

  Returns the seed (long) or nil for SecureRandom (which has no seed)."
  [rng]
  (cond
    (instance? Java17SplittableRandom rng) (.-seed ^Java17SplittableRandom rng)
    (instance? SecureRandomWrapper rng) nil
    :else (throw (ex-info "Unknown RNG type" {:rng rng}))))

(s/fdef rng-seed
  :args (s/cat :rng ::rng)
  :ret (s/nilable ::seed))

;;;BATCH GENERATION
(defn rnd-doubles
  "Generates a vector of `n` random doubles from `rng`.

  Parameters:
    `rng` - Random number generator
    `n` - Number of doubles to generate

  Returns a vector of doubles in [0, 1)."
  [rng n]
  (mapv rand-double (take n (rng-lazy rng))))

(s/fdef rnd-doubles
  :args (s/cat :rng ::rng :n ::n)
  :ret ::rnd-vector)

(defn rnd-doubles!
  "Generates a vector of `n` random doubles using the bound RNG.

  Parameters:
    `n` - Number of doubles to generate

  Returns a vector of doubles in [0, 1)."
  [n]
  (rnd-doubles (rng!) n))

(s/fdef rnd-doubles!
  :args (s/cat :n ::n)
  :ret ::rnd-vector)

(defn rnd-longs
  "Generates a vector of `n` random longs from `rng`.

  Parameters:
    `rng` - Random number generator
    `n` - Number of longs to generate

  Returns a vector of random longs."
  [rng n]
  (mapv rand-long (take n (rng-lazy rng))))

(s/fdef rnd-longs
  :args (s/cat :rng ::rng :n ::n)
  :ret (s/coll-of ::m/long :kind vector?))

(defn rnd-longs!
  "Generates a vector of `n` random longs using the bound RNG.

  Parameters:
    `n` - Number of longs to generate

  Returns a vector of random longs."
  [n]
  (rnd-longs (rng!) n))

(s/fdef rnd-longs!
  :args (s/cat :n ::n)
  :ret (s/coll-of ::m/long :kind vector?))

(defn rnd-normals
  "Generates a vector of `n` standard normal values from `rng`.

  Parameters:
    `rng` - Random number generator
    `n` - Number of normal values to generate

  Returns a vector of standard normal values (mean=0, std=1)."
  [rng n]
  (mapv (comp random-normal rand-double) (take n (rng-lazy rng))))

(s/fdef rnd-normals
  :args (s/cat :rng ::rng :n ::n)
  :ret (s/coll-of ::m/num :kind vector?))

(defn rnd-normals!
  "Generates a vector of `n` standard normal values using the bound RNG.

  Parameters:
    `n` - Number of normal values to generate

  Returns a vector of standard normal values (mean=0, std=1)."
  [n]
  (rnd-normals (rng!) n))

(s/fdef rnd-normals!
  :args (s/cat :n ::n)
  :ret (s/coll-of ::m/num :kind vector?))

;;;CONVENIENCE FUNCTIONS
(defn rnd-int
  "Generates a random integer from `rng` in [0, upper).

  Parameters:
    `rng` - Random number generator
    `upper` - Exclusive upper bound (positive integer)

  Returns a random integer in [0, upper)."
  [rng upper]
  (int (m/floor (* upper (rand-double rng)))))

(s/fdef rnd-int
  :args (s/cat :rng ::rng :upper ::m/int+)
  :ret ::m/int-non-)

(defn rnd-int!
  "Generates a random integer using the bound RNG in [0, upper).

  Parameters:
    `upper` - Exclusive upper bound (positive integer)

  Returns a random integer in [0, upper)."
  [upper]
  (rnd-int (rng!) upper))

(s/fdef rnd-int!
  :args (s/cat :upper ::m/int+)
  :ret ::m/int-non-)

(defn rnd-gaussian
  "Generates a Gaussian (normal) distributed value from `rng`.

  Parameters:
    `rng` - Random number generator
    `mean` - Mean of the distribution
    `std` - Standard deviation of the distribution

  Returns a normally distributed value with given mean and std."
  [rng mean std]
  (+ mean (* std (rnd-normal rng))))

(s/fdef rnd-gaussian
  :args (s/cat :rng ::rng :mean ::m/num :std ::m/non-)
  :ret ::m/num)

(defn rnd-gaussian!
  "Generates a Gaussian (normal) distributed value using the bound RNG.

  Parameters:
    `mean` - Mean of the distribution
    `std` - Standard deviation of the distribution

  Returns a normally distributed value with given mean and std."
  [mean std]
  (rnd-gaussian (rng!) mean std))

(s/fdef rnd-gaussian!
  :args (s/cat :mean ::m/num :std ::m/non-)
  :ret ::m/num)

;;;COLLECTION UTILITIES
(defn rnd-choice
  "Picks a random element from `coll` using `rng`.

  Parameters:
    `rng` - Random number generator
    `coll` - Collection to pick from (must be non-empty)

  Returns a random element from the collection."
  [rng coll]
  (let [v (vec coll)
        n (count v)]
    (when (pos? n)
      (nth v (rnd-int rng n)))))

(s/fdef rnd-choice
  :args (s/cat :rng ::rng :coll (s/coll-of any?))
  :ret any?)

(defn rnd-choice!
  "Picks a random element from `coll` using the bound RNG.

  Parameters:
    `coll` - Collection to pick from (must be non-empty)

  Returns a random element from the collection."
  [coll]
  (rnd-choice (rng!) coll))

(s/fdef rnd-choice!
  :args (s/cat :coll (s/coll-of any?))
  :ret any?)

(defn rnd-shuffle
  "Returns a randomly shuffled vector of `coll` using Fisher-Yates algorithm.

  Parameters:
    `rng` - Random number generator
    `coll` - Collection to shuffle

  Returns a new vector with elements in random order."
  [rng coll]
  (let [v (transient (vec coll))
        n (count v)
        rngs (rng-lazy rng)]
    (loop [i (dec n)
           rngs rngs]
      (if (pos? i)
        (let [j (rnd-int (first rngs) (inc i))
              vi (get v i)
              vj (get v j)]
          (assoc! v i vj)
          (assoc! v j vi)
          (recur (dec i) (rest rngs)))
        (persistent! v)))))

(s/fdef rnd-shuffle
  :args (s/cat :rng ::rng :coll (s/coll-of any?))
  :ret vector?)

(defn rnd-shuffle!
  "Returns a randomly shuffled vector of `coll` using the bound RNG.

  Parameters:
    `coll` - Collection to shuffle

  Returns a new vector with elements in random order."
  [coll]
  (rnd-shuffle (rng!) coll))

(s/fdef rnd-shuffle!
  :args (s/cat :coll (s/coll-of any?))
  :ret vector?)

(defn rnd-sample
  "Samples `n` elements from `coll` without replacement using `rng`.

  Parameters:
    `rng` - Random number generator
    `n` - Number of elements to sample
    `coll` - Collection to sample from

  Returns a vector of `n` randomly selected elements (or fewer if coll is smaller)."
  [rng n coll]
  (let [v (vec coll)
        k (min n (count v))]
    (subvec (rnd-shuffle rng v) 0 k)))

(s/fdef rnd-sample
  :args (s/cat :rng ::rng :n ::n :coll (s/coll-of any?))
  :ret vector?)

(defn rnd-sample!
  "Samples `n` elements from `coll` without replacement using the bound RNG.

  Parameters:
    `n` - Number of elements to sample
    `coll` - Collection to sample from

  Returns a vector of `n` randomly selected elements (or fewer if coll is smaller)."
  [n coll]
  (rnd-sample (rng!) n coll))

(s/fdef rnd-sample!
  :args (s/cat :n ::n :coll (s/coll-of any?))
  :ret vector?)

(defn rnd-weighted-choice
  "Picks a random element from `coll` with probability proportional to `weights`.

  Parameters:
    `rng` - Random number generator
    `weights` - Sequence of non-negative weights (same length as coll)
    `coll` - Collection to pick from

  Returns a randomly selected element, with selection probability proportional to weight."
  [rng weights coll]
  (let [v (vec coll)
        ws (vec weights)
        total (reduce + 0.0 ws)
        r (* total (rand-double rng))]
    (loop [i 0
           cum-sum 0.0]
      (if (< i (count v))
        (let [cum-sum' (+ cum-sum (double (nth ws i)))]
          (if (< r cum-sum')
            (nth v i)
            (recur (inc i) cum-sum')))
        (peek v)))))

(s/def ::weighted-choice-args
  (s/with-gen
    (s/and (s/cat :rng ::rng
             :weights (s/coll-of ::m/non- :min-count 1)
             :coll (s/coll-of any? :min-count 1))
      #(= (count (:weights %)) (count (:coll %))))
    #(gen/bind (gen/choose 1 mdl)
       (fn [n]
         (gen/tuple (s/gen ::rng)
           (gen/vector (s/gen ::m/non-) n)
           (gen/vector (s/gen any?) n))))))

(s/fdef rnd-weighted-choice
  :args ::weighted-choice-args
  :ret any?)

(defn rnd-weighted-choice!
  "Picks a random element from `coll` with probability proportional to `weights`.

  Parameters:
    `weights` - Sequence of non-negative weights (same length as coll)
    `coll` - Collection to pick from

  Returns a randomly selected element, with selection probability proportional to weight."
  [weights coll]
  (rnd-weighted-choice (rng!) weights coll))

(s/def ::weighted-choice!-args
  (s/with-gen
    (s/and (s/cat :weights (s/coll-of ::m/non- :min-count 1)
             :coll (s/coll-of any? :min-count 1))
      #(= (count (:weights %)) (count (:coll %))))
    #(gen/bind (gen/choose 1 mdl)
       (fn [n]
         (gen/tuple (gen/vector (s/gen ::m/non-) n)
           (gen/vector (s/gen any?) n))))))

(s/fdef rnd-weighted-choice!
  :args ::weighted-choice!-args
  :ret any?)

;;;PARALLEL UTILITIES
(defn parallel-sample
  "Samples `n` values in parallel using split RNGs.

  Parameters:
    `rng` - Random number generator
    `sample-fn` - Function that takes an RNG and returns a sample
    `n` - Number of samples to generate

  Returns a vector of `n` samples generated using independent RNGs."
  [rng sample-fn n]
  (let [rngs (split-n rng n)]
    (into [] (pmap sample-fn rngs))))

(s/def ::sample-fn
  (s/with-gen fn?
    #(gen/return (fn [rng] (rnd rng)))))

(s/fdef parallel-sample
  :args (s/cat :rng ::rng :sample-fn ::sample-fn :n ::n)
  :ret vector?)

(defn parallel-fold
  "Parallel fold over `n` random samples using split RNGs.

  Parameters:
    `rng` - Random number generator
    `n` - Number of samples
    `combine-fn` - Associative combining function (called with 0 args for identity)
    `reduce-fn` - Reducing function (acc, sample) -> acc
    `sample-fn` - Function that takes an RNG and returns a sample

  Returns the folded result."
  [rng n combine-fn reduce-fn sample-fn]
  (let [rngs (split-n rng n)
        samples (pmap sample-fn rngs)]
    (reduce reduce-fn (combine-fn) samples)))

;; Generic HOF function specs - use fn? because these accept ANY function
;; Generators provide sample functions for spec-checking
(s/def ::combine-fn
  (s/with-gen fn?
    #(gen/return (fn [] []))))

(s/def ::reduce-fn
  (s/with-gen fn?
    #(gen/return (fn [acc sample] (conj (if (vector? acc) acc []) sample)))))

(s/fdef parallel-fold
  :args (s/cat :rng ::rng
          :n ::n
          :combine-fn ::combine-fn
          :reduce-fn ::reduce-fn
          :sample-fn ::sample-fn)
  :ret any?)
