(ns provisdom.math.random
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.math.core :as m]
    [provisdom.math.intervals :as intervals]
    [provisdom.math.special-functions :as special-fns]
    [provisdom.math.internal-splittable-random :as split]
    [clojure.core.reducers :as reducers]))

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
  "Returns a random long within the given interval (or any long by default)."
  ([rnd] (random-long rnd [m/min-long m/max-long]))
  ([rnd [lower upper]]
   (m/floor' (+ lower (* (- (inc (double upper)) lower) rnd)))))

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
  (special-fns/inv-cdf-standard-normal rnd))

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
  "Returns a random long within the given interval (or any long by default)."
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
  "Returns a lazy sequence of RNG where each iteration is split from the
  previous."
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
  "Returns a function that will generate random numbers from the bound or other
  static RNG."
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
  "Returns a lazy sequence of RNG where each iteration is split from the
  previous."
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
;; This is needed due to a circular dependency on functions when initially
;; compiled.
;; You will get an "Attempting to call unbound fn" error if you try to directly
;; set *rng-gen* to `(rng-gen)`.
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