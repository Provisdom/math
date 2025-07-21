(ns provisdom.math.intervals
  "Interval arithmetic and bounds checking utilities.
  
  Provides comprehensive interval and bounds manipulation for:
  - Simple intervals [a, b] with inclusive endpoints
  - Complex bounds with open/closed endpoint specifications  
  - Interval operations (intersection, union, containment tests)
  - Bounds generation for optimization and constraint problems
  - Specialized bounds for positive definite matrices
  
  Intervals are represented as [lower, upper] vectors.
  Bounds are maps with `::lower`, `::upper`, `::open-lower?`, `::open-upper?`
    keys."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.math.core :as m]))

(def mdl 6)

(s/def ::size
  (s/with-gen ::m/int-non- #(gen/large-integer* {:min 0 :max mdl})))

(defmacro interval-spec
  ([spec] `(interval-spec ~spec ~spec))
  ([lower upper & {:keys [compare-op]
                   :or   {compare-op `>=}}]
   (let [lower-is-keyword? (keyword? lower)
         upper-is-keyword? (keyword? upper)
         lower-spec (if lower-is-keyword? lower `number?)
         upper-spec (if upper-is-keyword? upper `number?)
         lower-gen (if lower-is-keyword? 
                     `(s/gen ~lower) 
                     `(gen/return ~lower))
         upper-gen (if upper-is-keyword? 
                     `(s/gen ~upper) 
                     `(gen/return ~upper))]
     `(s/with-gen
        (s/and (s/tuple ~lower-spec ~upper-spec)
          (fn [[~'x1 ~'x2]] (or (~compare-op ~'x2 ~'x1)
                              (and (m/nan? ~'x1) (m/nan? ~'x2)))))
        #(gen/fmap (comp vec sort) 
                   (gen/tuple ~lower-gen ~upper-gen))))))

(defmacro strict-interval-spec
  ([spec] `(strict-interval-spec ~spec ~spec))
  ([lower upper & opts]
   `(interval-spec ~lower ~upper :compare-op > ~@opts)))

(s/def ::strict-interval (strict-interval-spec ::m/number))

(s/def ::interval (interval-spec ::m/number))

(s/def ::num-interval (interval-spec ::m/num))

(s/def ::pos-interval (interval-spec ::m/pos))

(s/def ::finite-interval (interval-spec ::m/finite))

(s/def ::finite+-interval (interval-spec ::m/finite+))

(s/def ::prob-interval (interval-spec ::m/prob))

(s/def ::open-prob-interval (interval-spec ::m/open-prob))

(s/def ::finite+-interval (interval-spec ::m/finite+))

(s/def ::finite-non--interval (interval-spec ::m/finite-non-))

(s/def ::int-interval (interval-spec ::m/int))

(s/def ::int+-interval (interval-spec ::m/int+))

(s/def ::int-non--interval (interval-spec ::m/int-non-))

(s/def ::long-interval (interval-spec ::m/long))

(s/def ::long+-interval (interval-spec ::m/long+))

(defn long-interval-gen
  ""
  [min max]
  (gen/tuple (gen/large-integer* {:min min :max max})
    (gen/large-integer* {:min min :max max})))

(s/def ::lower ::m/number)
(s/def ::upper ::m/number)
(s/def ::open-lower? boolean?)
(s/def ::open-upper? boolean?)

(s/def ::bounds
  (s/and (s/keys :req [::lower ::upper ::open-lower? ::open-upper?])
    (fn [{::keys [lower upper open-lower? open-upper?]}]
      (or (and (m/nan? lower) (m/nan? upper))
        (> upper lower)
        (and (== upper lower)
          (not open-lower?)
          (not open-upper?))))))

(s/def ::bounds-num
  (s/and (s/keys :req [::lower ::upper ::open-lower? ::open-upper?])
    (fn [{::keys [lower upper open-lower? open-upper?]}]
      (or (> upper lower)
        (and (== upper lower)
          (not open-lower?)
          (not open-upper?))))))

(s/def ::bounds-finite
  (s/and (s/keys :req [::lower ::upper ::open-lower? ::open-upper?])
    (fn [{::keys [lower upper open-lower? open-upper?]}]
      (and (m/finite? lower)
        (m/finite? upper)
        (or (> upper lower)
          (and (== upper lower)
            (not open-lower?)
            (not open-upper?)))))))

(s/def ::vector-bounds
  (s/with-gen
    (s/coll-of ::bounds
      :kind vector?
      :into [])
    #(gen/vector (s/gen ::bounds) 0 mdl)))

(s/def ::by-upper? boolean?)

;;INTERVALS
(defn in-interval?
  "Tests if a number lies within an interval (inclusive endpoints).
  
  Returns true if `number` is in the closed interval [lower, upper].
  
  Examples:
    (in-interval? [1 5] 3)    ;=> true
    (in-interval? [1 5] 5)    ;=> true (inclusive)
    (in-interval? [1 5] 6)    ;=> false"
  [[lower upper] number]
  (and (>= number lower) (<= number upper)))

(s/fdef in-interval?
  :args (s/cat :interval ::interval
          :number ::m/number)
  :ret boolean?)

(defn in-interval-roughly?
  "Tests whether `number` is roughly inside the interval."
  [[lower upper] number accu]
  (and (>= number (- lower accu)) (<= number (+ upper accu))))

(s/fdef in-interval-roughly?
  :args (s/cat :interval ::interval
          :number ::m/number
          :accu ::m/accu)
  :ret boolean?)

(defn bound-by-interval
  "Constrains a number to lie within an interval.
  
  Returns the closest value to `number` that lies within [lower, upper].
  If `number` is already in the interval, returns `number` unchanged.
  
  Examples:
    (bound-by-interval [0 10] 5)   ;=> 5 (unchanged)
    (bound-by-interval [0 10] 15)  ;=> 10 (clamped to upper bound)
    (bound-by-interval [0 10] -3)  ;=> 0 (clamped to lower bound)"
  [[lower upper] number]
  (max lower (min upper number)))

(s/fdef bound-by-interval
  :args (s/cat :interval ::interval
          :number ::m/number)
  :ret ::m/number)

(defn bound-by-strict-interval
  "Bounds a `number` to a strict interval."
  [[lower upper] number]
  (cond (>= number upper) (m/next-down upper)
    (<= number lower) (m/next-up lower)
    :else number))

(s/fdef bound-by-strict-interval
  :args (s/cat :strict-interval ::strict-interval
          :number ::m/number)
  :ret ::m/nan-or-finite)

;;;BOUNDS TEST
(defn in-bounds?
  "Tests whether `number` is inside the bounds."
  [{::keys [lower upper open-lower? open-upper?]} number]
  (and (if open-lower?
         (> number lower)
         (>= number lower))
    (if open-upper?
      (< number upper)
      (<= number upper))))

(s/fdef in-bounds?
  :args (s/cat :bounds ::bounds :number ::m/number)
  :ret boolean?)

;;;BOUNDS CONSTRUCTORS
(defn bounds
  "Creates bounds with optional open/closed endpoints.
  
  Bounds extend intervals by allowing open endpoints. Useful for optimization
  constraints and mathematical domains where endpoints may be excluded.
  
  Default bounds span from -∞ to +∞ with closed endpoints.
  
  Parameters:
  - No args: Creates unbounded domain (-∞, +∞)
  - [lower upper]: Closed bounds [lower, upper] 
  - lower, upper: Closed bounds [lower, upper]
  - With open flags: Specify open (true) or closed (false) endpoints
  
  Examples:
    (bounds)                    ;=> unbounded
    (bounds 0 10)              ;=> [0, 10] (closed)
    (bounds 0 10 true false)   ;=> (0, 10] (open lower, closed upper)"
  ([] (bounds m/inf- m/inf+))
  ([[lower upper]] (bounds lower upper false false))
  ([lower upper] (bounds lower upper false false))
  ([[lower upper] open-lower? open-upper?]
   (bounds lower upper open-lower? open-upper?))
  ([lower upper open-lower? open-upper?]
   {::lower       lower
    ::upper       upper
    ::open-lower? open-lower?
    ::open-upper? open-upper?}))

(s/fdef bounds
  :args (s/or :zero (s/cat)
          :one (s/cat :interval ::interval)
          :two (s/and (s/cat :lower ::lower :upper ::upper)
                 (fn [{:keys [lower upper]}]
                   (or (and (m/nan? lower) (m/nan? upper))
                     (>= upper lower))))
          :three (s/and (s/cat :interval ::interval
                          :open-lower? ::open-lower?
                          :open-upper? ::open-upper?)
                   (fn [{:keys [interval open-lower? open-upper?]}]
                     (let [[lower upper] interval]
                       (or (and (m/nan? lower) (m/nan? upper))
                         (> upper lower)
                         (and (== upper lower)
                           (not open-lower?)
                           (not open-upper?))))))
          :four (s/and (s/cat :lower ::lower
                         :upper ::upper
                         :open-lower? ::open-lower?
                         :open-upper? ::open-upper?)
                  (fn [{:keys [lower upper open-lower? open-upper?]}]
                    (or (and (m/nan? lower) (m/nan? upper))
                      (> upper lower)
                      (and (== upper lower)
                        (not open-lower?)
                        (not open-upper?))))))
  :ret ::bounds)

(def bounds-num
  "Unbounded numeric domain spanning from -∞ to +∞ with closed endpoints."
  (bounds))

(def bounds-finite
  "Finite numeric domain (-∞, +∞) with open endpoints, excluding infinities."
  (bounds m/inf- m/inf+ true true))

(def bounds-finite+
  "Finite positive domain (0, +∞) with open endpoints, excluding zero and infinity."
  (bounds 0.0 m/inf+ true true))

(def bounds-finite-
  "Finite negative domain (-∞, 0) with open endpoints, excluding zero and infinity."
  (bounds m/inf- 0.0 true true))

(def bounds-finite-non-
  "Finite non-negative domain [0, +∞) including zero, excluding infinity."
  (bounds 0.0 m/inf+ false true))

(def bounds+
  "Positive domain (0, +∞] with open lower bound, closed upper bound."
  (bounds 0.0 m/inf+ true false))

(def bounds-non-
  "Non-negative domain [0, +∞] including zero and infinity."
  (bounds 0.0 m/inf+))

(def bounds-prob
  "Probability domain [0, 1] with closed endpoints for standard probabilities."
  (bounds 0.0 1.0))

(def bounds-open-prob
  "Open probability domain (0, 1) excluding endpoints 0 and 1."
  (bounds 0.0 1.0 true true))

(def bounds-long-non-
  "Non-negative long integer domain [0, max-long] with closed endpoints."
  (bounds 0 m/max-long))

(def bounds-long
  "Full long integer domain [min-long, max-long] with closed endpoints."
  (bounds m/min-long m/max-long false false))

(def bounds-long+
  "Positive long integer domain (0, max-long] excluding zero."
  (bounds 0 m/max-long true false))

(defn vector-bounds
  "Returns a vector of bounds."
  ([size] (vector-bounds size (bounds)))
  ([size bounds] (into [] (repeat size bounds))))

(s/fdef vector-bounds
  :args (s/cat :size ::size :bounds (s/? ::bounds))
  :ret ::vector-bounds)

(defn pos-definite-matrix-bounds
  "Returns a vector of bounds flattened for a symmetric positive matrix."
  [size]
  (let [m (if (zero? size)
            [[]]
            (mapv (fn [row]
                    (mapv (fn [column]
                            (if (== row column) bounds+ (bounds)))
                      (range size)))
              (range size)))]
    (vec (for [r (range size)
               c (range r size)]
           (get-in m [r c])))))

(s/fdef pos-definite-matrix-bounds
  :args (s/cat :size ::size)
  :ret ::vector-bounds)

(defn finite-pos-definite-matrix-bounds
  "Returns a vector of bounds flattened for a finite symmetric positive matrix."
  [size]
  (let [m (if (zero? size)
            [[]]
            (mapv (fn [row]
                    (mapv (fn [column]
                            (if (== row column) bounds-finite+ bounds-finite))
                      (range size)))
              (range size)))]
    (vec (for [r (range size)
               c (range r size)]
           (get-in m [r c])))))

(s/fdef finite-pos-definite-matrix-bounds
  :args (s/cat :size ::size)
  :ret ::vector-bounds)

(defn get-interval
  "Returns Interval from bounds."
  [{::keys [lower upper open-lower? open-upper?]}]
  (let [l (if open-lower? (m/next-up lower) lower)
        u (if open-upper? (m/next-down upper) upper)]
    [l u]))

(s/fdef get-interval
  :args (s/cat :bounds ::bounds)
  :ret ::interval)

;;;BOUNDS MANIPULATION
(defn- min-bound
  [bound-coll]
  (reduce (fn [[bound open-bound?] [bound2 open-bound2?]]
            (cond (< bound bound2) [bound open-bound?]
              (< bound2 bound) [bound2 open-bound2?]
              :else [bound (and open-bound? open-bound2?)]))
    [m/inf+ false]
    bound-coll))

(defn- max-bound
  [bound-coll]
  (reduce (fn [[bound open-bound?] [bound2 open-bound2?]]
            (cond (> bound bound2) [bound open-bound?]
              (> bound2 bound) [bound2 open-bound2?]
              :else [bound (and open-bound? open-bound2?)]))
    [m/inf- false]
    bound-coll))

(defn sort-bounds
  "Returns a vector of bounds sorted by lower bound first (by default) or upper
  bound first."
  ([vector-bounds] (sort-bounds vector-bounds {}))
  ([vector-bounds {::keys [by-upper?] :or {by-upper? false}}]
   (let [f (if by-upper?
             (fn [{::keys [lower upper open-lower? open-upper?]}]
               (vector (- upper) (not open-upper?) (- lower) open-lower?))
             (fn [{::keys [lower upper open-lower? open-upper?]}]
               (vector lower (not open-lower?) upper open-upper?)))]
     (vec (sort-by f vector-bounds)))))

(s/fdef sort-bounds
  :args (s/cat :vector-bounds ::vector-bounds
          :opts (s/? (s/keys :opt [::by-upper?])))
  :ret ::vector-bounds)

(defn intersection
  "Returns the bounds intersection or nil."
  [vector-bounds]
  (let [[lower open-lower?] (max-bound (map
                                         #(vector (::lower %) (::open-lower? %))
                                         vector-bounds))
        [upper open-upper?] (min-bound (map
                                         #(vector (::upper %) (::open-upper? %))
                                         vector-bounds))]
    (when (or (< lower upper)
            (and (== upper lower)
              (not open-lower?)
              (not open-upper?)))
      (bounds lower upper open-lower? open-upper?))))

(s/fdef intersection
  :args (s/cat :vector-bounds ::vector-bounds)
  :ret (s/nilable ::bounds))

(defn union
  "Returns a vector of non-overlapping bounds."
  [vector-bounds]
  (if (empty? vector-bounds)
    []
    (loop [sep []
           [a b & c] (sort-bounds vector-bounds)]
      (if b
        (let [g (intersection [a b])]
          (if g
            (recur sep (cons g c))
            (recur (conj sep a) (cons b c))))
        (conj sep a)))))

(s/fdef union
  :args (s/cat :vector-bounds ::vector-bounds)
  :ret ::vector-bounds)

(defn encompassing-bounds
  "Returns smallest bounds that encompass the bounds in `vector-bounds`."
  [vector-bounds]
  (let [[lower open-lower?] (min-bound
                              (map (fn [bounds]
                                     (vector (::lower bounds)
                                       (::open-lower? bounds)))
                                vector-bounds))
        [upper open-upper?] (max-bound
                              (map (fn [bounds]
                                     (vector (::upper bounds)
                                       (::open-upper? bounds)))
                                vector-bounds))]
    (bounds lower upper open-lower? open-upper?)))

(s/fdef encompassing-bounds
  :args (s/cat :vector-bounds (s/and ::vector-bounds
                                (fn [vb]
                                  (pos? (count vb)))))
  :ret ::bounds)
  

