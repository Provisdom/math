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

;; Forward declarations for functions used before definition
(declare intersection)

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
(s/def ::finite-non--interval (interval-spec ::m/finite-non-))
(s/def ::int-interval (interval-spec ::m/int))
(s/def ::int+-interval (interval-spec ::m/int+))
(s/def ::int-non--interval (interval-spec ::m/int-non-))
(s/def ::long-interval (interval-spec ::m/long))
(s/def ::long+-interval (interval-spec ::m/long+))

(defn long-interval-gen
  "Generator for long integer intervals within [`min`, `max`] bounds.

  Returns a generator that produces `[lower, upper]` tuples where both values are long integers
  within the specified range. Note: generated intervals may not be sorted (`lower` could be
  greater than `upper`).

  Examples:
    (gen/sample (long-interval-gen 0 100))
    ;=> ([45 23] [0 99] [12 78] ...)"
  [min max]
  (gen/tuple (gen/large-integer* {:min min :max max})
    (gen/large-integer* {:min min :max max})))

(s/def ::lower ::m/number)
(s/def ::upper ::m/number)
(s/def ::open-lower? boolean?)
(s/def ::open-upper? boolean?)

(s/def ::bounds
  (s/and (s/keys :req [::lower ::open-lower? ::open-upper? ::upper])
    (fn [{::keys [lower open-lower? open-upper? upper]}]
      (or (and (m/nan? lower) (m/nan? upper))
        (> upper lower)
        (and (== upper lower)
          (not open-lower?)
          (not open-upper?))))))

(s/def ::bounds-num
  (s/and (s/keys :req [::lower ::open-lower? ::open-upper? ::upper])
    (fn [{::keys [lower open-lower? open-upper? upper]}]
      (or (> upper lower)
        (and (== upper lower)
          (not open-lower?)
          (not open-upper?))))))

(s/def ::bounds-finite
  (s/and (s/keys :req [::lower ::open-lower? ::open-upper? ::upper])
    (fn [{::keys [lower open-lower? open-upper? upper]}]
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
(s/def ::bound-entry (s/tuple ::m/number boolean?))
(s/def ::bound-coll (s/coll-of ::bound-entry :kind sequential?))

;;INTERVALS
(defn in-interval?
  "Tests if a number lies within an interval (inclusive endpoints).

  Returns `true` if `number` is in the closed interval `[lower, upper]`.

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
  "Tests whether `number` is roughly inside the interval within tolerance.

  Returns `true` if `number` is within `accu` (accuracy/tolerance) distance of the interval. The
  interval is effectively expanded by `accu` on both sides.

  Examples:
    (in-interval-roughly? [0 10] 10.5 1.0)   ;=> true (within tolerance)
    (in-interval-roughly? [0 10] -0.5 1.0)   ;=> true (within tolerance)
    (in-interval-roughly? [0 10] 12.0 1.0)   ;=> false (outside tolerance)"
  [[lower upper] number accu]
  (and (>= number (- lower accu)) (<= number (+ upper accu))))

(s/fdef in-interval-roughly?
  :args (s/cat :interval ::interval
          :number ::m/number
          :accu ::m/accu)
  :ret boolean?)

(defn bound-by-interval
  "Constrains a number to lie within an interval.

  Returns the closest value to `number` that lies within `[lower, upper]`. If `number` is already
  in the interval, returns `number` unchanged.

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

(defn interval-width
  "Returns the width (span) of an interval.

  Examples:
    (interval-width [0 10])   ;=> 10.0
    (interval-width [-5 5])   ;=> 10.0
    (interval-width [3 3])    ;=> 0.0"
  [[lower upper]]
  (- upper lower))

(s/fdef interval-width
  :args (s/cat :interval ::interval)
  :ret ::m/non-)

(defn interval-midpoint
  "Returns the midpoint (center) of an interval.

  Examples:
    (interval-midpoint [0 10])   ;=> 5.0
    (interval-midpoint [-5 5])   ;=> 0.0
    (interval-midpoint [2 8])    ;=> 5.0"
  [[lower upper]]
  (/ (+ lower upper) 2.0))

(s/fdef interval-midpoint
  :args (s/cat :interval ::interval)
  :ret ::m/number)

;;;BOUNDS TEST
(defn in-bounds?
  "Tests whether `number` is inside the bounds.

  Respects open/closed endpoint specifications. Open endpoints exclude the boundary value, closed
  endpoints include it.

  Examples:
    (in-bounds? ([[bounds]] 0 10) 5)           ;=> true
    (in-bounds? ([[bounds]] 0 10) 0)           ;=> true (closed lower)
    (in-bounds? ([[bounds]] 0 10 true false) 0) ;=> false (open lower)"
  [{::keys [lower open-lower? open-upper? upper]} number]
  (and (if open-lower?
         (> number lower)
         (>= number lower))
    (if open-upper?
      (< number upper)
      (<= number upper))))

(s/fdef in-bounds?
  :args (s/cat :bounds ::bounds :number ::m/number)
  :ret boolean?)

(defn bound-by-bounds
  "Constrains a number to lie within bounds.

  Returns the closest value to `number` that lies within the bounds. For open endpoints, uses
  [[m/next-up]] or [[m/next-down]] to stay inside. If `number` is already in the bounds, returns
  `number` unchanged.

  Examples:
    (bound-by-bounds ([[bounds]] 0 10) 5)       ;=> 5 (unchanged)
    (bound-by-bounds ([[bounds]] 0 10) 15)      ;=> 10 (clamped to upper)
    (bound-by-bounds ([[bounds]] 0 10 true false) -1)  ;=> 4.9E-324 (just above 0)"
  [{::keys [lower open-lower? open-upper? upper]} number]
  (let [effective-lower (if open-lower? (m/next-up lower) lower)
        effective-upper (if open-upper? (m/next-down upper) upper)]
    (max effective-lower (min effective-upper number))))

(s/fdef bound-by-bounds
  :args (s/cat :bounds ::bounds :number ::m/number)
  :ret ::m/number)

(defn bounds-width
  "Returns the width (span) of bounds.

  Examples:
    (bounds-width ([[bounds]] 0 10))   ;=> 10.0
    (bounds-width ([[bounds]] -5 5))   ;=> 10.0"
  [{::keys [lower upper]}]
  (- upper lower))

(s/fdef bounds-width
  :args (s/cat :bounds ::bounds)
  :ret ::m/non-)

(defn bounds-midpoint
  "Returns the midpoint (center) of bounds.

  Examples:
    (bounds-midpoint ([[bounds]] 0 10))   ;=> 5.0
    (bounds-midpoint ([[bounds]] -5 5))   ;=> 0.0"
  [{::keys [lower upper]}]
  (/ (+ lower (double upper)) 2.0))

(s/fdef bounds-midpoint
  :args (s/cat :bounds ::bounds)
  :ret ::m/number)

(defn overlaps?
  "Tests whether two bounds overlap (have any points in common).

  Returns `true` if there is any [[intersection]] between the bounds.

  Examples:
    (overlaps? ([[bounds]] 0 5) ([[bounds]] 3 8))    ;=> true
    (overlaps? ([[bounds]] 0 5) ([[bounds]] 6 10))   ;=> false
    (overlaps? ([[bounds]] 0 5) ([[bounds]] 5 10))   ;=> true (touch at endpoint)"
  [bounds1 bounds2]
  (some? (intersection [bounds1 bounds2])))

(s/fdef overlaps?
  :args (s/cat :bounds1 ::bounds :bounds2 ::bounds)
  :ret boolean?)

(defn contains-bounds?
  "Tests whether `outer` bounds fully contain `inner` bounds.

  Returns `true` if every point in `inner` is also in `outer`.

  Examples:
    (contains-bounds? ([[bounds]] 0 10) ([[bounds]] 2 8))   ;=> true
    (contains-bounds? ([[bounds]] 0 10) ([[bounds]] -1 8))  ;=> false
    (contains-bounds? ([[bounds]] 0 10 true true) ([[bounds]] 0 10))  ;=> false"
  [{o-lower ::lower o-open-lower? ::open-lower? o-open-upper? ::open-upper? o-upper ::upper}
   {i-lower ::lower i-open-lower? ::open-lower? i-open-upper? ::open-upper? i-upper ::upper}]
  (and
    ;; Lower bound check: outer lower must be <= inner lower
    ;; If outer is open and inner is closed at same point, outer doesn't contain inner
    (or (< o-lower i-lower)
      (and (== o-lower i-lower)
        (or (not o-open-lower?) i-open-lower?)))
    ;; Upper bound check: outer upper must be >= inner upper
    (or (> o-upper i-upper)
      (and (== o-upper i-upper)
        (or (not o-open-upper?) i-open-upper?)))))

(s/fdef contains-bounds?
  :args (s/cat :outer ::bounds :inner ::bounds)
  :ret boolean?)

;;;BOUNDS CONSTRUCTORS
(defn bounds
  "Creates bounds with optional open/closed endpoints.

  Bounds extend intervals by allowing open endpoints. Useful for optimization constraints and
  mathematical domains where endpoints may be excluded.

  Default bounds span from -Infinity to +Infinity with closed endpoints.

  Parameters:
  - No args: Creates unbounded domain (-Infinity, +Infinity)
  - `[lower upper]`: Closed bounds [lower, upper]
  - `lower`, `upper`: Closed bounds [lower, upper]
  - With open flags: Specify open (`true`) or closed (`false`) endpoints

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
    ::open-lower? open-lower?
    ::open-upper? open-upper?
    ::upper       upper}))

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
  "Unbounded numeric domain spanning from -Infinity to +Infinity with closed endpoints."
  (bounds))

(def bounds-finite
  "Finite numeric domain (-Infinity, +Infinity) with open endpoints, excluding infinities."
  (bounds m/inf- m/inf+ true true))

(def bounds-finite+
  "Finite positive domain (0, +Infinity) with open endpoints, excluding zero and infinity."
  (bounds 0.0 m/inf+ true true))

(def bounds-finite-
  "Finite negative domain (-Infinity, 0) with open endpoints, excluding zero and infinity."
  (bounds m/inf- 0.0 true true))

(def bounds-finite-non-
  "Finite non-negative domain [0, +Infinity) including zero, excluding infinity."
  (bounds 0.0 m/inf+ false true))

(def bounds+
  "Positive domain (0, +Infinity] with open lower bound, closed upper bound."
  (bounds 0.0 m/inf+ true false))

(def bounds-non-
  "Non-negative domain [0, +Infinity] including zero and infinity."
  (bounds 0.0 m/inf+))

(def bounds-prob
  "Probability domain [0, 1] with closed endpoints for standard probabilities."
  (bounds 0.0 1.0))

(def bounds-open-prob
  "Open probability domain (0, 1) excluding endpoints 0 and 1."
  (bounds 0.0 1.0 true true))

(def bounds-long-non-
  "Non-negative long integer domain [0, `m/max-long`] with closed endpoints."
  (bounds 0 m/max-long))

(def bounds-long
  "Full long integer domain [`m/min-long`, `m/max-long`] with closed endpoints."
  (bounds m/min-long m/max-long false false))

(def bounds-long+
  "Positive long integer domain (0, `m/max-long`] excluding zero."
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
  "Converts bounds to an interval `[lower, upper]`.

  For open endpoints, adjusts to the nearest representable value inside the bounds using
  [[m/next-up]] for lower and [[m/next-down]] for upper.

  Examples:
    (get-interval ([[bounds]] 0 10))              ;=> [0 10]
    (get-interval ([[bounds]] 0 10 true false))   ;=> [4.9E-324 10] (open lower)
    (get-interval ([[bounds]] 0 10 false true))   ;=> [0 9.999999999999998] (open upper)"
  [{::keys [lower open-lower? open-upper? upper]}]
  (let [l (if open-lower? (m/next-up lower) lower)
        u (if open-upper? (m/next-down upper) upper)]
    [l u]))

(s/fdef get-interval
  :args (s/cat :bounds ::bounds)
  :ret ::interval)

;;;BOUNDS MANIPULATION
(defn min-bound
  "Finds the minimum bound value from a collection of `[value open?]` pairs.

  Each entry is a tuple of `[number, open-bound?]`. Returns `[min-value, open?]` where `open?` is
  `true` only if ALL entries with the minimum value are open.

  Used for finding lower bounds in [[intersection]] operations.

  Examples:
    (min-bound [[5 false] [3 true] [7 false]])
    ;=> [3 true]

    (min-bound [[3 false] [3 true]])
    ;=> [3 false]  ; closed wins when values equal"
  [bound-coll]
  (reduce (fn [[bound open-bound?] [bound2 open-bound2?]]
            (cond (< bound bound2) [bound open-bound?]
              (< bound2 bound) [bound2 open-bound2?]
              :else [bound (and open-bound? open-bound2?)]))
    [m/inf+ false]
    bound-coll))

(s/fdef min-bound
  :args (s/cat :bound-coll ::bound-coll)
  :ret ::bound-entry)

(defn max-bound
  "Finds the maximum bound value from a collection of `[value open?]` pairs.

  Each entry is a tuple of `[number, open-bound?]`. Returns `[max-value, open?]` where `open?` is
  `true` only if ALL entries with the maximum value are open.

  Used for finding upper bounds in [[intersection]] operations.

  Examples:
    (max-bound [[5 false] [3 true] [7 false]])
    ;=> [7 false]

    (max-bound [[7 false] [7 true]])
    ;=> [7 false]  ; closed wins when values equal"
  [bound-coll]
  (reduce (fn [[bound open-bound?] [bound2 open-bound2?]]
            (cond (> bound bound2) [bound open-bound?]
              (> bound2 bound) [bound2 open-bound2?]
              :else [bound (and open-bound? open-bound2?)]))
    [m/inf- false]
    bound-coll))

(s/fdef max-bound
  :args (s/cat :bound-coll ::bound-coll)
  :ret ::bound-entry)

(defn sort-bounds
  "Returns a vector of bounds sorted by lower bound first (by default) or upper
  bound first."
  ([vector-bounds] (sort-bounds vector-bounds {}))
  ([vector-bounds {::keys [by-upper?] :or {by-upper? false}}]
   (let [f (if by-upper?
             (fn [{::keys [lower open-lower? open-upper? upper]}]
               (vector (- upper) (not open-upper?) (- lower) open-lower?))
             (fn [{::keys [lower open-lower? open-upper? upper]}]
               (vector lower (not open-lower?) upper open-upper?)))]
     (vec (sort-by f vector-bounds)))))

(s/fdef sort-bounds
  :args (s/cat :vector-bounds ::vector-bounds
          :opts (s/? (s/keys :opt [::by-upper?])))
  :ret ::vector-bounds)

(defn intersection
  "Returns the [[intersection]] of all bounds, or `nil` if no intersection exists.

  The intersection is the region where all bounds overlap. Returns `nil` when the bounds do not
  overlap.

  Examples:
    (intersection [([[bounds]] 0 10) ([[bounds]] 5 15)])
    ;=> {::lower 5, ::upper 10, ...}

    (intersection [([[bounds]] 0 5) ([[bounds]] 10 15)])
    ;=> nil (no overlap)"
  [vector-bounds]
  (let [[lower open-lower?] (max-bound (map #(vector (::lower %) (::open-lower? %))
                                         vector-bounds))
        [upper open-upper?] (min-bound (map #(vector (::upper %) (::open-upper? %))
                                         vector-bounds))]
    (when (or (< lower upper)
            (and (== upper lower)
              (not open-lower?)
              (not open-upper?)))
      (bounds lower upper open-lower? open-upper?))))

(s/fdef intersection
  :args (s/cat :vector-bounds ::vector-bounds)
  :ret (s/nilable ::bounds))

(defn encompassing-bounds
  "Returns smallest bounds that encompass all bounds in `vector-bounds`.

  Finds the minimum lower bound and maximum upper bound across all bounds, preserving open/closed
  status appropriately.

  Examples:
    (encompassing-bounds [([[bounds]] 0 5) ([[bounds]] 3 10)])
    ;=> {::lower 0, ::upper 10, ::open-lower? false, ::open-upper? false}

    (encompassing-bounds [([[bounds]] 0 5 true false) ([[bounds]] -2 3 false true)])
    ;=> {::lower -2, ::upper 5, ::open-lower? false, ::open-upper? false}"
  [vector-bounds]
  (let [[lower open-lower?] (min-bound
                              (map (fn [bounds]
                                     (vector (::lower bounds) (::open-lower? bounds)))
                                vector-bounds))
        [upper open-upper?] (max-bound
                              (map (fn [bounds]
                                     (vector (::upper bounds) (::open-upper? bounds)))
                                vector-bounds))]
    (bounds lower upper open-lower? open-upper?)))

(s/fdef encompassing-bounds
  :args (s/cat :vector-bounds (s/and ::vector-bounds
                                (fn [vb]
                                  (pos? (count vb)))))
  :ret ::bounds)

(defn union
  "Merges overlapping bounds into a minimal set of non-overlapping bounds.

  Takes a vector of bounds and combines any that overlap into single bounds using
  [[encompassing-bounds]]. Non-overlapping bounds are kept separate.

  Examples:
    ;; Overlapping bounds merge into one
    (union [([[bounds]] 0 5) ([[bounds]] 3 8)])
    ;=> [{::lower 0, ::upper 8, ::open-lower? false, ::open-upper? false}]

    ;; Non-overlapping bounds stay separate
    (union [([[bounds]] 0 2) ([[bounds]] 5 8)])
    ;=> [{...lower 0 upper 2...} {...lower 5 upper 8...}]

    ;; Multiple overlapping bounds merge progressively
    (union [([[bounds]] 0 3) ([[bounds]] 2 5) ([[bounds]] 4 8)])
    ;=> [{::lower 0, ::upper 8, ...}]"
  [vector-bounds]
  (if (empty? vector-bounds)
    []
    (loop [sep []
           [a b & c] (sort-bounds vector-bounds)]
      (if b
        (let [g (intersection [a b])]
          (if g
            (recur sep (cons (encompassing-bounds [a b]) c))
            (recur (conj sep a) (cons b c))))
        (conj sep a)))))

(s/fdef union
  :args (s/cat :vector-bounds ::vector-bounds)
  :ret ::vector-bounds)
  

