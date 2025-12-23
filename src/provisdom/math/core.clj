(ns provisdom.math.core
  "Core mathematical functions and constants for numerical computation.

  This namespace provides the foundation for all provisdom.math libraries:

  ## Constants
  Mathematical constants with full double precision: PI, E, sqrt-two, sqrt-pi,
  log-two, log-pi, inv-sqrt-two-pi, and many derived values. Also includes
  system limits: max-dbl, min-dbl, max-long, tiny-dbl, etc.

  ## Type Predicates
  IEEE 754-aware predicates that correctly handle NaN and infinity:
  - Sign: pos?, neg?, non-?, non+?
  - Finiteness: finite?, finite+?, finite-?, inf?, inf+?, inf-?, nan?
  - Ranges: prob? (0-1), open-prob?, corr? (-1 to 1), open-corr?
  - Integer types: long?, int?, long-able?, roughly-round?

  ## Arithmetic
  Safe operations that handle edge cases gracefully:
  - div: division with proper 0/0 → NaN, n/0 → ±Inf handling
  - sq, cube, sqrt, cbrt, pow, exp, log, log2, log10, logn
  - sgn: sign function returning -1, 0, 1, or NaN

  ## Rounding & Comparison
  - floor, ceil, round with configurable rounding modes
  - roughly?: approximate equality within tolerance
  - roughly-floor, roughly-ceil: tolerance-aware rounding
  - round-significant: round to N significant digits

  ## Trigonometry
  Standard and hyperbolic: sin, cos, tan, asin, acos, atan, atan2,
  sinh, cosh, tanh, asinh, acosh, atanh, hypot

  ## Angle Conversion
  radians->angle', angle->radians', reduce-angle', reduce-radians'

  ## Apostrophe Convention
  Functions marked with ' (e.g., floor', mod', sq') return longs when the
  result fits in long range, otherwise doubles. Unmarked functions always
  return doubles for consistency and to avoid overflow surprises."
  (:refer-clojure :exclude [abs int? neg? pos?])
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]))

;;;DECLARATIONS
(declare ceil' floor floor' nan? next-down next-up non-? non+? roughly-round?)

;;;MATH CONSTANTS
(def ^:const ^long sgl-digits 6)
(def ^:const ^double sgl-close 1e-6)
(def ^:const ^double dbl-close 1e-15)
(def ^:const ^double quad-close 1e-33)
(def ^:const ^double E Math/E)
(def ^:const ^double PI Math/PI)
(def ^:const ^double nan Double/NaN)
(def ^:const ^double inf+ Double/POSITIVE_INFINITY)
(def ^:const ^double inf- Double/NEGATIVE_INFINITY)
(def ^:const ^double max-dbl Double/MAX_VALUE)
(def ^:const ^double tiny-inv-finite+ (/ (inc dbl-close) max-dbl))
(def ^:const ^double tiny-dbl Double/MIN_VALUE)
(def ^:const ^double min-dbl (- max-dbl))
(def ^:const ^float max-sgl Float/MAX_VALUE)
(def ^:const ^float tiny-sgl Float/MIN_VALUE)
(def ^:const ^float min-sgl (- max-sgl))
(def ^:const ^long max-long Long/MAX_VALUE)
(def ^:const ^long min-long Long/MIN_VALUE)
(def ^:const ^long max-int Integer/MAX_VALUE)
(def ^:const ^long min-int Integer/MIN_VALUE)
(def ^:const ^double log-half (Math/log 0.5))               ;;since marked as const, should use Math/log
(def ^:const ^double log-two (Math/log 2.0))
(def ^:const ^double log-pi (Math/log PI))
(def ^:const ^double log-two-pi (+ log-two log-pi))
(def ^:const ^double log-two-pi-e (+ (Math/log E) log-two-pi))
(def ^:const ^double half-log-two-pi (* 0.5 log-two-pi))
(def ^:const ^double half-pi (* 0.5 PI))
(def ^:const ^double two-pi (* 2.0 PI))
(def ^:const ^double two-pi-e (* two-pi E))
(def ^:const ^double pi-squared (* PI PI))
(def ^:const ^double sqrt-two (Math/sqrt 2.0))
(def ^:const ^double sqrt-half (/ sqrt-two))
(def ^:const ^double sqrt-pi (Math/sqrt PI))
(def ^:const ^double sqrt-half-pi (* sqrt-half sqrt-pi))
(def ^:const ^double inv-pi (/ PI))
(def ^:const ^double inv-sqrt-pi (/ sqrt-pi))
(def ^:const ^double inv-sqrt-two (/ sqrt-two))
(def ^:const ^double inv-two-pi (* 0.5 inv-pi))
(def ^:const ^double two-div-pi (* 2.0 inv-pi))
(def ^:const ^double inv-sqrt-two-pi (* inv-sqrt-two inv-sqrt-pi))

;;;TEST FOR NUMERIC TYPES
(defn- long-range?
  "Returns true if x is within the range representable by a long."
  [x]
  (and (<= x max-long) (>= x min-long)))

(defn- maybe-long-range
  "Coerces x to long if it's within long range, otherwise returns x unchanged.
  Does NOT check if x is an integer - use maybe-long-able for that."
  [x]
  (if (long-range? x) (long x) x))

(defn- int-range?
  "Returns true if x is within the range representable by an int."
  [x]
  (and (<= x max-int) (>= x min-int)))

(defn- sgl-range?
  [x]
  (and (<= x max-sgl) (>= x min-sgl)))

(s/def ::number
  (s/spec number?
          :gen #(gen/one-of [(gen/double) (gen/large-integer)])))

(defn numbers?
  "Tests if a collection contains only numbers.
  
  Returns true if `x` is sequential and every element is a number.
  
  Examples:
    (numbers? [1 2 3])        ;=> true
    (numbers? [1 :a 3])       ;=> false  
    (numbers? #{1 2 3})       ;=> false (not sequential)"
  [x]
  (and (sequential? x) (every? number? x)))

(def mdl 6)                                                 ;max-dim-length for generators

(s/def ::numbers
  (s/with-gen
    numbers?
    #(s/gen (s/or :v (s/coll-of ::number
                                :min-count 0
                                :max-count mdl
                                :kind vector?
                                :into [])
                  :l (s/coll-of ::number
                                :min-count 0
                                :max-count mdl
                                :kind list?
                                :into '())))))

(defn num?
  "Tests if a value is a valid (non-NaN) number.
  
  Returns true if `x` is a number and not NaN. Uses the property that 
  NaN is not equal to itself (NaN != NaN).
  
  Examples:
    (num? 42)        ;=> true
    (num? 3.14)      ;=> true  
    (num? ##NaN)     ;=> false
    (num? \"foo\")    ;=> false"
  [x]
  (and (number? x) (== x x)))

(s/def ::num
  (s/spec num?
          :gen #(gen/one-of [(gen/double* {:NaN? false}) (gen/large-integer)])))

(defn nan?
  "Tests if a value is NaN (Not a Number).
  
  Returns true if `x` is NaN. Uses the IEEE 754 property that NaN != NaN.
  Only returns true for actual NaN values, not other non-numeric types.
  
  Examples:
    (nan? ##NaN)           ;=> true
    (nan? (/ 0.0 0.0))     ;=> true
    (nan? 42)              ;=> false
    (nan? \"not-a-number\") ;=> false"
  [x]
  (and (number? x) (not (== x x))))

(s/def ::nan (s/spec nan? :gen #(gen/return nan)))

(defn pos?
  "Tests if a number is positive (> 0).
  
  Returns true if `x` is a number and positive. Unlike clojure.core/pos?,
  this function ensures the input is actually a number first.
  
  Examples:
    (pos? 5)     ;=> true
    (pos? 0)     ;=> false
    (pos? -3)    ;=> false
    (pos? ##NaN) ;=> false"
  [x]
  (and (number? x) (clojure.core/pos? x)))

(s/def ::pos
  (s/spec pos?
          :gen #(gen/one-of
                  [(gen/double* {:min tiny-dbl :NaN? false})
                   (gen/large-integer* {:min 1})])))

(s/def ::nan-or-pos
  (s/spec #(or (nan? %) (pos? %))
          :gen #(gen/one-of
                  [(gen/double* {:min tiny-dbl})
                   (gen/large-integer* {:min 1})])))

(defn neg?
  "Tests if a number is negative (< 0).
  
  Returns true if `x` is a number and negative. Unlike clojure.core/neg?,
  this function ensures the input is actually a number first.
  
  Examples:
    (neg? -5)    ;=> true
    (neg? 0)     ;=> false
    (neg? 3)     ;=> false
    (neg? ##NaN) ;=> false"
  [x]
  (and (number? x) (clojure.core/neg? x)))

(s/def ::neg
  (s/spec neg?
          :gen #(gen/one-of
                  [(gen/double* {:max (- tiny-dbl) :NaN? false})
                   (gen/large-integer* {:max -1})])))

(s/def ::nan-or-neg
  (s/spec #(or (nan? %) (neg? %))
          :gen #(gen/one-of
                  [(gen/double* {:max (- tiny-dbl)})
                   (gen/large-integer* {:max -1})])))

(defn non-?
  "Tests if a number is non-negative (>= 0).
  
  Returns true if `x` is a number and greater than or equal to zero.
  
  Examples:
    (non-? 5)     ;=> true
    (non-? 0)     ;=> true
    (non-? -3)    ;=> false
    (non-? ##NaN) ;=> false"
  [x]
  (and (number? x) (>= x 0)))

(s/def ::non-
  (s/spec non-?
          :gen #(gen/one-of
                  [(gen/double* {:min 0.0 :NaN? false})
                   (gen/large-integer* {:min 0})])))

(s/def ::nan-or-non-
  (s/spec #(or (nan? %) (non-? %))
          :gen #(gen/one-of
                  [(gen/double* {:min 0.0})
                   (gen/large-integer* {:min 0})])))

(defn non+?
  "Tests if a number is non-positive (<= 0).
  
  Returns true if `x` is a number and less than or equal to zero.
  
  Examples:
    (non+? -5)    ;=> true
    (non+? 0)     ;=> true
    (non+? 3)     ;=> false
    (non+? ##NaN) ;=> false"
  [x]
  (and (number? x) (<= x 0)))

(s/def ::non+
  (s/spec non+?
          :gen #(gen/one-of
                  [(gen/double* {:max 0.0 :NaN? false})
                   (gen/large-integer* {:max 0})])))

(s/def ::nan-or-non+
  (s/spec #(or (nan? %) (non+? %))
          :gen #(gen/one-of
                  [(gen/double* {:max 0.0})
                   (gen/large-integer* {:max 0})])))

(defn finite?
  "Tests if a number is finite (not infinite or NaN).
  
  Returns true if `x` is a valid number that is neither infinite nor NaN.
  
  Examples:
    (finite? 42)      ;=> true
    (finite? -3.14)   ;=> true
    (finite? ##Inf)   ;=> false
    (finite? ##NaN)   ;=> false"
  [x]
  (and (num? x) (not (infinite? (double x)))))

(s/def ::finite
  (s/spec finite?
          :gen #(gen/one-of
                  [(gen/double* {:infinite? false :NaN? false})
                   (gen/large-integer)])))

(defmacro finite-spec
  [{m1  :min
    m2  :max
    :or {m1 min-dbl
         m2 max-dbl}}]
  ;;longs will sometimes get tossed out (prevents execution errors)
  `(s/with-gen (s/and finite?
                      (partial <= ~m1)
                      (partial >= ~m2))
               #(gen/one-of
                  [(gen/double* {:min ~m1 :max ~m2 :NaN? false})
                   (gen/large-integer*
                     {:min (max min-long (floor' ~m1))
                      :max (min max-long (ceil' ~m2))})])))

(s/def ::nan-or-finite
  (s/spec #(or (nan? %) (finite? %))
          :gen #(gen/one-of
                  [(gen/double* {:infinite? false})
                   (gen/large-integer)])))

(defn finite+?
  "Returns true if `x` is a positive finite number."
  [x]
  (and (pos? x) (not (infinite? x))))

(s/def ::finite+
  (s/spec finite+?
          :gen #(gen/one-of
                  [(gen/double* {:min tiny-dbl :infinite? false :NaN? false})
                   (gen/large-integer* {:min 1})])))

(defmacro finite+-spec
  [m2]
  ;;longs will sometimes get tossed out (prevents execution errors)
  `(s/with-gen (s/and finite+? (partial >= ~m2))
               #(gen/one-of
                  [(gen/double* {:min tiny-dbl :max ~m2 :NaN? false})
                   (gen/large-integer*
                     {:min 1
                      :max (min max-long (ceil' ~m2))})])))

(s/def ::nan-or-finite+
  (s/spec #(or (nan? %) (finite+? %))
          :gen #(gen/one-of
                  [(gen/double* {:min tiny-dbl :infinite? false})
                   (gen/large-integer* {:min 1})])))

(defn finite-?
  "Returns true if `x` is a negative finite number."
  [x]
  (and (neg? x) (not (infinite? x))))

(s/def ::finite-
  (s/spec finite-?
          :gen #(gen/one-of
                  [(gen/double* {:max       (- tiny-dbl)
                                 :infinite? false
                                 :NaN?      false})
                   (gen/large-integer* {:max -1})])))

(s/def ::nan-or-finite-
  (s/spec #(or (nan? %) (finite-? %))
          :gen #(gen/one-of
                  [(gen/double* {:max       (- tiny-dbl)
                                 :infinite? false})
                   (gen/large-integer* {:max -1})])))

(defn finite-non-?
  "Returns true if `x` is a non-negative finite number."
  [x]
  (and (non-? x) (not (infinite? x))))

(s/def ::finite-non-
  (s/spec finite-non-?
          :gen #(gen/one-of
                  [(gen/double* {:min 0.0 :infinite? false :NaN? false})
                   (gen/large-integer* {:min 0})])))

(defmacro finite-non--spec
  [m2]
  ;;longs will sometimes get tossed out (prevents execution errors)
  `(s/with-gen (s/and finite-non-? (partial >= ~m2))
               #(gen/one-of
                  [(gen/double* {:min 0.0 :max ~m2 :NaN? false})
                   (gen/large-integer*
                     {:min 0
                      :max (min max-long (ceil' ~m2))})])))

(s/def ::nan-or-finite-non-
  (s/spec #(or (nan? %) (finite-non-? %))
          :gen #(gen/one-of [(gen/double* {:min 0.0 :infinite? false})
                             (gen/large-integer* {:min 0})])))

(defn finite-non+?
  "Returns true if `x` is a non-positive finite number."
  [x]
  (and (non+? x) (not (infinite? x))))

(s/def ::finite-non+
  (s/spec finite-non+?
          :gen #(gen/one-of [(gen/double* {:max       0.0
                                           :infinite? false
                                           :NaN?      false})
                             (gen/large-integer* {:max 0})])))

(s/def ::nan-or-finite-non+
  (s/spec #(or (nan? %) (finite-non+? %))
          :gen #(gen/one-of [(gen/double* {:max 0.0 :infinite? false})
                             (gen/large-integer* {:max 0})])))

(s/def ::double
  (s/spec double? :gen #(gen/double)))

(defn double-finite?
  "Returns true if `x` is a double and finite."
  [x]
  (and (double? x) (== x x) (not (infinite? x))))

(s/def ::double-finite
  (s/spec double-finite? :gen #(gen/double* {:infinite? false :NaN? false})))

(defn double-finite+?
  "Returns true if `x` is a double and finite+."
  [x]
  (and (finite+? x) (double? x)))

(s/def ::double-finite+
  (s/spec double-finite+?
          :gen #(gen/double* {:infinite? false
                              :min       tiny-dbl
                              :NaN?      false})))

(defn single?
  "Returns true if `x` is a single."
  [x]
  (and (double? x)
       (or (sgl-range? x)
           (not (== x x))
           (infinite? x))))

(s/def ::single
  (s/spec single? :gen #(gen/double)))

(defn single-finite?
  "Returns true if `x` is a single and finite."
  [x]
  (and (double? x) (sgl-range? x)))

(s/def ::single-finite
  (s/spec single-finite?
          :gen #(gen/double* {:infinite? false
                              :NaN?      false
                              :min       min-sgl
                              :max       max-sgl})))

(defn long?
  "Returns true if `x` is a long."
  [x]
  (and (number? x) (or (clojure.core/int? x) (instance? Long x))))

(s/def ::long
  (s/spec long? :gen gen/large-integer))

(defmacro long-spec
  [{m1  :min
    m2  :max
    :or {m1 min-long
         m2 max-long}}]
  `(s/with-gen (s/and long?
                      (partial <= ~m1)
                      (partial >= ~m2))
               #(gen/large-integer*
                  {:min (max min-long (floor' ~m1))
                   :max (min max-long (ceil' ~m2))})))

(defn long+?
  "Returns true if `x` is a long and is positive."
  [x]
  (and (pos? x) (long? x)))

(s/def ::long+
  (s/spec long+? :gen #(s/gen (s/int-in 1 max-long))))

(defmacro long+-spec
  [m2]
  `(s/with-gen (s/and long+? (partial >= ~m2))
               #(gen/large-integer*
                  {:min 1 :max (min max-long (ceil' ~m2))})))

(defn long-?
  "Returns true if `x` is a long and is negative."
  [x]
  (and (neg? x) (long? x)))

(s/def ::long-
  (s/spec long-? :gen #(s/gen (s/int-in min-long 0))))

(defn long-non-?
  "Returns true if `x` is a long and is non-negative."
  [x]
  (and (non-? x) (long? x)))

(s/def ::long-non-
  (s/spec long-non-? :gen #(s/gen (s/int-in 0 max-long))))

(defmacro long-non--spec
  [m2]
  `(s/with-gen (s/and long-non-? (partial >= ~m2))
               #(gen/large-integer*
                  {:min 0 :max (min max-long (ceil' ~m2))})))

(defn long-non+?
  "Returns true if `x` is a long and is non-positive."
  [x]
  (and (non+? x) (long? x)))

(s/def ::long-non+
  (s/spec long-non+? :gen #(s/gen (s/int-in min-long 1))))

(defn int?
  "Returns true if `x` is an integer that is within the int range."
  [x]
  (and (integer? x) (int-range? x)))

(s/def ::int
  (s/spec int? :gen gen/int))

(defmacro int-spec
  [{m1  :min
    m2  :max
    :or {m1 min-int
         m2 max-int}}]
  `(s/with-gen (s/and int?
                      (partial <= ~m1)
                      (partial >= ~m2))
               #(gen/large-integer*
                  {:min (max min-int (floor' ~m1))
                   :max (min max-int (ceil' ~m2))})))

(defn int+?
  "Returns true if `x` is an int and is positive."
  [x]
  (and (int? x) (pos? x)))

(s/def ::int+
  (s/spec int+? :gen #(s/gen (s/int-in 1 (inc max-int)))))

(defmacro int+-spec
  [m2]
  `(s/with-gen (s/and int+? (partial >= ~m2))
               #(gen/large-integer*
                  {:min 1 :max (min max-int (ceil' ~m2))})))

(defn int-?
  "Returns true if `x` is an int and is negative."
  [x]
  (and (int? x) (neg? x)))

(s/def ::int-
  (s/spec int-? :gen #(s/gen (s/int-in min-int 0))))

(defn int-non-?
  "Returns true if `x` is an int and is non-negative."
  [x]
  (and (int? x) (non-? x)))

(s/def ::int-non-
  (s/spec int-non-? :gen #(s/gen (s/int-in 0 (inc max-int)))))

(defmacro int-non--spec
  [m2]
  `(s/with-gen (s/and int-non-? (partial >= ~m2))
               #(gen/large-integer*
                  {:min 0 :max (min max-int (ceil' ~m2))})))

(defn int-non+?
  "Returns true if `x` is an int and is non-positive."
  [x]
  (and (int? x) (non+? x)))

(s/def ::int-non+
  (s/spec int-non+? :gen #(s/gen (s/int-in min-int 1))))

(defn long-able?
  "Returns true if `x` is a number that can be converted to a long."
  [x]
  (and (number? x)
       (roughly-round? x 0.0)
       (long-range? x)))

(defn long-able+?
  "Returns true if `x` is a number that can be converted to a long, and is
  positive."
  [x]
  (and (long-able? x) (pos? x)))

(defn long-able-?
  "Returns true if `x` is a number that can be converted to a long, and is
  negative."
  [x]
  (and (long-able? x) (neg? x)))

(defn long-able-non+?
  "Returns true if `x` is a number that can be converted to a long, and is
  non+."
  [x]
  (and (long-able? x) (non+? x)))

(defn long-able-non-?
  "Returns true if `x` is a number that can be converted to a long, and is
  non-."
  [x]
  (and (long-able? x) (non-? x)))

(s/def ::non-roughly-round-non+
  (s/spec #(and (num? %) (not (roughly-round? % 0.0)))
          :gen #(s/gen (s/double-in :NaN? false))))

(s/def ::nan-or-non-roughly-round-non+
  (s/spec #(and (number? %) (or (nan? %) (not (roughly-round? % 0.0))))
          :gen #(s/gen (s/double-in :NaN? true))))

(defn inf+?
  "Returns true if `x` is Inf+."
  [x]
  (and (number? x)
       (infinite? x)
       (pos? x)))

(s/def ::non-inf+
  (s/spec #(and (num? %) (not (inf+? %)))
          :gen #(s/gen (s/double-in :NaN? false))))

(s/def ::nan-or-non-inf+
  (s/spec #(and (number? %) (not (inf+? %)))
          :gen #(s/gen (s/double-in :NaN? true))))

(defn inf-?
  "Returns true if `x` is Inf-."
  [x]
  (and (number? x) (infinite? (double x)) (neg? x)))

(s/def ::non-inf-
  (s/spec #(and (num? %) (not (inf-? %)))
          :gen #(s/gen (s/double-in :NaN? false))))

(s/def ::nan-or-non-inf-
  (s/spec #(and (number? %) (not (inf-? %)))
          :gen #(s/gen (s/double-in :NaN? true))))

(defn inf?
  "Returns true if `x` is Inf+ or Inf-."
  [x]
  (and (number? x) (infinite? (double x))))

(s/def ::inf
  (s/spec inf?
          :gen #(gen/one-of
                  [(gen/return inf-)
                   (gen/return inf+)])))

(defn one?
  "Returns true if `x` if equal to one."
  [x]
  (and (number? x) (== 1 x)))

(s/def ::one
  (s/spec one? :gen #(gen/return 1.0)))

(defn prob?
  "Returns true if `x` is between 0 and 1, inclusive."
  [x]
  (and (non-? x) (<= x 1)))

(s/def ::prob
  (s/spec prob? :gen #(s/gen (s/double-in :min 0.0 :max 1.0 :NaN? false))))

(s/def ::nan-or-prob
  (s/spec #(or (nan? %) (prob? %))
          :gen #(s/gen (s/double-in :min 0.0 :max 1.0))))

(defn open-prob?
  "Returns true if `x` is between 0 and 1, exclusive."
  [x]
  (and (pos? x) (< x 1)))

(s/def ::open-prob
  (s/spec open-prob?
          :gen #(s/gen (s/double-in :min tiny-dbl
                                    :max (next-down 1.0)
                                    :NaN? false))))

(s/def ::nan-or-open-prob
  (s/spec #(or (nan? %) (open-prob? %))
          :gen #(s/gen (s/double-in :min tiny-dbl :max (next-down 1.0)))))

(defn corr?
  "Returns true if `x` is between -1 and 1, inclusive."
  [x]
  (and (number? x)
       (<= x 1)
       (>= x -1)))

(s/def ::corr
  (s/spec corr? :gen #(s/gen (s/double-in :min -1.0 :max 1.0 :NaN? false))))

(s/def ::nan-or-corr
  (s/spec #(or (nan? %) (corr? %))
          :gen #(s/gen (s/double-in :min -1.0 :max 1.0))))

(defn open-corr?
  "Returns true if `x` is between -1 and 1, exclusive."
  [x]
  (and (number? x)
       (< x 1)
       (> x -1)))

(s/def ::open-corr
  (s/spec open-corr?
          :gen #(s/gen (s/double-in :min (next-up -1.0)
                                    :max (next-down 1.0)
                                    :NaN? false))))

(s/def ::nan-or-open-corr
  (s/spec #(or (nan? %) (open-corr? %))
          :gen #(s/gen (s/double-in :min (next-up -1.0) :max (next-down 1.0)))))

(defn maybe-long-able
  "Coerces x to long if it's an integer value within long range, otherwise
  returns x unchanged. Unlike maybe-long-range (private), this checks that x
  is actually an integer (via roughly-round?) before converting."
  [x]
  (if (long-able? x)
    (long x)
    x))

;;;BASIC MATH
(defn ===
  "Equality for numbers that works with NaN."
  ([_number] true)
  ([number1 number2]
   (or (and (nan? number1) (nan? number2))
       (== number1 number2)))
  ([number1 number2 & more]
   (and (=== number1 number2)
        (apply === number2 more))))

(s/fdef ===
  :args (s/or :one (s/cat :number ::number)
              :two+ (s/cat :number1 ::number
                           :number2 ::number
                           :more (s/* ::number)))
  :ret boolean?)

(defn next-up
  "Returns the next representable floating-point value greater than `number`.
  
  Uses IEEE 754 nextAfter operation to find the smallest possible increment.
  Useful for creating strict upper bounds or testing floating-point precision.
  
  Examples:
    (next-up 1.0)  ;=> 1.0000000000000002
    (next-up 0.0)  ;=> 4.9E-324 (smallest positive double)"
  [number]
  (Math/nextAfter (double number) inf+))

(s/fdef next-up
  :args (s/cat :number ::number)
  :ret ::number)

(defn next-down
  "Returns the next representable floating-point value less than `number`.
  
  Uses IEEE 754 nextAfter operation to find the smallest possible decrement.
  Useful for creating strict lower bounds or testing floating-point precision.
  
  Examples:
    (next-down 1.0)  ;=> 0.9999999999999998
    (next-down 0.0)  ;=> -4.9E-324 (smallest negative double)"
  [number]
  (Math/nextAfter (double number) inf-))

(s/fdef next-down
  :args (s/cat :number ::number)
  :ret ::number)

(defn div
  "Divides numbers with proper handling of division by zero.
  
  Returns `number1` divided by `number2`, or (1 / `number2`) if only one argument.
  Handles edge cases:
  - Positive / 0 → +Infinity  
  - Negative / 0 → -Infinity
  - 0 / 0 → NaN (or custom value via `zero-div-by-zero`)
  - NaN / 0 → NaN
  
  Examples:
    (div 6 2)     ;=> 3.0
    (div 1 0)     ;=> ##Inf
    (div 0 0)     ;=> ##NaN
    (div 0 0 42)  ;=> 42"
  ([number2] (div 1 number2))
  ([number1 number2] (div number1 number2 nan))
  ([number1 number2 zero-div-by-zero]
   (if (zero? number2)
     (cond (nan? number1) nan
           (zero? number1) zero-div-by-zero
           (pos? number1) inf+
           :else inf-)
     (/ number1 number2))))

(s/fdef div
  :args (s/or :one (s/cat :number2 ::number)
              :two-three (s/cat :number1 ::number
                                :number2 ::number
                                :zero-div-by-zero (s/? ::number)))
  :ret ::number)

(defn one-
  "Computes 1 minus the sum of arguments.
  
  Returns (1 - `number`) for single argument, or (1 - sum) for multiple arguments.
  Always returns a double when multiple arguments are provided.
  
  Examples:
    (one- 0.3)      ;=> 0.7
    (one- 0.2 0.3)  ;=> 0.5 (1 - 0.2 - 0.3)
    (one- 1)        ;=> 0"
  ([number] (inc (- number)))
  ([number & numbers] (inc (- (apply + (double number) numbers)))))

(s/fdef one-
  :args (s/cat :number ::number :numbers (s/* ::number))
  :ret ::number)

(defn sq
  "Computes the square of a number.
  
  Returns `number` × `number`. Always returns a double.
  
  Examples:
    (sq 5)     ;=> 25.0
    (sq -3)    ;=> 9.0
    (sq 1.5)   ;=> 2.25"
  [number]
  (* (double number) number))

(s/fdef sq
  :args (s/cat :number ::number)
  :ret ::nan-or-non-)

(defn sq'
  "Computes the square of a number, returning a long when possible.
  
  Returns `number` × `number` as a long if the result fits in long range,
  otherwise returns a double.
  
  Examples:
    (sq' 5)       ;=> 25 (long)
    (sq' 1.5)     ;=> 2.25 (double, non-integer result)"
  [number]
  (maybe-long-able
    (* (double number) number)))

(s/fdef sq'
  :args (s/cat :number ::number)
  :ret ::nan-or-non-)

(defn cube
  "Returns cube of `number`."
  [number]
  (* (double number) number number))

(s/fdef cube
  :args (s/cat :number ::number)
  :ret ::number)

(defn cube'
  "Returns cube of `number` as a long if possible."
  [number]
  (maybe-long-able
    (* (double number) number number)))

(s/fdef cube'
  :args (s/cat :number ::number)
  :ret ::number)

(defn sgn
  "Returns the sign of a number.
  
  Returns:
  - 1 if `number` is positive
  - 0 if `number` is zero  
  - -1 if `number` is negative
  - NaN if `number` is NaN
  
  Examples:
    (sgn 5)     ;=> 1
    (sgn 0)     ;=> 0
    (sgn -3)    ;=> -1
    (sgn ##NaN) ;=> ##NaN"
  [number]
  (cond (zero? number) 0
        (neg? number) -1
        (pos? number) 1
        :else nan))

(s/fdef sgn
  :args (s/cat :number ::number)
  :ret (s/or :nan ::nan
             :ret #{-1 0 1}))

(defn exp
  "Returns e^`number`."
  [number]
  (Math/exp (double number)))

(s/fdef exp
  :args (s/cat :number ::number)
  :ret ::nan-or-non-)

(defn dec-exp
  "Returns e^`number` minus one. Useful for positive numbers less than 1e-15."
  [number]
  (Math/expm1 (double number)))

(s/fdef dec-exp
  :args (s/cat :number ::number)
  :ret ::number)

(defn log
  "Returns log `number`."
  [number]
  (Math/log (double number)))

(s/fdef log
  :args (s/cat :number ::number)
  :ret ::number)

(defn log-inc
  "Returns log of one plus `number`. Useful for positive numbers less than
  1e-15."
  [number]
  (Math/log1p (double number)))

(s/fdef log-inc
  :args (s/cat :number ::number)
  :ret ::number)

(defn log2
  "Returns base 2 log of `number`."
  [number]
  (/ (Math/log (double number)) log-two))

(s/fdef log2
  :args (s/cat :number ::number)
  :ret ::number)

(defn log10
  "Returns base 10 log of `number`."
  [number]
  (Math/log10 (double number)))

(s/fdef log10
  :args (s/cat :number ::number)
  :ret ::number)

(defn logn
  "Returns `base` log of `number`."
  [number base]
  (div (Math/log (double number))
       (Math/log (double base))))

(s/fdef logn
  :args (s/cat :number ::number :base ::number)
  :ret ::number)

(defn pow
  "Returns `number1` to the power of `number2`."
  [number1 number2]
  (Math/pow (double number1) (double number2)))

(s/fdef pow
  :args (s/cat :number1 ::number :number2 ::number)
  :ret ::number)

(defn abs
  "Computes the absolute value of a number.
  
  Returns |`number`|, always non-negative. Always returns a double."
  [number]
  (Math/abs (double number)))

(s/fdef abs
  :args (s/cat :number ::number)
  :ret ::nan-or-non-)

(defn abs'
  "Returns absolute value of `number` as a long if possible."
  [number]
  (maybe-long-able
    (Math/abs (double number))))

(s/fdef abs'
  :args (s/cat :number ::number)
  :ret ::nan-or-non-)

(defn sqrt
  "Computes the square root of a number.
  
  Returns √`number`. For negative numbers, returns NaN.
  
  Examples:
    (sqrt 9)     ;=> 3.0
    (sqrt 2)     ;=> 1.4142135623730951
    (sqrt 0)     ;=> 0.0
    (sqrt -1)    ;=> ##NaN"
  [number]
  (Math/sqrt (double number)))

(s/fdef sqrt
  :args (s/cat :number ::number)
  :ret ::nan-or-non-)

(defn cbrt
  "Returns cube root of `number`. Handles negative numbers correctly."
  [number]
  (Math/cbrt (double number)))

(s/fdef cbrt
  :args (s/cat :number ::number)
  :ret ::number)

;;;TRIGONOMETRY
(defn sin
  "Returns sine of `number`."
  [number]
  (Math/sin (double number)))

(s/fdef sin
  :args (s/cat :number ::number)
  :ret ::nan-or-corr)

(defn sinh
  "Returns hyperbolic sine of `number`."
  [number]
  (Math/sinh (double number)))

(s/fdef sinh
  :args (s/cat :number ::number)
  :ret ::number)

(defn asin
  "Returns inverse sine of `number`."
  [number]
  (Math/asin (double number)))

(s/fdef asin
  :args (s/cat :number ::number)
  :ret ::number)

(defn asinh
  "Returns inverse hyperbolic sine of `number`."
  [number]
  (-> (double number) sq inc sqrt (+ number) log))

(s/fdef asinh
  :args (s/cat :number ::number)
  :ret ::number)

(defn cos
  "Returns cosine of `number`."
  [number]
  (Math/cos (double number)))

(s/fdef cos
  :args (s/cat :number ::number)
  :ret ::nan-or-corr)

(defn cosh
  "Returns hyperbolic cosine of `number`."
  [number]
  (Math/cosh (double number)))

(s/fdef cosh
  :args (s/cat :number ::number)
  :ret (s/and ::nan-or-pos #(or (nan? %) (>= % 1.0))))

(defn acos
  "Returns inverse cosine of `number`."
  [number]
  (Math/acos (double number)))

(s/fdef acos
  :args (s/cat :number ::number)
  :ret ::number)

(defn acosh
  "Returns inverse hyperbolic cosine of `number`.

  Uses asymptotic formula log(2x) for large x to avoid precision loss."
  [number]
  (cond (not (>= number 1)) nan
        ;; For large x, use log(2x) = log(2) + log(x) to avoid overflow in x^2
        (> number 1e8) (+ log-two (log number))
        :else (-> (double number) sq dec sqrt (+ number) log)))

(s/fdef acosh
  :args (s/cat :number ::number)
  :ret ::number)

(defn tan
  "Returns tangent of `number`."
  [number]
  (Math/tan (double number)))

(s/fdef tan
  :args (s/cat :number ::number)
  :ret ::number)

(defn tanh
  "Returns hyperbolic tangent of `number`."
  [number]
  (Math/tanh (double number)))

(s/fdef tanh
  :args (s/cat :number ::number)
  :ret ::nan-or-corr)

(defn atan
  "Returns inverse tangent of `number`."
  [number]
  (Math/atan (double number)))

(s/fdef atan
  :args (s/cat :number ::number)
  :ret ::number)

(defn atan2
  "Returns inverse tangent with two arguments."
  [number1 number2]
  (Math/atan2 (double number1) (double number2)))

(s/fdef atan2
  :args (s/cat :number1 ::number :number2 ::number)
  :ret ::number)

(defn atanh
  "Returns inverse hyperbolic tangent of `number`.

  Computes atanh(x) = 0.5 × ln((1+x)/(1-x)).
  Domain: (-1, 1), returns NaN outside this range.
  atanh(±1) = ±∞"
  [number]
  (cond (not (corr? number)) nan
        (one? number) inf+
        (== number -1) inf-
        :else (* 0.5 (- (log (inc (double number)))
                        (log (- 1.0 number))))))

(s/fdef atanh
  :args (s/cat :number ::number)
  :ret ::number)

(defn hypot
  "Returns hypotenuse with sides `number1` and `number2`."
  [number1 number2]
  (Math/hypot (double number1) (double number2)))

(s/fdef hypot
  :args (s/cat :number1 ::number :number2 ::number)
  :ret ::number)

;;;ROUNDING
(s/def ::accu
  (s/with-gen ::non-
              #(gen/double* {:min tiny-dbl :max 1e-3 :NaN? false})))

(s/def ::round-type #{:up :down :away-from-zero :toward-zero :toward-even})

(defn round'
  "Rounds to nearest integer, with tie-breaking rule specified by `round-type`.
  Returns a long if possible. Otherwise, returns `number`.
    `round-type` (all round to nearest, differs only at halfway points):
      `:up` - ties go toward +∞ (0.5 → 1, -0.5 → 0)
      `:down` - ties go toward -∞ (0.5 → 0, -0.5 → -1)
      `:away-from-zero` - ties go away from zero (0.5 → 1, -0.5 → -1)
      `:toward-zero` - ties go toward zero (0.5 → 0, -0.5 → 0)
      `:toward-even` - ties go to nearest even (0.5 → 0, 1.5 → 2, 2.5 → 2)"
  [number round-type]
  (if-not (long-range? number)
    number
    (let [number (double number)
          number (case round-type
                   :down (* -1 (Math/round (- number)))

                   :away-from-zero
                   (* (sgn number) (Math/round ^double (abs number)))

                   :toward-zero
                   (* -1 (sgn number) (Math/round ^double (- (abs number))))

                   :toward-even
                   (long (Math/rint number))

                   (Math/round number))]
      number)))

(s/fdef round'
  :args (s/cat :number ::number
               :round-type ::round-type)
  :ret ::number)

(def round
  "Alias for round'. See round' for documentation."
  round')

(defn round-significant
  "Round a number to the specified number of significant digits. This function
  can be used in conjunction with functions like floor, roughly-floor, etc. to
  get 'floor-significant', etc."
  [number significant-digits round-type]
  (if (or (nan? number) (zero? number) (inf? number))
    number
    (let [f (fn [n]
              (let [magnitude (floor (log10 (abs n)))
                    factor (pow 10 (- significant-digits magnitude 1))
                    rounded (/ (round' (* n factor) round-type) factor)]
                rounded))
          rounded (f number)]
      (cond (finite? rounded) rounded
            (nan? rounded) (* 1e-300 (f (* 1e300 number)))
            :else (* 1e300 (f (* 1e-300 number)))))))

(s/fdef round-significant
  :args (s/cat :number ::number
               :significant-digits ::int+
               :round-type ::round-type)
  :ret ::number)

(defn floor
  "Rounds a number down to the nearest integer.
  
  Returns the largest integer ≤ `number`. Always returns a double."
  [number]
  (Math/floor number))

(s/fdef floor
  :args (s/cat :number ::number)
  :ret ::number)

(defn floor'
  "Rounds a number down, returning a long when possible.
  
  Returns the largest integer ≤ `number` as a long if it fits in long range,
  otherwise returns a double."
  [number]
  (maybe-long-range (floor number)))

(s/fdef floor'
  :args (s/cat :number ::number)
  :ret ::number)

(defn ceil
  "Rounds a number up to the nearest integer.
  
  Returns the smallest integer ≥ `number`. Always returns a double."
  [number]
  (Math/ceil number))

(s/fdef ceil
  :args (s/cat :number ::number)
  :ret ::number)

(defn ceil'
  "Rounds up. Returns a long if possible, otherwise a double."
  [number]
  (maybe-long-range
    (ceil number)))

(s/fdef ceil'
  :args (s/cat :number ::number)
  :ret ::number)

(defn roughly-floor
  "Rounds down unless within `accu`, then rounds up. Returns a double."
  [number accu]
  (floor (+ number (double accu))))

(s/fdef roughly-floor
  :args (s/cat :number ::number :accu ::accu)
  :ret ::number)

(defn roughly-floor'
  "Rounds down unless within `accu`, then rounds up. Returns a long if possible,
  otherwise a double."
  [number accu]
  (floor' (+ number (double accu))))

(s/fdef roughly-floor'
  :args (s/cat :number ::number :accu ::accu)
  :ret ::number)

(defn roughly-ceil
  "Rounds up unless within `accu`, then rounds down. Returns a double."
  [number accu]
  (ceil (- number (double accu))))

(s/fdef roughly-ceil
  :args (s/cat :number ::number :accu ::accu)
  :ret ::number)

(defn roughly-ceil'
  "Rounds up unless within `accu`, then rounds down. Returns a long if possible,
  otherwise a double."
  [number accu]
  (ceil' (- number (double accu))))

(s/fdef roughly-ceil'
  :args (s/cat :number ::number :accu ::accu)
  :ret ::number)

(defn roughly?
  "Tests if two numbers are approximately equal within a tolerance.
  
  Returns true if `number1` and `number2` are within `accu` of each other.
  Handles special cases:
  - NaN arguments → false
  - Infinite tolerance → true  
  - Infinite arguments → false (unless both same infinity)
  
  Useful for floating-point comparisons where exact equality fails due to
  rounding errors.
  
  Examples:
    (roughly? 1.0 1.0000001 1e-6)  ;=> true
    (roughly? 1.0 1.1 0.05)        ;=> false  
    (roughly? ##NaN 1.0 0.1)       ;=> false"
  [number1 number2 accu]
  (cond (or (nan? number1) (nan? number2)) false
        (inf+? accu) true
        (or (inf? number1) (inf? number2)) false
        :else (<= (abs (- number1 (double number2))) accu)))

(s/fdef roughly?
  :args (s/cat :number1 ::number
               :number2 ::number
               :accu ::accu)
  :ret boolean?)

(defn roughly-round?
  "Returns true if `number` is equal to a whole number or within `accu` of a
  whole number, or within double accuracy."
  [number accu]
  (cond (nan? number) false
        (inf+? accu) true
        (inf? number) false
        :else (<= (abs (- (round' number :up) (double number))) accu)))

(s/fdef roughly-round?
  :args (s/cat :number ::number :accu ::accu)
  :ret boolean?)

(defn roughly-round-non-?
  "Returns true if `number` is non- and roughly a whole number, or within
  double accuracy."
  [number accu]
  (and (non-? number) (roughly-round? number accu)))

(s/fdef roughly-round-non-?
  :args (s/cat :number ::number :accu ::accu)
  :ret boolean?)

(defn roughly-round-non+?
  "Returns true if `number` is non+ and roughly a whole number, or within
  double accuracy."
  [number accu]
  (and (non+? number) (roughly-round? number accu)))

(s/fdef roughly-round-non+?
  :args (s/cat :number ::number :accu ::accu)
  :ret boolean?)

(defn roughly-round+?
  "Returns true if `number` is positive and roughly a whole number, or within
  double accuracy."
  [number accu]
  (and (pos? number) (roughly-round? number accu)))

(s/fdef roughly-round+?
  :args (s/cat :number ::number :accu ::accu)
  :ret boolean?)

(defn roughly-round-?
  "Returns true if `number` is negative and roughly a whole number, or within
  double accuracy."
  [number accu]
  (and (neg? number) (roughly-round? number accu)))

(s/fdef roughly-round-?
  :args (s/cat :number ::number :accu ::accu)
  :ret boolean?)

(defn roughly-non-?
  "Returns true if `number` is positive or within `accu` to zero."
  [number accu]
  (>= number (- accu)))

(s/fdef roughly-non-?
  :args (s/cat :number ::number :accu ::accu)
  :ret boolean?)

(defn roughly-non+?
  "Returns true if `number` is negative or within `accu` to zero."
  [number accu]
  (<= number accu))

(s/fdef roughly-non+?
  :args (s/cat :number ::number :accu ::accu)
  :ret boolean?)

(defn roughly-prob?
  "Returns true if `number` is a prob or within `accu` of a prob."
  [number accu]
  (and (>= number (- accu)) (<= number (inc (double accu)))))

(s/fdef roughly-prob?
  :args (s/cat :number ::number :accu ::accu)
  :ret boolean?)

(defn roughly-corr?
  "Returns true if `number` is a corr or within `accu` of a corr."
  [number accu]
  (and (>= number (dec (- (double accu))))
       (<= number (inc (double accu)))))

(s/fdef roughly-corr?
  :args (s/cat :number ::number :accu ::accu)
  :ret boolean?)

;;;QUOTIENTS
(defn- quotient-invalid?
  "Returns true if numerator/divisor pair is invalid for quotient operations."
  [numerator divisor]
  (or (nan? divisor)
      (nan? numerator)
      (inf? numerator)
      (inf? divisor)
      (zero? divisor)))

(defn quot'
  "Quotient of dividing `numerator` by `divisor`. Returns a long if possible."
  [numerator divisor]
  (if (quotient-invalid? numerator divisor)
    nan
    (let [d (div numerator divisor)]
      (if (or (inf? d) (nan? d))
        d
        (maybe-long-range
          (quot numerator divisor))))))

(s/fdef quot'
  :args (s/cat :numerator ::number
               :divisor ::number)
  :ret ::number)

(defn mod'
  "Modulus of `numerator` and `divisor`. Truncates toward negative infinity. Has
  sign of `divisor` unless numerical rounding error with [[quot']]. Will stay
  consistent with [[quot']]. Returns a long if possible."
  [numerator divisor]
  (if (quotient-invalid? numerator divisor)
    nan
    (let [d (div numerator divisor)]
      (if (or (inf? d) (nan? d))
        nan
        (maybe-long-able
          (mod numerator divisor))))))

(s/fdef mod'
  :args (s/cat :numerator ::number
               :divisor ::number)
  :ret ::number)

(defn rem'
  "Remainder of dividing `numerator` by `divisor`. Has sign of `numerator`
  unless numerical rounding error with [[quot']]. Will stay consistent with
  [[quot']]. Returns a long if possible."
  [numerator divisor]
  (if (quotient-invalid? numerator divisor)
    nan
    (let [d (div numerator divisor)]
      (if (or (inf? d) (nan? d))
        nan
        (maybe-long-able (rem numerator divisor))))))

(s/fdef rem'
  :args (s/cat :numerator ::number :divisor ::number)
  :ret ::number)

(defn quot-and-rem'
  "Returns a tuple of longs if possible. Quotient of dividing `numerator` by
  `divisor`. Remainder of dividing `numerator` by `divisor`. Has sign of
  `numerator` unless numerical rounding error with [[quot']]. Will stay
  consistent with [[quot']]."
  [numerator divisor]
  [(quot' numerator divisor) (rem' numerator divisor)])

(s/fdef quot-and-rem'
  :args (s/cat :numerator ::number :divisor ::number)
  :ret (s/tuple ::number ::number))

(defn quot-and-mod'
  "Returns a tuple of longs if possible. Quotient of dividing `numerator` by
  `divisor`. Modulus of `numerator` and `divisor`. Truncates toward negative
  infinity. Has sign of `divisor` unless numerical rounding error with
  [[quot']]. Will stay consistent with [[quot']]."
  [numerator divisor]
  (let [q (quot' numerator divisor)
        m (mod' numerator divisor)
        q (if (and (not (zero? numerator)) (= (sgn numerator) (- (sgn m))))
            (maybe-long-able (dec (double q)))
            q)]
    [q m]))

(s/fdef quot-and-mod'
  :args (s/cat :numerator ::number :divisor ::number)
  :ret (s/tuple ::number ::number))

(defn gcd
  "Returns the Greatest Common Divisor (Denominator) of two longs.

  Handles negative numbers by taking absolute values. Returns a non-negative
  result. gcd(0, 0) returns 0."
  [long1 long2]
  (let [a (abs' long1)
        b (abs' long2)]
    (if (zero? b)
      a
      (recur b (mod' a b)))))

(s/fdef gcd
  :args (s/cat :long1 ::long
               :long2 ::long)
  :ret ::long-non-)

(defn lcm'
  "Returns the Least Common Multiple of two longs. Returns a long if possible.

  Handles negative numbers by taking absolute values. Returns a non-negative
  result. lcm(0, n) = lcm(n, 0) = 0. Returns a double if the result overflows
  long range."
  [long1 long2]
  (let [a (abs' long1)
        b (abs' long2)]
    (if (or (zero? a) (zero? b))
      0
      (maybe-long-able (* (double (quot' a (gcd a b))) b)))))

(s/fdef lcm'
  :args (s/cat :long1 ::long
               :long2 ::long)
  :ret ::non-)

;;;ANGLES
(defn reduce-angle'
  "Returns an `angle` between 0 and 360. Returns a long if possible."
  [angle]
  (let [m (mod' angle 360)]
    (if (or (nan? m)
            (inf? m)
            (and (pos? m) (< m 360.0)))
      m
      (let [m2 (mod' m 360)]
        (if (or (nan? m2)
                (inf? m2)
                (and (pos? m2) (< m2 360.0)))
          m2
          (mod' m2 360))))))

(s/fdef reduce-angle'
  :args (s/cat :angle ::number)
  :ret (s/or :int (s/int-in 0 360)
             :dbl (s/double-in :min 0.0 :max 360.0)
             :nan ::nan
             :inf ::inf))

(defn reduce-radians'
  "Returns `radians` between 0 and 2 × PI. Returns a long if possible."
  [radians]
  (let [m (mod' radians two-pi)]
    (if (or (nan? m)
            (inf? m)
            (and (pos? m) (< m two-pi)))
      m
      (let [m2 (mod' m two-pi)]
        (if (or (nan? m2)
                (inf? m2)
                (and (pos? m2) (< m2 two-pi)))
          m2
          (mod' m2 two-pi))))))

(s/fdef reduce-radians'
  :args (s/cat :radians ::number)
  :ret (s/or :int (s/int-in 0 7)
             :dbl (s/double-in :min 0.0 :max two-pi)
             :nan ::nan
             :inf ::inf))

(defn radians->angle'
  "Returns the reduced angle from `radians`, where
  angles = 180 × `radians` / PI. Returns a long if possible."
  [radians]
  (if (inf? radians)
    radians
    (reduce-angle' (Math/toDegrees radians))))

(s/fdef radians->angle'
  :args (s/cat :radians ::number)
  :ret (s/or :int (s/int-in 0 360)
             :dbl (s/double-in :min 0.0 :max 360.0)
             :nan ::nan
             :inf ::inf))

(defn angle->radians'
  "Returns the reduced radians from the `angle`, where
  radians = `angle` × PI / 180. Returns a long if possible."
  [angle]
  (if (inf? angle)
    angle
    (maybe-long-able (Math/toRadians (reduce-angle' angle)))))

(s/fdef angle->radians'
  :args (s/cat :angle ::number)
  :ret (s/or :int (s/int-in 0 7)
             :dbl (s/double-in :min 0.0 :max two-pi)
             :nan ::nan
             :inf ::inf))
