(ns provisdom.math.core
  (:refer-clojure :exclude [pos? neg? int?])
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;;;DECLARATIONS
(declare nan? roughly-round? non-? non+? next-up next-down)

;;;MATH CONSTANTS
(def ^:const ^long sgl-digits 6)
(def ^:const ^long dbl-digits 15)
(def ^:const ^long long-digits 18)
(def ^:const ^long quad-digits 33)
(def ^:const ^double sgl-close 1e-6)
(def ^:const ^double dbl-close 1e-15)
(def ^:const ^double quad-close 1e-33)
(def ^:const half (/ 2))
(def ^:const ^double E Math/E)
(def ^:const ^double PI Math/PI)
(def ^:const ^double nan Double/NaN)
(def ^:const ^double inf+ Double/POSITIVE_INFINITY)
(def ^:const ^double inf- Double/NEGATIVE_INFINITY)
(def ^:const ^double max-dbl Double/MAX_VALUE)
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
(def ^:const ^double log-two (Math/log 2))
(def ^:const ^double log-ten (Math/log 10))
(def ^:const ^double log-pi (Math/log PI))
(def ^:const ^double log-pi-squared (* 2 log-pi))
(def ^:const ^double log-two-pi (+ log-two log-pi))
(def ^:const ^double log-two-pi-e (+ (Math/log E) log-two-pi))
(def ^:const ^double half-log-two-pi (* 0.5 log-two-pi))
(def ^:const ^double two-pi (* 2 PI))
(def ^:const ^double two-pi-e (* two-pi E))
(def ^:const ^double pi-squared (* PI PI))
(def ^:const ^double sqrt-two (Math/sqrt 2))
(def ^:const ^double sqrt-half (/ sqrt-two))
(def ^:const ^double sqrt-pi (Math/sqrt PI))
(def ^:const ^double sqrt-two-pi (* sqrt-two sqrt-pi))
(def ^:const ^double sqrt-half-pi (* sqrt-half sqrt-pi))
(def ^:const ^double inv-pi (/ PI))
(def ^:const ^double inv-sqrt-pi (/ sqrt-pi))
(def ^:const ^double inv-sqrt-two (/ sqrt-two))
(def ^:const ^double inv-two-pi (* 0.5 inv-pi))
(def ^:const ^double inv-sqrt-two-pi (* inv-sqrt-two inv-sqrt-pi))
(def ^:const ^double inv-log-two (/ log-two))

;;;TEST FOR NUMERIC TYPES
(defn- long-range? [x] (and (<= x max-long) (>= x min-long)))

(defn- maybe-long-range [x] (if (long-range? x) (long x) x))

(defn- int-range? [x] (and (<= x max-int) (>= x min-int)))

(defn- sgl-range? [x] (and (<= x max-sgl) (>= x min-sgl)))

(s/def ::number (s/spec number? :gen #(gen/one-of [(gen/double) (gen/large-integer)])))

(defn numbers?
  "Returns true if a collection of numbers."
  [x]
  (and (sequential? x) (every? number? x)))

(def mdl 6)                                                 ;max-dim-length for generators

(s/def ::numbers
  (s/with-gen
    numbers?
    #(s/gen (s/or :v (s/coll-of ::number :min-count 0 :max-count mdl :kind vector? :into [])
                  :l (s/coll-of ::number :min-count 0 :max-count mdl :kind list? :into '())))))

(defn num?
  "Returns true if x is a number and not nan."
  [x]
  (and (number? x) (== x x)))

(s/def ::num (s/spec num? :gen #(gen/one-of [(gen/double* {:NaN? false}) (gen/large-integer)])))

(defn nan?
  "Returns true if x is nan."
  [x]
  (and (number? x) (not (== x x))))

(s/def ::nan (s/spec nan? :gen #(gen/return nan)))

(defn pos?
  "Returns true if x is a number that is positive."
  [x]
  (and (number? x) (clojure.core/pos? x)))

(s/def ::pos (s/spec pos? :gen #(gen/one-of [(gen/double* {:min tiny-dbl :NaN? false}) (gen/large-integer* {:min 1})])))
(s/def ::nan-or-pos (s/spec #(or (nan? %) (pos? %))
                            :gen #(gen/one-of [(gen/double* {:min tiny-dbl}) (gen/large-integer* {:min 1})])))

(defn neg?
  "Returns true if x is a number that is negative."
  [x]
  (and (number? x) (clojure.core/neg? x)))

(s/def ::neg
  (s/spec neg? :gen #(gen/one-of [(gen/double* {:max (- tiny-dbl) :NaN? false}) (gen/large-integer* {:max -1})])))
(s/def ::nan-or-neg (s/spec #(or (nan? %) (neg? %))
                            :gen #(gen/one-of [(gen/double* {:max (- tiny-dbl)}) (gen/large-integer* {:max -1})])))

(defn non-?
  "Returns true if x is non-negative."
  [x]
  (and (number? x) (>= x 0)))

(s/def ::non- (s/spec non-? :gen #(gen/one-of [(gen/double* {:min 0.0 :NaN? false}) (gen/large-integer* {:min 0})])))
(s/def ::nan-or-non- (s/spec #(or (nan? %) (non-? %))
                             :gen #(gen/one-of [(gen/double* {:min 0.0}) (gen/large-integer* {:min 0})])))

(defn non+?
  "Returns true if x is non-positive."
  [x]
  (and (number? x) (<= x 0)))

(s/def ::non+ (s/spec non+? :gen #(gen/one-of [(gen/double* {:max 0.0 :NaN? false}) (gen/large-integer* {:max 0})])))
(s/def ::nan-or-non+ (s/spec #(or (nan? %) (non+? %))
                             :gen #(gen/one-of [(gen/double* {:max 0.0}) (gen/large-integer* {:max 0})])))

(defn finite?
  "Returns true if x is a finite number."
  [x]
  (and (num? x) (not (Double/isInfinite ^double x))))

(s/def ::finite (s/spec finite? :gen #(gen/one-of [(gen/double* {:infinite? false :NaN? false}) (gen/large-integer)])))
(s/def ::nan-or-finite (s/spec #(or (nan? %) (finite? %))
                               :gen #(gen/one-of [(gen/double* {:infinite? false}) (gen/large-integer)])))

(defn finite+?
  "Returns true if x is a positive finite number."
  [x]
  (and (pos? x) (not (Double/isInfinite ^double x))))

(s/def ::finite+
  (s/spec finite+? :gen #(gen/one-of [(gen/double* {:min tiny-dbl :infinite? false :NaN? false})
                                      (gen/large-integer* {:min 1})])))
(s/def ::nan-or-finite+
  (s/spec #(or (nan? %) (finite+? %)) :gen #(gen/one-of [(gen/double* {:min tiny-dbl :infinite? false})
                                                         (gen/large-integer* {:min 1})])))

(defn finite-?
  "Returns true if x is a negative finite number."
  [x]
  (and (neg? x) (not (Double/isInfinite ^double x))))

(s/def ::finite-
  (s/spec finite-? :gen #(gen/one-of [(gen/double* {:max (- tiny-dbl) :infinite? false :NaN? false})
                                      (gen/large-integer* {:max -1})])))
(s/def ::nan-or-finite-
  (s/spec #(or (nan? %) (finite-? %)) :gen #(gen/one-of [(gen/double* {:max (- tiny-dbl) :infinite? false})
                                                         (gen/large-integer* {:max -1})])))

(defn finite-non-?
  "Returns true if x is a non-negative finite number."
  [x]
  (and (non-? x) (not (Double/isInfinite ^double x))))

(s/def ::finite-non- (s/spec finite-non-? :gen #(gen/one-of [(gen/double* {:min 0.0 :infinite? false :NaN? false})
                                                             (gen/large-integer* {:min 0})])))
(s/def ::nan-or-finite-non-
  (s/spec #(or (nan? %) (finite-non-? %)) :gen #(gen/one-of [(gen/double* {:max 0.0 :infinite? false})
                                                             (gen/large-integer* {:max 0})])))

(defn finite-non+?
  "Returns true if x is a non-positive finite number."
  [x]
  (and (non+? x) (not (Double/isInfinite ^double x))))

(s/def ::finite-non+ (s/spec finite-non+? :gen #(gen/one-of [(gen/double* {:max 0.0 :infinite? false :NaN? false})
                                                             (gen/large-integer* {:max 0})])))
(s/def ::nan-or-finite-non+
  (s/spec #(or (nan? %) (finite-non+? %)) :gen #(gen/one-of [(gen/double* {:max 0.0 :infinite? false})
                                                             (gen/large-integer* {:max 0})])))

(s/def ::double (s/spec double? :gen #(gen/double)))

(defn double-finite?
  "Returns true if x is a double and finite."
  [x]
  (and (double? x)
       (== x x)
       (not (Double/isInfinite ^double x))))

(s/def ::double-finite (s/spec double-finite? :gen #(gen/double* {:infinite? false :NaN? false})))

(defn single?
  "Returns true if x is a single."
  [x]
  (and (double? x)
       (or (sgl-range? x)
           (not (== x x))
           (Double/isInfinite ^double x))))

(s/def ::single (s/spec single? :gen #(gen/double)))

(defn single-finite?
  "Returns true if x is a single and finite."
  [x]
  (and (double? x) (sgl-range? x)))

(s/def ::single-finite
  (s/spec single-finite? :gen #(gen/double* {:infinite? false :NaN? false :min min-sgl :max max-sgl})))

(defn long?
  "Returns true if x is a long."
  [x]
  (and (number? x) (instance? Long x)))

(s/def ::long (s/spec long? :gen gen/large-integer))

(defn long+?
  "Returns true if x is a long and is positive."
  [x]
  (and (pos? x) (long? x)))

(s/def ::long+ (s/spec long+? :gen #(s/gen (s/int-in 1 max-long))))

(defn long-?
  "Returns true if x is a long and is negative."
  [x]
  (and (neg? x) (long? x)))

(s/def ::long- (s/spec long-? :gen #(s/gen (s/int-in min-long 0))))

(defn long-non-?
  "Returns true if x is a long and is non-negative."
  [x]
  (and (non-? x) (long? x)))

(s/def ::long-non- (s/spec long-non-? :gen #(s/gen (s/int-in 0 max-long))))

(defn long-non+?
  "Returns true if x is a long and is non-positive."
  [x]
  (and (non+? x) (long? x)))

(s/def ::long-non+ (s/spec long-non+? :gen #(s/gen (s/int-in min-long 1))))

(defn int?
  "Returns true is x is an integer that is within the int range."
  [x]
  (and (integer? x) (int-range? x)))

(s/def ::int (s/spec int? :gen gen/int))

(defn int+?
  "Returns true if x is an int and is positive."
  [x]
  (and (int? x) (pos? x)))

(s/def ::int+ (s/spec int+? :gen #(s/gen (s/int-in 1 (inc max-int)))))

(defn int-?
  "Returns true if x is an int and is negative."
  [x]
  (and (int? x) (neg? x)))

(s/def ::int- (s/spec int-? :gen #(s/gen (s/int-in min-int 0))))

(defn int-non-?
  "Returns true if x is an int and is non-negative."
  [x]
  (and (int? x) (non-? x)))

(s/def ::int-non- (s/spec int-non-? :gen #(s/gen (s/int-in 0 (inc max-int)))))

(defn int-non+?
  "Returns true if x is an int and is non-positive."
  [x]
  (and (int? x) (non+? x)))

(s/def ::int-non+ (s/spec int-non+? :gen #(s/gen (s/int-in min-int 1))))

(defn long-able?
  "Returns true if x is a number that can be converted to a long."
  [x]
  (and (number? x)
       (roughly-round? x 0.0)
       (long-range? x)))

(s/def ::long-able (s/spec long-able? :gen gen/large-integer))

(defn long-able+?
  "Returns true if x is a number that can be converted to a long, and is positive."
  [x]
  (and (long-able? x) (pos? x)))

(s/def ::long-able+ (s/spec long-able+? :gen #(s/gen (s/int-in 1 max-long))))

(defn long-able-?
  "Returns true if x is a number that can be converted to a long, and is negative."
  [x]
  (and (long-able? x) (neg? x)))

(s/def ::long-able- (s/spec long-able-? :gen #(s/gen (s/int-in min-long 0))))

(defn long-able-non+?
  "Returns true if x is a number that can be converted to a long, and is non+."
  [x]
  (and (long-able? x) (non+? x)))

(s/def ::long-able-non+ (s/spec long-able-non+? :gen #(s/gen (s/int-in min-long 1))))
(s/def ::non-long-able-non+
  (s/spec #(and (num? %) (not (long-able-non+? %))) :gen #(s/gen (s/double-in :NaN? false))))
(s/def ::nan-or-non-long-able-non+
  (s/spec #(and (number? %) (not (long-able-non+? %))) :gen #(s/gen (s/double-in :NaN? true))))

(defn long-able-non-?
  "Returns true if x is a number that can be converted to a long, and is non-."
  [x]
  (and (long-able? x) (non-? x)))

(s/def ::long-able-non- (s/spec long-able-non-? :gen #(s/gen (s/int-in 0 max-long))))

(defn inf+?
  "Returns true if x is inf+."
  [x]
  (and (number? x)
       (Double/isInfinite ^double x)
       (pos? x)))

(s/def ::non-inf+ (s/spec #(and (num? %) (not (inf+? %))) :gen #(s/gen (s/double-in :NaN? false))))
(s/def ::nan-or-non-inf+ (s/spec #(and (number? %) (not (inf+? %))) :gen #(s/gen (s/double-in :NaN? true))))

(defn inf-?
  "Returns true if x is inf-."
  [x]
  (and (number? x) (Double/isInfinite ^double x) (neg? x)))

(s/def ::non-inf- (s/spec #(and (num? %) (not (inf-? %))) :gen #(s/gen (s/double-in :NaN? false))))
(s/def ::nan-or-non-inf- (s/spec #(and (number? %) (not (inf-? %))) :gen #(s/gen (s/double-in :NaN? true))))

(defn inf?
  "Returns true if x is inf+ or inf-."
  [x]
  (and (number? x) (Double/isInfinite ^double x)))

(s/def ::inf (s/spec inf? :gen #(gen/one-of [(gen/return inf-) (gen/return inf+)])))

(defn one?
  "Returns true if x if equal to one."
  [x]
  (and (number? x) (== 1 x)))

(s/def ::one (s/spec one? :gen #(gen/return 1.0)))

(defn prob?
  "Returns true if x is between 0 and 1, inclusive."
  [x]
  (and (non-? x) (<= x 1)))

(s/def ::prob (s/spec prob? :gen #(s/gen (s/double-in :min 0.0 :max 1.0 :NaN? false))))
(s/def ::nan-or-prob (s/spec #(or (nan? %) (prob? %)) :gen #(s/gen (s/double-in :min 0.0 :max 1.0))))

(defn open-prob?
  "Returns true if x is between 0 and 1, exclusive."
  [x]
  (and (pos? x) (< x 1)))

(s/def ::open-prob (s/spec open-prob? :gen #(s/gen (s/double-in :min tiny-dbl :max (next-down 1.0) :NaN? false))))
(s/def ::nan-or-open-prob
  (s/spec #(or (nan? %) (open-prob? %)) :gen #(s/gen (s/double-in :min tiny-dbl :max (next-down 1.0)))))

(defn corr?
  "Returns true if x is between -1 and 1, inclusive."
  [x]
  (and (number? x)
       (<= x 1)
       (>= x -1)))

(s/def ::corr (s/spec corr? :gen #(s/gen (s/double-in :min -1.0 :max 1.0 :NaN? false))))
(s/def ::nan-or-corr (s/spec #(or (nan? %) (corr? %)) :gen #(s/gen (s/double-in :min -1.0 :max 1.0))))

(defn open-corr?
  "Returns true if x is between -1 and 1, exclusive."
  [x]
  (and (number? x)
       (< x 1)
       (> x -1)))

(s/def ::open-corr
  (s/spec open-corr? :gen #(s/gen (s/double-in :min (next-up -1.0) :max (next-down 1.0) :NaN? false))))
(s/def ::nan-or-open-corr
  (s/spec #(or (nan? %) (open-corr? %)) :gen #(s/gen (s/double-in :min (next-up -1.0) :max (next-down 1.0)))))

(defn maybe-long-able
  "Returns x as a long if possible.  Otherwise returns x."
  [x]
  (if (long-able? x) (long x) x))

;;;BASIC MATH
(defn ===
  "Equality for numbers that works with NaN."
  ([number] true)
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
  "Returns `number` plus smallest amount to make a change."
  [number]
  (Math/nextAfter (double number) inf+))

(s/fdef next-up
        :args (s/cat :number ::number)
        :ret ::number)

(defn next-down
  "Returns `number` minus smallest amount to make a change."
  [number]
  (Math/nextAfter (double number) inf-))

(s/fdef next-down
        :args (s/cat :number ::number)
        :ret ::number)

(defn div
  "Returns `number1` divided by `number2`.
  Or 1 divided by `number2`.
  Dividing a positive number divided by zero returns inf+.
  Dividing a negative number divided by zero returns inf-.
  Dividing zero by zero will return NaN by default.
  Optionally, can include alternative return value for 0/0, 'zero-div-by-zero'."
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
  "Returns (1 - `number`).
  Will always return a double if `numbers` is used."
  ([number] (inc (- number)))
  ([number & numbers] (inc (- (apply + (double number) numbers)))))

(s/fdef one-
        :args (s/cat :number ::number :numbers (s/* ::number))
        :ret ::number)

(defn sq
  "Returns square of `number`."
  [number]
  (* (double number) number))

(s/fdef sq
        :args (s/cat :number ::number)
        :ret ::nan-or-non-)

(defn sq'
  "Returns square of `number` as a long if possible."
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
  "Returns 1 if `number` positive, 0 if 0, -1 if negative."
  [number]
  (cond (zero? number) 0
        (neg? number) -1
        (pos? number) 1
        :else nan))

(s/fdef sgn
        :args (s/cat :number ::number)
        :ret (s/or :nan ::nan :ret #{-1 0 1}))

(defn exp
  "Returns e^`number`."
  [number]
  (Math/exp (double number)))

(s/fdef exp
        :args (s/cat :number ::number)
        :ret ::nan-or-non-)

(defn log
  "Returns log `number`."
  [number]
  (Math/log (double number)))

(s/fdef log
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
  "Returns absolute value of `number`."
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
  "Returns square root of `number`."
  [number]
  (Math/sqrt (double number)))

(s/fdef sqrt
        :args (s/cat :number ::number)
        :ret ::nan-or-non-)

(defn cbrt
  "Returns cube root of `number`."
  [number]
  (* (sgn number) (pow (abs number) (/ 3.0))))

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

(defn acos
  "Returns inverse cosine of `number`."
  [number]
  (Math/acos (double number)))

(s/fdef acos
        :args (s/cat :number ::number)
        :ret ::number)

(defn acosh
  "Returns inverse hyperbolic cosine of `number`."
  [number]
  (if-not (>= number 1)
    nan
    (-> (double number) sq dec sqrt (+ number) log)))

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
  "Returns inverse hyperbolic tangent."
  [number]
  (cond (not (corr? number)) nan
        (one? number) inf+
        (== number -1) inf-
        :else (-> (double number) inc log (* -0.5))))

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
(s/def ::accu (s/with-gen ::non- #(gen/double* {:min tiny-dbl :max 1e-3 :NaN? false})))

(defn round
  "Returns a long if possible.
  Otherwise, returns `number`.
    `type`:
      `:up`
      `:down`
      `:away-from-zero`
      `:toward-zero`."
  [number type]
  (if-not (long-range? number)
    number
    (let [number (double number)
          number (case type
                   :down (* -1 (Math/round (- number)))
                   :away-from-zero (* (sgn number) (Math/round ^double (abs number)))
                   :toward-zero (* -1 (sgn number) (Math/round ^double (- (abs number))))
                   (Math/round number))]
      number)))

(s/fdef round
        :args (s/cat :number ::number :t #{:up :down :away-from-zero :toward-zero})
        :ret ::number)

(defn floor
  "Rounds down. Returns a double."
  [number]
  (Math/floor number))

(s/fdef floor
        :args (s/cat :number ::number)
        :ret ::number)

(defn floor'
  "Rounds down. Returns a long if possible, otherwise a double."
  [number]
  (maybe-long-range (floor number)))

(s/fdef floor'
        :args (s/cat :number ::number)
        :ret ::number)

(defn ceil
  "Rounds up. Returns a double."
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
  "Rounds down unless within `accu`, then rounds up.
  Returns a double."
  [number accu]
  (floor (+ number (double accu))))

(s/fdef roughly-floor
        :args (s/cat :number ::number :accu ::accu)
        :ret ::number)

(defn roughly-floor'
  "Rounds down unless within `accu`, then rounds up.
  Returns a long if possible, otherwise a double."
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
  "Rounds up unless within `accu`, then rounds down. Returns a long
  if possible, otherwise a double."
  [number accu]
  (ceil' (- number (double accu))))

(s/fdef roughly-ceil'
        :args (s/cat :number ::number :accu ::accu)
        :ret ::number)

(defn roughly?
  "Returns true if `number1` and `number2` are within `accu` of each
   other, or within double accuracy."
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
  "Returns true if `number` is equal to a whole number or within `accu`
  of a whole number, or within double accuracy."
  [number accu]
  (cond (nan? number) false
        (inf+? accu) true
        (inf? number) false
        :else (<= (abs (- (round number :up) (double number))) accu)))

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
  "Returns true if `number` is positive and roughly a whole number, or within double accuracy."
  [number accu]
  (and (pos? number) (roughly-round? number accu)))

(s/fdef roughly-round+?
        :args (s/cat :number ::number :accu ::accu)
        :ret boolean?)

(defn roughly-round-?
  "Returns true if `number` is negative and roughly a whole number, or within double accuracy."
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
(defn quot'
  "Quotient of dividing `numerator` by `divisor`.
  Returns a long if possible."
  [numerator divisor]
  (if (or (nan? divisor)
          (nan? numerator)
          (inf? numerator)
          (inf? divisor)
          (zero? divisor))
    nan
    (let [d (div numerator divisor)]
      (if (or (inf? d) (nan? d))
        d
        (maybe-long-range
          (quot numerator divisor))))))

(s/fdef quot'
        :args (s/cat :numerator ::number :divisor ::number)
        :ret ::number)

(defn mod'
  "Modulus of `numerator` and `divisor`.
  Truncates toward negative infinity.
  Has sign of `divisor` unless numerical rounding error with [[quot']].
  Will stay consistent with [[quot']].
  Returns a long if possible."
  [numerator divisor]
  (if (or (nan? divisor)
          (nan? numerator)
          (inf? numerator)
          (inf? divisor)
          (zero? divisor))
    nan
    (let [d (div numerator divisor)]
      (if (or (inf? d) (nan? d))
        nan
        (maybe-long-able
          (mod numerator divisor))))))

(s/fdef mod'
        :args (s/cat :numerator ::number :divisor ::number)
        :ret ::number)

(defn rem'
  "Remainder of dividing `numerator` by `divisor`.
  Has sign of `numerator` unless numerical rounding error with [[quot']].
  Will stay consistent with [[quot']].
  Returns a long if possible."
  [numerator divisor]
  (if (or (nan? divisor)
          (nan? numerator)
          (inf? numerator)
          (inf? divisor)
          (zero? divisor))
    nan
    (let [d (div numerator divisor)]
      (if (or (inf? d) (nan? d))
        nan
        (maybe-long-able (rem numerator divisor))))))

(s/fdef rem'
        :args (s/cat :numerator ::number :divisor ::number)
        :ret ::number)

(defn quot-and-rem'
  "Returns a tuple of longs if possible.
  Quotient of dividing `numerator` by `divisor`.
  Remainder of dividing `numerator` by `divisor`.
  Has sign of `numerator` unless numerical rounding error with [[quot']].
  Will stay consistent with [[quot']]."
  [numerator divisor]
  [(quot' numerator divisor) (rem' numerator divisor)])

(s/fdef quot-and-rem'
        :args (s/cat :numerator ::number :divisor ::number)
        :ret (s/tuple ::number ::number))

(defn quot-and-mod'
  "Returns a tuple of longs if possible.
  Quotient of dividing `numerator` by `divisor`.
  Modulus of `numerator` and `divisor`.
  Truncates toward negative infinity.
  Has sign of `divisor` unless numerical rounding error with [[quot']].
  Will stay consistent with [[quot']]."
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

;;;ANGLES
(defn reduce-angle'
  "Returns an `angle` between 0 and 360.
  Returns a long if possible."
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
  "Returns `radians` between 0 and 2 × PI.
  Returns a long if possible."
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
  "Returns the reduced angle from `radians`, where angles = 180 × `radians` / PI.
  Returns a long if possible."
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
  "Returns the reduced radians from the `angle`, where radians = `angle` × PI / 180.
  Returns a long if possible."
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