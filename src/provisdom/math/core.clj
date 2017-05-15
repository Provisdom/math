(ns provisdom.math.core
  (:refer-clojure :exclude [pos? neg? int?])
  (:require [provisdom.utility-belt.core :as co]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.spec.test :as st]))

(set! *warn-on-reflection* true)

;;;DECLARATIONS
(declare nan? roughly-round? non-? non+? tiny-up tiny-down)

;;;DYNAMIC VARIABLES
(def ^:dynamic *sgl-digits* 6)
(def ^:dynamic *dbl-digits* 15)
(def ^:dynamic *long-digits* 18)
(def ^:dynamic *quad-digits* 33)
(def ^:dynamic *sgl-close* 1e-6)
(def ^:dynamic *dbl-close* 1e-15)
(def ^:dynamic *quad-close* 1e-33)
(def ^:dynamic *min-iter* 10)
(def ^:dynamic *max-iter* 10000)

;;;MATH CONSTANTS
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
(def ^:const ^double log-half (Math/log 0.5))
;;since marked as const, should use Math/log
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

(s/def ::boolean (s/spec clojure.core/boolean? :gen #(gen/boolean)))
(s/def ::number (s/spec number? :gen #(s/gen (s/double-in))))

(defn num?
  "Returns true if x is a number and not nan"
  [x] (and (number? x) (== x x)))

(s/def ::num (s/spec num? :gen #(s/gen (s/double-in :NaN? false))))

(defn nan?
  "Returns true if x is nan"
  [x] (and (number? x) (not (== x x))))

(s/def ::nan (s/spec nan? :gen #(gen/return nan)))

(defn pos?
  "Returns true if x is a number that is positive."
  [x] (and (number? x) (clojure.core/pos? x)))

(s/def ::pos (s/spec pos? :gen #(s/gen (s/double-in :min tiny-dbl :NaN? false))))
(s/def ::nan-or-pos (s/spec #(or (nan? %) (pos? %)) :gen #(s/gen (s/double-in :min tiny-dbl))))

(defn neg?
  "Returns true if x is a number that is negative."
  [x] (and (number? x) (clojure.core/neg? x)))

(s/def ::neg (s/spec neg? :gen #(s/gen (s/double-in :max (- tiny-dbl) :NaN? false))))
(s/def ::nan-or-neg (s/spec #(or (nan? %) (neg? %)) :gen #(s/gen (s/double-in :max (- tiny-dbl)))))

(defn non-?
  "Returns true if x is non-negative"
  [x] (and (number? x) (>= x 0)))

(s/def ::non- (s/spec non-? :gen #(s/gen (s/double-in :min 0.0 :NaN? false))))
(s/def ::nan-or-non- (s/spec #(or (nan? %) (non-? %)) :gen #(s/gen (s/double-in :min 0.0))))

(defn non+?
  "Returns true if x is non-positive"
  [x] (and (number? x) (<= x 0)))

(s/def ::non+ (s/spec non+? :gen #(s/gen (s/double-in :max 0.0 :NaN? false))))
(s/def ::nan-or-non+ (s/spec #(or (nan? %) (non+? %)) :gen #(s/gen (s/double-in :max 0.0))))

(defn finite?
  "Returns true if x is a finite number."
  [x] (and (num? x) (not (Double/isInfinite ^double x))))

(s/def ::finite (s/spec finite? :gen #(s/gen (s/double-in :infinite? false :NaN? false))))
(s/def ::nan-or-finite (s/spec #(or (nan? %) (finite? %)) :gen #(s/gen (s/double-in :infinite? false))))

(defn finite+?
  "Returns true if x is a positive finite number."
  [x] (and (pos? x) (not (Double/isInfinite ^double x))))

(s/def ::finite+ (s/spec finite+? :gen #(s/gen (s/double-in :min tiny-dbl :infinite? false :NaN? false))))
(s/def ::nan-or-finite+ (s/spec #(or (nan? %) (finite+? %)) :gen #(s/gen (s/double-in :min tiny-dbl :infinite? false))))

(defn finite-?
  "Returns true if x is a negative finite number."
  [x] (and (neg? x) (not (Double/isInfinite ^double x))))

(s/def ::finite- (s/spec finite-? :gen #(s/gen (s/double-in :max (- tiny-dbl) :infinite? false :NaN? false))))
(s/def ::nan-or-finite- (s/spec #(or (nan? %) (finite-? %)) :gen #(s/gen (s/double-in :max (- tiny-dbl) :infinite? false))))

(defn finite-non-?
  "Returns true if x is a non-negative finite number."
  [x] (and (non-? x) (not (Double/isInfinite ^double x))))

(s/def ::finite-non- (s/spec finite-non-? :gen #(s/gen (s/double-in :min 0.0 :infinite? false :NaN? false))))
(s/def ::nan-or-finite-non- (s/spec #(or (nan? %) (finite-non-? %)) :gen #(s/gen (s/double-in :min 0.0 :infinite? false))))

(defn finite-non+?
  "Returns true if x is a non-positive finite number."
  [x] (and (non+? x) (not (Double/isInfinite ^double x))))

(s/def ::finite-non+ (s/spec finite-non+? :gen #(s/gen (s/double-in :max 0.0 :infinite? false :NaN? false))))
(s/def ::nan-or-finite-non+ (s/spec #(or (nan? %) (finite-non+? %)) :gen #(s/gen (s/double-in :max 0.0 :infinite? false))))

(defn single?
  "Returns true if x is a single."
  [x] (and (number? x) (or (sgl-range? x) (not (== x x)) (Double/isInfinite ^double x))))

(s/def ::single (s/spec single? :gen #(s/gen (s/double-in))))

(defn single-finite?
  "Returns true if x is a single and finite."
  [x] (and (number? x) (sgl-range? x)))

(s/def ::single-finite (s/spec single-finite? :gen #(s/gen (s/double-in :infinite? false :NaN? false))))

(defn long?
  "Returns true if x is a long."
  [x] (and (number? x) (instance? Long x)))

(s/def ::long (s/spec long? :gen gen/large-integer))

(defn long+?
  "Returns true if x is a long and is positive."
  [x] (and (pos? x) (long? x)))

(s/def ::long+ (s/spec long+? :gen #(s/gen (s/int-in 1 max-long))))

(defn long-?
  "Returns true if x is a long and is negative."
  [x] (and (neg? x) (long? x)))

(s/def ::long- (s/spec long-? :gen #(s/gen (s/int-in min-long 0))))

(defn long-non-?
  "Returns true if x is a long and is non-negative."
  [x] (and (non-? x) (long? x)))

(s/def ::long-non- (s/spec long-non-? :gen #(s/gen (s/int-in 0 max-long))))

(defn long-non+?
  "Returns true if x is a long and is non-positive."
  [x] (and (non+? x) (long? x)))

(s/def ::long-non+ (s/spec long-non+? :gen #(s/gen (s/int-in min-long 1))))

(defn int?
  "Returns true is x is an integer that is within the int range"
  [x] (and (integer? x) (int-range? x)))

(s/def ::int (s/spec int? :gen gen/int))

(defn int+?
  "Returns true if x is an int and is positive."
  [x] (and (int? x) (pos? x)))

(s/def ::int+ (s/spec int+? :gen #(s/gen (s/int-in 1 (inc max-int)))))

(defn int-?
  "Returns true if x is an int and is negative."
  [x] (and (int? x) (neg? x)))

(s/def ::int- (s/spec int-? :gen #(s/gen (s/int-in min-int 0))))

(defn int-non-?
  "Returns true if x is an int and is non-negative."
  [x] (and (int? x) (non-? x)))

(s/def ::int-non- (s/spec int-non-? :gen #(s/gen (s/int-in 0 (inc max-int)))))

(defn int-non+?
  "Returns true if x is an int and is non-positive."
  [x] (and (int? x) (non+? x)))

(s/def ::int-non+ (s/spec int-non+? :gen #(s/gen (s/int-in min-int 1))))

(defn long-able?
  "Returns true if x is a number that can be converted to a long"
  [x] (and (number? x) (roughly-round? x 0.0) (long-range? x)))

(s/def ::long-able (s/spec long-able? :gen gen/large-integer))

(defn long-able+?
  "Returns true if x is a number that can be converted to a long, and is positive"
  [x] (and (long-able? x) (pos? x)))

(s/def ::long-able+ (s/spec long-able+? :gen #(s/gen (s/int-in 1 max-long))))

(defn long-able-?
  "Returns true if x is a number that can be converted to a long, and is negative"
  [x] (and (long-able? x) (neg? x)))

(s/def ::long-able- (s/spec long-able-? :gen #(s/gen (s/int-in min-long 0))))

(defn long-able-non+?
  "Returns true if x is a number that can be converted to a long, and is non+"
  [x] (and (long-able? x) (non+? x)))

(s/def ::long-able-non+ (s/spec long-able-non+? :gen #(s/gen (s/int-in min-long 1))))

(s/def ::non-long-able-non+
  (s/spec #(and (num? %) (not (long-able-non+? %))) :gen #(s/gen (s/double-in :NaN? false))))

(defn long-able-non-?
  "Returns true if x is a number that can be converted to a long, and is non-"
  [x] (and (long-able? x) (non-? x)))

(s/def ::long-able-non- (s/spec long-able-non-? :gen #(s/gen (s/int-in 0 max-long))))

(defn inf+?
  "Returns true if x is inf+"
  [x] (and (number? x) (Double/isInfinite ^double x) (pos? x)))

(s/def ::non-inf+ (s/spec #(and (number? %) (not (inf+? %))) :gen #(s/gen (s/double-in :NaN? false))))

(defn inf-?
  "Returns true if x is inf-"
  [x] (and (number? x) (Double/isInfinite ^double x) (neg? x)))

(s/def ::non-inf- (s/spec #(and (number? %) (not (inf-? %))) :gen #(s/gen (s/double-in :NaN? false))))

(defn inf?
  "Returns true if x is inf+ or inf-"
  [x] (and (number? x) (Double/isInfinite ^double x)))

(s/def ::inf (s/spec inf? :gen #(gen/one-of [(gen/return inf-) (gen/return inf+)])))

(defn one?
  "Returns true if x if equal to one"
  [x] (and (number? x) (== 1 x)))

(s/def ::one (s/spec one? :gen #(gen/return 1.0)))

(defn prob?
  "Returns true if x is between 0 and 1, inclusive"
  [x] (and (non-? x) (<= x 1) (not (nan? x))))

(s/def ::prob (s/spec prob? :gen #(s/gen (s/double-in :min 0.0 :max 1.0 :NaN? false))))
(s/def ::nan-or-prob (s/spec #(or (nan? %) (prob? %)) :gen #(s/gen (s/double-in :min 0.0 :max 1.0))))

(defn open-prob?
  "Returns true if x is between 0 and 1, exclusive"
  [x] (and (number? x) (pos? x) (< x 1)))

(s/def ::open-prob (s/spec open-prob? :gen #(s/gen (s/double-in :min tiny-dbl :max (tiny-down 1.0) :NaN? false))))
(s/def ::nan-or-open-prob (s/spec #(or (nan? %) (open-prob? %)) :gen #(s/gen (s/double-in :min tiny-dbl :max (tiny-down 1.0)))))

(defn corr?
  "Returns true if x is between -1 and 1, inclusive"
  [x] (and (number? x) (<= x 1) (>= x -1) (not (nan? x))))

(s/def ::corr (s/spec corr? :gen #(s/gen (s/double-in :min -1.0 :max 1.0 :NaN? false))))
(s/def ::nan-or-corr (s/spec #(or (nan? %) (corr? %)) :gen #(s/gen (s/double-in :min -1.0 :max 1.0))))

(defn open-corr?
  "Returns true if x is between -1 and 1, exclusive"
  [x] (and (number? x) (< x 1) (> x -1)))

(s/def ::open-corr (s/spec open-corr? :gen #(s/gen (s/double-in :min (tiny-up -1.0) :max (tiny-down 1.0) :NaN? false))))
(s/def ::nan-or-open-corr (s/spec #(or (nan? %) (open-corr? %)) :gen #(s/gen (s/double-in :min (tiny-up -1.0) :max (tiny-down 1.0)))))

(defn maybe-long-able
  "Returns x as a long if possible.  Otherwise returns x."
  [x] (if (long-able? x) (long x) x))

;;;BASIC MATH
(defn tiny-up
  "Returns x plus tiny-dbl"
  [^double x] (+ x tiny-dbl))

(defn tiny-down
  "Returns x minus tiny-dbl"
  [^double x] (- x tiny-dbl))

(defn one-
  "Returns (1 - x)"
  ([x] (inc (- x)))
  ([x & y] (inc (- (+ x (apply + y))))))

(defn sq
  "Returns square of x"
  [x] (* x x))

(defn cube
  "Returns cube of x"
  [x] (* x x x))

(defn sgn
  "Returns 1 if x positive, 0 if 0, -1 if negative"
  [x] (cond (zero? x) 0, (neg? x) -1, (pos? x) 1, :else nan))

(defn exp
  "Returns e^x"
  ^double [^double x] (Math/exp x))

(defn log
  "Returns log x"
  ^double [^double x] (Math/log x))

(defn log2
  "Returns base 2 log of x"
  ^double [^double x] (/ (Math/log x) log-two))

(defn log10
  "Returns base 10 log of x"
  ^double [^double x] (Math/log10 x))

(defn logn
  "Returns base n log of x"
  ^double [^double x ^double n] (/ (Math/log x) (Math/log n)))

(defn pow
  "Returns x1 to the power of x2"
  ^double [^double x1 ^double x2] (Math/pow x1 x2))

(defn abs
  "Returns absolute value of x"
  ^double [^double x] (Math/abs x))

(defn abs'
  "Returns absolute value of x as a long if possible"
  [^double x] (maybe-long-able (Math/abs x)))

(defn sqrt
  "Returns square root of x"
  ^double [^double x] (Math/sqrt x))

(defn cbrt
  "Returns cube root of x"
  ^double [^double x] (* (sgn x) (pow (abs x) (/ 3.0))))

;;;TRIGONOMETRY
(defn sin
  "Returns sine of x"
  ^double [^double x] (Math/sin x))

(defn asin
  "Returns inverse sine of x"
  ^double [^double x] (Math/asin x))

(defn asinh
  "Returns inverse hyperbolic sine of x"
  ^double [^double x] (-> x sq inc sqrt (+ x) log))

(defn cos
  "Returns cosine of x"
  ^double [^double x] (Math/cos x))

(defn acos
  "Returns inverse cosine of x"
  ^double [^double x] (Math/acos x))

(defn acosh
  "Returns inverse hyperbolic cosine of x"
  ^double [^double x] (if-not (>= x 1) nan (-> x sq dec sqrt (+ x) log)))

(defn tan
  "Returns tangent of x"
  ^double [^double x] (Math/tan x))

(defn atan
  "Returns inverse tangent of x"
  ^double [^double x] (Math/atan x))

(defn atan2
  "Returns inverse tangent with two arguments"
  ^double [^double x1 ^double x2] (Math/atan2 x1 x2))

(defn atanh
  "Returns inverse hyperbolic tangent"
  ^double [^double x]
  (cond (not (corr? x)) nan, (one? x) inf+, (== x -1) inf-, :else (-> x inc log (* -0.5))))

(defn hypot
  "Returns hypotenuse with sides x1 and x2."
  ^double [^double x1 ^double x2] (Math/hypot x1 x2))

;;;ROUNDING
(defn round
  "Returns a long if possible.  Otherwise, returns x.
    type:
      :up (default)
      :down
      :away (from zero)
      :toward (zero)"
  [x type]
  (if-not (long-range? x)
    x
    (let [x (case type
              :down (* -1 (Math/round (- ^double x)))
              :away (* (sgn x) (Math/round (abs x)))
              :toward (* -1 (sgn x) (Math/round (- (abs x))))
              (Math/round ^double x))]
      (long x))))

(s/fdef round
        :args (s/cat :x ::number :t #{:up :down :away :toward})
        :ret ::number)

(defn floor
  "Rounds down.  Returns a long if possible, otherwise a double."
  [x] (maybe-long-range (Math/floor x)))

(s/fdef floor
        :args (s/cat :x ::number)
        :ret ::number)

(defn ceil
  "Rounds up.  Returns a long if possible, otherwise a double."
  [x] (maybe-long-range (Math/ceil x)))

(s/fdef ceil
        :args (s/cat :x ::number)
        :ret ::number)

(defn roughly-floor
  "Rounds down unless within accu, then rounds up. Returns a long if possible, otherwise a double."
  [x accu]
  (floor (+ x accu)))

(s/fdef roughly-floor
        :args (s/cat :x ::number :accu ::non-)
        :ret ::number)

(defn roughly-ceil
  "Rounds up unless within accu, then rounds down. Returns a long if possible, otherwise a double."
  [x accu]
  (ceil (- x accu)))

(s/fdef roughly-ceil
        :args (s/cat :x ::number :accu ::non-)
        :ret ::number)

(defn roughly?
  "Returns true if x1 and x2 are within accu of each other, or within double accuracy."
  [x1 x2 accu]
  (cond (or (nan? accu) (nan? x1) (nan? x2)) false
        (inf+? accu) true
        (or (inf? x1) (inf? x2)) false
        :else (<= (abs (- x1 x2)) accu)))

(s/fdef roughly?
        :args (s/cat :x1 ::number :x2 ::number :accu ::non-)
        :ret ::boolean)

(defn roughly-round?
  "Returns true if x is equal to a whole number or within accu of a whole number, or within double accuracy."
  [x accu]
  (cond (or (nan? accu) (nan? x)) false
        (inf+? accu) true
        (inf? x) false
        :else (<= (abs (- (round x :up) x)) accu)))

(s/fdef roughly-round?
        :args (s/cat :x ::number :accu ::non-)
        :ret ::boolean)

(defn roughly-round-non-?
  "Returns true if x is non- and roughly a whole number, or within double accuracy."
  [x accu]
  (and (non-? x) (roughly-round? x accu)))

(s/fdef roughly-round-non-?
        :args (s/cat :x ::number :accu ::non-)
        :ret ::boolean)

(defn roughly-round-non+?
  "Returns true if x is non+ and roughly a whole number, or within double accuracy."
  [x accu]
  (and (non+? x) (roughly-round? x accu)))

(s/fdef roughly-round-non+?
        :args (s/cat :x ::number :accu ::non-)
        :ret ::boolean)

(defn roughly-round+?
  "Returns true if x is positive and roughly a whole number, or within double accuracy."
  [x accu]
  (and (pos? x) (roughly-round? x accu)))

(s/fdef roughly-round+?
        :args (s/cat :x ::number :accu ::non-)
        :ret ::boolean)

(defn roughly-round-?
  "Returns true if x is negative and roughly a whole number, or within double accuracy."
  [x accu]
  (and (neg? x) (roughly-round? x accu)))

(s/fdef roughly-round-?
        :args (s/cat :x ::number :accu ::non-)
        :ret ::boolean)

(defn roughly-non-?
  "Returns true if x is positive or within accu to zero."
  [x accu]
  (>= x (- accu)))

(s/fdef roughly-non-?
        :args (s/cat :x ::number :accu ::non-)
        :ret ::boolean)

(defn roughly-non+?
  "Returns true if x is negative or within accu to zero."
  [x accu]
  (<= x accu))

(s/fdef roughly-non+?
        :args (s/cat :x ::number :accu ::non-)
        :ret ::boolean)

(defn roughly-prob?
  "Returns true if x is a prob or within accu of a prob."
  [x accu]
  (and (>= x (- accu)) (<= x (inc accu))))

(s/fdef roughly-prob?
        :args (s/cat :x ::number :accu ::non-)
        :ret ::boolean)

(defn roughly-corr?
  "Returns true if x is a corr or within accu of a corr."
  [x accu]
  (and (>= x (dec (- accu))) (<= x (inc accu))))

(s/fdef roughly-corr?
        :args (s/cat :x ::number :accu ::non-)
        :ret ::boolean)

;;;QUOTIENTS
(defn quot'
  "Quotient of dividing numerator by denominator. Returns a long if possible."
  [num div]
  (if (or (nan? div) (nan? num) (inf? num) (inf? div) (zero? div))
    nan
    (let [d (/ num div)]
      (if (inf? d)
        d
        (maybe-long-range (quot num div))))))

(s/fdef quot'
        :args (s/cat :num ::number :div ::number)
        :ret ::number)

(defn mod'
  "Modulus of num and div. Truncates toward negative infinity.
  Has sign of div unless numerical rounding error with 'quot'.
  Will stay consistent with 'quot'.
  Returns a long if possible."
  [num div]
  (if (or (nan? div) (nan? num) (inf? num) (inf? div) (zero? div))
    nan
    (let [d (/ num div)]
      (if (inf? d)
        nan
        (maybe-long-able (mod num div))))))

(s/fdef mod'
        :args (s/cat :num ::number :div ::number)
        :ret ::number)

(defn rem'
  "Remainder of dividing numerator by denominator.
  Has sign of num unless numerical rounding error with 'quot'.
  Will stay consistent with 'quot'.
  Returns a long if possible."
  [num div]
  (if (or (nan? div) (nan? num) (inf? num) (inf? div) (zero? div))
    nan
    (let [d (/ num div)]
      (if (inf? d)
        nan
        (maybe-long-able (rem num div))))))

(s/fdef rem'
        :args (s/cat :num ::number :div ::number)
        :ret ::number)

(defn quot-and-rem
  "Returns a tuple of longs if possible.
  Quotient of dividing numerator by denominator.
  Remainder of dividing numerator by denominator.
  Has sign of num unless numerical rounding error with 'quot'.
  Will stay consistent with 'quot'."
  [num div] [(quot' num div) (rem' num div)])

(s/fdef quot-and-rem
        :args (s/cat :num ::number :div ::number)
        :ret (s/tuple ::number ::number))

(defn quot-and-mod
  "Returns a tuple of longs if possible.
  Quotient of dividing numerator by denominator.
  Modulus of num and div.
  Truncates toward negative infinity.
  Has sign of div unless numerical rounding error with 'quot'.
  Will stay consistent with 'quot'."
  [num div]
  (let [q (quot' num div)
        m (mod' num div)
        q (if (and (not (zero? num)) (= (sgn num) (- (sgn m)))) (dec q) q)]
    [q m]))

(s/fdef quot-and-mod
        :args (s/cat :num ::number :div ::number)
        :ret (s/tuple ::number ::number))

;;;ANGLES
(defn reduce-angle
  "Returns an angle between 0 and 360.  Returns a long if possible."
  [angle]
  (let [m (mod' angle 360)]
    (if (or (nan? m) (inf? m) (and (pos? m) (< m 360.0)))
      m
      (let [m2 (mod' m 360)]
        (if (or (nan? m2) (inf? m2) (and (pos? m2) (< m2 360.0)))
          m2
          (mod' m2 360))))))

(s/fdef reduce-angle
        :args (s/cat :angle ::number)
        :ret (s/or :int (s/int-in 0 360)
                   :dbl (s/double-in :min 0.0 :max 360.0)
                   :nan ::nan
                   :inf ::inf))

(defn reduce-radians
  "Returns radians between 0 and 2 * PI.  Returns a long if possible."
  [radians]
  (let [m (mod' radians two-pi)]
    (if (or (nan? m) (inf? m) (and (pos? m) (< m two-pi)))
      m
      (let [m2 (mod' m two-pi)]
        (if (or (nan? m2) (inf? m2) (and (pos? m2) (< m2 two-pi)))
          m2
          (mod' m2 two-pi))))))

(s/fdef reduce-radians
        :args (s/cat :radians ::number)
        :ret (s/or :int (s/int-in 0 7)
                   :dbl (s/double-in :min 0.0 :max two-pi)
                   :nan ::nan
                   :inf ::inf))

(defn radians->angle
  "Returns the reduced angle from radians, where angles = 180 * radians / PI. Returns a long if possible."
  [radians]
  (if (inf? radians) radians (reduce-angle (Math/toDegrees radians))))

(s/fdef radians->angle
        :args (s/cat :radians ::number)
        :ret (s/or :int (s/int-in 0 360)
                   :dbl (s/double-in :min 0.0 :max 360.0)
                   :nan ::nan
                   :inf ::inf))

(defn angle->radians
  "Returns the reduced radians from the angle, where radians = angle * PI / 180. Returns a long if possible."
  [angle]
  (if (inf? angle) angle (maybe-long-able (Math/toRadians (reduce-angle angle)))))

(s/fdef angle->radians
        :args (s/cat :angle ::number)
        :ret (s/or :int (s/int-in 0 7)
                   :dbl (s/double-in :min 0.0 :max two-pi)
                   :nan ::nan
                   :inf ::inf))