(ns provisdom.math.core
  (:require [provisdom.utility-belt.core :as co]
            [taoensso.truss :as truss :refer (have have! have?)]))

(set! *warn-on-reflection* true)

;;;DECLARATIONS
(declare nan? roughly-round? non-? non+?)

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
(defn- long-range? [x] (and (not (nan? x)) (<= x max-long) (>= x min-long)))

(defn- maybe-long-range [x] (if (long-range? x) (long x) x))

(defn- int-range? [x] (and (<= x max-int) (>= x min-int)))

(defn long-able?
  "Returns true if x is a number that can be converted to a long"
  [x] (and (number? x) (roughly-round? x 0.0) (long-range? x)))

(defn long-able+?
  "Returns true if x is a number that can be converted to a long,
   and is positive"
  [x] (and (long-able? x) (pos? x)))

(defn long-able-?
  "Returns true if x is a number that can be converted to a long,
   and is negative"
  [x] (and (long-able? x) (neg? x)))

(defn long-able-non+?
  "Returns true if x is a number that can be converted to a long,
   and is non+"
  [x] (and (long-able? x) (non+? x)))

(defn long-able-non-?
  "Returns true if x is a number that can be converted to a long,
   and is non-"
  [x] (and (long-able? x) (non-? x)))

(defn maybe-long-able
  "Returns x as a long if possible.  Otherwise returns x."
  [x] (if (long-able? x) (long x) x))

(defn int?
  "Returns true is x is an integer that is within the int range"
  [x] (and (integer? x) (int-range? x)))

(defn inf+?
  "Returns true if x is inf+"
  [x] (and (number? x) (Double/isInfinite ^double x) (pos? x)))

(defn inf-?
  "Returns true if x is inf-"
  [x] (and (number? x) (Double/isInfinite ^double x) (neg? x)))

(defn inf?
  "Returns true if x is inf+ or inf-"
  [x] (and (number? x) (Double/isInfinite ^double x)))

(defn nan?
  "Returns true if x is nan"
  [x] (and (number? x) (not (== x x))))

(defn one?
  "Returns true if x if equal to one"
  [x] (and (number? x) (== 1 x)))

(defn non-?
  "Returns true if x is non-negative"
  [x] (and (number? x) (>= x 0) (not (nan? x))))

(defn non+?
  "Returns true if x is non-positive"
  [x] (and (number? x) (<= x 0) (not (nan? x))))

(defn prob?
  "Returns true if x is between 0 and 1, inclusive"
  [x] (and (non-? x) (<= x 1) (not (nan? x))))

(defn open-prob?
  "Returns true if x is between 0 and 1, exclusive"
  [x] (and (number? x) (pos? x) (< x 1)))

(defn corr?
  "Returns true if x is between -1 and 1, inclusive"
  [x] (and (number? x) (<= x 1) (>= x -1) (not (nan? x))))

(defn open-corr?
  "Returns true if x is between -1 and 1, exclusive"
  [x] (and (number? x) (< x 1) (> x -1)))

;;;BASIC MATH
(defn one-
  "Returns (1 - x)"
  [x] (inc (- x)))

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
  (cond (not (corr? x)) nan, (one? x) inf+, (== x -1) inf-,
        :else (-> x inc log (* -0.5))))

(defn hypot
  "Returns hypotenuse with sides x1 and x2."
  ^double [^double x1 ^double x2] (Math/hypot x1 x2))

;;;ROUNDING
(defn round
  "Returns a long if possible.  Otherwise, returns x.
type can be :up (default), :down, :away (from zero), :toward (zero)"
  [x & {:keys [type]}]
  {:pre [(have? number? x)]}
  (if-not (long-range? x)
    x
    (let [x (case type
              :down (* -1 (Math/round (- ^double x)))
              :away (* (sgn x) (Math/round (abs x)))
              :toward (* -1 (sgn x) (Math/round (- (abs x))))
              (Math/round ^double x))]
      (long x))))

(defn floor
  "Rounds down.  Returns a long if possible, otherwise a double."
  [^double x] (maybe-long-range (Math/floor x)))

(defn ceil
  "Rounds up.  Returns a long if possible, otherwise a double."
  [^double x] (maybe-long-range (Math/ceil x)))

(defn roughly-floor
  "Rounds down unless within accu, then rounds up.
Returns a long if possible, otherwise a double."
  [^double x ^double accu]
  {:pre [(have? non-? accu)]}
  (floor (+ x accu)))

(defn roughly-ceil
  "Rounds up unless within accu, then rounds down.
Returns a long if possible, otherwise a double."
  [^double x ^double accu]
  {:pre [(have? non-? accu)]}
  (ceil (- x accu)))

(defn roughly?
  "Returns true if x1 and x2 are within accu of each other, or within double
   accuracy."
  [^double x1 ^double x2 ^double accu]
  {:pre [(have? non-? accu)]}
  (cond (or (nan? accu) (nan? x1) (nan? x2)) false
        (inf+? accu) true
        (or (inf? x1) (inf? x2)) false
        :else (<= (abs (- x1 x2)) accu)))

(defn roughly-round?
  "Returns true if x is equal to a whole number or within accu of a whole 
   number, or within double accuracy."
  [^double x ^double accu]
  {:pre [(have? non-? accu)]}
  (cond (or (nan? accu) (nan? x)) false
        (inf+? accu) true
        (inf? x) false
        :else (<= (abs (- (round x) x)) accu)))

(defn roughly-round-non-?
  "Returns true if x is non- and roughly a whole number, 
   or within double accuracy."
  [^double x ^double accu]
  {:pre [(have? non-? accu)]}
  (and (non-? x) (roughly-round? x accu)))

(defn roughly-round-non+?
  "Returns true if x is non+ and roughly a whole number, 
   or within double accuracy."
  [^double x ^double accu]
  {:pre [(have? non-? accu)]}
  (and (non+? x) (roughly-round? x accu)))

(defn roughly-round+?
  "Returns true if x is positive and roughly a whole number, 
   or within double accuracy."
  [^double x ^double accu]
  {:pre [(have? non-? accu)]}
  (and (pos? x) (roughly-round? x accu)))

(defn roughly-round-?
  "Returns true if x is negative and roughly a whole number, 
   or within double accuracy."
  [^double x ^double accu]
  {:pre [(have? non-? accu)]}
  (and (neg? x) (roughly-round? x accu)))

(defn roughly-non-?
  "Returns true if x is positive or within accu to zero."
  [^double x ^double accu]
  {:pre [(have? non-? accu)]}
  (>= x (- accu)))

(defn roughly-non+?
  "Returns true if x is negative or within accu to zero."
  [^double x ^double accu]
  {:pre [(have? non-? accu)]}
  (<= x accu))

(defn roughly-prob?
  "Returns true if x is a prob or within accu of a prob."
  [^double x ^double accu]
  {:pre [(have? non-? accu)]}
  (and (>= x (- accu)) (<= x (inc accu))))

(defn roughly-corr?
  "Returns true if x is a corr or within accu of a corr."
  [^double x ^double accu]
  {:pre [(have? non-? accu)]}
  (and (>= x (dec (- accu))) (<= x (inc accu))))

;;;QUOTIENTS
(defn quot'
  "Quotient of dividing numerator by denominator.  
Returns a long if possible."
  [num div]
  (cond (or (nan? num) (nan? div) (and (inf? num) (inf? div))) nan
        (inf? num) (/ num div)
        (inf? div) 0
        :else (maybe-long-range (quot num div))))

(defn mod'
  "Modulus of num and div. Truncates toward negative infinity.  
Has sign of div.  Returns a long if possible."
  [num div]
  (cond (or (nan? div) (nan? num) (inf? num) (zero? div)) nan
        (inf? div) (maybe-long-able (* (sgn div) (abs num)))
        :else (maybe-long-able (mod num div))))

(defn rem'
  "Remainder of dividing numerator by denominator. 
Has sign of num.  Returns a long if possible."
  [num div]
  (cond (or (nan? div) (nan? num) (inf? num) (zero? div)) nan
        (inf? div) (maybe-long-able num)
        :else (maybe-long-able (rem num div))))

(defn quot-and-rem
  "Returns a tuple of longs if possible.  
Quotient of dividing numerator by denominator.
Remainder of dividing numerator by denominator.  Has sign of num."
  [num div] [(quot' num div) (rem' num div)])

(defn quot-and-mod
  "Returns a tuple of longs if possible.
Quotient of dividing numerator by denominator.
Modulus of num and div. Truncates toward negative infinity.  Has sign of div."
  [num div]
  (let [q (quot' num div), m (mod' num div),
        q (if (and (not (zero? num)) (= (sgn num) (- (sgn m)))) (dec q) q)]
    [q m]))

;;;ANGLES
(defn reduce-angle
  "Returns an angle between 0 and 360.  Returns a long if possible."
  [angle] (mod' angle 360))

(defn reduce-radians
  "Returns radians between 0 and 2 * PI.  Returns a long if possible."
  [radians] (mod' radians two-pi))

(defn radians->angle
  "Returns the reduced angle from radians, where angles = 180 * radians / PI.
Returns a long if possible."
  [^double radians]
  (if (inf? radians) radians (reduce-angle (Math/toDegrees radians))))

(defn angle->radians
  "Returns the reduced radians from the angle, 
   where radians = angle * PI / 180.
Returns a long if possible."
  [angle]
  (if (inf? angle) angle (maybe-long-able (Math/toRadians (reduce-angle angle)))))