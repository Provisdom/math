(ns provisdom.math.complex
  "Complex number arithmetic over `{::re ::im}` maps.

  Each complex number is a map with finite double real and imaginary parts under `::re` and `::im`.
  Arithmetic functions return an anomaly when the result is not finite (overflow, NaN, division by
  zero), so callers can flow anomalies through with `provisdom.utility-belt.anomalies`.

  Polar conversions live alongside the rectangular constructor:
  `->polar` rebuilds a complex from magnitude/angle, while `magnitude` and `angle` extract those
  quantities.

  Examples:
    (mul i i)                  ;=> {::im 0.0 ::re -1.0}
    (exp {::im m/PI ::re 0.0}) ;=> {::im 1.22e-16 ::re -1.0}"
  (:refer-clojure :exclude [zero? abs])
  (:require
    [clojure.spec.alpha :as s]
    [provisdom.math.core :as m]
    [provisdom.utility-belt.anomalies :as anomalies]))

;;;CONSTANTS
(def zero
  "The complex number 0 + 0i."
  {::im 0.0 ::re 0.0})

(def one
  "The complex number 1 + 0i."
  {::im 0.0 ::re 1.0})

(def i
  "The imaginary unit 0 + 1i."
  {::im 1.0 ::re 0.0})

;;;SPECS
(s/def ::im ::m/finite)
(s/def ::re ::m/finite)
(s/def ::complex (s/keys :req [::im ::re]))

;;;HELPERS
(defn- finalize
  "Returns a complex map from finite re/im, or an anomaly if either is non-finite."
  [fn-var re im]
  (if (and (m/finite? re) (m/finite? im))
    {::im (double im) ::re (double re)}
    {::anomalies/category ::anomalies/no-solve
     ::anomalies/fn       fn-var
     ::anomalies/message  (str "Non-finite complex result: re=" re " im=" im)}))

(defn- parts
  "Returns the `[re im]` parts of complex `c` as doubles."
  [c]
  (let [{::keys [im re]} c]
    [(double re) (double im)]))

;;;PREDICATES
(defn complex?
  "Returns `true` if `x` is a complex number map with finite `::re` and `::im`."
  [x]
  (s/valid? ::complex x))

(s/fdef complex?
  :args (s/cat :x any?)
  :ret boolean?)

(defn zero?
  "Returns `true` if `c` is the complex zero (both `::re` and `::im` are 0)."
  [c]
  (let [[re im] (parts c)]
    (and (clojure.core/zero? re)
      (clojure.core/zero? im))))

(s/fdef zero?
  :args (s/cat :c ::complex)
  :ret boolean?)

;;;CONSTRUCTORS AND ACCESSORS
(defn ->complex
  "Builds a complex number from finite real `re` and imaginary `im` parts.

  Returns an anomaly if either input fails to round-trip as a finite double."
  [re im]
  (finalize (var ->complex) re im))

(s/fdef ->complex
  :args (s/cat :re ::m/finite :im ::m/finite)
  :ret (s/or :anomaly ::anomalies/anomaly
         :complex ::complex))

(defn real
  "Returns the real part of complex number `c`."
  [c]
  (::re c))

(s/fdef real
  :args (s/cat :c ::complex)
  :ret ::m/finite)

(defn imaginary
  "Returns the imaginary part of complex number `c`."
  [c]
  (::im c))

(s/fdef imaginary
  :args (s/cat :c ::complex)
  :ret ::m/finite)

(defn ->polar
  "Builds a complex number from polar form `mag` (magnitude) and `angle` in radians. Returns an
  anomaly when the resulting rectangular components are non-finite."
  [mag angle]
  (finalize (var ->polar)
    (* (double mag) (m/cos angle))
    (* (double mag) (m/sin angle))))

(s/fdef ->polar
  :args (s/cat :mag ::m/finite :angle ::m/finite)
  :ret (s/or :anomaly ::anomalies/anomaly
         :complex ::complex))

(defn ->rectangular
  "Alias for [[->polar]]: builds a complex from magnitude and angle in radians. Returns an anomaly
  if the rectangular components are non-finite."
  [mag angle]
  (finalize (var ->rectangular)
    (* (double mag) (m/cos angle))
    (* (double mag) (m/sin angle))))

(s/fdef ->rectangular
  :args (s/cat :mag ::m/finite :angle ::m/finite)
  :ret (s/or :anomaly ::anomalies/anomaly
         :complex ::complex))

;;;ARITHMETIC
(defn add
  "Returns `c1` plus `c2`. Anomaly on overflow."
  [c1 c2]
  (let [[re1 im1] (parts c1)
        [re2 im2] (parts c2)]
    (finalize (var add) (+ re1 re2) (+ im1 im2))))

(s/fdef add
  :args (s/cat :c1 ::complex :c2 ::complex)
  :ret (s/or :anomaly ::anomalies/anomaly
         :complex ::complex))

(defn sub
  "Returns `c1` minus `c2`. Anomaly on overflow."
  [c1 c2]
  (let [[re1 im1] (parts c1)
        [re2 im2] (parts c2)]
    (finalize (var sub) (- re1 re2) (- im1 im2))))

(s/fdef sub
  :args (s/cat :c1 ::complex :c2 ::complex)
  :ret (s/or :anomaly ::anomalies/anomaly
         :complex ::complex))

(defn mul
  "Returns `c1` times `c2`. Anomaly on overflow."
  [c1 c2]
  (let [[re1 im1] (parts c1)
        [re2 im2] (parts c2)]
    (finalize (var mul)
      (- (* re1 re2) (* im1 im2))
      (+ (* re1 im2) (* im1 re2)))))

(s/fdef mul
  :args (s/cat :c1 ::complex :c2 ::complex)
  :ret (s/or :anomaly ::anomalies/anomaly
         :complex ::complex))

(defn conjugate
  "Returns the complex conjugate of `c` (negates the imaginary part)."
  [c]
  (let [[re im] (parts c)]
    (finalize (var conjugate) re (- im))))

(s/fdef conjugate
  :args (s/cat :c ::complex)
  :ret (s/or :anomaly ::anomalies/anomaly
         :complex ::complex))

(defn div
  "Returns `c1` divided by `c2`. Returns an anomaly when `c2` is zero or when the result is
  non-finite."
  [c1 c2]
  (if (zero? c2)
    {::anomalies/category ::anomalies/no-solve
     ::anomalies/fn       (var div)
     ::anomalies/message  "Division by complex zero."}
    (let [[re1 im1] (parts c1)
          [re2 im2] (parts c2)
          ;; Smith's algorithm: avoids unnecessary overflow when |re2|,|im2| differ.
          [re im] (if (>= (m/abs re2) (m/abs im2))
                    (let [r (/ im2 re2)
                          den (+ re2 (* im2 r))]
                      [(/ (+ re1 (* im1 r)) den)
                       (/ (- im1 (* re1 r)) den)])
                    (let [r (/ re2 im2)
                          den (+ im2 (* re2 r))]
                      [(/ (+ (* re1 r) im1) den)
                       (/ (- (* im1 r) re1) den)]))]
      (finalize (var div) re im))))

(s/fdef div
  :args (s/cat :c1 ::complex :c2 ::complex)
  :ret (s/or :anomaly ::anomalies/anomaly
         :complex ::complex))

(defn abs
  "Returns the modulus (absolute value) of `c`: `sqrt(re^2 + im^2)`.

  Uses `m/hypot` for numerical stability against overflow. Returns an anomaly when the result is
  non-finite."
  [c]
  (let [[re im] (parts c)
        r (m/hypot re im)]
    (if (m/finite? r)
      (double r)
      {::anomalies/category ::anomalies/no-solve
       ::anomalies/fn       (var abs)
       ::anomalies/message  (str "Non-finite modulus: " r)})))

(s/fdef abs
  :args (s/cat :c ::complex)
  :ret (s/or :anomaly ::anomalies/anomaly
         :modulus ::m/finite-non-))

(defn magnitude
  "Alias for [[abs]]: returns the magnitude `sqrt(re^2 + im^2)`."
  [c]
  (let [[re im] (parts c)
        r (m/hypot re im)]
    (if (m/finite? r)
      (double r)
      {::anomalies/category ::anomalies/no-solve
       ::anomalies/fn       (var magnitude)
       ::anomalies/message  (str "Non-finite magnitude: " r)})))

(s/fdef magnitude
  :args (s/cat :c ::complex)
  :ret (s/or :anomaly ::anomalies/anomaly
         :modulus ::m/finite-non-))

(defn arg
  "Returns the argument (angle) of `c` in radians on the principal branch `(-pi, pi]`. Returns 0.0
  for the complex zero."
  [c]
  (let [[re im] (parts c)]
    (m/atan2 im re)))

(s/fdef arg
  :args (s/cat :c ::complex)
  :ret ::m/finite)

(defn angle
  "Alias for [[arg]]: returns the principal angle of `c` in radians."
  [c]
  (arg c))

(s/fdef angle
  :args (s/cat :c ::complex)
  :ret ::m/finite)

(defn exp
  "Returns `e^c` for complex `c`: `e're * (cos im + i sin im)`. Anomaly on overflow."
  [c]
  (let [[re im] (parts c)
        e (m/exp re)]
    (finalize (var exp)
      (* e (m/cos im))
      (* e (m/sin im)))))

(s/fdef exp
  :args (s/cat :c ::complex)
  :ret (s/or :anomaly ::anomalies/anomaly
         :complex ::complex))

(defn log
  "Returns the principal complex logarithm of `c`:
  `log|c| + i * arg(c)`. Returns an anomaly when `c` is zero or when `log|c|` is non-finite."
  [c]
  (if (zero? c)
    {::anomalies/category ::anomalies/no-solve
     ::anomalies/fn       (var log)
     ::anomalies/message  "log of complex zero is undefined."}
    (let [[re im] (parts c)
          mag (m/hypot re im)]
      (finalize (var log)
        (m/log mag)
        (m/atan2 im re)))))

(s/fdef log
  :args (s/cat :c ::complex)
  :ret (s/or :anomaly ::anomalies/anomaly
         :complex ::complex))
