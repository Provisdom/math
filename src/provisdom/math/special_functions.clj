(ns provisdom.math.special-functions
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.math.core :as m])
  (:import
    [org.apache.commons.math3.special Gamma Beta Erf]))

;;;DECLARATIONS
(declare regularized-gamma-p erfc log-beta)

;;;CONSTANTS
(def ^:const euler-mascheroni-constant
  0.57721566490153286060651209008240243104215933593992M)

(def ^:const lanczos-coefficients
  "Lanczos Coefficients."
  [0.99999999999980993, 676.5203681218851, -1259.1392167224028,
   771.32342877765313, -176.61502916214059, 12.507343278686905,
   -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7])

(def ^:const exact-stirling-errors
  "Expansion error for values 0.0 through 15.0 by 0.5."
  [0.0, 0.1534264097200273452913848, 0.0810614667953272582196702,
   0.0548141210519176538961390, 0.0413406959554092940938221,
   0.03316287351993628748511048, 0.02767792568499833914878929,
   0.02374616365629749597132920, 0.02079067210376509311152277,
   0.01848845053267318523077934, 0.01664469118982119216319487,
   0.01513497322191737887351255, 0.01387612882307074799874573,
   0.01281046524292022692424986, 0.01189670994589177009505572,
   0.01110455975820691732662991, 0.010411265261972096497478567,
   0.009799416126158803298389475, 0.009255462182712732917728637,
   0.008768700134139385462952823, 0.008330563433362871256469318,
   0.007934114564314020547248100, 0.007573675487951840794972024,
   0.007244554301320383179543912, 0.006942840107209529865664152,
   0.006665247032707682442354394, 0.006408994188004207068439631,
   0.006171712263039457647532867, 0.005951370112758847735624416,
   0.005746216513010115682023589, 0.005554733551962801371038690])

(def ^:const rational-approximation-coefficients
  [[2.506628277459239e+00 -3.066479806614716e+01 1.383577518672690e+02
    -2.759285104469687e+02 2.209460984245205e+02 -3.969683028665376e+01]
   [1.0 -1.328068155288572e+01 6.680131188771972e+01 -1.556989798598866e+02
    1.615858368580409e+02 -5.447609879822406e+01]
   [2.938163982698783e+00 4.374664141464968e+00 -2.549732539343734e+00
    -2.400758277161838e+00 -3.223964580411365e-01 -7.784894002430293e-03]
   [1.0 3.754408661907416e+00 2.445134137142996e+00 3.224671290700398e-01
    7.784695709041462e-03]])

;;;ERROR FUNCTIONS
(defn erf
  "Returns the error function:
  2 / (sqrt PI) × integral[0, `x`] (e ^ - (t ^ 2) × dt)."
  [x]
  (let [x (double x)]
    (cond (zero? x) 0.0
          (m/inf+? x) 1.0
          (m/inf-? x) -1.0
          :else (* (m/sgn x)
                   (regularized-gamma-p m/half (m/sq x))))))

(s/fdef erf
        :args (s/cat :x ::m/num)
        :ret ::m/corr)

(defn erf-diff
  "Returns the difference between [[erf]](`x1`) and [[erf]](`x2`)."
  [x1 x2]
  (if (> x1 x2)
    (- (erf-diff x2 x1))
    (if (< x1 -0.4769362762044697)
      (if (neg? x2)
        (- (erfc (- x2)) (erfc (- x1)))
        (- (erf x2) (erf x1)))
      (if (and (> x2 0.4769362762044697) (pos? x1))
        (- (erfc x1) (erfc x2))
        (- (erf x2) (erf x1))))))

(s/fdef erf-diff
        :args (s/cat :x1 ::m/num :x2 ::m/num)
        :ret (s/double-in :min -2.0 :max 2.0))

(defn erf-derivative
  "Returns the derivative of the error function."
  [x]
  (* 2.0 m/inv-sqrt-pi (m/exp (- (m/sq x)))))

(s/fdef erf-derivative
        :args (s/cat :x ::m/num)
        :ret ::m/non-)

(defn erfc
  "Returns the complementary error function."
  [x]
  (m/one- (erf x)))

(s/fdef erfc
        :args (s/cat :x ::m/num)
        :ret (s/or :non-nan (s/double-in :min 0.0 :max 2.0)))

(defn inv-erf
  "Returns the inverse error function."
  [x]
  (cond (m/roughly? 1.0 x m/dbl-close) m/inf+
        (m/roughly? -1.0 x m/dbl-close) m/inf-
        (zero? x) 0.0
        :else (Erf/erfInv (double x))))

(s/fdef inv-erf
        :args (s/cat :x ::m/corr)
        :ret ::m/num)

(defn inv-erfc
  "Returns the inverse complementary error function."
  [x]
  (cond (m/roughly? x 0.0 m/dbl-close) m/inf+
        (m/roughly? x 2.0 m/dbl-close) m/inf-
        (m/one? x) 0.0
        :else (inv-erf (m/one- x))))

(s/fdef inv-erfc
        :args (s/cat :x (s/double-in :min 0.0 :max 2.0))
        :ret ::m/num)

(defn inv-cdf-standard-normal
  "Returns the standard Normal inverse cdf."
  [cumulative-prob]
  (cond (zero? cumulative-prob) m/inf-
        (m/one? cumulative-prob) m/inf+
        (== 0.5 cumulative-prob) 0.0
        :else (* m/sqrt-two (inv-erf (dec (* 2.0 cumulative-prob))))))

(s/fdef inv-cdf-standard-normal
        :args (s/cat :cumulative-prob ::m/prob)
        :ret ::m/num)

(defn cdf-standard-normal
  "Returns the standard Normal cdf."
  [x]
  (cond (m/inf+? x) 1.0
        (m/inf-? x) 0.0
        (zero? x) 0.5
        :else (* 0.5 (inc (erf (* x m/inv-sqrt-two))))))

(s/fdef cdf-standard-normal
        :args (s/cat :x ::m/num)
        :ret ::m/prob)

;GAMMA FUNCTIONS
(defn gamma
  "Returns the gamma function: integral[0, inf] (t ^ (`a`- 1) × e ^ -t × dt).
  Although gamma is defined for positive `a`, this function also allows for all
  non-roughly-round-non+ `a`."
  [a]
  (cond (> a 172) m/inf+
        (< a -170) 0.0
        :else (Gamma/gamma a)))

(s/fdef gamma
        :args (s/cat :a (s/or :pos ::m/pos
                              :non+ ::m/non-roughly-round-non+))
        :ret ::m/non-inf-)

(defn lower-gamma
  "Returns the lower incomplete gamma function: 
   integral[0, `x`] (t ^ (`a` - 1) × e ^ -t × dt)."
  [a x]
  (cond (zero? x) 0.0
        (m/one? a) (m/one- (m/exp (- x)))
        (> x 1.0e150) 1.0
        (m/inf+? x) (gamma a)
        :else (* (gamma a)
                 (min 1.0
                      (max 0.0
                           (Gamma/regularizedGammaP (double a) (double x)))))))

(s/fdef lower-gamma
        :args (s/cat :a ::m/pos
                     :x ::m/non-)
        :ret ::m/nan-or-non-)

(defn upper-gamma
  "Returns the upper incomplete gamma function: 
   integral[`x`, inf] (t ^ (`a` - 1) × e ^ -t × dt)."
  [a x]
  (cond (zero? x) (gamma a)
        (m/one? a) (m/exp (- x))
        (> x 1.0e150) 0.0
        :else (* (gamma a)
                 (min 1.0
                      (max 0.0
                           (Gamma/regularizedGammaQ (double a) (double x)))))))

(s/fdef upper-gamma
        :args (s/cat :a ::m/pos :x ::m/non-)
        :ret ::m/nan-or-non-)

(defn upper-gamma-derivative-x
  "Returns the upper gamma derivative `x`."
  [a x]
  (if (> x 1.0e150)
    0.0
    (let [v (* (m/exp (- x))
               (m/pow x (dec a))
               (/ (gamma a)))]
      (if (m/inf-? v) m/inf+ v))))

(s/fdef upper-gamma-derivative-x
        :args (s/cat :a ::m/pos :x ::m/non-)
        :ret ::m/nan-or-non-)

(defn regularized-gamma-p
  "Returns the regularized gamma function P(`a`, `x`) = 1 - Q(`a`, `x`). Equal
  to [[lower-gamma]] function (a, x) divided by [[gamma]] function (`a`)."
  [a x]
  (cond (m/nan? a) m/nan
        (zero? x) 0.0
        (> x 1.0e150) 1.0
        :else (min 1.0
                   (max 0.0
                        (Gamma/regularizedGammaP (double a) (double x))))))

(s/fdef regularized-gamma-p
        :args (s/cat :a ::m/pos :x ::m/non-)
        :ret ::m/nan-or-prob)

(defn regularized-gamma-q
  "Returns the regularized gamma function Q(`a`, `x`) = 1 - P(`a`, `x`). Equal
  to [[upper-gamma]] function (`a`, `x`) divided by [[gamma]] function (`a`)."
  [a x]
  (cond (or (m/nan? a) (m/nan? x)) m/nan
        (zero? x) 1.0
        (> x 1.0e150) 0.0
        :else (min 1.0
                   (max 0.0
                        (Gamma/regularizedGammaQ (double a) (double x))))))

(s/fdef regularized-gamma-q
        :args (s/cat :a ::m/pos :x ::m/non-)
        :ret ::m/nan-or-prob)

(defn log-gamma
  "Returns the log gamma of `a`."
  [a]
  (if (m/inf+? a)
    m/inf+
    (Gamma/logGamma (double a))))

(s/fdef log-gamma
        :args (s/cat :a ::m/pos)
        :ret ::m/non-inf-)

(defn log-gamma-derivative
  "Returns the derivative of the log gamma of `a`. `a` > -3e8 because
  it becomes slow. Taken from Apache. Could use a better algorithm."
  [a]
  (let [a (double a)]
    (if (m/roughly-round-non+? a m/sgl-close)
      m/inf-
      (loop [x a
             tot 0.0]
        (let [inv-x (/ x)]
          (if (or (m/nan? x) (m/inf? x))
            x
            (cond (and (pos? x) (<= x 1.0e-5)) (- tot 0.5772156649015329 inv-x)
                  (>= x 49.0) (let [inv2-x (m/pow x -2.0)]
                                (+ tot
                                   (m/log x)
                                   (* -0.5 inv-x)
                                   (* (- inv2-x)
                                      (+ (/ 12.0)
                                         (* inv2-x
                                            (- (/ 120.0) (/ inv2-x 252.0)))))))
                  :else (recur (inc x) (- tot inv-x)))))))))

(s/fdef log-gamma-derivative
        :args (s/cat :a (s/and ::m/num #(> % -3e8)))
        :ret ::m/num)

(def digamma log-gamma-derivative)

(defn gamma-derivative
  "Returns the derivative of the gamma of `a`. `a` > -3e8 because it becomes
  slow."
  [a]
  (* (gamma a) (log-gamma-derivative a)))

(s/fdef gamma-derivative
        :args (s/cat :a (s/or :pos ::m/pos
                              :non+ (s/and ::m/non-roughly-round-non+
                                           #(> % -3e8))))
        :ret ::m/number)

(defn trigamma
  "Returns the trigamma (2nd derivative of log-gamma) of `a`. Approximated for
  `a` < -1e7 because it becomes slow. Taken from Apache. Could use a better
  algorithm."
  [a]
  (let [a (double a)]
    (if (m/roughly-round-non+? a m/sgl-close)
      m/inf+
      (loop [x a
             tot 0.0]
        (let [inv2-x (m/pow x -2.0)]
          (cond (or (m/nan? x) (m/inf? x)) x
                (< x -1e7) (let [r (m/round (+ x 9e6) :toward-zero)]
                             (recur (- x r) (+ tot 1.1263618e-5))) ;approx
                :else (cond (and (pos? x) (<= x 1.0e-5)) (+ tot inv2-x)
                            (>= x 49.0) (let [inv-x (/ x)]
                                          (+ tot
                                             inv-x
                                             (/ inv2-x 2.0)
                                             (* inv2-x
                                                inv-x
                                                (- (/ 6.0)
                                                   (* inv2-x
                                                      (+ (/ 3) (/ inv2-x 42.0)))))))
                            :else (recur (inc x) (+ tot inv2-x)))))))))

(s/fdef trigamma
        :args (s/cat :a ::m/num)
        :ret ::m/num)

(defn multivariate-gamma
  "Returns the multivariate gamma of `a` with dimension `p`."
  [a p]
  (let [p (if (= p m/max-long)
            (double p)
            p)]
    (* (m/pow m/PI (* 0.25 p (dec p)))
       (apply * (map (fn [i]
                       (gamma (+ a (* 0.5 (m/one- i)))))
                     (range 1 (inc p)))))))

(s/fdef multivariate-gamma
        :args (s/and (s/cat :a ::m/num
                            :p (s/with-gen ::m/int-non-
                                           #(gen/large-integer* {:min 0 :max 20})))
                     (fn [{:keys [a p]}]
                       (and (< p 1e7)                       ;for speed
                            (not (and (m/roughly-round?
                                        (+ a (* 0.5 (m/one- p)))
                                        0.0)
                                      (m/non+? (+ a (* 0.5 (m/one- p))))))
                            (or (zero? p)
                                (not (and (m/roughly-round?
                                            (+ a (* 0.5 (m/one- (dec p))))
                                            0.0)
                                          (m/non+? (+ a (* 0.5 (m/one- (dec p)))))))))))
        :ret ::m/non-inf-)

(defn multivariate-log-gamma
  "Returns the multivariate log gamma of `a` with dimension `p`."
  [a p]
  (+ (* m/log-pi 0.25 p (dec p))
     (apply + (map (fn [i]
                     (log-gamma (+ a (* 0.5 (m/one- i)))))
                   (range 1 (inc p))))))

(s/fdef multivariate-log-gamma
        :args (s/and (s/cat :a ::m/pos
                            :p (s/with-gen ::m/int-non-
                                           #(gen/large-integer* {:min 0 :max 20})))
                     (fn [{:keys [a p]}]
                       (and (< p 1e7)                       ;speed
                            (or (m/nan? a) (> a (* 0.5 p))))))
        :ret ::m/non-inf-)

;;;BETA FUNCTIONS
(defn beta
  "Returns the beta of `x` and `y`:
  integral[0, 1] (t ^ (`x` - 1) × (1 - t) ^ (`y` - 1) * dt."
  [x y]
  (let [log-b (log-beta x y)]
    (when log-b
      (m/exp log-b))))

(s/fdef beta
        :args (s/cat :x ::m/pos :y ::m/pos)
        :ret ::m/nan-or-non-)

(defn log-beta
  "Returns the log-beta of `x` and `y`."
  [x y]
  (Beta/logBeta (double x) (double y)))

(s/fdef log-beta
        :args (s/cat :x ::m/pos :y ::m/pos)
        :ret ::m/nan-or-non-inf-)

(defn regularized-beta
  "Returns the regularized beta. Equal to incomplete beta function divided by
  beta function."
  [c x y]
  (if (zero? c)
    0.0
    (Beta/regularizedBeta c x y)))

(s/fdef regularized-beta
        :args (s/cat :c ::m/prob
                     :x (s/and ::m/finite+ #(< % 1E154))
                     :y (s/and ::m/finite+ #(< % 1E154)))
        :ret ::m/number)

(defn incomplete-beta
  "Returns the lower beta:
  integral[0, `c`] (t ^ (`x` - 1) × (1 - t) ^ (`y` - 1) × dt."
  [c x y]
  (if (zero? c)
    0.0
    (* (Beta/regularizedBeta c x y) (beta x y))))

(s/fdef incomplete-beta
        :args (s/cat :c ::m/prob
                     :x (s/and ::m/finite+ #(< % 1E154))
                     :y (s/and ::m/finite+ #(< % 1E154)))
        :ret ::m/nan-or-finite)