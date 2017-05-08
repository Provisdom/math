(ns provisdom.math.special-functions
  (:require [clojure.spec :as s]
            [clojure.spec.test :as st]
            [clojure.spec.gen :as gen]
            [provisdom.utility-belt.core :as co]
            [provisdom.math [core :as m]
             [matrix :as mx]
             [apache :as ap]]
            [taoensso.truss :as truss :refer (have have! have?)])
  (:import [cern.jet.stat.tdouble Gamma]))

(set! *warn-on-reflection* true)

;;;DECLARATIONS
(declare regularized-gamma-p)

;;;CONSTANTS
(def ^:const euler-mascheroni-constant
  0.57721566490153286060651209008240243104215933593992M)

(def ^:const lanczos-coefficients
  "lanczos coefficients"
  [0.99999999999980993, 676.5203681218851, -1259.1392167224028,
   771.32342877765313, -176.61502916214059, 12.507343278686905,
   -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7])

(def ^:const exact-stirling-errors
  "expansion error for values 0.0 through 15.0 by 0.5"
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

;;;SPECS
(s/def ::num-or-inf (s/double-in :NaN? false))
(s/def ::nan m/nan?)
(s/def ::nan-or-non-                                        ;;note that spec wasn't working with both :NaN? and :min.
  (s/or :non-nan (s/double-in :infinite? false :min 0.0) :nan ::nan))
(s/def ::corr (s/double-in :infinite? false :NaN? false :min -1.0 :max 1.0))
(s/def ::nan-or-corr (s/or :corr ::corr :nan ::nan))
(s/def ::num (s/double-in :infinite? false :NaN? false))
(s/def ::num-non- (s/double-in :infinite? false :NaN? false :min 0.0))
(s/def ::num+ (s/double-in :infinite? false :NaN? false :min (m/next-after 0.0 1.0)))
(s/def ::prob (s/double-in :infinite? false :NaN? false :min 0.0 :max 1.0))
(s/def ::nan-or-prob (s/or :prob ::prob :nan ::nan))


;;;ERROR FUNCTIONS
(defn erf
  "Returns the error function: 2 / (sqrt PI) * integral[0, x] (e^-(t*t) * dt)"
  (^double [^double x]
   (cond (zero? x) 0.0
         (m/inf+? x) 1.0
         (m/inf-? x) -1.0
         :else (* (m/sgn x) (->> x m/sq (regularized-gamma-p m/half)))))
  (^double [^double x1 ^double x2] (ap/erf x1 x2)))

(s/fdef erf
        :args (s/cat :x ::num-or-inf)
        :ret ::nan-or-corr)

(defn erf-derivative
  "Returns the derivative of the error function"
  ^double [^double x] (* 2.0 m/inv-sqrt-pi (m/exp (- (m/sq x)))))

(defn erfc
  "Returns the complementary error function"
  ^double [^double x] (m/one- (erf x)))

(defn inv-erf
  "Returns the inverse error function"
  ^double [^double x]
  {:pre [(have? #(>= % -1) x) (have? #(<= % 1) x)]}
  (cond (m/roughly? 1.0 x m/*dbl-close*) m/inf+
        (m/roughly? -1.0 x m/*dbl-close*) m/inf-
        (zero? x) 0.0
        :else (ap/erf-inv x)))

(s/fdef inv-erf
        :args (s/cat :x ::corr)
        :ret ::num-or-inf)

(defn inv-erfc
  "Returns the inverse complementary error function"
  ^double [^double x]
  {:pre [(have? m/non-? x) (have? #(<= % 2) x)]}
  (cond (m/roughly? x 0.0 m/*dbl-close*) m/inf+
        (m/roughly? x 2.0 m/*dbl-close*) m/inf-
        (m/one? x) 0.0 :else (ap/erfc-inv x)))

(defn inv-cdf-standard-normal
  "Returns the standard Normal inverse cdf"
  [^double cumul-prob]
  {:pre [(have? m/prob? cumul-prob)]}
  (cond (zero? cumul-prob) m/inf-
        (m/one? cumul-prob) m/inf+
        (== 0.5 cumul-prob) 0.0
        :else (* m/sqrt-two (inv-erf (dec (* 2 cumul-prob))))))

(s/fdef inv-cdf-standard-normal
        :args (s/cat :cumul-prob ::prob)
        :ret ::num-or-inf)

(defn cdf-standard-normal
  "Returns the standard Normal cdf"
  [^double x]
  (cond (m/inf+? x) 1.0
        (m/inf-? x) 0.0
        (zero? x) 0.5
        :else (* 0.5 (inc (erf (* x m/inv-sqrt-two))))))

(s/fdef cdf-standard-normal
        :args (s/cat :x ::num-or-inf)
        :ret ::nan-or-prob)

;GAMMA FUNCTIONS
(defn gamma
  "Returns the gamma function: integral[0, inf] (t^(a-1) * e^-t * dt).
Although gamma is defined for pos a, this function allows for all non-long-able-non+ a."
  ^double [^double a]
  {:pre [(have? (complement m/long-able-non+?) a)]}
  (cond (> a 709.7) m/inf+
        (< a -709.7) 0.0
        :else (Gamma/gamma a)))

(defn lower-gamma
  "Returns the lower incomplete gamma function: 
   integral[0, x] (t^(a-1) * e^-t * dt)"
  ^double [^double a ^double x]
  {:pre [(have? m/non-? x) (have? pos? a)]}
  (cond (zero? x) 0.0
        (m/one? a) (m/one- (m/exp (- x)))
        (< a -709.7) 0.0
        :else (* (gamma a) (ap/regularized-gamma-p a x))))

(defn upper-gamma
  "Returns the upper incomplete gamma function: 
   integral[x, inf] (t^(a-1) * e^-t * dt)"
  ^double [^double a ^double x]
  {:pre [(have? m/non-? x) (have? pos? a)]}
  (cond (zero? x) (gamma a)
        (m/one? a) (m/exp (- x))
        (< a -709.7) 1.0
        :else (* (gamma a) (ap/regularized-gamma-q a x))))

(defn upper-gamma-derivative-x
  "Returns the upper gamma derivative x"
  ^double [^double a ^double x]
  {:pre [(have? m/non-? x) (have? pos? a)]}
  (->> a dec (m/pow a) - (* (m/exp (- x)))))

(defn regularized-gamma-p
  "Returns the regularized gamma function P(a, x) = 1 - Q(a, x).
Equal to lower incomplete gamma function (a, x) divided by gamma function (a)"
  ^double [^double a ^double x]
  {:pre [(have? m/non-? x) (have? pos? a)]}
  (cond (zero? x) 0.0
        (> x 1.0e150) 1.0
        :else (ap/regularized-gamma-p a x)))

(s/fdef regularized-gamma-p
        :args (s/cat :a ::num+ :x ::num-non-)
        :ret ::nan-or-non-)

(defn regularized-gamma-q
  "Returns the regularized gamma function Q(a, x) = 1 - P(a, x).
Equal to upper incomplete gamma function (a, x) divided by gamma function (a)"
  ^double [^double a ^double x]
  {:pre [(have? m/non-? x) (have? pos? a)]}
  (if (zero? x) 1.0 (ap/regularized-gamma-q a x)))

(defn log-gamma
  "Returns the log gamma of a"
  ^double [^double a]
  {:pre [(have? pos? a)]}
  (ap/log-gamma a))

(defn log-gamma-derivative
  "Returns the derivative of the log gamma of a"
  ^double [^double a]
  {:pre [(have? pos? a)]}
  (ap/digamma a))

(defn digamma
  "Equivalent to log-gamma-derivative"
  ^double [^double a]
  {:pre [(have? pos? a)]}
  (log-gamma-derivative a))

(defn gamma-derivative
  "Returns the derivative of the gamma of a"
  ^double [^double a]
  {:pre [(have? pos? a)]}
  (* (gamma a) (log-gamma-derivative a)))

(defn trigamma
  ^double [^double a]
  (ap/trigamma a))

(defn multivariate-gamma
  "Returns the multivariate gamma of a with dimension p"
  ^double [^double a p]
  {:pre [(have? #(m/roughly-round? % 0.0) p) (have? m/non-? p)]}
  (* (m/pow m/PI (* 0.25 p (dec p)))
     (mx/eproduct (fn [i] (gamma (+ a (* 0.5 (m/one- i))))) (range 1 (inc p)))))

(defn multivariate-log-gamma
  "Returns the multivariate log gamma of a with dimension p"
  ^double [^double a p]
  {:pre [(have? #(m/roughly-round? % 0.0) p) (have? m/non-? p)]}
  (+ (* m/log-pi 0.25 p (dec p))
     (mx/esum (fn [i] (log-gamma (+ a (* 0.5 (m/one- i)))))
              (range 1 (inc p)))))

;;;BETA FUNCTIONS
(defn beta
  "Returns the beta of x and y: integral[0, 1] (t^(x-1) * (1-t)^(y-1) * dt
Although beta is defined for pos x and y, this function allows for all 
   non-zero x and y."
  ^double [^double x ^double y]
  {:pre [(have? (complement zero?) x y)]}
  (Gamma/beta x y))

(defn log-beta
  "Returns the log-beta of x and y"
  ^double [^double x ^double y]
  {:pre [(have? pos? x y)]}
  (ap/log-beta x y))

(defn regularized-beta
  "Returns the regularized beta.  
Equal to incomplete beta function divided by beta function"
  ^double [^double c ^double x ^double y]
  {:pre [(have? pos? x y) (have? m/prob? c)]}
  (if (zero? c) 0.0 (Gamma/incompleteBeta c x y)))          ;cern misnamed this

(defn incomplete-beta
  "Returns the lower beta: integral[0, c] (t^(x-1) * (1-t)^(y-1) * dt"
  ^double [^double c ^double x ^double y]
  {:pre [(have? pos? x y) (have? m/prob? c)]}
  ;;cern misnamed this
  (if (zero? c) 0.0 (* (Gamma/incompleteBeta c x y) (Gamma/beta x y))))