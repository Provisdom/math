(ns provisdom.math.special-functions
  (:require [provisdom.utility-belt.core :as co]
            [provisdom.math [core :as m]
             [matrix :as mx]
             [apache :as ap]])
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
    -2.759285104469687e+02 2.209460984245205e+02 -3.969683028665376e+01 ]
   [1.0 -1.328068155288572e+01 6.680131188771972e+01 -1.556989798598866e+02 
    1.615858368580409e+02 -5.447609879822406e+01]
   [2.938163982698783e+00 4.374664141464968e+00 -2.549732539343734e+00 
    -2.400758277161838e+00 -3.223964580411365e-01 -7.784894002430293e-03 ]
   [1.0 3.754408661907416e+00 2.445134137142996e+00 3.224671290700398e-01 
    7.784695709041462e-03]])

;;;ERROR FUNCTIONS
(defn erf 
  "Returns the error function: 2 / (sqrt PI) * integral[0, x] (e^-(t*t) * dt)"
  (^double [^double x]
    (cond (zero? x) 0.0, (m/inf+? x) 1.0, (m/inf-? x) -1.0, 
          :else (* (m/sgn x) (->> x m/sq (regularized-gamma-p m/half)))))
  (^double [^double x1 ^double x2] (ap/erf x1 x2)))

(defn erf-derivative 
  "Returns the derivative of the error function"
  ^double [^double x] (* 2.0 m/inv-sqrt-pi (m/exp (- (m/sq x)))))

(defn erfc 
  "Returns the complementary error function"
  ^double [^double x] (m/rev (erf x)))

(defn inv-erf 
  "Returns the inverse error function"
  ^double [^double x] 
  (when-not (and (>= x -1) (<= x 1)) (m/exc-out-of-range x (var inv-erf)))
  (cond (m/roughly? 1.0 x m/*dbl-close*) m/inf+, 
        (m/roughly? -1.0 x m/*dbl-close*) m/inf-, 
        (zero? x) 0.0 :else (ap/erf-inv x)))

(defn inv-erfc 
  "Returns the inverse complementary error function"
  ^double [^double x]
  (when-not (and (m/non-? x) (<= x 2)) (m/exc-out-of-range x (var inv-erfc)))
  (cond (m/roughly? x 0.0 m/*dbl-close*) m/inf+, 
        (m/roughly? x 2.0 m/*dbl-close*) m/inf-, 
        (m/one? x) 0.0 :else (ap/erfc-inv x)))

(defn inv-cdf-standard-normal
  "Returns the standard Normal inverse cdf"
  [^double cumul-prob]
  (when-not (m/prob? cumul-prob) 
    (m/exc-out-of-range cumul-prob (var inv-cdf-standard-normal)))
  (cond (zero? cumul-prob) m/inf-, (m/one? cumul-prob) m/inf+, 
        (== 0.5 cumul-prob) 0.0 
        :else (* m/sqrt-two (inv-erf (dec (* 2 cumul-prob))))))

;GAMMA FUNCTIONS
(defn gamma
  "Returns the gamma function: integral[0, inf] (t^(x-1) * e^-t * dt).
Although gamma is defined for pos x, this function allows for all non-zero x."
  ^double [^double x] 
  (when (zero? x) (m/exc-out-of-range x (var gamma)))
  (Gamma/gamma x))

(defn lower-gamma 
  "Returns the lower incomplete gamma function: 
   integral[0, c] (t^(x-1) * e^-t * dt)"
  ^double [^double c ^double x]
  (when (neg? c) (m/exc- c (var lower-gamma)))
  (when (m/non+? x) (m/exc-non+ x (var lower-gamma)))
  (cond (zero? c) 0.0, (m/one? x) (m/rev (m/exp (- c))), 
        :else (* (gamma x) (ap/regularized-gamma-p c x))))

(defn upper-gamma
  "Returns the upper incomplete gamma function: 
   integral[c, inf] (t^(x-1) * e^-t * dt)"
  ^double [^double c ^double x]
  (when (neg? c) (m/exc- c (var upper-gamma)))
  (when (m/non+? x) (m/exc-non+ x (var upper-gamma)))
  (cond (zero? c) (gamma x), (m/one? x) (m/exp (- c)), 
        :else (* (gamma x) (ap/regularized-gamma-q c x))))

(defn upper-gamma-derivative-c 
  "Returns the upper gamma derivative c"
  ^double [^double c ^double x]
  (when (neg? c) (m/exc- c (var upper-gamma-derivative-c)))
  (when (m/non+? x) (m/exc-non+ x (var upper-gamma-derivative-c)))
  (->> x dec (m/pow c) - (* (m/exp (- c)))))

(defn regularized-gamma-p 
  "Returns the regularized gamma function P(c, x) = 1 - Q(c, x).  
Equal to lower incomplete gamma function divided by gamma function"
  ^double [^double c ^double x]
  (when (neg? c) (m/exc- c (var regularized-gamma-p)))
  (when (m/non+? x) (m/exc-non+ x (var regularized-gamma-p)))
  (if (zero? c) 0.0 (ap/regularized-gamma-p c x)))

(defn regularized-gamma-q 
  "Returns the regularized gamma function Q(c, x) = 1 - P(c, x).  
Equal to upper incomplete gamma function divided by gamma function"
  ^double [^double c ^double x]
  (when (neg? c) (m/exc- c (var regularized-gamma-q)))
  (when (m/non+? x) (m/exc-non+ x (var regularized-gamma-q)))
  (if(zero? c) 1.0 (ap/regularized-gamma-q c x)))

(defn log-gamma 
  "Returns the log gamma of x"
  ^double [^double x]
  (when (m/non+? x) (m/exc-non+ x (var log-gamma)))
  (ap/log-gamma x))

(defn log-gamma-derivative
  "Returns the derivative of the log gamma of x"
  ^double [^double x]
  (when (m/non+? x) (m/exc-non+ x (var log-gamma-derivative)))
  (ap/digamma x))

(defn digamma  
  "Equivalent to log-gamma-derivative" 
  ^double [^double x]
  (when (m/non+? x) (m/exc-non+ x (var digamma)))
  (log-gamma-derivative x))

(defn gamma-derivative 
  "Returns the derivative of the gamma of x"
  ^double [^double x]
  (when (m/non+? x) (m/exc-non+ x (var gamma-derivative)))
  (* (gamma x) (log-gamma-derivative x)))

(defn trigamma  
  ^double [^double x]
  (ap/trigamma x))

(defn multivariate-gamma
  "Returns the multivariate gamma of x with dimension p"
  ^double [^double x p]
  (when-not (and (m/roughly-round? p 0.0) (m/non-? p)) 
    (m/exc-out-of-range p (var multivariate-gamma)))
  (* (m/pow m/PI (* 0.25 p (dec p))) 
     (mx/eproduct (fn [i] (gamma (+ x (* 0.5 (m/rev i))))) (range 1 (inc p)))))

(defn multivariate-log-gamma
  "Returns the multivariate log gamma of x with dimension p"
  ^double [^double x p]
  (when-not (and (m/roughly-round? p 0.0) (m/non-? p)) 
    (m/exc-out-of-range p (var multivariate-log-gamma)))
   (+ (* m/log-pi 0.25 p (dec p)) 
      (mx/esum (fn [i] (log-gamma (+ x (* 0.5 (m/rev i))))) 
               (range 1 (inc p)))))

;;;BETA FUNCTIONS
(defn beta 
  "Returns the beta of x and y: integral[0, 1] (t^(x-1) * (1-t)^(y-1) * dt
Although beta is defined for pos x and y, this function allows for all 
   non-zero x and y."
  ^double [^double x ^double y]
  (when-not (and (not (zero? x)) (not (zero? y)))
    (co/exc-ill-arg (var beta)))
  (Gamma/beta x y))

(defn log-beta 
  "Returns the log-beta of x and y"
  ^double [^double x ^double y]
  (when (m/non+? x) (m/exc-non+ x (var log-beta)))
  (when (m/non+? y) (m/exc-non+ y (var log-beta)))
  (ap/log-beta x y))

(defn regularized-beta 
  "Returns the regularized beta.  
Equal to incomplete beta function divided by beta function"
  ^double [^double c ^double x ^double y]
  (when-not (and (pos? x) (pos? y) (m/prob? c))
    (co/exc-ill-arg (var regularized-beta)))
  (if (zero? c) 0.0 (Gamma/incompleteBeta x y c))) ;cern misnamed this

(defn incomplete-beta 
  "Returns the lower beta: integral[0, c] (t^(x-1) * (1-t)^(y-1) * dt"
  ^double [^double c ^double x ^double y]
  (when-not (and (pos? x) (pos? y) (m/prob? c))
    (co/exc-ill-arg (var incomplete-beta)))
  ;;cern misnamed this
  (if (zero? c) 0.0 (* (Gamma/incompleteBeta x y c) (Gamma/beta x y)))) 