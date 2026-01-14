(ns provisdom.math.special-functions
  "Special mathematical functions for advanced numerical computation.

  Implements a comprehensive collection of special functions including:
  - Gamma functions (gamma, log-gamma, digamma, trigamma, multivariate)
  - Incomplete gamma functions (lower, upper, regularized P and Q)
  - Beta functions (beta, log-beta, incomplete, regularized)
  - Error functions (erf, erfc, inverse erf/erfc, derivatives)
  - Sigmoid functions (logistic, logit, probit, with derivatives)
  - Standard normal CDF and inverse CDF
  - Log-sum-exp for numerical stability
  - Bessel functions (J_v, Y_v, I_v, K_v for real orders)
  - Hypergeometric functions (confluent 1F1, Gaussian 2F1)

  Uses high-precision algorithms with series expansions, continued fractions,
  and asymptotic expansions as appropriate for different parameter ranges."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.math.core :as m]
    [provisdom.math.series :as series]
    [provisdom.utility-belt.anomalies :as anomalies]))

;;;DECLARATIONS
(declare erfc log-beta log-gamma log-gamma-inc regularized-gamma-p regularized-gamma-q)

;;;CONSTANTS
(def ^:const aperys-constant 1.202056903159594285399738161511449990764986292)

(def ^:const lanczos-coefficients
  "Lanczos Coefficients."
  [0.99999999999980993 676.5203681218851 -1259.1392167224028 771.32342877765313 -176.61502916214059
   12.507343278686905 -0.13857109526572012 9.9843695780195716e-6 1.5056327351493116e-7])

(def ^:const lanczos-coefficients2
  [0.9999999999999971 57.15623566586292 -59.59796035547549 14.136097974741746 -0.4919138160976202
   3.399464998481189E-5 4.652362892704858E-5 -9.837447530487956E-5 1.580887032249125E-4
   -2.1026444172410488E-4 2.1743961811521265E-4 -1.643181065367639E-4 8.441822398385275E-5
   -2.6190838401581408E-5 3.6899182659531625E-6])

(def ^:const exact-stirling-errors
  "Expansion error for values 0.0 through 15.0 by 0.5."
  [0.0 0.1534264097200273452913848 0.0810614667953272582196702 0.0548141210519176538961390
   0.0413406959554092940938221 0.03316287351993628748511048 0.02767792568499833914878929
   0.02374616365629749597132920 0.02079067210376509311152277 0.01848845053267318523077934
   0.01664469118982119216319487 0.01513497322191737887351255 0.01387612882307074799874573
   0.01281046524292022692424986 0.01189670994589177009505572 0.01110455975820691732662991
   0.010411265261972096497478567 0.009799416126158803298389475 0.009255462182712732917728637
   0.008768700134139385462952823 0.008330563433362871256469318 0.007934114564314020547248100
   0.007573675487951840794972024 0.007244554301320383179543912 0.006942840107209529865664152
   0.006665247032707682442354394 0.006408994188004207068439631 0.006171712263039457647532867
   0.005951370112758847735624416 0.005746216513010115682023589 0.005554733551962801371038690])

(def ^:const rational-approximation-coefficients
  [[2.506628277459239e+00 -3.066479806614716e+01 1.383577518672690e+02
    -2.759285104469687e+02 2.209460984245205e+02 -3.969683028665376e+01]
   [1.0 -1.328068155288572e+01 6.680131188771972e+01 -1.556989798598866e+02
    1.615858368580409e+02 -5.447609879822406e+01]
   [2.938163982698783e+00 4.374664141464968e+00 -2.549732539343734e+00
    -2.400758277161838e+00 -3.223964580411365e-01 -7.784894002430293e-03]
   [1.0 3.754408661907416e+00 2.445134137142996e+00 3.224671290700398e-01 7.784695709041462e-03]])

(def ^:private ^:const delta-coefficients
  [0.08333333333333333 -2.777777777777778E-5 7.936507936507937E-8 -5.952380952380953E-10
   8.417508417508329E-12 -1.917526917518546E-13 6.410256405103255E-15 -2.955065141253382E-16
   1.7964371635940225E-17 -1.3922896466162779E-18 1.338028550140209E-19 -1.542460098679661E-20
   1.9770199298095743E-21 -2.3406566479399704E-22 1.713480149663986E-23])

(def ^:private ^:const inv-erf-coeffs1
  [-3.64441206401782E-21 -1.6850591381820166E-19 1.28584807152564E-18 1.1157877678025181E-17
   -1.333171662854621E-16 2.0972767875968562E-17 6.637638134358324E-15 -4.054566272975207E-14
   -8.151934197605472E-14 2.6335093153082323E-12 -1.2975133253453532E-11 -5.415412054294628E-11
   1.0512122733215323E-9 -4.112633980346984E-9 -2.9070369957882005E-8 4.2347877827932404E-7
   -1.3654692000834679E-6 -1.3882523362786469E-5 1.8673420803405714E-4 -7.40702534166267E-4
   -0.006033670871430149 0.24015818242558962 1.6536545626831027])

(def ^:private ^:const inv-erf-coeffs2
  [2.2137376921775787E-9 9.075656193888539E-8 -2.7517406297064545E-7 1.8239629214389228E-8
   1.5027403968909828E-6 -4.013867526981546E-6 2.9234449089955446E-6 1.2475304481671779E-5
   -4.7318229009055734E-5 6.828485145957318E-5 2.4031110387097894E-5 -3.550375203628475E-4
   9.532893797373805E-4 -0.0016882755560235047 0.002491442096107851 -0.003751208507569241
   0.005370914553590064 1.0052589676941592 3.0838856104922208])

(def ^:private ^:const inv-erf-coeffs3
  [-2.7109920616438573E-11 -2.555641816996525E-10 1.5076572693500548E-9 -3.789465440126737E-9
   7.61570120807834E-9 -1.496002662714924E-8 2.914795345090108E-8 -6.771199775845234E-8
   2.2900482228026655E-7 -9.9298272942317E-7 4.526062597223154E-6 -1.968177810553167E-5
   7.599527703001776E-5 -2.1503011930044477E-4 -1.3871931833623122E-4 1.0103004648645344
   4.849906401408584])

(def ^:private ^:const inv-gamma1-pm1-b-coeffs
  [1.9575583661463974E-10 -6.077618957228252E-8 9.926418406727737E-7 -6.4304548177935305E-6
   -8.514194324403149E-6 4.939449793824468E-4 0.026620534842894922 0.203610414066807 1.0])

(def ^:private ^:const inv-gamma1-pm1-c-coeffs
  [1.133027231981696E-6 -1.2504934821426706E-6 -2.013485478078824E-5 1.280502823881162E-4
   -2.1524167411495098E-4 -0.0011651675918590652 0.0072189432466631 -0.009621971527876973
   -0.04219773455554433 0.16653861138229148 -0.04200263503409524 -0.6558780715202539
   -0.42278433509846713])

(def ^:private ^:const inv-gamma1-pm1-d-coeffs
  [4.343529937408594E-15 -1.2494415722763663E-13 1.5728330277104463E-12 4.686843322948848E-11
   6.820161668496171E-10 6.8716741130671986E-9 6.116095104481416E-9])

(def ^:private ^:const inv-gamma1-pm1-e-coeffs
  [2.6923694661863613E-4 0.004956830093825887 0.054642130860422966 0.3056961078365221 1.0])

(def ^:private ^:const inv-gamma1-pm1-f-coeffs
  [1.133027231981696E-6 -1.2504934821426706E-6 -2.013485478078824E-5 1.280502823881162E-4
   -2.1524167411495098E-4 -0.0011651675918590652 0.0072189432466631 -0.009621971527876973
   -0.04219773455554433 0.16653861138229148 -0.04200263503409524 -0.6558780715202539
   0.5772156649015329])

;;;LOG-SUM-EXP
(defn log-sum-exp
  "Computes log(sum(e^xi)) for sequence `numbers` in a numerically stable way.

  Avoids overflow/underflow when computing the log of sums of exponentials for very large or very
  small numbers. Uses the log-sum-exp trick.

  Examples:
    (log-sum-exp [1200 1210]) => 1210.0000453988991
    (log-sum-exp [-1200 -1210]) => -1199.9999546011009"
  [numbers]
  (if (empty? numbers)
    0.0
    (let [b (double (apply max numbers))]
      (if (> b 700.0)
        (+ b
          (m/log (reduce +
                   (map
                     (fn [val]
                       (m/exp (- val b)))
                     numbers))))
        (let [a (double (apply min numbers))]
          (if (< a -700.0)
            (+ a
              (m/log (reduce +
                       (map
                         (fn [val]
                           (m/exp (- val a)))
                         numbers))))
            (m/log (reduce + (map m/exp numbers)))))))))

(s/fdef log-sum-exp
  :args (s/cat :v ::m/numbers)
  :ret ::m/number)

;;;ERROR FUNCTIONS
(defn erf
  "Computes the error function erf(`x`).

  Defined as (2/sqrt(pi)) * integral(0 to x, e^(-t^2) dt). Represents the probability that a random
  variable from a standard normal distribution falls within [-`x`*sqrt(2), `x`*sqrt(2)].

  Properties:
  - erf(0) = 0
  - erf(inf) = 1
  - erf(-x) = -erf(x) (odd function)

  Examples:
    (erf 0.0) => 0.0
    (erf 1.0) => 0.842700792949715
    (erf -1.0) => -0.842700792949715"
  [x]
  (cond (zero? x) 0.0
    (> x 6.0) 1.0
    (< x -6.0) -1.0
    :else (* (m/sgn x)
            (regularized-gamma-p 0.5 (m/sq x)))))

(s/fdef erf
  :args (s/cat :x ::m/num)
  :ret ::m/corr)

(defn erf-diff
  "Computes erf(`x2`) - erf(`x1`) with improved numerical stability.

  Uses optimized algorithms to avoid loss of precision when computing the difference of two error
  function values.

  Examples:
    (erf-diff -1.0 1.0) => 1.68540158589943
    (erf-diff 1.0 2.0) => 0.1526214720692377"
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
  "Computes the derivative of the error function: d/d`x` erf(`x`).

  The derivative is (2/sqrt(pi)) * e^(-x^2), which is the standard normal probability density
  function scaled by sqrt(2*pi).

  Examples:
    (erf-derivative 0.0) => 1.1283791670955126 (2/sqrt(pi))
    (erf-derivative 1.0) => 0.41510749742502713"
  [x]
  (* 2.0 m/inv-sqrt-pi (m/exp (- (m/sq x)))))

(s/fdef erf-derivative
  :args (s/cat :x ::m/num)
  :ret ::m/non-)

(defn- erfc-asymptotic
  "Computes erfc(x) using asymptotic expansion for large x > 4.
  erfc(x) ~ exp(-x^2)/(x*sqrt(pi)) * [1 - 1/(2x^2) + 1*3/(2x^2)^2 - 1*3*5/(2x^2)^3 + ...]"
  [x]
  (let [x2 (m/sq x)
        inv-2x2 (/ (* 2.0 x2))
        ;; Coefficients are (-1)^n * (2n-1)!! where (2n-1)!! = 1, 1, 3, 15, 105, 945, ...
        ;; We sum until terms start growing (asymptotic series)
        prefactor (* (m/exp (- x2)) m/inv-sqrt-pi (/ x))]
    (loop [n 0
           term 1.0
           sum 1.0
           prev-term 2.0]                                   ;; larger than first term
      (if (or (> n 50) (> (m/abs term) (m/abs prev-term)))
        (* prefactor sum)
        (let [;; next term = prev-term * (-(2n+1)) / (2x²)
              next-term (* term (- (inc (* 2.0 n))) inv-2x2)]
          (recur (inc n) next-term (+ sum next-term) term))))))

(defn erfc
  "Computes the complementary error function erfc(`x`) = 1 - erf(`x`).

  Uses numerically stable algorithms:
  - Asymptotic expansion for large x (> 4) to avoid precision loss
  - Standard computation for moderate x where 1 - erf(x) is accurate
  - Symmetry relation for negative x

  Examples:
    (erfc 0.0) => 1.0
    (erfc 3.0) => 2.2090496998585441e-5
    (erfc 5.0) => 1.5374597944280349e-12
    (erfc 10.0) => 2.0884875837625447e-45"
  [x]
  (cond
    (zero? x) 1.0
    (m/inf+? x) 0.0
    (m/inf-? x) 2.0
    (neg? x) (- 2.0 (erfc (- x)))
    (> x 26.5) 0.0                                          ;; underflow to 0
    (> x 4.0) (erfc-asymptotic x)
    :else (m/one- (erf x))))

(s/fdef erfc
  :args (s/cat :x ::m/num)
  :ret (s/double-in :min 0.0 :max 2.0))

(defn- coeffs-sum
  [a coeffs]
  (reduce (fn [acc v]
            (+ v (* acc a)))
    (first coeffs)
    (rest coeffs)))

(defn inv-erf
  "Computes the inverse error function erf^(-1)(`x`).

  Finds y such that erf(y) = `x`. Uses rational approximations with different coefficient sets for
  different ranges.

  Domain: [-1, 1]
  Range: (-inf, inf)

  Examples:
    (inv-erf 0.0) => 0.0
    (inv-erf 0.8427) => ~1.0 (approximately)"
  [x]
  (cond (m/roughly? 1.0 x m/dbl-close) m/inf+
    (m/roughly? -1.0 x m/dbl-close) m/inf-
    (zero? x) 0.0
    :else
    (let [w (- (m/log (* (m/one- x) (inc x))))]
      (cond
        (m/inf+? w)
        m/inf+

        (< w 6.25)
        (let [w (- w 3.125)
              p (coeffs-sum w inv-erf-coeffs1)]
          (* p x))

        (< w 16.0)
        (let [w (- (m/sqrt w) 3.25)
              p (coeffs-sum w inv-erf-coeffs2)]
          (* p x))

        :else
        (let [w (- (m/sqrt w) 5.0)
              p (coeffs-sum w inv-erf-coeffs3)]
          (* p x))))))

(s/fdef inv-erf
  :args (s/cat :x ::m/corr)
  :ret ::m/num)

(defn inv-erfc
  "Computes the inverse complementary error function erfc^(-1)(`x`).

  Finds y such that erfc(y) = `x`. Equivalent to [[inv-erf]](1-`x`).

  Domain: [0, 2]
  Range: (-inf, inf)

  Examples:
    (inv-erfc 1.0) => 0.0
    (inv-erfc 0.0) => inf
    (inv-erfc 2.0) => -inf"
  [x]
  (cond (m/roughly? x 0.0 m/dbl-close) m/inf+
    (m/roughly? x 2.0 m/dbl-close) m/inf-
    (m/one? x) 0.0
    :else (inv-erf (m/one- x))))

(s/fdef inv-erfc
  :args (s/cat :x (s/double-in :min 0.0 :max 2.0))
  :ret ::m/num)

;;;SIGMOID FUNCTIONS
(defn inv-cdf-standard-normal
  "Computes the inverse cumulative distribution function of the standard normal.

  Finds the value x such that Phi(x) = `cumulative-prob`, where Phi is the standard normal CDF.
  Also known as the probit function or normal quantile function.

  Examples:
    (inv-cdf-standard-normal 0.5) => 0.0 (median)
    (inv-cdf-standard-normal 0.975) => ~1.96 (97.5th percentile)"
  [cumulative-prob]
  (cond (zero? cumulative-prob) m/inf-
    (m/one? cumulative-prob) m/inf+
    (== 0.5 cumulative-prob) 0.0
    :else (* m/sqrt-two (inv-erf (dec (* 2.0 cumulative-prob))))))

(s/fdef inv-cdf-standard-normal
  :args (s/cat :cumulative-prob ::m/prob)
  :ret ::m/num)

(defn cdf-standard-normal
  "Computes the cumulative distribution function of the standard normal.

  Calculates Phi(`x`) = P(Z <= `x`) where Z ~ N(0,1). Related to the error function by
  Phi(`x`) = (1 + erf(`x`/sqrt(2)))/2.

  Examples:
    (cdf-standard-normal 0.0) => 0.5
    (cdf-standard-normal 1.96) => ~0.975
    (cdf-standard-normal -1.96) => ~0.025"
  [x]
  (cond (m/inf+? x) 1.0
    (m/inf-? x) 0.0
    (zero? x) 0.5
    :else (* 0.5 (inc (erf (* x m/inv-sqrt-two))))))

(s/fdef cdf-standard-normal
  :args (s/cat :x ::m/num)
  :ret ::m/prob)

(def ^{:doc "See [[inv-cdf-standard-normal]]"} probit inv-cdf-standard-normal)

(def ^{:doc "See [[cdf-standard-normal]]"} inv-probit cdf-standard-normal)

(defn logistic
  "Computes the logistic (sigmoid) function: 1/(1 + e^(-`x`)).

  Maps real numbers to (0,1). Often used as an activation function in neural networks and in
  logistic regression.

  Properties:
  - logistic(0) = 0.5
  - logistic(-x) = 1 - logistic(x)
  - Derivative is logistic(x) * (1 - logistic(x))

  Examples:
    (logistic 0.0) => 0.5
    (logistic 2.0) => 0.8807970779778823
    (logistic -2.0) => 0.11920292202211755"
  [x]
  (+ 0.5 (* 0.5 (m/tanh (* 0.5 x)))))

(s/fdef logistic
  :args (s/cat :x ::m/num)
  :ret ::m/prob)

(defn logistic-derivative
  "Computes the derivative of the logistic function at `x`.

  The derivative is logistic(`x`) * (1 - logistic(`x`)), which has its maximum of 0.25 at `x` = 0.
  This is efficiently computed as 0.25 * (1 - tanh(`x`/2)^2).

  Examples:
    (logistic-derivative 0.0) => 0.25
    (logistic-derivative 2.0) => 0.10499358540350662"
  [x]
  (* 0.25 (m/one- (m/sq (m/tanh (* 0.5 x))))))

(s/fdef logistic-derivative
  :args (s/cat :x ::m/num)
  :ret ::m/prob)

(defn logit
  "Computes the logit function: ln(`p`/(1-`p`)).

  The inverse of the [[logistic]] function. Maps probabilities in (0,1) to real numbers. Used in
  logistic regression and odds ratios.

  Examples:
    (logit 0.5) => 0.0
    (logit 0.75) => 1.0986122886681098 (ln(3))
    (logit 0.25) => -1.0986122886681098"
  [p]
  (cond (zero? p) m/inf-
    (m/one? p) m/inf+
    :else (m/log (/ p (m/one- p)))))

(s/fdef logit
  :args (s/cat :p ::m/prob)
  :ret ::m/num)

(defn logit-derivative
  "Computes the derivative of the logit function: d/d`p` logit(`p`).

  The derivative is 1/(`p`(1-`p`)), which approaches infinity as `p` approaches 0 or 1.

  Examples:
    (logit-derivative 0.5) => 4.0
    (logit-derivative 0.25) => 5.333333333333333"
  [p]
  (if (or (zero? p) (m/one? p))
    m/inf+
    (+ (/ p) (/ (m/one- p)))))

(s/fdef logit-derivative
  :args (s/cat :p ::m/prob)
  :ret ::m/pos)

;GAMMA FUNCTIONS
(defn- lanczos2
  [x]
  (reduce (fn [acc i]
            (+ acc (/ (get lanczos-coefficients2 i)
                     (+ x (double i)))))
    (first lanczos-coefficients2)
    (range 1 (count lanczos-coefficients2))))

(defn- inv-gamma1-pm1
  [x]
  (if (or (< x -0.5) (> x 1.5))
    nil
    (let [t (if (<= x 0.5) x (dec x))]
      (if (neg? t)
        (let [a (+ 6.116095104481416E-9 (* t 6.247308301164655E-9))
              b (coeffs-sum t inv-gamma1-pm1-b-coeffs)
              c-first (+ -2.056338416977607E-7 (* t a (/ b)))
              c (coeffs-sum t (cons c-first inv-gamma1-pm1-c-coeffs))]
          (if (> x 0.5)
            (* t c (/ x))
            (* x (inc c))))
        (let [d (coeffs-sum t inv-gamma1-pm1-d-coeffs)
              e (coeffs-sum t inv-gamma1-pm1-e-coeffs)
              f-first (+ -2.056338416977607E-7 (* d t (/ e)))
              f (coeffs-sum t (cons f-first inv-gamma1-pm1-f-coeffs))]
          (if (> x 0.5)
            (* t (/ x) (dec f))
            (* x f)))))))

(defn gamma
  "Computes the gamma function Gamma(`a`).

  Defined as Gamma(`a`) = integral(0 to inf, t^(`a`-1) * e^(-t) dt) for `a` > 0.
  Extended to negative nonintegers using the reflection formula.

  Properties:
  - Gamma(n) = (n-1)! for positive integers n
  - Gamma(1/2) = sqrt(pi)
  - Gamma(a+1) = a * Gamma(a) (recurrence relation)

  Examples:
    (gamma 1.0) => 1.0 (0!)
    (gamma 2.0) => 1.0 (1!)
    (gamma 3.0) => 2.0 (2!)
    (gamma 0.5) => 1.7724538509055159 (sqrt(pi))"
  [a]
  (cond (>= a 141.8) (m/exp (log-gamma a))
    (<= a -141.8) 0.0
    :else
    (let [abs-x (m/abs a)]
      (if (<= abs-x 20.0)
        (if (>= a 1.0)
          (let [[prod t] (loop [t a
                                prod 1.0]
                           (if (> t 2.5)
                             (recur (dec t) (* (dec t) prod))
                             [prod t]))]
            (m/div prod (inc (inv-gamma1-pm1 (dec t)))))
          (let [[prod t] (loop [t a
                                prod a]
                           (if (< t 0.5)
                             (recur (inc t) (* (inc t) prod))
                             [prod t]))]
            (m/div (* prod (inc (inv-gamma1-pm1 t))))))
        (let [prod (+ 5.2421875 abs-x)
              t (* 2.5066282746310007
                  (/ abs-x)
                  (m/pow prod (+ 0.5 abs-x))
                  (m/exp (- prod))
                  (lanczos2 abs-x))]
          (if (pos? a)
            t
            (/ (- m/PI)
              (* a t (m/sin (* (- m/PI) a))))))))))


(s/fdef gamma
  :args (s/cat :a (s/or :pos ::m/pos
                    :non+ ::m/non-roughly-round-non+))
  :ret ::m/non-inf-)

(defn lower-gamma
  "Computes the lower incomplete gamma function gamma(`a`,`x`).

  Defined as gamma(`a`,`x`) = integral(0 to `x`, t^(`a`-1) * e^(-t) dt).
  Represents the \"tail\" of the gamma function from 0 to `x`.

  Related to the regularized gamma P function by gamma(`a`,`x`) = Gamma(`a`) * P(`a`,`x`).

  Examples:
    (lower-gamma 2.0 1.0) => 0.6321205588285577
    (lower-gamma 1.0 2.0) => 0.8646647167633873"
  [a x]
  (cond (zero? x) 0.0
    (m/one? a) (- (m/dec-exp (- x)))
    (m/inf+? x) (gamma a)
    :else (* (gamma a) (regularized-gamma-p a x))))

(s/fdef lower-gamma
  :args (s/cat :a ::m/pos :x ::m/non-)
  :ret ::m/nan-or-non-)

(defn upper-gamma
  "Computes the upper incomplete gamma function Gamma(`a`,`x`).

  Defined as Gamma(`a`,`x`) = integral(`x` to inf, t^(`a`-1) * e^(-t) dt).
  Represents the \"tail\" of the gamma function from `x` to infinity.

  Related to the regularized gamma Q function by Gamma(`a`,`x`) = Gamma(`a`) * Q(`a`,`x`).
  Satisfies gamma(`a`,`x`) + Gamma(`a`,`x`) = Gamma(`a`).

  Examples:
    (upper-gamma 2.0 1.0) => 0.36787944117144233
    (upper-gamma 1.0 2.0) => 0.1353352832366127"
  [a x]
  (cond (zero? x) (gamma a)
    (m/one? a) (m/exp (- x))
    :else (* (gamma a) (regularized-gamma-q a x))))

(s/fdef upper-gamma
  :args (s/cat :a ::m/pos :x ::m/non-)
  :ret ::m/nan-or-non-)

(defn upper-gamma-derivative-x
  "Computes `x`^(`a`-1) * e^(-`x`) / Gamma(`a`).

  This equals the gamma distribution PDF with shape `a` and rate 1. Also equals d/d`x` P(`a`,`x`)
  = -d/d`x` Q(`a`,`x`), where P and Q are the regularized lower and upper incomplete gamma
  functions.

  Examples:
    (upper-gamma-derivative-x 2.0 1.0) => 0.36787944117144233
    (upper-gamma-derivative-x 1.0 0.0) => 1.0"
  [a x]
  (let [v (* (m/exp (- x))
            (m/pow x (dec a))
            (/ (gamma a)))]
    (if (m/inf-? v) m/inf+ v)))

(s/fdef upper-gamma-derivative-x
  :args (s/cat :a ::m/pos :x ::m/non-)
  :ret ::m/nan-or-non-)

(defn regularized-gamma-p
  "Computes the regularized lower incomplete gamma function P(`a`,`x`).

  Defined as P(`a`,`x`) = gamma(`a`,`x`) / Gamma(`a`), where gamma is the lower incomplete gamma.
  Represents the cumulative distribution function of the gamma distribution.

  Properties:
  - P(a,0) = 0
  - P(a,inf) = 1
  - P(a,x) + Q(a,x) = 1

  Examples:
    (regularized-gamma-p 2.0 1.0) => 0.6321205588285577"
  [a x]
  (cond (zero? x) 0.0
    (>= x (inc a)) (m/one- (regularized-gamma-q a x))
    :else (let [an (/ 1.0 a)
                [n sum] (loop [n 0.0
                               an an
                               sum an]
                          (if (and (not (m/inf+? sum))
                                (> (m/abs (/ an sum)) 1e-14)
                                (< n m/max-int))
                            (let [an (* an x (/ (+ a (inc n))))]
                              (recur (inc n) an (+ an sum)))
                            [n sum]))]
            (cond (>= n m/max-int) m/nan
              (m/inf+? sum) 1.0
              :else (min 1.0
                      (* sum
                        (m/exp (+ (* a (m/log x))
                                 (- x)
                                 (- (log-gamma a))))))))))

(s/fdef regularized-gamma-p
  :args (s/cat :a ::m/pos :x ::m/non-)
  :ret ::m/nan-or-prob)

(defn regularized-gamma-q
  "Computes the regularized upper incomplete gamma function Q(`a`,`x`).

  Defined as Q(`a`,`x`) = Gamma(`a`,`x`) / Gamma(`a`), where Gamma is the upper incomplete gamma.
  Represents the survival function (1 - CDF) of the gamma distribution.

  Properties:
  - Q(a,0) = 1
  - Q(a,inf) = 0
  - P(a,x) + Q(a,x) = 1

  Examples:
    (regularized-gamma-q 2.0 1.0) => 0.36787944117144233"
  [a x]
  (cond (zero? x) 1.0
    (< x (inc (double a))) (m/one- (regularized-gamma-p a x))
    :else (let [a-term-series (map
                                (fn [n]
                                  (+ (* 2.0 n) (- a) 1.0 x))
                                (range))
                b-term-series (map
                                (fn [n]
                                  (* n (- a (double n))))
                                (drop 1 (range)))
                gcf (series/multiplicative-generalized-continued-fraction
                      a-term-series b-term-series)
                sum (series/multiplicative-sum-convergent-series gcf)]
            (if (anomalies/anomaly? sum)
              m/nan
              (min 1.0
                (/ (m/exp (+ (* a (m/log x))
                            (- x)
                            (- (log-gamma a))))
                  sum))))))

(s/fdef regularized-gamma-q
  :args (s/cat :a ::m/pos :x ::m/non-)
  :ret ::m/nan-or-prob)

(defn log-gamma
  "Computes the natural logarithm of the gamma function ln(Gamma(`a`)).

  More numerically stable than computing log([[gamma]](`a`)) directly, especially for large values
  of `a` where [[gamma]](`a`) would overflow.

  Examples:
    (log-gamma 1.0) => 0.0 (ln(1))
    (log-gamma 2.0) => 0.0 (ln(1))
    (log-gamma 10.0) => 12.801827480081469 (ln(9!))"
  [a]
  (cond (m/inf+? a) m/inf+
    (< a 0.5) (- (log-gamma-inc a) (m/log a))
    (<= a 2.5) (log-gamma-inc (dec a))
    (<= a 8.0) (let [n (m/floor' (- a 1.5))
                     prod (reduce (fn [acc i]
                                    (* acc (- a (double i))))
                            1.0
                            (range 1 (inc n)))]
                 (+ (log-gamma-inc (- a (double (inc n)))) (m/log prod)))
    :else (let [sum (lanczos2 a)
                tmp (+ a 5.2421875)]
            (+ (* (+ a 0.5) (m/log tmp))
              (- tmp)
              m/half-log-two-pi
              (m/log (/ sum a))))))

(s/fdef log-gamma
  :args (s/cat :a ::m/pos)
  :ret ::m/non-inf-)

(defn log-gamma-inc
  "Computes ln(Gamma(1+`a`)) with improved accuracy for small `a`.

  More numerically stable than computing [[log-gamma]](1+`a`) directly when `a` is close to zero,
  avoiding cancellation errors.

  Examples:
    (log-gamma-inc 0.0) => 0.0 (ln(Gamma(1)))
    (log-gamma-inc 0.1) => -0.04987244125983972"
  [a]
  (if (or (< a -0.5) (> a 1.5))
    (log-gamma (inc a))
    (- (m/log-inc (inv-gamma1-pm1 a)))))

(s/fdef log-gamma-inc
  :args (s/cat :a (s/or :finite (m/finite-spec {:min (m/next-up -1.0)})
                    :inf+ (s/with-gen m/inf+? #(gen/return m/inf+))))
  :ret ::m/non-inf-)

(defn log-gamma-derivative
  "Computes the derivative of ln(Gamma(`a`)), also known as the digamma function psi(`a`).

  The digamma function is d/d`a` ln(Gamma(`a`)) = Gamma'(`a`)/Gamma(`a`).

  Properties:
  - psi(1) = -gamma (negative Euler-Mascheroni constant)
  - psi(n) = -gamma + sum(1/k) for k=1 to n-1 (positive integers)

  Examples:
    (log-gamma-derivative 1.0) => -0.5772156649015329 (-gamma)
    (log-gamma-derivative 2.0) => 0.42278433509846713"
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
  "Computes the derivative of the gamma function Gamma'(`a`).

  The derivative is Gamma'(`a`) = Gamma(`a`) * psi(`a`), where psi is the digamma function.

  Examples:
    (gamma-derivative 1.0) => -0.5772156649015329
    (gamma-derivative 2.0) => 0.42278433509846713"
  [a]
  (* (gamma a) (log-gamma-derivative a)))

(s/fdef gamma-derivative
  :args (s/cat :a (s/or :pos ::m/pos
                    :non+ (s/and ::m/non-roughly-round-non+
                            #(> % -3e8))))
  :ret ::m/number)

(defn trigamma
  "Computes the trigamma function psi'(`a`), the second derivative of ln(Gamma(`a`)).

  The trigamma function is d^2/d`a`^2 ln(Gamma(`a`)) = d/d`a` psi(`a`).
  Related to the variance of certain probability distributions.

  Properties:
  - psi'(1) = pi^2/6 (related to Apery's constant)
  - psi'(n) = pi^2/6 - sum(1/k^2) for k=1 to n-1

  Examples:
    (trigamma 1.0) => 1.6449340668482264 (pi^2/6)"
  [a]
  (let [a (double a)]
    (if (m/roughly-round-non+? a m/sgl-close)
      m/inf+
      (loop [x a
             tot 0.0]
        (let [inv2-x (m/pow x -2.0)]
          (cond (or (m/nan? x) (m/inf? x)) x
            (< x -1.0e7) (let [r (m/round' (+ x 9.0e6) :toward-zero)]
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
                                          (+ (/ 3.0)
                                            (/ inv2-x 42.0)))))))
                    :else (recur (inc x) (+ tot inv2-x)))))))))

(s/fdef trigamma
  :args (s/cat :a ::m/num)
  :ret ::m/num)

(defn multivariate-gamma
  "Computes the multivariate gamma function Gamma_`p`(`a`).

  Defined as Gamma_p(a) = pi^(p(p-1)/4) * product(j=1 to p, Gamma(a + (1-j)/2)).
  Used in multivariate statistics, particularly with Wishart distributions.

  Examples:
    (multivariate-gamma 2.0 1) => 1.0 (same as univariate gamma)
    (multivariate-gamma 2.0 2) => 0.8862269254527579"
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
            (and (< p 1e7)                                  ;for speed
              (not (and (m/roughly-round?
                          (+ a (* 0.5 (m/one- p)))
                          0.0)
                     (m/non+? (+ a (* 0.5 (m/one- p))))))
              (or (zero? p)
                (not (and (m/roughly-round?
                            (+ a (* 0.5 (m/one- (dec p))))
                            0.0)
                       (m/non+?
                         (+ a (* 0.5 (m/one- (dec p)))))))))))
  :ret ::m/non-inf-)

(defn multivariate-log-gamma
  "Computes the natural logarithm of the multivariate gamma function.

  More numerically stable than computing log([[multivariate-gamma]](`a`, `p`)) directly, especially
  for large values.

  Examples:
    (multivariate-log-gamma 2.0 1) => 0.0
    (multivariate-log-gamma 2.0 2) => -0.12078223763524518"
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
            (and (< p 1.0e7)                                ;speed
              (or (m/nan? a) (> a (* 0.5 p))))))
  :ret ::m/non-inf-)

;;;BETA FUNCTIONS
(defn beta
  "Computes the beta function B(`x`,`y`).

  Defined as B(x,y) = integral(0 to 1, t^(x-1) * (1-t)^(y-1) dt).
  Related to the gamma function by B(x,y) = Gamma(x)*Gamma(y)/Gamma(x+y).

  Properties:
  - B(x,y) = B(y,x) (symmetric)
  - B(1,1) = 1
  - B(m,n) = (m-1)!(n-1)!/(m+n-1)! for positive integers

  Examples:
    (beta 1.0 1.0) => 1.0
    (beta 2.0 3.0) => 0.08333333333333333 (1/12)"
  [x y]
  (m/exp (log-beta x y)))

(s/fdef beta
  :args (s/cat :x ::m/pos :y ::m/pos)
  :ret ::m/nan-or-non-)

(defn- delta-minus-delta-sum
  [a b]
  (let [h (/ a b)
        p (/ h (inc h))
        q (/ (inc h))
        q2 (* q q)
        dc (count delta-coefficients)
        s (vec (take dc (iterate (fn [acc]
                                   (+ 1.0 q (* q2 acc)))
                          1.0)))
        sqrt-t (/ 10.0 b)
        t (* sqrt-t sqrt-t)
        w (* (get delta-coefficients (dec dc)) (get s (dec dc)))
        res (reduce (fn [acc i]
                      (+ (* (get delta-coefficients i) (get s i)) (* acc t)))
              w
              (range (- dc 2) -1 -1))]
    (* res p (/ b))))

(defn- sum-delta-minus-delta-sum
  [p q]
  (let [a (min p q)
        b (max p q)
        sqrt-t (/ 10.0 a)
        t (* sqrt-t sqrt-t)
        dc (count delta-coefficients)
        res (reduce (fn [acc i]
                      (+ (get delta-coefficients i) (* acc t)))
              (get delta-coefficients (dec dc))
              (range (- dc 2) -1 -1))]
    (+ (/ res a) (delta-minus-delta-sum a b))))

(defn- log-gamma-minus-log-gamma-sum
  [a b]
  (let [d (+ b a (- 0.5))
        w (if (<= a b)
            (delta-minus-delta-sum a b)
            (delta-minus-delta-sum b a))
        u (* d (m/log-inc (/ a b)))
        v (* a (dec (m/log b)))]
    (- w u v)))

(defn- log-gamma-sum
  [a b]
  (let [x (+ a b (- 2.0))]
    (cond (<= x 0.5) (log-gamma-inc (inc x))
      (<= x 1.5) (+ (log-gamma-inc x) (m/log-inc x))
      :else (+ (log-gamma-inc (dec x)) (m/log (* x (inc x)))))))

(defn log-beta
  "Computes the natural logarithm of the beta function ln(B(`x`,`y`)).

  More numerically stable than computing log([[beta]](`x`, `y`)) directly.
  Uses the relation ln(B(x,y)) = ln(Gamma(x)) + ln(Gamma(y)) - ln(Gamma(x+y)).

  Examples:
    (log-beta 1.0 1.0) => 0.0
    (log-beta 2.0 3.0) => -2.4849066497880004"
  [x y]
  (let [a (min x y)
        b (max x y)]
    (cond (>= a 10.0)
      (let [prod1 (sum-delta-minus-delta-sum a b)
            ared (/ a b)
            prod2 (/ ared (inc ared))
            bred (* (m/log prod2) (- 0.5 a))
            v (* b (m/log-inc ared))]
        (+ (* -0.5 (m/log b))
          0.9189385332046727
          prod1
          (- bred)
          (- v)))

      (and (> a 2.0) (> b 1000.0))
      (let [n (m/floor' (dec a))
            [ared prod] (loop [ared a
                               prod 1.0
                               i 0]
                          (if (< i n)
                            (recur (dec ared)
                              (* prod (dec ared) (/ (inc (/ ared b))))
                              (inc i))
                            [ared prod]))]
        (+ (m/log prod)
          (* (- n) (m/log b))
          (log-gamma ared)
          (log-gamma-minus-log-gamma-sum ared b)))

      (> a 2.0)
      (let [[prod1 ared] (loop [prod1 1.0
                                ared a]
                           (if (> ared 2.0)
                             (let [ared (dec ared)
                                   prod2 (/ ared b)]
                               (recur (* prod1 prod2 (/ (inc prod2))) ared))
                             [prod1 ared]))]
        (if (>= b 10.0)
          (+ (m/log prod1)
            (log-gamma ared)
            (log-gamma-minus-log-gamma-sum ared b))
          (let [[prod2 bred] (loop [prod2 1.0
                                    bred b]
                               (if (> bred 2.0)
                                 (recur (* prod2
                                          (dec bred)
                                          (/ (dec (+ ared bred))))
                                   (dec bred))
                                 [prod2 bred]))]
            (+ (m/log prod1)
              (m/log prod2)
              (log-gamma ared)
              (log-gamma bred)
              (- (log-gamma-sum ared bred))))))

      (< a 1.0)
      (if (>= b 10.0)
        (+ (log-gamma a)
          (log-gamma-minus-log-gamma-sum a b))
        (m/log (* (gamma a) (gamma b) (/ (gamma (+ a b))))))

      (<= b 2.0)
      (+ (log-gamma a)
        (log-gamma b)
        (- (log-gamma-sum a b)))

      (>= b 10.0)
      (+ (log-gamma a) (log-gamma-minus-log-gamma-sum a b))

      :else
      (let [[prod1 ared] (loop [prod1 1.0
                                ared b]
                           (if (> ared 2.0)
                             (recur (* prod1
                                      (dec ared)
                                      (/ (dec (+ a ared))))
                               (dec ared))
                             [prod1 ared]))]
        (+ (m/log prod1)
          (log-gamma a)
          (log-gamma ared)
          (- (log-gamma-sum a ared)))))))

(s/fdef log-beta
  :args (s/cat :x ::m/pos :y ::m/pos)
  :ret ::m/nan-or-non-inf-)

(defn regularized-beta
  "Computes the regularized incomplete beta function I_`c`(`x`,`y`).

  Defined as I_c(x,y) = B_c(x,y) / B(x,y), where B_c is the incomplete beta.
  Represents the cumulative distribution function of the beta distribution.

  Properties:
  - I_0(x,y) = 0
  - I_1(x,y) = 1
  - I_c(x,y) = 1 - I_{1-c}(y,x)

  Examples:
    (regularized-beta 0.5 2.0 3.0) => 0.875
    (regularized-beta 0.0 2.0 3.0) => 0.0
    (regularized-beta 1.0 2.0 3.0) => 1.0"
  [c x y]
  (let [x (double x)
        y (double y)
        c (double c)]
    (cond (zero? c) 0.0
      (m/one? c) 1.0

      (and (> c (/ (inc x) (+ 2.0 x y)))
        (<= (m/one- c) (/ (inc y) (+ 2.0 x y))))
      (m/one- (regularized-beta (m/one- c) y x))

      :else
      (let [a-series (repeat 1.0)
            ;;do not simplify by replacing m with n below
            b-series (map (fn [n]
                            (if (zero? (mod n 2))
                              (let [m (* n 0.5)
                                    res (* m
                                          (- y m)
                                          c
                                          (m/div (* (dec (+ x (* 2.0 m)))
                                                   (+ x (* 2.0 m)))))]
                                res)
                              (let [m (* 0.5 (dec n))
                                    res (* (+ x m)
                                          (+ x y m)
                                          c
                                          (m/div (* (+ x (* 2.0 m))
                                                   (inc (+ x (* 2.0 m)))))
                                          (- 1.0))]
                                res)))
                       (drop 1 (range)))
            gcf (series/multiplicative-generalized-continued-fraction
                  a-series b-series)
            sum (series/multiplicative-sum-convergent-series gcf)]
        (if (anomalies/anomaly? sum)
          m/nan
          (m/div (m/exp (+ (* x (m/log c))
                          (* y (m/log-inc (- c)))
                          (- (m/log x))
                          (- (log-beta x y))))
            sum))))))

(s/fdef regularized-beta
  :args (s/cat :c ::m/prob
          :x ::m/finite+
          :y ::m/finite+)
  :ret ::m/number)

(def regularized-incomplete-beta regularized-beta)

(defn incomplete-beta
  "Computes the incomplete beta function B_`c`(`x`,`y`).

  Defined as B_c(x,y) = integral(0 to c, t^(x-1) * (1-t)^(y-1) dt).
  Related to the regularized incomplete beta by B_c(x,y) = B(x,y) * I_c(x,y).

  Examples:
    (incomplete-beta 0.5 2.0 3.0) => 0.072916666666666663
    (incomplete-beta 0.0 2.0 3.0) => 0.0"
  [c x y]
  (if (zero? c)
    0.0
    (* (regularized-beta c x y) (beta x y))))

(s/fdef incomplete-beta
  :args (s/cat :c ::m/prob
          :x ::m/finite+
          :y ::m/finite+)
  :ret ::m/number)

;;;BESSEL FUNCTIONS
(s/def ::bessel-order
  (s/with-gen ::m/num
    #(gen/double* {:min -100.0 :max 100.0 :infinite? false :NaN? false})))

(defn- bessel-j-series
  "Power series for J_ν(x) for small x.
  J_ν(x) = (x/2)^ν * Σ_{k=0}^∞ (-1)^k * (x/2)^(2k) / (k! * Γ(ν+k+1))"
  [order x max-terms]
  (let [x-half (* 0.5 x)
        x2-neg (- (m/sq x-half))
        prefactor (m/pow x-half order)]
    (loop [k 0
           term (/ (gamma (inc order)))
           sum term]
      (if (or (>= k max-terms)
            (and (not (zero? sum))
              (< (m/abs (/ term sum)) 1e-15)))
        (* prefactor sum)
        (let [next-term (* term x2-neg
                          (/ (* (inc k) (+ order k 1))))]
          (recur (inc k) next-term (+ sum next-term)))))))

(defn- bessel-j-asymptotic
  "Asymptotic expansion for J_ν(x) for large x.
  J_ν(x) ~ √(2/(πx)) * [P(ν,x)*cos(χ) - Q(ν,x)*sin(χ)]
  where χ = x - νπ/2 - π/4"
  [order x]
  (let [chi (- x (* order 0.5 m/PI) (* 0.25 m/PI))
        sqrt-factor (m/sqrt (/ 2.0 m/PI x))
        ;; Compute P and Q using asymptotic series
        mu (* 4.0 (m/sq order))
        x8 (* 8.0 x)
        ;; P ~ 1 - (μ-1)(μ-9)/(2!(8x)²) + ...
        ;; Q ~ (μ-1)/(8x) - (μ-1)(μ-9)(μ-25)/(3!(8x)³) + ...
        p (loop [k 0
                 term 1.0
                 sum 1.0]
            (if (>= k 10)
              sum
              (let [factor (/ (* (- mu (m/sq (dec (* 4 k 2))))
                                (- mu (m/sq (inc (* 4 k 2)))))
                             (* -1.0 (inc (* 2 k)) (+ 2 (* 2 k)) (m/sq x8)))
                    next-term (* term factor)]
                (if (< (m/abs next-term) 1e-16)
                  (+ sum next-term)
                  (recur (inc k) next-term (+ sum next-term))))))
        q (loop [k 0
                 term (/ (dec mu) x8)
                 sum term]
            (if (>= k 10)
              sum
              (let [factor (/ (* (- mu (m/sq (+ 1 (* 4 k 2))))
                                (- mu (m/sq (+ 3 (* 4 k 2)))))
                             (* -1.0 (+ 2 (* 2 k)) (+ 3 (* 2 k)) (m/sq x8)))
                    next-term (* term factor)]
                (if (< (m/abs next-term) 1e-16)
                  (+ sum next-term)
                  (recur (inc k) next-term (+ sum next-term))))))]
    (* sqrt-factor (- (* p (m/cos chi)) (* q (m/sin chi))))))

(defn bessel-j
  "Computes the Bessel function of the first kind J_`order`(`x`).

  J_v(x) is the solution to Bessel's equation that is finite at x=0.
  For integer orders n, J_n(x) = (-1)^n * J_{-n}(x).

  Parameters:
    `order` - the order v (any real number)
    `x` - the argument (x >= 0)

  Examples:
    (bessel-j 0 0.0) => 1.0
    (bessel-j 0 1.0) => 0.7651976865579666
    (bessel-j 1 1.0) => 0.4400505857449335"
  [order x]
  (cond
    (m/inf? x) 0.0                                          ;; J_ν(∞) = 0 (oscillates to 0)
    (zero? x) (if (zero? order) 1.0 0.0)
    (neg? x) (if (m/roughly-round? order m/sgl-close)
               (* (m/pow -1 (m/round' order :toward-zero)) (bessel-j order (- x)))
               m/nan)
    ;; For negative integer orders: J_{-n}(x) = (-1)^n * J_n(x)
    (and (neg? order) (m/roughly-round? order m/sgl-close))
    (let [n (m/round' (- order) :toward-zero)]
      (* (m/pow -1 n) (bessel-j n x)))
    ;; Use asymptotic for large x (x > 4|ν| + 15)
    (> x (+ (* 4.0 (m/abs order)) 15.0))
    (bessel-j-asymptotic order x)
    ;; Use power series for small x
    :else (bessel-j-series order x 100)))

(s/fdef bessel-j
  :args (s/cat :order ::bessel-order
          :x (s/with-gen ::m/non-
               #(gen/double* {:min 0.0 :max 1000.0 :infinite? false :NaN? false})))
  :ret ::m/num)

(defn- bessel-y-series
  "Computes Y_ν(x) for noninteger order using the relation:
  Y_ν(x) = (J_ν(x)*cos(νπ) - J_{-ν}(x)) / sin(νπ)"
  [order x]
  (let [j-nu (bessel-j order x)
        j-neg-nu (bessel-j (- order) x)
        nu-pi (* order m/PI)]
    (/ (- (* j-nu (m/cos nu-pi)) j-neg-nu)
      (m/sin nu-pi))))

(defn- bessel-y0-small
  "Computes Y_0(x) for small x using series expansion.
  Y_0(x) = (2/π){J_0(x)[ln(x/2) + γ] + Σ_{m=1}^∞ [(-1)^{m+1}/(m!)²](x/2)^{2m} H_m}
  where H_m = 1 + 1/2 + ... + 1/m is the m-th harmonic number."
  [x]
  (let [j0 (bessel-j 0 x)
        ln-x2 (m/log (* 0.5 x))
        x-half (* 0.5 x)
        x2 (m/sq x-half)
        ;; First term: (2/π) * J_0(x) * [ln(x/2) + γ]
        first-term (* (/ 2.0 m/PI) j0 (+ ln-x2 m/euler-mascheroni-constant))
        ;; Series: Σ_{m=1}^∞ [(-1)^{m+1}/(m!)²](x/2)^{2m} H_m
        series-sum (loop [m 1
                          h-m 1.0
                          m-fact 1.0
                          x-pow x2
                          sum 0.0]
                     (if (> m 30)
                       sum
                       (let [sign (if (odd? m) 1.0 -1.0)
                             term (* sign x-pow (/ (* m-fact m-fact)) h-m)
                             next-m (inc m)
                             next-h-m (+ h-m (/ next-m))
                             next-m-fact (* m-fact next-m)
                             next-x-pow (* x-pow x2)]
                         (if (and (> m 5) (< (m/abs term) 1e-16))
                           (+ sum term)
                           (recur next-m next-h-m next-m-fact next-x-pow
                             (+ sum term))))))]
    (+ first-term (* (/ 2.0 m/PI) series-sum))))

(defn- bessel-y1-small
  "Computes Y_1(x) for small x using Wronskian relation."
  [x]
  ;; Wronskian: J_0(x)*Y_1(x) - J_1(x)*Y_0(x) = -2/(πx)
  ;; So: Y_1(x) = [J_1(x)*Y_0(x) - 2/(πx)] / J_0(x)
  (let [y0 (bessel-y0-small x)
        j0 (bessel-j 0 x)
        j1 (bessel-j 1 x)]
    (/ (- (* j1 y0) (/ 2.0 (* m/PI x))) j0)))

(defn- bessel-y-integer
  "Computes Y_n(x) for integer order n."
  [n x]
  (let [n (long (m/abs n))]
    (cond
      (zero? n) (bessel-y0-small x)
      (m/one? n) (bessel-y1-small x)
      :else
      ;; Use forward recurrence: Y_{n+1} = (2n/x)Y_n - Y_{n-1}
      (loop [k 1
             y-prev (bessel-y0-small x)
             y-curr (bessel-y1-small x)]
        (cond
          (>= k n) y-curr
          ;; Handle overflow - once y-curr is -Infinity, result stays -Infinity
          (m/inf-? y-curr) m/inf-
          :else
          (let [y-next (- (* (/ (* 2.0 k) x) y-curr) y-prev)]
            (recur (inc k) y-curr y-next)))))))

(defn bessel-y
  "Computes the Bessel function of the second kind Y_`order`(`x`).

  Also called Neumann function N_v(x). Y_v(x) is the second linearly independent solution to
  Bessel's equation. Has a logarithmic singularity at x=0.

  Parameters:
    `order` - the order v (any real number)
    `x` - the argument (x > 0)

  Examples:
    (bessel-y 0 1.0) => 0.08825696421567691
    (bessel-y 1 1.0) => -0.7812128213002887"
  [order x]
  (cond
    (<= x 0.0) m/inf-
    (m/roughly-round? order m/sgl-close)
    (bessel-y-integer (m/round' order :toward-zero) x)
    :else (bessel-y-series order x)))

(s/fdef bessel-y
  :args (s/cat :order ::bessel-order
          :x (s/with-gen ::m/finite+
               #(gen/double* {:min 0.01 :max 1000.0 :infinite? false :NaN? false})))
  :ret ::m/num)

(defn- bessel-i-series
  "Power series for I_v(x) for small to moderate x.
  I_v(x) = (x/2)^v * sum(k=0 to inf, (x/2)^(2k) / (k! * Gamma(v+k+1)))"
  [order x max-terms]
  (let [x-half (* 0.5 x)
        x2 (m/sq x-half)
        prefactor (m/pow x-half order)]
    (loop [k 0
           term (/ (gamma (inc order)))
           sum term]
      (if (or (>= k max-terms)
            (and (not (zero? sum))
              (< (m/abs (/ term sum)) 1e-15)))
        (* prefactor sum)
        (let [next-term (* term x2
                          (/ (* (inc k) (+ order k 1))))]
          (recur (inc k) next-term (+ sum next-term)))))))

(defn- bessel-i-asymptotic
  "Asymptotic expansion for I_v(x) for large x.
  I_v(x) ~ exp(x)/sqrt(2*pi*x) * [1 - (mu-1)/(8x) + ...]
  where mu = 4*v^2"
  [order x]
  (let [prefactor (/ (m/exp x) (m/sqrt (* 2.0 m/PI x)))
        mu (* 4.0 (m/sq order))
        x8 (* 8.0 x)]
    (* prefactor
      (loop [k 0
             term 1.0
             sum 1.0]
        (if (>= k 20)
          sum
          (let [factor (/ (- mu (m/sq (inc (* 2 k))))
                         (* -1.0 x8 (inc k)))
                next-term (* term factor)]
            (if (< (m/abs next-term) 1e-16)
              (+ sum next-term)
              (recur (inc k) next-term (+ sum next-term)))))))))

(defn bessel-i
  "Computes the modified Bessel function of the first kind I_`order`(`x`).

  I_v(x) is the solution to the modified Bessel equation that is finite at x=0. Related to J_v
  by I_v(x) = i^(-v) * J_v(ix).

  Parameters:
    `order` - the order v (any real number)
    `x` - the argument (x >= 0)

  Examples:
    (bessel-i 0 0.0) => 1.0
    (bessel-i 0 1.0) => 1.2660658777520082
    (bessel-i 1 1.0) => 0.5651591039924851"
  [order x]
  (cond
    (zero? x) (if (zero? order) 1.0 0.0)
    (neg? x) (if (m/roughly-round? order m/sgl-close)
               (bessel-i order (- x))
               m/nan)
    ;; For negative integer orders: I_{-n}(x) = I_n(x)
    (and (neg? order) (m/roughly-round? order m/sgl-close))
    (bessel-i (- order) x)
    ;; Use asymptotic for large x
    (> x (+ (* 4.0 (m/abs order)) 20.0))
    (bessel-i-asymptotic order x)
    ;; Use power series
    :else (bessel-i-series order x 100)))

(s/fdef bessel-i
  :args (s/cat :order (s/with-gen ::bessel-order
                        #(gen/double* {:min 0.0 :max 100.0 :infinite? false :NaN? false}))
          :x (s/with-gen ::m/non-
               #(gen/double* {:min 0.0 :max 100.0 :infinite? false :NaN? false})))
  :ret ::m/nan-or-non-)

(defn- bessel-k-asymptotic
  "Asymptotic expansion for K_v(x) for large x.
  K_v(x) ~ sqrt(pi/(2x)) * e^(-x) * [1 + (mu-1)/(8x) + ...]
  where mu = 4*v^2"
  [order x]
  (let [prefactor (* (m/sqrt (/ m/PI (* 2.0 x))) (m/exp (- x)))
        mu (* 4.0 (m/sq order))
        x8 (* 8.0 x)]
    (* prefactor
      (loop [k 0
             term 1.0
             sum 1.0]
        (if (>= k 20)
          sum
          (let [factor (/ (- mu (m/sq (inc (* 2 k))))
                         (* x8 (inc k)))
                next-term (* term factor)]
            (if (< (m/abs next-term) 1e-16)
              (+ sum next-term)
              (recur (inc k) next-term (+ sum next-term)))))))))

(defn- bessel-k-series
  "Computes K_v(x) for non-integer order using the relation:
  K_v(x) = (pi/2) * (I_{-v}(x) - I_v(x)) / sin(v*pi)"
  [order x]
  (let [i-nu (bessel-i order x)
        i-neg-nu (bessel-i (- order) x)
        nu-pi (* order m/PI)]
    (* 0.5 m/PI (/ (- i-neg-nu i-nu) (m/sin nu-pi)))))

(defn- bessel-k-integer
  "Computes K_n(x) for integer order n using series or asymptotic expansion."
  [n x]
  (let [n (long (m/abs n))]
    (cond
      ;; For large x, use asymptotic for base cases to avoid numerical issues
      (> x 10.0)
      (if (zero? n)
        (bessel-k-asymptotic 0 x)
        (if (m/one? n)
          (bessel-k-asymptotic 1 x)
          ;; Use forward recurrence from asymptotic K_0 and K_1
          (loop [k 1
                 k-prev (bessel-k-asymptotic 0 x)
                 k-curr (bessel-k-asymptotic 1 x)]
            (if (>= k n)
              k-curr
              (let [k-next (+ (* (/ (* 2.0 k) x) k-curr) k-prev)]
                (recur (inc k) k-curr k-next))))))

      (zero? n)
      ;; K_0(x) = -[gamma + ln(x/2)]I_0(x) + sum(k=1 to inf, (x/2)^{2k} * psi(k) / (k!)^2)
      ;; where psi(k) = H_k = 1 + 1/2 + ... + 1/k
      (let [ln-x2 (m/log (* 0.5 x))
            x-half (* 0.5 x)
            x2 (m/sq x-half)
            i0 (bessel-i 0 x)
            series-sum (loop [k 1
                              psi-k 1.0
                              factorial-sq 1.0
                              sum (* psi-k x2)]
                         (if (> k 30)
                           sum
                           (let [next-k (inc k)
                                 next-psi-k (+ psi-k (/ next-k))
                                 next-factorial-sq (* factorial-sq next-k next-k)
                                 next-term (* next-psi-k
                                             (m/pow x2 next-k)
                                             (/ next-factorial-sq))]
                             (if (< (m/abs next-term) 1e-16)
                               (+ sum next-term)
                               (recur next-k next-psi-k next-factorial-sq
                                 (+ sum next-term))))))]
        (+ (* -1.0 (+ m/euler-mascheroni-constant ln-x2) i0) series-sum))

      (m/one? n)
      ;; K_1(x) via Wronskian: I_0*K_1 + I_1*K_0 = 1/x
      ;; So K_1 = (1/x - I_1*K_0) / I_0
      (let [k0 (bessel-k-integer 0 x)
            i0 (bessel-i 0 x)
            i1 (bessel-i 1 x)]
        (/ (- (/ x) (* i1 k0)) i0))

      :else
      ;; Use forward recurrence: K_{n+1} = (2n/x)K_n + K_{n-1}
      (loop [k 1
             k-prev (bessel-k-integer 0 x)
             k-curr (bessel-k-integer 1 x)]
        (if (>= k n)
          k-curr
          (let [k-next (+ (* (/ (* 2.0 k) x) k-curr) k-prev)]
            (recur (inc k) k-curr k-next)))))))

(defn bessel-k
  "Computes the modified Bessel function of the second kind K_`order`(`x`).

  K_v(x) is the second linearly independent solution to the modified Bessel equation. Decays
  exponentially for large x. Has a singularity at x=0.

  Parameters:
    `order` - the order v (any real number)
    `x` - the argument (x > 0)

  Examples:
    (bessel-k 0 1.0) => 0.42102443824070834
    (bessel-k 1 1.0) => 0.6019072301972346"
  [order x]
  (let [abs-order (m/abs order)]                            ;; K_ν(x) = K_{-ν}(x), so use |ν|
    (cond
      (<= x 0.0) m/inf+
      ;; Use asymptotic for large x (lower threshold to avoid catastrophic
      ;; cancellation in series when I_ν and I_{-ν} are both large)
      (> x (+ (* 2.0 abs-order) 10.0))
      (bessel-k-asymptotic abs-order x)
      (m/roughly-round? abs-order m/sgl-close)
      (bessel-k-integer (m/round' abs-order :toward-zero) x)
      :else (bessel-k-series abs-order x))))

;; For noninteger orders > 5, the series method has numerical issues when x is small.
;; The generator constrains to ranges where the implementation is numerically stable.
(s/fdef bessel-k
  :args (s/cat :order
          (s/with-gen ::m/num
            #(gen/one-of [(gen/double* {:min -5.0 :max 5.0 :infinite? false :NaN? false})
                          (gen/fmap double (gen/large-integer* {:min -50 :max 50}))]))
          :x (s/with-gen ::m/finite+
               #(gen/double* {:min 0.1 :max 50.0 :infinite? false :NaN? false})))
  :ret ::m/nan-or-non-)

;;;HYPERGEOMETRIC FUNCTIONS
(defn- hypergeometric-1f1-series
  "Direct series for 1F1(a; b; z) = sum(n=0 to inf, (a)_n z^n / ((b)_n n!))
  where (a)_n is the Pochhammer symbol (rising factorial)."
  [a b z max-terms]
  (loop [n 0
         term 1.0
         sum 1.0]
    (if (or (>= n max-terms)
          (and (not (zero? sum))
            (< (m/abs (/ term sum)) 1e-15)))
      sum
      (let [;; term_{n+1} = term_n * a_n * z / (b_n * (n+1))
            ;; where a_n = a + n, b_n = b + n
            next-term (* term (+ a n) z (/ (* (+ b n) (inc n))))]
        (recur (inc n) next-term (+ sum next-term))))))

(defn hypergeometric-1f1
  "Computes the confluent hypergeometric function 1F1(`a`; `b`; `z`).

  Also known as Kummer's function M(a,b,z). Defined by:
  1F1(a; b; z) = sum(n=0 to inf, (a)_n z^n / ((b)_n n!))

  Parameters:
    `a` - first parameter (any real number)
    `b` - second parameter (must not be zero or negative integer)
    `z` - the argument (any real number)

  Special cases:
    1F1(a; a; z) = e^z
    1F1(0; b; z) = 1
    1F1(1; 2; 2z) = (e^z - 1)/z * sinh(z)/z for small z

  Examples:
    (hypergeometric-1f1 1 1 1) => e (approx 2.718281828)
    (hypergeometric-1f1 0.5 1.5 -1) => 0.7468241328"
  [a b z]
  (cond
    ;; b is non-positive integer: undefined (pole)
    (and (m/roughly-round-non+? b m/sgl-close)
      (not (and (m/roughly-round-non+? a m/sgl-close)
             (>= a b))))
    m/nan

    ;; a = 0: result is 1
    (m/roughly? a 0.0 m/sgl-close)
    1.0

    ;; z = 0: result is 1
    (zero? z)
    1.0

    ;; a = b: result is e^z
    (m/roughly? a b m/sgl-close)
    (m/exp z)

    ;; Large negative z: use Kummer transformation
    ;; M(a,b,z) = e^z * M(b-a,b,-z)
    (< z -20.0)
    (* (m/exp z) (hypergeometric-1f1 (- b a) b (- z)))

    ;; Standard series
    :else
    (hypergeometric-1f1-series a b z 300)))

(s/fdef hypergeometric-1f1
  :args (s/cat :a (s/with-gen ::m/num
                    #(gen/double* {:min -10.0 :max 10.0 :infinite? false :NaN? false}))
          :b (s/with-gen
               (s/and ::m/num #(not (m/roughly-round-non+? % m/sgl-close)))
               #(gen/double* {:min 0.1 :max 10.0 :infinite? false :NaN? false}))
          :z (s/with-gen ::m/num
               #(gen/double* {:min -10.0 :max 10.0 :infinite? false :NaN? false})))
  :ret ::m/num)

(defn- hypergeometric-2f1-series
  "Direct series for 2F1(a, b; c; z) = sum(n=0 to inf, (a)_n (b)_n z^n / ((c)_n n!))
  Converges for |z| < 1."
  [a b c z max-terms]
  (loop [n 0
         term 1.0
         sum 1.0]
    (if (or (>= n max-terms)
          (and (not (zero? sum))
            (< (m/abs (/ term sum)) 1e-15)))
      sum
      (let [;; term_{n+1} = term_n * (a+n)(b+n) * z / ((c+n)(n+1))
            next-term (* term
                        (+ a n)
                        (+ b n)
                        z
                        (/ (* (+ c n) (inc n))))]
        (recur (inc n) next-term (+ sum next-term))))))

(defn hypergeometric-2f1
  "Computes the Gaussian hypergeometric function 2F1(`a`, `b`; `c`; `z`).

  Defined by:
  2F1(a, b; c; z) = sum(n=0 to inf, (a)_n (b)_n z^n / ((c)_n n!))

  The series converges for |z| < 1. For |z| >= 1, various transformations are used (Euler, Pfaff,
  etc.).

  Parameters:
    `a`, `b` - numerator parameters (any real numbers)
    `c` - denominator parameter (must not be zero or negative integer)
    `z` - the argument (real number, |z| < 1 for convergence)

  Special cases:
    2F1(a, b; c; 0) = 1
    2F1(1, 1; 2; z) = -ln(1-z)/z
    2F1(1, b; b; z) = 1/(1-z)

  Examples:
    (hypergeometric-2f1 1 1 2 0.5) => 2*ln(2) (approx 1.386294)
    (hypergeometric-2f1 0.5 0.5 1.5 0.5) => (2/sqrt(pi))*arcsin(sqrt(0.5))"
  [a b c z]
  (cond
    ;; c is non-positive integer: undefined (pole)
    ;; unless a or b is also non-positive integer with |a| or |b| < |c|
    (and (m/roughly-round-non+? c m/sgl-close)
      (not (or (and (m/roughly-round-non+? a m/sgl-close) (> a c))
             (and (m/roughly-round-non+? b m/sgl-close) (> b c)))))
    m/nan

    ;; z = 0: result is 1
    (zero? z)
    1.0

    ;; a = 0 or b = 0: result is 1
    (or (m/roughly? a 0.0 m/sgl-close)
      (m/roughly? b 0.0 m/sgl-close))
    1.0

    ;; a = c or b = c: reduces to 1F0 = (1-z)^(-b) or (1-z)^(-a)
    (m/roughly? a c m/sgl-close)
    (m/pow (- 1.0 z) (- b))

    (m/roughly? b c m/sgl-close)
    (m/pow (- 1.0 z) (- a))

    ;; |z| < 1: direct series
    (< (m/abs z) 1.0)
    (hypergeometric-2f1-series a b c z 300)

    ;; z = 1: Gauss summation theorem (when c > a + b)
    (and (m/roughly? z 1.0 m/sgl-close)
      (> c (+ a b)))
    (/ (* (gamma c) (gamma (- c a b)))
      (* (gamma (- c a)) (gamma (- c b))))

    ;; z > 1 or z < 0: use linear transformation
    ;; 2F1(a,b;c;z) = (1-z)^(-a) * 2F1(a, c-b; c; z/(z-1))  [Pfaff transformation]
    (or (> z 1.0) (< z 0.0))
    (let [z-transformed (/ z (- z 1.0))]
      (if (< (m/abs z-transformed) 1.0)
        (* (m/pow (- 1.0 z) (- a))
          (hypergeometric-2f1 a (- c b) c z-transformed))
        ;; Try alternative: (1-z)^(-b) * 2F1(c-a, b; c; z/(z-1))
        (* (m/pow (- 1.0 z) (- b))
          (hypergeometric-2f1 (- c a) b c z-transformed))))

    :else
    m/nan))

(s/fdef hypergeometric-2f1
  :args (s/cat :a (s/with-gen ::m/num
                    #(gen/double* {:min -10.0 :max 10.0 :infinite? false :NaN? false}))
          :b (s/with-gen ::m/num
               #(gen/double* {:min -10.0 :max 10.0 :infinite? false :NaN? false}))
          :c (s/with-gen
               (s/and ::m/num #(not (m/roughly-round-non+? % m/sgl-close)))
               #(gen/double* {:min 0.1 :max 10.0 :infinite? false :NaN? false}))
          :z (s/with-gen ::m/num
               #(gen/double* {:min -0.99 :max 0.99 :infinite? false :NaN? false})))
  :ret ::m/num)
