(ns provisdom.math.special-functions
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.utility-belt.anomalies :as anomalies]
    [provisdom.math.core :as m]
    [provisdom.math.series :as series]))

;;;DECLARATIONS
(declare regularized-gamma-p regularized-gamma-q log-gamma erfc log-beta)

;;;CONSTANTS
(def ^:const euler-mascheroni-constant
  0.57721566490153286060651209008240243104215933593992M)

(def ^:const lanczos-coefficients
  "Lanczos Coefficients."
  [0.99999999999980993, 676.5203681218851, -1259.1392167224028,
   771.32342877765313, -176.61502916214059, 12.507343278686905,
   -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7])

(def ^:const lanczos-coefficients2
  [0.9999999999999971, 57.15623566586292, -59.59796035547549,
   14.136097974741746, -0.4919138160976202, 3.399464998481189E-5,
   4.652362892704858E-5, -9.837447530487956E-5, 1.580887032249125E-4,
   -2.1026444172410488E-4, 2.1743961811521265E-4, -1.643181065367639E-4,
   8.441822398385275E-5, -2.6190838401581408E-5, 3.6899182659531625E-6])

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

(def ^:const delta-coefficients
  [0.08333333333333333, -2.777777777777778E-5, 7.936507936507937E-8,
   -5.952380952380953E-10, 8.417508417508329E-12, -1.917526917518546E-13,
   6.410256405103255E-15, -2.955065141253382E-16, 1.7964371635940225E-17,
   -1.3922896466162779E-18, 1.338028550140209E-19, -1.542460098679661E-20,
   1.9770199298095743E-21, -2.3406566479399704E-22, 1.713480149663986E-23])

(def ^:const inv-erf-coeffs1
  [-3.64441206401782E-21 -1.6850591381820166E-19 1.28584807152564E-18
   1.1157877678025181E-17 -1.333171662854621E-16 2.0972767875968562E-17
   6.637638134358324E-15 -4.054566272975207E-14 -8.151934197605472E-14
   2.6335093153082323E-12 -1.2975133253453532E-11 -5.415412054294628E-11
   1.0512122733215323E-9 -4.112633980346984E-9 -2.9070369957882005E-8
   4.2347877827932404E-7 -1.3654692000834679E-6 -1.3882523362786469E-5
   1.8673420803405714E-4 -7.40702534166267E-4 -0.006033670871430149
   0.24015818242558962 1.6536545626831027])

(def ^:const inv-erf-coeffs2
  [2.2137376921775787E-9 9.075656193888539E-8 -2.7517406297064545E-7
   1.8239629214389228E-8 1.5027403968909828E-6 -4.013867526981546E-6
   2.9234449089955446E-6 1.2475304481671779E-5 -4.7318229009055734E-5
   6.828485145957318E-5 2.4031110387097894E-5 -3.550375203628475E-4
   9.532893797373805E-4 -0.0016882755560235047 0.002491442096107851
   -0.003751208507569241 0.005370914553590064 1.0052589676941592
   3.0838856104922208])

(def ^:const inv-erf-coeffs3
  [-2.7109920616438573E-11 -2.555641816996525E-10 1.5076572693500548E-9
   -3.789465440126737E-9 7.61570120807834E-9 -1.496002662714924E-8
   2.914795345090108E-8 -6.771199775845234E-8 2.2900482228026655E-7
   -9.9298272942317E-7 4.526062597223154E-6 -1.968177810553167E-5
   7.599527703001776E-5 -2.1503011930044477E-4 -1.3871931833623122E-4
   1.0103004648645344 4.849906401408584])

(def ^:const inv-gamma1-pm1-b-coeffs
  [1.9575583661463974E-10 -6.077618957228252E-8 9.926418406727737E-7
   -6.4304548177935305E-6 -8.514194324403149E-6 4.939449793824468E-4
   0.026620534842894922 0.203610414066807 1.0])

(def ^:const inv-gamma1-pm1-c-coeffs
  [1.133027231981696E-6 -1.2504934821426706E-6 -2.013485478078824E-5
   1.280502823881162E-4 -2.1524167411495098E-4 -0.0011651675918590652
   0.0072189432466631 -0.009621971527876973 -0.04219773455554433
   0.16653861138229148 -0.04200263503409524 -0.6558780715202539
   -0.42278433509846713])

(def ^:const inv-gamma1-pm1-d-coeffs
  [4.343529937408594E-15 -1.2494415722763663E-13 1.5728330277104463E-12
   4.686843322948848E-11 6.820161668496171E-10 6.8716741130671986E-9
   6.116095104481416E-9])

(def ^:const inv-gamma1-pm1-e-coeffs
  [2.6923694661863613E-4 0.004956830093825887 0.054642130860422966
   0.3056961078365221 1.0])

(def ^:const inv-gamma1-pm1-f-coeffs
  [1.133027231981696E-6 -1.2504934821426706E-6 -2.013485478078824E-5
   1.280502823881162E-4 -2.1524167411495098E-4 -0.0011651675918590652
   0.0072189432466631 -0.009621971527876973 -0.04219773455554433
   0.16653861138229148 -0.04200263503409524 -0.6558780715202539
   0.5772156649015329])

;;;LOG-SUM-EXP
(defn log-sum-exp
  "Special function for taking the log of the sum of the exponent of numbers
  that are either very large or very small."
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
  "Returns the error function:
  2 / (sqrt PI) × integral[0, `x`] (e ^ - (t ^ 2) × dt)."
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

(defn- coeffs-sum
  [a coeffs]
  (reduce (fn [acc v]
            (+ v (* acc a)))
    (first coeffs)
    (rest coeffs)))

(defn inv-erf
  "Returns the inverse error function."
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
  "Returns the inverse complementary error function."
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

(def ^{:doc "See [[inv-cdf-standard-normal]]"} probit inv-cdf-standard-normal)

(def ^{:doc "See [[cdf-standard-normal]]"} inv-probit cdf-standard-normal)

(defn logistic
  ""
  [x]
  (/ (inc (m/exp (- x)))))

(s/fdef logistic
  :args (s/cat :x ::m/num)
  :ret ::m/prob)

(defn logistic-derivative
  ""
  [x]
  (let [ex (m/exp x)]
    (if (m/inf+? ex)
      0.0
      (/ ex (m/sq (inc ex))))))

(s/fdef logistic-derivative
  :args (s/cat :x ::m/num)
  :ret ::m/prob)

(defn logit
  ""
  [p]
  (cond (zero? p) m/inf-
        (m/one? p) m/inf+
        :else (m/log (/ p (m/one- p)))))

(s/fdef logit
  :args (s/cat :p ::m/prob)
  :ret ::m/num)

(defn logit-derivative
  ""
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

(defn- log-gamma-1p
  [x]
  (- (m/log (inc (inv-gamma1-pm1 x)))))

(defn gamma
  "Returns the gamma function: integral[0, inf] (t ^ (`a`- 1) × e ^ -t × dt).
  Although gamma is defined for positive `a`, this function also allows for all
  non-roughly-round-non+ `a`."
  [a]
  (cond (> a 172) m/inf+
        (< a -170) 0.0
        :else
        (let [abs-x (m/abs a)]
          (if (<= abs-x 20.0)
            (if (>= a 1.0)
              (let [[prod t] (loop [t a
                                    prod 1.0]
                               (if (> t 2.5)
                                 (recur (dec t) (* (dec t) prod))
                                 [prod t]))]
                (/ prod (inc (inv-gamma1-pm1 (dec t)))))
              (let [[prod t] (loop [t a
                                    prod a]
                               (if (< t 0.5)
                                 (recur (inc t) (* (inc t) prod))
                                 [prod t]))]
                (/ (* prod (inc (inv-gamma1-pm1 t))))))
            (let [prod (+ 5.2421875 abs-x)
                  t (* 2.5066282746310007
                      (/ abs-x)
                      (m/pow prod (+ 0.5 abs-x))
                      (m/exp (- prod))
                      (lanczos2 abs-x))]
              (if (pos? a)
                t
                (/ (- m/PI)
                  (* a t (m/sin (* (- m/PI) a))))
                ))))))

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
        :else (* (gamma a) (regularized-gamma-p a x))))

(s/fdef lower-gamma
  :args (s/cat :a ::m/pos :x ::m/non-)
  :ret ::m/nan-or-non-)

(defn upper-gamma
  "Returns the upper incomplete gamma function:
   integral[`x`, inf] (t ^ (`a` - 1) × e ^ -t × dt)."
  [a x]
  (cond (zero? x) (gamma a)
        (m/one? a) (m/exp (- x))
        (> x 1.0e150) 0.0
        :else (* (gamma a) (regularized-gamma-q a x))))

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
  (cond (zero? x) 0.0
        (> x 1.0e150) 1.0
        (>= x (inc (double a))) (m/one- (regularized-gamma-q a x))
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
  "Returns the regularized gamma function Q(`a`, `x`) = 1 - P(`a`, `x`). Equal
 to [[upper-gamma]] function (`a`, `x`) divided by [[gamma]] function (`a`)."
  [a x]
  (cond (zero? x) 1.0
        (> x 1.0e150) 0.0
        (< x (inc a)) (m/one- (regularized-gamma-p a x))
        :else (let [a-term-series (map
                                    (fn [n]
                                      (+ (* 2.0 n) 1.0 x (- a)))
                                    (range))
                    b-term-series (map
                                    (fn [n]
                                      (* n (- a (double n))))
                                    (drop 1 (range)))
                    gcf (series/generalized-continued-fraction
                          a-term-series b-term-series)
                    sum (series/sum-convergent-series gcf)]
                (if (anomalies/anomaly? sum)
                  m/nan
                  (min 1.0 (/ (m/exp (+ (* a (m/log x))
                                       (- x)
                                       (- (log-gamma a))))
                             sum))))))

(s/fdef regularized-gamma-q
  :args (s/cat :a ::m/pos :x ::m/non-)
  :ret ::m/nan-or-prob)

(defn log-gamma
  "Returns the log gamma of `a`."
  [a]
  (cond (m/inf+? a) m/inf+
        (< a 0.5) (- (log-gamma-1p a) (m/log a))
        (<= a 2.5) (log-gamma-1p (dec a))
        (<= a 8.0) (let [n (m/floor' (- a 1.5))
                         prod (reduce (fn [acc i]
                                        (* acc (- a (double i))))
                                1.0
                                (range 1 (inc n)))]
                     (+ (log-gamma-1p (- a (double (inc n)))) (m/log prod)))
        :else (let [sum (lanczos2 a)
                    tmp (+ a 5.2421875)]
                (+ (* (+ a 0.5) (m/log tmp))
                  (- tmp)
                  m/half-log-two-pi
                  (m/log (/ sum a))))))

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
                                                  (+ (/ 3)
                                                    (/ inv2-x 42.0)))))))
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
            (and (< p 1e7)                                  ;speed
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
        u (* d (m/log (inc (/ a b))))
        v (* a (dec (m/log b)))]
    (- w u v)))

(defn- log-gamma-sum
  [a b]
  (let [x (+ a b (- 2.0))]
    (cond (<= x 0.5) (log-gamma-1p (inc x))
          (<= x 1.5) (+ (log-gamma-1p x) (m/log (inc x)))
          :else (+ (log-gamma-1p (dec x)) (m/log (* x (inc x)))))))

(defn log-beta
  "Returns the log-beta of `x` and `y`."
  [x y]
  (let [a (min x y)
        b (max x y)]
    (cond (>= a 10.0)
          (let [prod1 (sum-delta-minus-delta-sum a b)
                ared (/ a b)
                prod2 (/ ared (inc ared))
                bred (* (m/log prod2) (- 0.5 a))
                v (* b (m/log (inc ared)))]
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
  "Returns the regularized beta. Equal to incomplete beta function divided by
  beta function. If solution doesn't converge, then NaN is returned."
  [c x y]
  (let [x (double x)
        y (double y)
        c (double c)]
    (cond (zero? c) 0.0
          (m/one? c) 1.0

          (and (> c (/ (inc x) (+ 2.0 x y)))
            (<= (- 1.0 c) (/ (inc y) (+ 2.0 x y))))
          (m/one- (regularized-beta (m/one- c) y x))

          :else
          (let [a-term-series (repeat 1.0)
                b-term-series (map (fn [n]
                                     (if (zero? (mod n 2))
                                       (let [m (* n 0.5)
                                             res (* m
                                                   (- y m)
                                                   c
                                                   (/ (dec (+ x (* 2.0 m))))
                                                   (/ (+ x (* 2.0 m))))]
                                         res)
                                       (let [m (* 0.5 (dec n))
                                             res (* (- 1.0)
                                                   (+ x m)
                                                   (+ x y m)
                                                   c
                                                   (/ (+ x (* 2.0 m)))
                                                   (/ (inc (+ x (* 2.0 m)))))]
                                         res)))
                                (drop 1 (range)))
                gcf (series/generalized-continued-fraction
                      a-term-series b-term-series)
                sum (series/sum-convergent-series gcf)]
            (if (anomalies/anomaly? sum)
              m/nan
              (/ (m/exp (+ (* x (m/log c))
                          (* y (m/log (m/one- c)))
                          (- (m/log x))
                          (- (log-beta x y))))
                sum))))))

(s/fdef regularized-beta
  :args (s/cat :c ::m/prob
          :x ::m/finite+
          :y ::m/finite+)
  :ret ::m/number)

(defn incomplete-beta
  "Returns the lower beta:
  integral[0, `c`] (t ^ (`x` - 1) × (1 - t) ^ (`y` - 1) × dt. If solution
  doesn't converge, then NaN is returned."
  [c x y]
  (if (zero? c)
    0.0
    (* (regularized-beta c x y) (beta x y))))

(s/fdef incomplete-beta
  :args (s/cat :c ::m/prob
          :x ::m/finite+
          :y ::m/finite+)
  :ret ::m/number)
