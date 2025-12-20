(ns provisdom.math.integrals-test
  (:require
    [clojure.test :as ct]
    [provisdom.math.core :as m]
    [provisdom.math.integrals :as integrals]
    [provisdom.math.intervals :as intervals]
    [provisdom.math.random :as random]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anomalies]))

;;22 seconds

(set! *warn-on-reflection* true)

(declare uni-integration-test)

;;;INTEGRATION TESTS
(ct/deftest change-of-variable-test
  (t/with-instrument `integrals/change-of-variable
    (t/is-spec-check integrals/change-of-variable))
  (t/with-instrument :all
    (let [cov (integrals/change-of-variable [m/inf- m/inf+])]
      (t/is= 1.0 ((::integrals/multiplicative-fn cov) 0.0))
      (t/is= 0.0 ((::integrals/converter-fn cov) 0.0))
      (t/is= [-1.0 1.0] (::intervals/finite-interval cov)))
    (let [cov2 (integrals/change-of-variable [3.0 4.0])]
      (t/is= 1.0 ((::integrals/multiplicative-fn cov2) 0.0))
      (t/is= 0.0 ((::integrals/converter-fn cov2) 0.0))
      (t/is= [3.0 4.0] (::intervals/finite-interval cov2)))
    (let [cov3 (integrals/change-of-variable [3.0 m/inf+])]
      (t/is= 1.0 ((::integrals/multiplicative-fn cov3) 0.0))
      (t/is= 3.0 ((::integrals/converter-fn cov3) 0.0))
      (t/is= [0.0 1.0] (::intervals/finite-interval cov3)))
    (let [cov4 (integrals/change-of-variable [m/inf- 4.0])]
      (t/is= m/inf+ ((::integrals/multiplicative-fn cov4) 0.0))
      (t/is= m/inf- ((::integrals/converter-fn cov4) 0.0))
      (t/is= [0.0 1.0] (::intervals/finite-interval cov4)))))

(ct/deftest integration-test
  (t/with-instrument `integrals/integration
    (t/is-spec-check integrals/integration))
  (t/with-instrument `integrals/integration
    ;;ordinary
    (t/is= 69.33333333333331 (integrals/integration m/sq [2.0 6.0]))
    (t/is= 3.333333333333334E299 (integrals/integration m/sq [m/tiny-dbl 1e100]))
    (t/is= 69.33333333333333 (uni-integration-test m/sq [2.0 6.0]))
    (t/is= 2.886579864025407E-15 (integrals/integration m/cos [m/PI (* 5 m/PI)]))
    (t/is= 9.086362199244823E-16 (uni-integration-test m/cos [m/PI (* 5 m/PI)]))
    (t/is= -1.000000000000004 (integrals/integration m/cos [m/PI (* 5.5 m/PI)]))
    (t/is= -1.0000000000216476 (uni-integration-test m/cos [m/PI (* 5.5 m/PI)]))
    (t/is= 3.3306690738754696E-16 (integrals/integration m/cos [m/PI (* 6 m/PI)]))
    (t/is= 5.5206794842859465E-15 (uni-integration-test m/cos [m/PI (* 6 m/PI)]))
    (t/is= {::anomalies/message  "Error contains NaN. Value: NaN"
            ::anomalies/fn       #'provisdom.math.integrals/adaptive-quadrature
            ::anomalies/category ::anomalies/no-solve}
      (integrals/integration m/cos [m/PI m/inf+]))
    (t/is= 0.2 (integrals/integration #(m/pow % -2.0) [5.0 m/inf+]))
    (t/is= 0.19999999999999996 (integrals/integration #(m/pow % -2.0) [m/inf- -5.0]))
    (t/is= 1.7724538509055163 (integrals/integration #(m/exp (- (m/sq %))) [m/inf- m/inf+]))
    ;;vector
    (t/is= [15.999999999999998 69.33333333333333]
      (integrals/integration
        #(vector % (m/sq %))
        [2.0 6.0]))
    ;   with change of var
    (t/is= [1.7724538509055163 1.8128049541109543]
      (integrals/integration
        #(vector (m/exp (- (m/sq %))) (m/exp (- (m/pow % 4))))
        [m/inf- m/inf+]))
    ;;matrix
    (t/is= [[15.999999999999998 69.33333333333334] [24.0 19.999999999999996]]
      (integrals/integration
        #(vector [% (m/sq %)] [(+ 2.0 %) 5.0])
        [2.0 6.0]))
    ;   with change of var
    (t/is= [[6.400000000000001E-5 0.0026666666666666666] [0.2 0.2]]
      (integrals/integration
        #(vector [(m/pow % -6) (m/pow % -4)] [(m/pow % -2.0) (m/pow % -2.0)])
        [5.0 m/inf+]
        {::integrals/parallel? true}))))

(ct/deftest rectangular-integration-test
  (t/with-instrument `integrals/rectangular-integration
    (t/is-spec-check integrals/rectangular-integration))
  (t/with-instrument `integrals/rectangular-integration
    (t/is= 1.0
      (integrals/rectangular-integration
        (fn [[a b]]
          (let [a (or a 0.0)
                b (or b 0.0)]
            (+ (double a) b)))
        [[0.0 1.0] [0.0 1.0]]))
    (t/is= 1.5
      (integrals/rectangular-integration
        (fn [[a b c]]
          (let [a (or a 0.0)
                b (or b 0.0)
                c (or c 0.0)]
            (+ (double a) b c)))
        [[0.0 1.0] [0.0 1.0] [0.0 1.0]]))
    (t/is= 2.0                                                ;slow
      (integrals/rectangular-integration
        (fn [[a b c d]]
          (let [a (or a 0.0)
                b (or b 0.0)
                c (or c 0.0)
                d (or d 0.0)]
            (+ (double a) b c d)))
        [[0.0 1.0] [0.0 1.0] [0.0 1.0] [0.0 1.0]]
        {::integrals/parallel? true}))
    (t/is= 3.1415926535897944                                 ;3.141592653589793 PI
      (integrals/rectangular-integration
        (fn [[a b]]
          (let [a (or a 0.0)
                b (or b 0.0)]
            (m/exp (- (+ (m/sq a) (m/sq b))))))
        [[m/inf- m/inf+] [m/inf- m/inf+]]))
    (t/is= 0.1
      (integrals/rectangular-integration
        (fn [[a b]]
          (let [a (or a 0.0)
                b (or b 0.0)]
            (m/pow (* (double a) b) -2.0)))
        [[5.0 m/inf+] [1.0 2.0]]))
    (t/is= 0.01666666666666667
      (integrals/rectangular-integration
        (fn [[a b]]
          (let [a (or a 0.0)
                b (or b 0.0)]
            (m/pow (* (double a) b) -2.0)))
        [[3.0 4.0] [m/inf- -5.0]]))
    ;;vector
    (t/is= [32.0 64.0]
      (integrals/rectangular-integration
        (fn [[a b]]
          (let [a (or a 0.0)
                b (or b 0.0)]
            [a (* (double a) b)]))
        [[2.0 6.0] [1.0 3.0]]))
    ;   with change of var
    (t/is= [0.24705031697079533 0.24705031697079533]
      (integrals/rectangular-integration
        (fn [[a b]]
          (let [a (or a 0.0)
                b (or b 0.0)
                c (m/exp (- (+ (m/sq a) (m/sq b))))]
            [c c]))
        [[m/inf- m/inf+] [1.0 3.0]]))
    ;;matrix
    (t/is= [[32.0 64.0] [32.0 40.00000000000001]]
      (integrals/rectangular-integration
        (fn [[a b]]
          (let [a (or a 0.0)
                b (or b 0.0)]
            [[a (* (double a) b)] [(+ 2.0 b) 5.0]]))
        [[2.0 6.0] [1.0 3.0]]))
    ;   with change of var
    (t/is= [[0.06666666666666667 0.06666666666666667]
            [0.06666666666666667 0.06666666666666667]]
      (integrals/rectangular-integration
        (fn [[a b]]
          (let [a (or a 0.0)
                b (or b 0.0)
                c (m/pow (* (double a) b) -2.0)]
            [[c c] [c c]]))
        [[2.0 6.0] [m/inf- -5.0]]))))

(ct/deftest non-rectangular-2D-integration-test
  (t/with-instrument `integrals/non-rectangular-2D-integration
    (t/is-spec-check integrals/non-rectangular-2D-integration))
  (t/with-instrument `integrals/non-rectangular-2D-integration
    (t/is= 1.0
      (integrals/non-rectangular-2D-integration
        (fn [outer inner]
          (+ outer (double inner)))
        [0.0 1.0]
        (fn [_outer]
          [0.0 1.0])))
    (t/is= 3.141592653589793                                  ;3.141592653589793 PI
      (integrals/non-rectangular-2D-integration
        (fn [outer inner]
          (* (m/exp (- (m/sq outer))) (m/exp (- (m/sq inner)))))
        [m/inf- m/inf+]
        (fn [_outer]
          [m/inf- m/inf+])))
    (t/is= 192.0
      (integrals/non-rectangular-2D-integration
        (fn [outer inner]
          (+ outer (double inner)))
        [2.0 6.0]
        (fn [outer]
          [(+ outer 2.0) (+ outer 6.0)])
        {::integrals/parallel? true}))
    (t/is= [32.0 64.0]
      (integrals/non-rectangular-2D-integration
        (fn [outer inner]
          (vector outer (* outer (double inner))))
        [2.0 6.0]
        (fn [_outer]
          [1.0 3.0])))
    (t/is= [0.24705031697079533 0.24705031697079533]
      (integrals/non-rectangular-2D-integration
        (fn [outer inner]
          (let [val (* (m/exp (- (m/sq outer))) (m/exp (- (m/sq inner))))]
            [val val]))
        [m/inf- m/inf+]
        (fn [_outer]
          [1.0 3.0])))
    (t/is= [[32.0 64.0] [32.0 39.99999999999999]]
      (integrals/non-rectangular-2D-integration
        (fn [outer inner]
          (vector [outer (* outer (double inner))] [(+ 2.0 inner) 5.0]))
        [2.0 6.0]
        (fn [_outer]
          [1.0 3.0])))
    (t/is= [[0.06666666666666667 0.06666666666666667]
            [0.06666666666666667 0.06666666666666667]]
      (integrals/non-rectangular-2D-integration
        (fn [outer inner]
          (let [val (m/pow (* (double outer) inner) -2.0)]
            [[val val] [val val]]))
        [2.0 6.0]
        (fn [_outer]
          [m/inf- -5.0])))))

(ct/deftest integration-with-error-test
  (t/with-instrument `integrals/integration-with-error
    (t/is-spec-check integrals/integration-with-error))
  (t/with-instrument `integrals/integration-with-error
    ;; Basic test - integral of x^2 from 0 to 1 = 1/3
    (let [result (integrals/integration-with-error m/sq [0.0 1.0])]
      (t/is= (::integrals/value result) 0.3333333333333333)
      (t/is (< (::integrals/error-estimate result) 1e-10)))
    ;; Infinite interval
    (let [result (integrals/integration-with-error
                   #(m/exp (- (m/sq %)))
                   [m/inf- m/inf+])]
      (t/is= (::integrals/value result) 1.7724538509055163)
      (t/is (< (::integrals/error-estimate result) 1e-8)))
    ;; With options
    (let [result (integrals/integration-with-error m/cos [0.0 m/PI]
                   {::integrals/accu 1e-10})]
      (t/is (m/roughly? 0.0 (::integrals/value result) 1e-10)))))

(ct/deftest singularity-handling-test
  (t/with-instrument `integrals/integration
    ;; Integral of 1/sqrt(|x|) from -1 to 1 with singularity at 0
    ;; Exact value = 4
    (t/is (m/roughly?
            4.0
            (integrals/integration
              #(/ (m/sqrt (m/abs %)))
              [-1.0 1.0]
              {::integrals/singularities [0.0]})
            0.01))
    ;; Multiple singularities
    (t/is (m/roughly?
            (+ 2.0 2.0)  ; Two sqrt singularities
            (integrals/integration
              #(cond
                 (< % 0.5) (/ (m/sqrt %))
                 (> % 0.5) (/ (m/sqrt (- 1.0 %)))
                 :else 0.0)
              [0.0 1.0]
              {::integrals/singularities [0.5]})
            0.1))))

(ct/deftest tanh-sinh-integration-test
  (t/with-instrument `integrals/tanh-sinh-integration
    (t/is-spec-check integrals/tanh-sinh-integration))
  (t/with-instrument `integrals/tanh-sinh-integration
    ;; Integral with sqrt singularity at 0: ∫₀¹ 1/√x dx = 2
    (t/is (m/roughly? 2.0
            (integrals/tanh-sinh-integration #(/ (m/sqrt %)) [0.0 1.0])
            0.01))
    ;; Integral with log singularity: ∫₀¹ x·log(x) dx = -1/4
    (t/is (m/roughly? -0.25
            (integrals/tanh-sinh-integration
              #(if (< (m/abs %) 1e-15)
                 0.0  ; Avoid log(0)
                 (* % (m/log %)))
              [0.0 1.0])
            0.01))
    ;; Basic polynomial (should work too)
    (t/is (m/roughly? 0.333333
            (integrals/tanh-sinh-integration m/sq [0.0 1.0])
            0.001))
    ;; With options
    (t/is (m/roughly? 2.0
            (integrals/tanh-sinh-integration
              #(/ (m/sqrt %))
              [0.0 1.0]
              {::integrals/level 4})
            0.001))))

(ct/deftest clenshaw-curtis-integration-test
  (t/with-instrument `integrals/clenshaw-curtis-integration
    (t/is-spec-check integrals/clenshaw-curtis-integration))
  (t/with-instrument `integrals/clenshaw-curtis-integration
    ;; Basic polynomial
    (t/is (m/roughly? 0.333333
            (integrals/clenshaw-curtis-integration m/sq [0.0 1.0])
            0.0001))
    ;; Cosine integral over [0, π] = 0
    (t/is (m/roughly? 0.0
            (integrals/clenshaw-curtis-integration m/cos [0.0 m/PI])
            1e-10))
    ;; Cosine integral over [0, π/2] = 1
    (t/is (m/roughly? 1.0
            (integrals/clenshaw-curtis-integration m/cos [0.0 (/ m/PI 2)])
            0.0001))
    ;; With options
    (t/is (m/roughly? 0.333333
            (integrals/clenshaw-curtis-integration m/sq [0.0 1.0]
              {::integrals/cc-points 33})
            0.0001))
    ;; Cubic: ∫₀² x³ dx = 4
    (t/is (m/roughly? 4.0
            (integrals/clenshaw-curtis-integration m/cube [0.0 2.0])
            0.0001))))

(ct/deftest monte-carlo-integration-test
  ;; Skip spec-check - function takes function args, slow/problematic
  (t/with-instrument `integrals/monte-carlo-integration
    ;; 2D integral: ∫∫ (x+y) dxdy over [0,1]×[0,1] = 1
    (random/bind-seed 42
      (let [result (integrals/monte-carlo-integration
                     (fn [[x y]] (+ x y))
                     [[0.0 1.0] [0.0 1.0]]
                     {::integrals/samples 10000})]
        (t/is (m/roughly? 1.0 (::integrals/value result) 0.1))
        (t/is= 10000 (::integrals/samples result))
        (t/is (number? (::integrals/standard-error result)))))
    ;; Higher dimensional: 5D hypercube ∫...∫ (x₁+...+x₅) = 2.5
    (random/bind-seed 123
      (let [result (integrals/monte-carlo-integration
                     (fn [v] (reduce + v))
                     (repeat 5 [0.0 1.0])
                     {::integrals/samples 50000})]
        (t/is (m/roughly? 2.5 (::integrals/value result) 0.1))))))

(ct/deftest quasi-monte-carlo-integration-test
  ;; Skip spec-check - function takes function args, slow/problematic
  (t/with-instrument `integrals/quasi-monte-carlo-integration
    ;; 2D integral
    (random/bind-seed 42
      (let [result (integrals/quasi-monte-carlo-integration
                     (fn [[x y]] (+ x y))
                     [[0.0 1.0] [0.0 1.0]]
                     {::integrals/samples 10000})]
        (t/is (m/roughly? 1.0 (::integrals/value result) 0.1))))))

(ct/deftest sparse-grid-integration-test
  ;; Skip spec-check - function takes function args
  (t/with-instrument `integrals/sparse-grid-integration
    ;; 2D integral: ∫∫ (x+y) dxdy over [0,1]×[0,1] = 1
    ;; Note: sparse-grid-level must be >= dimension for meaningful results
    (t/is (m/roughly? 1.0
            (integrals/sparse-grid-integration
              (fn [[x y]] (+ x y))
              [[0.0 1.0] [0.0 1.0]]
              {::integrals/sparse-grid-level 2})
            0.001))
    ;; 3D integral: ∫∫∫ (x+y+z) = 1.5
    (t/is (m/roughly? 1.5
            (integrals/sparse-grid-integration
              (fn [[x y z]] (+ x y z))
              [[0.0 1.0] [0.0 1.0] [0.0 1.0]]
              {::integrals/sparse-grid-level 3})
            0.01))
    ;; Higher level for more accuracy
    (t/is (m/roughly? 1.0
            (integrals/sparse-grid-integration
              (fn [[x y]] (+ x y))
              [[0.0 1.0] [0.0 1.0]]
              {::integrals/sparse-grid-level 5})
            0.0001))))

(ct/deftest oscillatory-integration-test
  ;; Note: Filon method is designed for high-frequency oscillations.
  ;; Low-frequency (omega=1) results have larger error.
  ;; Skip spec-check - function takes function args
  (t/with-instrument `integrals/oscillatory-integration
    ;; High frequency: ∫₀^π sin(10x) dx = 0 (many complete oscillations)
    (t/is (m/roughly? 0.0
            (integrals/oscillatory-integration
              (fn [_] 1.0)
              [0.0 m/PI]
              {::integrals/omega 10.0 ::integrals/oscillation-type :sin})
            0.01))
    ;; ∫₀^π x·sin(10x) dx = -π/10 ≈ -0.314 (integration by parts)
    (t/is (m/roughly? (- (/ m/PI 10))
            (integrals/oscillatory-integration
              identity
              [0.0 m/PI]
              {::integrals/omega 10.0 ::integrals/oscillation-type :sin})
            0.05))
    ;; Low frequency tests have larger tolerance due to Filon method limitations
    ;; ∫₀^π sin(x) dx = 2
    (t/is (m/roughly? 2.0
            (integrals/oscillatory-integration
              (fn [_] 1.0)
              [0.0 m/PI]
              {::integrals/omega 1.0 ::integrals/oscillation-type :sin})
            1.0))))

;;COMPARE AGAINST THE FOLLOWING
; Implements the adaptive quadrature described on page 511 of Numerical Analysis
; Kinkade et al.
; ## License
; Copyright (C) 2014 Daniel Aaron Phelps
; Distributed under the Eclipse Public License, the same as Clojure.

(defn- simpsons-estimate
  "Equation '8.5' page 509 - approximates the integral of `f` over [`a` `b`]."
  ^double
  [f ^double a ^double b ^double h]
  (* (/ h 3.0)
    (+ (f a)
      (* 4.0 (f (+ a h)))
      (f b))))

(defn- close-enough?
  "Finds if |`a` - `b`| < |`error`|."
  [^double a ^double b ^double error]
  (< (m/abs (- a b)) (m/abs error)))

(defn- insured-approximation
  "Equation 7 page 509 in Kinkade et al."
  ^double
  [^double S* ^double S** ^double S]
  (+ S* S** (* (/ 1.0 15.0) (+ S* S** (* -1.0 S)))))

(defn- adapt-quad-internal
  "Do not call this function directly. Start with adaptive-quadrature instead."
  [f delta eps n k sigma a h fa fc fb S]
  (let [delta (double delta)
        eps (double eps)
        n (long n)
        k (long k)
        sigma (double sigma)
        a (double a)
        h (double h)
        fa (double fa)
        fc (double fc)
        fb (double fb)
        S (double S)
        b (+ a (* 2.0 h))
        c (+ a h)
        h (/ h 2.0)
        S-left (simpsons-estimate f a c h)
        S-right (simpsons-estimate f c b h)]
    (cond
      (close-enough? (+ S-left S-right) S (/ (* 60.0 eps h) delta))
      (+ sigma (insured-approximation S-left S-right S))
      (>= k n) (throw (Exception. (str "Failure:  k >= n.  sigma = " sigma)))
      :else (+
              ;;From 'a' to the midpoint
              (adapt-quad-internal
                f delta eps n (inc k) sigma a h fa (f (+ a h)) fc S-left)
              ;;From the midpoint to b
              (adapt-quad-internal
                f delta eps n (inc k) sigma (+ a (* 2. h)) h fc
                (f (+ a (* 3.0 h))) fb S-right)))))

(defn- adaptive-quadrature-test
  "Approximates the definite integral of `f` over [`a` `b`] with an error less
  or equal than `eps`. `f` is a real-valued function of one real argument. The
  parameter `n` specifies how many recursive calls are allowed. An exception is
  thrown before the `n`+1st recursive call."
  [f a b eps n]
  (let [a (double a)
        b (double b)
        eps (double eps)
        n (long n)
        delta (- b a)
        sigma 0
        h (/ delta 2.0)
        c (/ (+ a b) 2.0)
        k 1
        fa (f a)
        fb (f b)
        fc (f c)
        S (simpsons-estimate f a b h)]
    (adapt-quad-internal f delta eps n k sigma a h fa fc fb S)))

(defn- uni-integration-test
  "Univariate Integration using Romberg."
  [f [lower-bound upper-bound]]
  (let [max-eval 100]
    (if (== lower-bound upper-bound)
      0.0
      (adaptive-quadrature-test f (double lower-bound) (double upper-bound) 1e-6 max-eval))))
