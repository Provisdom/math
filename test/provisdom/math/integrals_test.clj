(ns provisdom.math.integrals-test
  (:require
    [clojure.test :as ct]
    [provisdom.math.core :as m]
    [provisdom.math.integrals :as integrals]
    [provisdom.math.intervals :as intervals]
    [provisdom.math.random :as random]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anomalies]))

;;32 seconds

(set! *warn-on-reflection* true)

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
  ;;No instrumentation with function inputs
  (t/is-spec-check integrals/integration {:num-tests 10})
  (t/with-instrument `integrals/integration
    ;;ordinary
    (t/is= 69.33333333333331 (integrals/integration m/sq [2.0 6.0]))
    (t/is= 3.333333333333334E299 (integrals/integration m/sq [m/tiny-dbl 1e100]))
    (t/is= 2.886579864025407E-15 (integrals/integration m/cos [m/PI (* 5 m/PI)]))
    (t/is= -1.000000000000004 (integrals/integration m/cos [m/PI (* 5.5 m/PI)]))
    (t/is= 3.3306690738754696E-16 (integrals/integration m/cos [m/PI (* 6 m/PI)]))
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
        {::integrals/parallel? true})))
  ;; Tests with singular functions - skip instrumentation since functions have
  ;; domain restrictions that spec generators can't know about
  ;; Integral of 1/sqrt(|x|) from -1 to 1 with singularity at 0 = 4
  (t/is-approx= 4.0
    (integrals/integration
      #(/ (m/sqrt (m/abs %)))
      [-1.0 1.0]
      {::integrals/singularities [0.0]})
    :tolerance 0.01))

(ct/deftest rectangular-integration-test
  ;;No instrumentation with function inputs
  (t/is-spec-check integrals/rectangular-integration {:num-tests 10})
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
    (t/is= 2.0                                              ;slow
      (integrals/rectangular-integration
        (fn [[a b c d]]
          (let [a (or a 0.0)
                b (or b 0.0)
                c (or c 0.0)
                d (or d 0.0)]
            (+ (double a) b c d)))
        [[0.0 1.0] [0.0 1.0] [0.0 1.0] [0.0 1.0]]
        {::integrals/parallel? true}))
    (t/is= 3.1415926535897944                               ;3.141592653589793 PI
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
  ;; Spec-check skipped: functions with function arguments cause unreliable
  ;; generative testing due to fspec validation issues
  (t/with-instrument `integrals/non-rectangular-2D-integration
    (t/is= 1.0
      (integrals/non-rectangular-2D-integration
        (fn [outer inner]
          (+ outer (double inner)))
        [0.0 1.0]
        (fn [_outer]
          [0.0 1.0])))
    (t/is= 3.141592653589793                                ;3.141592653589793 PI
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
    (t/is-data-approx= [[0.06666666666666667 0.06666666666666667]
                        [0.06666666666666667 0.06666666666666667]]
      (integrals/non-rectangular-2D-integration
        (fn [outer inner]
          (let [val (m/pow (* (double outer) inner) -2.0)]
            [[val val] [val val]]))
        [2.0 6.0]
        (fn [_outer]
          [m/inf- -5.0])))))

(ct/deftest integration-with-error-test
  ;;No instrumentation with function inputs
  (t/is-spec-check integrals/integration-with-error {:num-tests 20})
  (t/with-instrument `integrals/integration-with-error
    ;; Basic test - integral of x^2 from 0 to 1 = 1/3
    (let [result (integrals/integration-with-error m/sq [0.0 1.0])]
      (t/is-approx= (::integrals/value result) 0.3333333333333333)
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
      (t/is-approx= 0.0 (::integrals/value result) :tolerance 1e-10))))

(ct/deftest tanh-sinh-integration-test
  (t/with-instrument `integrals/tanh-sinh-integration
    (t/is-spec-check integrals/tanh-sinh-integration))
  (t/with-instrument `integrals/tanh-sinh-integration
    ;; Basic polynomial (should work too)
    (t/is-approx= 0.333333
      (integrals/tanh-sinh-integration m/sq [0.0 1.0])
      :tolerance 0.001))
  ;; Tests with singular functions - skip instrumentation since functions have
  ;; domain restrictions that spec generators can't know about
  ;; Integral with log singularity: ∫₀¹ x·log(x) dx = -1/4
  (t/is-approx= -0.25
    (integrals/tanh-sinh-integration
      #(if (< (m/abs %) 1e-15)
         0.0                                                ; Avoid log(0)
         (* % (m/log %)))
      [0.0 1.0])
    :tolerance 0.01))
;; Integral with sqrt singularity at 0: ∫₀¹ 1/√x dx = 2
(t/is-approx= 2.0
  (integrals/tanh-sinh-integration #(/ (m/sqrt %)) [0.0 1.0])
  :tolerance 0.01)
;; With options
(t/is-approx= 2.0
  (integrals/tanh-sinh-integration
    #(/ (m/sqrt %))
    [0.0 1.0]
    {::integrals/level 4})
  :tolerance 0.001)

(ct/deftest clenshaw-curtis-integration-test
  ;;No instrumentation with function inputs
  (t/is-spec-check integrals/clenshaw-curtis-integration {:num-tests 3})
  (t/with-instrument `integrals/clenshaw-curtis-integration
    ;; Basic polynomial
    (t/is-approx= 0.333333
      (integrals/clenshaw-curtis-integration m/sq [0.0 1.0])
      :tolerance 0.0001)
    ;; Cosine integral over [0, π] = 0
    (t/is-approx= 0.0
      (integrals/clenshaw-curtis-integration m/cos [0.0 m/PI])
      :tolerance 1e-10)
    ;; Cosine integral over [0, π/2] = 1
    (t/is-approx= 1.0
      (integrals/clenshaw-curtis-integration m/cos [0.0 (/ m/PI 2)])
      :tolerance 0.0001)
    ;; With options
    (t/is-approx= 0.333333
      (integrals/clenshaw-curtis-integration m/sq [0.0 1.0]
        {::integrals/cc-points 33})
      :tolerance 0.0001)
    ;; Cubic: ∫₀² x³ dx = 4
    (t/is-approx= 4.0
      (integrals/clenshaw-curtis-integration m/cube [0.0 2.0])
      :tolerance 0.0001)))

(ct/deftest monte-carlo-integration-test
  ;;No instrumentation with function inputs
  ;; 2D integral: ∫∫ (x+y) dx*dy over [0,1]×[0,1] = 1
  (t/is-spec-check integrals/monte-carlo-integration {:num-tests 3})
  (t/with-instrument 'integrals/monte-carlo-integration
    (random/bind-seed 42
      (let [result (integrals/monte-carlo-integration
                     (fn [[x y]] (+ x y))
                     [[0.0 1.0] [0.0 1.0]]
                     {::integrals/samples 10000})]
        (t/is-approx= 1.0 (::integrals/value result) :tolerance 0.1)
        (t/is= 10000 (::integrals/samples result))
        (t/is (number? (::integrals/standard-error result)))))
    ;; Higher dimensional: 5D hypercube ∫...∫ (x₁+...+x₅) = 2.5
    (random/bind-seed 123
      (let [result (integrals/monte-carlo-integration
                     (fn [v] (reduce + v))
                     (repeat 5 [0.0 1.0])
                     {::integrals/samples 50000})]
        (t/is-approx= 2.5 (::integrals/value result) :tolerance 0.1)))))

(ct/deftest quasi-monte-carlo-integration-test
  ;;No instrumentation with function inputs
  ;; Halton sequences are deterministic, so no bind-seed needed
  ;; 2D integral: ∫∫ (x+y) dx*dy over [0,1]×[0,1] = 1
  (t/is-spec-check integrals/quasi-monte-carlo-integration {:num-tests 10})
  (t/with-instrument 'integrals/quasi-monte-carlo-integration
    (let [result (integrals/quasi-monte-carlo-integration
                   (fn [[x y]] (+ x y))
                   [[0.0 1.0] [0.0 1.0]]
                   {::integrals/samples 10000})]
      (t/is-approx= 1.0 (::integrals/value result) :tolerance 0.05)
      (t/is= 10000 (::integrals/samples result))
      (t/is (number? (::integrals/standard-error result))))
    ;; Higher dimensional: 5D hypercube ∫...∫ (x₁+...+x₅) = 2.5
    (let [result (integrals/quasi-monte-carlo-integration
                   (fn [v] (reduce + v))
                   (repeat 5 [0.0 1.0])
                   {::integrals/samples 50000})]
      (t/is-approx= 2.5 (::integrals/value result) :tolerance 0.05))
    ;; Test skip option (skip initial points)
    (let [result (integrals/quasi-monte-carlo-integration
                   (fn [[x]] (m/sq x))
                   [[0.0 1.0]]
                   {::integrals/samples 1000 ::integrals/skip 100})]
      (t/is-approx= 0.333333 (::integrals/value result) :tolerance 0.05))))

(ct/deftest sparse-grid-integration-test
  ;; 2D integral: ∫∫ (x+y) dx*dy over [0,1]×[0,1] = 1
  ;; Note: sparse-grid-level must be >= dimension for meaningful results
  (t/with-instrument 'integrals/sparse-grid-integration
    (t/is-spec-check integrals/sparse-grid-integration))
  (t/with-instrument 'integrals/sparse-grid-integration
    (t/is-approx= 1.0
      (integrals/sparse-grid-integration
        (fn [[x y]] (+ x y))
        [[0.0 1.0] [0.0 1.0]]
        {::integrals/sparse-grid-level 2})
      :tolerance 0.001)
    ;; 3D integral: ∫∫∫ (x+y+z) = 1.5
    (t/is-approx= 1.5
      (integrals/sparse-grid-integration
        (fn [[x y z]] (+ x y z))
        [[0.0 1.0] [0.0 1.0] [0.0 1.0]]
        {::integrals/sparse-grid-level 3})
      :tolerance 0.01)
    ;; Higher level for more accuracy
    (t/is-approx= 1.0
      (integrals/sparse-grid-integration
        (fn [[x y]] (+ x y))
        [[0.0 1.0] [0.0 1.0]]
        {::integrals/sparse-grid-level 5})
      :tolerance 0.0001)))

(ct/deftest oscillatory-integration-test
  ;; Note: Filon method is designed for high-frequency oscillations.
  ;; Low-frequency (omega=1) results have larger error.
  ;; High frequency: ∫₀^π sin(10x) dx = 0 (many complete oscillations)
  (t/with-instrument 'integrals/oscillatory-integration
    (t/is-spec-check integrals/oscillatory-integration))
  (t/with-instrument 'integrals/oscillatory-integration
    (t/is-approx= 0.0
      (integrals/oscillatory-integration
        (fn [_] 1.0)
        [0.0 m/PI]
        {::integrals/omega 10.0 ::integrals/oscillation-type :sin})
      :tolerance 0.01)
    ;; ∫₀^π x·sin(10x) dx = -π/10 ≈ -0.314 (integration by parts)
    (t/is-approx= (- (/ m/PI 10))
      (integrals/oscillatory-integration
        identity
        [0.0 m/PI]
        {::integrals/omega 10.0 ::integrals/oscillation-type :sin})
      :tolerance 0.05)
    ;; Low frequency tests have larger tolerance due to Filon method limitations
    ;; ∫₀^π sin(x) dx = 2
    (t/is-approx= 2.0
      (integrals/oscillatory-integration
        (fn [_] 1.0)
        [0.0 m/PI]
        {::integrals/omega 1.0 ::integrals/oscillation-type :sin})
      :tolerance 1.0)))
