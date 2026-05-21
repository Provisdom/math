(ns provisdom.math.integrals-test
  (:require
    [provisdom.math.core :as m]
    [provisdom.math.integrals :as integrals]
    [provisdom.math.intervals :as intervals]
    [provisdom.math.random :as random]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anomalies]))

;;40 seconds

(set! *warn-on-reflection* true)

;;;NUMERICAL INTEGRATION
(t/deftest change-of-variable-test
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

(t/deftest integration-test
  (t/with-instrument `integrals/integration
    ;;a bit slow -- ME
    (t/is-spec-check integrals/integration {:num-tests 150}))
  ;;no instrumentation; Orchestra fspec validation fails with fn args -- ME
  ;;ordinary
  ;;Math 69.333
  (t/is= 69.33333333333331 (integrals/integration m/sq [2.0 6.0]))
  ;;Math 3.3333e+299
  (t/is= 3.333333333333334E299 (integrals/integration m/sq [m/tiny-dbl 1e100]))
  (t/is-approx= 0.0 (integrals/integration m/cos [m/PI (* 5 m/PI)]) :tolerance 1e-12)
  (t/is-approx= -1.0 (integrals/integration m/cos [m/PI (* 5.5 m/PI)]) :tolerance 1e-12)
  (t/is-approx= 0.0 (integrals/integration m/cos [m/PI (* 6 m/PI)]) :tolerance 1e-12)
  (t/is= {::anomalies/category ::anomalies/no-solve
          ::anomalies/fn       #'provisdom.math.integrals/adaptive-quadrature
          ::anomalies/message  "Error contains NaN. Value: NaN"}
    (integrals/integration m/cos [m/PI m/inf+]))
  (t/is= 0.2 (integrals/integration #(m/pow % -2.0) [5.0 m/inf+]))
  (t/is= 0.19999999999999996 (integrals/integration #(m/pow % -2.0) [m/inf- -5.0]))
  ;;Math 1.7724
  (t/is-approx= (m/sqrt m/PI)
    (integrals/integration #(m/exp (- (m/sq %))) [m/inf- m/inf+])
    :tolerance 1e-12)
  ;;vector
  ;;Math 16.000, 69.333
  (t/is= [15.999999999999998 69.33333333333333]
    (integrals/integration #(vector % (m/sq %)) [2.0 6.0]))
  ;; with change of var
  ;;Math 1.7724, 1.8128
  (t/is-data-approx= [(m/sqrt m/PI) 1.8128049541109543]
    (integrals/integration #(vector (m/exp (- (m/sq %))) (m/exp (- (m/pow % 4))))
      [m/inf- m/inf+])
    :tolerance 1e-12)
  ;;matrix
  ;;Math 16.000, 69.333, 24.000, 20.000
  (t/is= [[15.999999999999998 69.33333333333334] [24.0 19.999999999999996]]
    (integrals/integration #(vector [% (m/sq %)] [(+ 2.0 %) 5.0]) [2.0 6.0]))
  ;; with change of var
  ;;Math 6.4000e-5, 0.0026666
  (t/is-data-approx= [[6.4e-5 (/ 4.0 1500.0)] [0.2 0.2]]
    (integrals/integration #(vector [(m/pow % -6) (m/pow % -4)] [(m/pow % -2.0) (m/pow % -2.0)])
      [5.0 m/inf+]
      {::integrals/parallel? true})
    :tolerance 1e-12)
  ;; Tests with singular functions - skip instrumentation since functions have
  ;; domain restrictions that spec generators can't know about
  ;; Integral of 1/sqrt(|x|) from -1 to 1 with singularity at 0 = 4
  (t/is-approx= 4.0
    (integrals/integration #(/ (m/sqrt (m/abs %))) [-1.0 1.0] {::integrals/singularities [0.0]}))
  ;; polynomial with known closed-form solutions
  ;; ∫₀¹ x³ dx = 1/4
  (t/is-approx= 0.25 (integrals/integration (fn [x] (m/pow x 3)) [0.0 1.0]) :tolerance 1e-10)
  ;; ∫₀² (x² + 2x + 1) dx = [x³/3 + x² + x]₀² = 8/3 + 4 + 2 = 26/3 ≈ 8.667
  ;;Math 8.6666
  (t/is-approx= (/ 26.0 3)
    (integrals/integration (fn [x] (+ (* x x) (* 2 x) 1)) [0.0 2.0]) :tolerance 1e-10)
  ;; singularity at endpoint: ∫₀¹ 1/√x dx = 2
  (t/is-approx= 2.0
    (integrals/integration
      #(/ (m/sqrt (max 1e-15 (m/abs %))))                   ; avoid exact zero
      [0.0 1.0]))
  ;; transcendental functions
  ;; ∫₀^π sin²(x) dx = π/2
  ;;Math 1.5707
  (t/is-approx= m/half-pi
    (integrals/integration (fn [x] (m/sq (m/sin x))) [0.0 m/PI]) :tolerance 1e-10)
  ;; ∫₀¹ e^(-x) dx = 1 - 1/e ≈ 0.6321
  ;;Math 0.63212
  (t/is-approx= (m/one- (/ m/E))
    (integrals/integration (fn [x] (m/exp (- x))) [0.0 1.0]) :tolerance 1e-10)
  ;; very small intervals
  ;; ∫₀^(1e-8) x² dx = (1e-8)³/3 = 3.33e-25
  ;;Math 3.3333e-25
  (t/is-approx= 3.333333333333e-25 (integrals/integration m/sq [0.0 1e-8]) :tolerance 1e-27)
  ;; ∫₀^(1e-6) sin(x) dx ≈ (1e-6)²/2 = 5e-13 (since sin(x) ≈ x for small x)
  ;; Actually, ∫sin(x)dx = -cos(x), so ∫₀^a = 1 - cos(a) ≈ a²/2 for small a
  ;;Math 4.9999e-13
  (t/is-approx= 5e-13 (integrals/integration m/sin [0.0 1e-6]) :tolerance 1e-15))

(t/deftest rectangular-integration-test
  (t/with-instrument `integrals/rectangular-integration
    ;; multi-dim Gauss-Kronrod over rectangular
    ;; regions is expensive per case; -- ME
    (t/is-spec-check integrals/rectangular-integration {:num-tests 60}))
  ;;no instrumentation; Orchestra fspec validation fails with fn args -- ME
  (t/is= 1.0
    (integrals/rectangular-integration (fn [[a b]]
                                         (let [a (or a 0.0)
                                               b (or b 0.0)]
                                           (+ (double a) b)))
      [[0.0 1.0] [0.0 1.0]]))
  (t/is= 1.5
    (integrals/rectangular-integration (fn [[a b c]]
                                         (let [a (or a 0.0)
                                               b (or b 0.0)
                                               c (or c 0.0)]
                                           (+ (double a) b c)))
      [[0.0 1.0] [0.0 1.0] [0.0 1.0]]))
  (t/is= 2.0                                                ;slow
    (integrals/rectangular-integration (fn [[a b c d]]
                                         (let [a (or a 0.0)
                                               b (or b 0.0)
                                               c (or c 0.0)
                                               d (or d 0.0)]
                                           (+ (double a) b c d)))
      [[0.0 1.0] [0.0 1.0] [0.0 1.0] [0.0 1.0]]
      {::integrals/parallel? true}))
  (t/is-approx= m/PI                                        ;3.141592653589793
    (integrals/rectangular-integration (fn [[a b]]
                                         (let [a (or a 0.0)
                                               b (or b 0.0)]
                                           (m/exp (- (+ (m/sq a) (m/sq b))))))
      [[m/inf- m/inf+] [m/inf- m/inf+]])
    :tolerance 1e-12)
  (t/is= 0.1
    (integrals/rectangular-integration (fn [[a b]]
                                         (let [a (or a 0.0)
                                               b (or b 0.0)]
                                           (m/pow (* (double a) b) -2.0)))
      [[5.0 m/inf+] [1.0 2.0]]))
  ;;Math 0.016666
  (t/is-approx= (/ 60.0)
    (integrals/rectangular-integration (fn [[a b]]
                                         (let [a (or a 0.0)
                                               b (or b 0.0)]
                                           (m/pow (* (double a) b) -2.0)))
      [[3.0 4.0] [m/inf- -5.0]])
    :tolerance 1e-12)
  ;;vector
  (t/is= [32.0 64.0]
    (integrals/rectangular-integration (fn [[a b]]
                                         (let [a (or a 0.0)
                                               b (or b 0.0)]
                                           [a (* (double a) b)]))
      [[2.0 6.0] [1.0 3.0]]))
  ;; with change of var
  ;;SciPy 0.24705
  (t/is-data-approx= [0.24705031697079533 0.24705031697079533]
    (integrals/rectangular-integration (fn [[a b]]
                                         (let [a (or a 0.0)
                                               b (or b 0.0)
                                               c (m/exp (- (+ (m/sq a) (m/sq b))))]
                                           [c c]))
      [[m/inf- m/inf+] [1.0 3.0]])
    :tolerance 1e-12)
  ;;matrix
  (t/is= [[32.0 64.0] [32.0 40.00000000000001]]
    (integrals/rectangular-integration (fn [[a b]]
                                         (let [a (or a 0.0)
                                               b (or b 0.0)]
                                           [[a (* (double a) b)] [(+ 2.0 b) 5.0]]))
      [[2.0 6.0] [1.0 3.0]]))
  ;; with change of var
  ;;Math 0.066666
  (t/is= [[0.06666666666666667 0.06666666666666667]
          [0.06666666666666667 0.06666666666666667]]
    (integrals/rectangular-integration (fn [[a b]]
                                         (let [a (or a 0.0)
                                               b (or b 0.0)
                                               c (m/pow (* (double a) b) -2.0)]
                                           [[c c] [c c]]))
      [[2.0 6.0] [m/inf- -5.0]])))

(t/deftest non-rectangular-2D-integration-test
  ;; Spec-check skipped: two fn args cause exponentially complex generator combinations -- ME
  ;;no instrumentation; Orchestra fspec validation fails with fn args -- ME
  (t/is= 1.0
    (integrals/non-rectangular-2D-integration (fn [outer inner]
                                                (+ outer (double inner)))
      [0.0 1.0]
      (fn [_outer] [0.0 1.0])))
  (t/is-approx= m/PI                                        ;3.141592653589793
    (integrals/non-rectangular-2D-integration
      (fn [outer inner]
        (* (m/exp (- (m/sq outer))) (m/exp (- (m/sq inner)))))
      [m/inf- m/inf+]
      (fn [_outer] [m/inf- m/inf+]))
    :tolerance 1e-12)
  (t/is= 192.0
    (integrals/non-rectangular-2D-integration (fn [outer inner]
                                                (+ outer (double inner)))
      [2.0 6.0]
      (fn [outer]
        [(+ outer 2.0) (+ outer 6.0)])
      {::integrals/parallel? true}))
  (t/is= [32.0 64.0]
    (integrals/non-rectangular-2D-integration (fn [outer inner]
                                                (vector outer (* outer (double inner))))
      [2.0 6.0]
      (fn [_outer] [1.0 3.0])))
  ;;SciPy 0.24705
  (t/is-data-approx= [0.24705031697079533 0.24705031697079533]
    (integrals/non-rectangular-2D-integration
      (fn [outer inner]
        (let [val (* (m/exp (- (m/sq outer))) (m/exp (- (m/sq inner))))]
          [val val]))
      [m/inf- m/inf+]
      (fn [_outer] [1.0 3.0]))
    :tolerance 1e-12)
  (t/is= [[32.0 64.0] [32.0 39.99999999999999]]
    (integrals/non-rectangular-2D-integration
      (fn [outer inner]
        (vector [outer (* outer (double inner))] [(+ 2.0 inner) 5.0]))
      [2.0 6.0]
      (fn [_outer] [1.0 3.0])))
  ;;Math 0.066666
  (t/is-data-approx= [[0.06666666666666667 0.06666666666666667]
                      [0.06666666666666667 0.06666666666666667]]
    (integrals/non-rectangular-2D-integration (fn [outer inner]
                                                (let [val (m/pow (* (double outer) inner) -2.0)]
                                                  [[val val] [val val]]))
      [2.0 6.0]
      (fn [_outer] [m/inf- -5.0]))))

;;;INTEGRATION WITH ERROR ESTIMATE
(t/deftest integration-with-error-test
  (t/with-instrument `integrals/integration-with-error
    ;; tensor-valued integrands × adaptive Gauss-Kronrod iter-interval (up to 20) -- ME
    (t/is-spec-check integrals/integration-with-error {:num-tests 250}))
  ;;no instrumentation; Orchestra fspec validation fails with fn args -- ME
  ;; Basic test - integral of x^2 from 0 to 1 = 1/3
  ;;Math 0.33333
  (let [result (integrals/integration-with-error m/sq [0.0 1.0])]
    (t/is-approx= 0.3333333333333333 (::integrals/value result) :tolerance 1e-12)
    (t/is= 4.1359030627651384E-25 (::integrals/error-estimate result)))
  ;; Infinite interval
  ;;Math 1.7724
  (let [result (integrals/integration-with-error
                 #(m/exp (- (m/sq %)))
                 [m/inf- m/inf+])]
    (t/is-approx= (m/sqrt m/PI) (::integrals/value result) :tolerance 1e-12)
    (t/is-approx= 0.0 (::integrals/error-estimate result) :tolerance 1e-11))
  ;; With options
  (let [result (integrals/integration-with-error m/cos [0.0 m/PI] {::integrals/accu 1e-10})]
    (t/is-approx= 0.0 (::integrals/value result) :tolerance 1e-10)))

;;;TANH-SINH (DOUBLE EXPONENTIAL) QUADRATURE
(t/deftest tanh-sinh-integration-test
  (t/with-instrument `integrals/tanh-sinh-integration
    (t/is-spec-check integrals/tanh-sinh-integration))
  ;;no instrumentation; Orchestra fspec validation fails with fn args -- ME
  ;; Basic polynomial (should work too)
  ;;Math 0.33333
  (t/is-approx= (/ 1.0 3.0)
    (integrals/tanh-sinh-integration m/sq [0.0 1.0])
    :tolerance 1e-15)
  ;; Tests with singular functions - skip instrumentation since functions have
  ;; domain restrictions that spec generators can't know about
  ;; Integral with log singularity: ∫₀¹ x·log(x) dx = -1/4
  (t/is-approx= -0.25
    (integrals/tanh-sinh-integration #(if (< (m/abs %) 1e-15)
                                        0.0                 ; Avoid log(0)
                                        (* % (m/log %)))
      [0.0 1.0])
    :tolerance 1e-15)
  ;; Integral with sqrt singularity at 0: ∫₀¹ 1/√x dx = 2
  (t/is-approx= 2.0
    (integrals/tanh-sinh-integration #(/ (m/sqrt %)) [0.0 1.0])
    :tolerance 1e-7)
  ;; With options
  (t/is-approx= 2.0
    (integrals/tanh-sinh-integration #(/ (m/sqrt %)) [0.0 1.0] {::integrals/level 4})
    :tolerance 1e-7))

;;;CLENSHAW-CURTIS QUADRATURE
(t/deftest clenshaw-curtis-integration-test
  (t/with-instrument `integrals/clenshaw-curtis-integration
    ;; Clenshaw-Curtis with up to 65 ::cc-points and tensor-valued integrands; -- ME
    (t/is-spec-check integrals/clenshaw-curtis-integration {:num-tests 30}))
  ;;no instrumentation; Orchestra fspec validation fails with fn args -- ME
  ;; Basic polynomial
  ;;Math 0.33333
  (t/is-approx= (/ 3.0) (integrals/clenshaw-curtis-integration m/sq [0.0 1.0]) :tolerance 1e-14)
  ;; Cosine integral over [0, π] = 0
  (t/is-approx= 0.0 (integrals/clenshaw-curtis-integration m/cos [0.0 m/PI]) :tolerance 1e-10)
  ;; Cosine integral over [0, π/2] = 1
  (t/is-approx= 1.0 (integrals/clenshaw-curtis-integration m/cos [0.0 (/ m/PI 2)]) :tolerance 1e-14)
  ;; With options
  ;;Math 0.33333
  (t/is-approx= (/ 3.0)
    (integrals/clenshaw-curtis-integration m/sq [0.0 1.0] {::integrals/cc-points 33})
    :tolerance 1e-14)
  ;; Cubic: ∫₀² x³ dx = 4
  (t/is-approx= 4.0 (integrals/clenshaw-curtis-integration m/cube [0.0 2.0]) :tolerance 1e-14))

;;;MONTE CARLO INTEGRATION
(t/deftest monte-carlo-integration-test
  ;; 2D integral: ∫∫ (x+y) dx*dy over [0,1]×[0,1] = 1
  (t/with-instrument `integrals/monte-carlo-integration
    ;; default ::samples 10k × v->tensor fspec validation -- ME
    (t/is-spec-check integrals/monte-carlo-integration {:num-tests 20}))
  ;;no instrumentation; Orchestra fspec validation fails with fn args -- ME
  (random/bind-seed 42
    (let [result (integrals/monte-carlo-integration (fn [[x y]] (+ x y))
                   [[0.0 1.0] [0.0 1.0]] {::integrals/samples 10000})]
      (t/is= 0.9990666401666448 (::integrals/value result))
      (t/is= 10000 (::integrals/samples result))
      (t/is= 0.004070935864701599 (::integrals/standard-error result))))
  ;; Higher dimensional: 5D hypercube ∫...∫ (x₁+...+x₅) = 2.5
  (random/bind-seed 123
    (let [result (integrals/monte-carlo-integration (fn [v] (reduce + v))
                   (repeat 5 [0.0 1.0]) {::integrals/samples 50000})]
      (t/is= 2.500949055499439 (::integrals/value result)))))

(t/deftest quasi-monte-carlo-integration-test
  ;; Halton sequences are deterministic, so no bind-seed needed
  ;; 2D integral: ∫∫ (x+y) dx*dy over [0,1]×[0,1] = 1 -- ME
  (t/with-instrument `integrals/quasi-monte-carlo-integration
    (t/is-spec-check integrals/quasi-monte-carlo-integration {:num-tests 300}))
  ;;no instrumentation; Orchestra fspec validation fails with fn args -- ME
  (let [result (integrals/quasi-monte-carlo-integration (fn [[x y]] (+ x y))
                 [[0.0 1.0] [0.0 1.0]] {::integrals/samples 10000})]
    (t/is= 0.9997497317281152 (::integrals/value result))
    (t/is= 10000 (::integrals/samples result))
    (t/is= 0.00408236926163272 (::integrals/standard-error result)))
  ;; Higher dimensional: 5D hypercube ∫...∫ (x₁+...+x₅) = 2.5
  (let [result (integrals/quasi-monte-carlo-integration (fn [v] (reduce + v))
                 (repeat 5 [0.0 1.0]) {::integrals/samples 50000})]
    (t/is= 2.4997762582588825 (::integrals/value result)))
  ;; Test skip option (skip initial points)
  ;;Math 0.33333
  (let [result (integrals/quasi-monte-carlo-integration (fn [[x]] (m/sq x))
                 [[0.0 1.0]] {::integrals/samples 1000 ::integrals/skip 100})]
    (t/is= 0.3328543577194214 (::integrals/value result))))

;;;SPARSE GRID (SMOLYAK) INTEGRATION
(t/deftest sparse-grid-integration-test
  ;; 2D integral: ∫∫ (x+y) dx*dy over [0,1]×[0,1] = 1
  ;; Note: sparse-grid-level must be >= dimension for meaningful results
  (t/with-instrument `integrals/sparse-grid-integration
    (t/is-spec-check integrals/sparse-grid-integration))
  ;;no instrumentation; Orchestra fspec validation fails with fn args -- ME
  (t/is-approx= 1.0
    (integrals/sparse-grid-integration (fn [[x y]] (+ x y))
      [[0.0 1.0] [0.0 1.0]] {::integrals/sparse-grid-level 2})
    :tolerance 1e-14)
  ;; 3D integral: ∫∫∫ (x+y+z) = 1.5
  (t/is-approx= 1.5
    (integrals/sparse-grid-integration (fn [[x y z]] (+ x y z))
      [[0.0 1.0] [0.0 1.0] [0.0 1.0]] {::integrals/sparse-grid-level 3})
    :tolerance 1e-14)
  ;; Higher level for more accuracy
  (t/is-approx= 1.0
    (integrals/sparse-grid-integration (fn [[x y]] (+ x y))
      [[0.0 1.0] [0.0 1.0]] {::integrals/sparse-grid-level 5})
    :tolerance 1e-14))

;;;OSCILLATORY INTEGRATION
(t/deftest oscillatory-integration-test
  (t/with-instrument `integrals/oscillatory-integration
    (t/is-spec-check integrals/oscillatory-integration))
  ;;no instrumentation; Orchestra fspec validation fails with fn args -- ME
  ;; High frequency: ∫₀^π sin(10x) dx = 0 (many complete oscillations)
  (t/is-approx= 0.0
    (integrals/oscillatory-integration (fn [_] 1.0) [0.0 m/PI] {::integrals/omega            10.0
                                                                ::integrals/oscillation-type :sin})
    :tolerance 1e-14)
  ;; ∫₀^π x·sin(10x) dx = -π/10 ≈ -0.314 (integration by parts)
  ;;Math -0.31415
  (t/is-approx= (- (/ m/PI 10))
    (integrals/oscillatory-integration identity [0.0 m/PI] {::integrals/omega            10.0
                                                            ::integrals/oscillation-type :sin})
    :tolerance 1e-14)
  ;; ∫₀^π sin(x) dx = 2
  (t/is-approx= 2.0
    (integrals/oscillatory-integration (fn [_] 1.0) [0.0 m/PI] {::integrals/omega            1.0
                                                                ::integrals/oscillation-type :sin})
    :tolerance 1e-12)
  ;; High frequency oscillatory integration (omega=1000+)
  ;; omega=1000: ∫₀^π sin(1000x) dx ≈ 0 (many complete cycles)
  (t/is-approx= 0.0
    (integrals/oscillatory-integration (fn [_] 1.0) [0.0 m/PI] {::integrals/omega            1000.0
                                                                ::integrals/oscillation-type :sin})
    :tolerance 1e-15)
  ;; omega=1000: ∫₀^π x·sin(1000x) dx = -π/1000
  ;; Math: Integrate[x*Sin[1000*x], {x, 0, Pi}] = -Pi/1000
  (t/is-approx= (- (/ m/PI 1000.0))
    (integrals/oscillatory-integration identity [0.0 m/PI] {::integrals/omega            1000.0
                                                            ::integrals/oscillation-type :sin})
    :tolerance 1e-15)
  ;; omega=500 with cosine: ∫₀^(2π) cos(500x) dx = 0
  (t/is-approx= 0.0
    (integrals/oscillatory-integration (fn [_] 1.0)
      [0.0 (* 2.0 m/PI)]
      {::integrals/omega            500.0
       ::integrals/oscillation-type :cos})
    :tolerance 1e-14))

;;;GAUSS-HERMITE QUADRATURE
(t/deftest gauss-hermite-integration-test
  (t/with-instrument `integrals/gauss-hermite-integration
    (t/is-spec-check integrals/gauss-hermite-integration))
  ;;no instrumentation; Orchestra fspec validation fails with fn args -- ME
  ;; f(x)=1: integral exp(-x^2) dx = sqrt(pi)
  ;;Math 1.7724
  (t/is= 1.7724538509055157 (integrals/gauss-hermite-integration (constantly 1.0)))
  ;; f(x)=x^2: integral x^2 exp(-x^2) dx = sqrt(pi)/2
  ;;Math 0.88622
  (t/is= 0.8862269254527579 (integrals/gauss-hermite-integration (fn [x] (* x x))))
  ;; f(x)=x^4: integral x^4 exp(-x^2) dx = 3*sqrt(pi)/4
  ;;Math 1.3293
  (t/is= 1.329340388179136 (integrals/gauss-hermite-integration (fn [x] (m/pow x 4))))
  ;; Test with 10 points
  ;;Math 1.7724
  (t/is-approx= 1.7724538509055159
    (integrals/gauss-hermite-integration (constantly 1.0) {::integrals/gh-points 10})
    :tolerance 1e-12)
  ;; Test with 20 points
  ;;Math 1.7724
  (t/is-approx= 1.7724538509055159
    (integrals/gauss-hermite-integration (constantly 1.0) {::integrals/gh-points 20})
    :tolerance 1e-12))
