(ns provisdom.math.derivatives-test
  (:require
    [clojure.test :as ct]
    [provisdom.test.core :as t]
    [provisdom.math.core :as m]
    [provisdom.math.derivatives :as deriv]))

;;1 SECONDS

(set! *warn-on-reflection* true)

(defn der-f
  [number]
  (+ (m/exp number)
    (* 7 (m/pow number 6))
    (* 3 (m/pow number 3))
    number))

(ct/deftest derivative-fn-test
  (t/with-instrument `deriv/derivative-fn
    (t/is-spec-check deriv/derivative-fn))
  (t/with-instrument :all
    (t/is= 28922.598150033144
      ((deriv/derivative-fn der-f {::deriv/derivative 0}) 4.0))
    ;;first derivative: 131624.413159103
    (t/is= 131624.41317399498 ((deriv/derivative-fn der-f) 5.0))
    (t/is= 131624.41315653265 ((deriv/derivative-fn der-f {::deriv/h 1e-5}) 5.0))
    (t/is= 131624.41340682562 ((deriv/derivative-fn der-f {::deriv/h 1e-7}) 5.0))
    (t/is= 131624.4131885469 ((deriv/derivative-fn der-f {::deriv/accuracy 8}) 5.0))
    (t/is= 131624.41315944307 ((deriv/derivative-fn der-f {::deriv/type :forward}) 5.0))
    (t/is= 131624.4131885469 ((deriv/derivative-fn der-f {::deriv/type :backward}) 5.0))
    ;;second derivative: 131488.413159103
    (t/is= 131488.41360816732 ((deriv/derivative-fn der-f {::deriv/derivative 2}) 5.0))
    (t/is= 131488.41841029935
      ((deriv/derivative-fn der-f {::deriv/derivative 2
                                   ::deriv/h          1e-6})
       5.0))
    (t/is= 131488.41389920563
      ((deriv/derivative-fn der-f {::deriv/derivative 2
                                   ::deriv/h          1e-8})
       5.0))
    (t/is= 131488.41040674597
      ((deriv/derivative-fn der-f {::deriv/derivative 2
                                   ::deriv/type       :forward})
       5.0))
    ;;second deriv of log: -0.04
    (t/is= -0.03999999886872274
      ((deriv/derivative-fn m/log {::deriv/derivative 2
                                   ::deriv/accuracy   2})
       5.0))
    (t/is= -0.039999989986938544
      ((deriv/derivative-fn m/log {::deriv/derivative 2
                                   ::deriv/accuracy   8})
       5.0))
    ;;third deriv: 105166.413159103
    (t/is= 105166.41574213281 ((deriv/derivative-fn der-f {::deriv/derivative 3}) 5.0))
    (t/is= 105166.41574213281
      ((deriv/derivative-fn der-f {::deriv/derivative 3
                                   ::deriv/h          1e-7})
       5.0))
    (t/is= 105166.13838262855
      ((deriv/derivative-fn der-f {::deriv/derivative 3
                                   ::deriv/h          1e-9})
       5.0))
    (t/is= 105166.41574213281
      ((deriv/derivative-fn der-f {::deriv/derivative 3
                                   ::deriv/accuracy   6})
       5.0))
    ;;fourth deriv: 63148.413159103
    (t/is= 63148.41448329389 ((deriv/derivative-fn der-f {::deriv/derivative 4}) 5.0))
    (t/is= 63148.41448329389
      ((deriv/derivative-fn der-f {::deriv/derivative 4
                                   ::deriv/h          1e-7})
       5.0))
    (t/is= 63148.56000244617
      ((deriv/derivative-fn der-f {::deriv/derivative 4
                                   ::deriv/h          1e-9})
       5.0))
    (t/is= 63148.41448329389
      ((deriv/derivative-fn der-f {::deriv/derivative 4
                                   ::deriv/accuracy   4})
       5.0))
    (t/is= 63148.41448329389
      ((deriv/derivative-fn der-f {::deriv/derivative 4
                                   ::deriv/accuracy   6})
       5.0))
    ;;fifth deriv: 25348.41315903
    (t/is= 25348.41462038457 ((deriv/derivative-fn der-f {::deriv/derivative 5}) 5.0))
    (t/is= 25348.537368699905
      ((deriv/derivative-fn der-f {::deriv/derivative 5
                                   ::deriv/h          1e-8})
       5.0))
    (t/is= 25348.660635063425
      ((deriv/derivative-fn der-f {::deriv/derivative 5
                                   ::deriv/h          1e-6})
       5.0))
    ;;sixth deriv: 5188.4131591
    (t/is= 5188.451371035215 ((deriv/derivative-fn der-f {::deriv/derivative 6}) 5.0))
    ;;seventh deriv: 148.413159103
    (t/is= 148.4122348483652 ((deriv/derivative-fn der-f {::deriv/derivative 7}) 5.0))
    ;;eighth deriv: 148.413159103
    (t/is= 148.37917386236595 ((deriv/derivative-fn der-f {::deriv/derivative 8}) 5.0))
    ;;example issues requiring 'h'
    (t/is= 2.0E8 ((deriv/derivative-fn m/sq) 1e8))          ;good
    (t/is= 1.92E9 ((deriv/derivative-fn m/sq) 1e9))         ;bad
    (t/is= 3.2768E10 ((deriv/derivative-fn m/sq) 1e10))     ;ugly
    (t/is= 0.0 ((deriv/derivative-fn m/sq) 1e11))           ;crazy
    (t/is= 2.0E11 ((deriv/derivative-fn m/sq {::deriv/h 1e8}) 1e11)) ;fixed by setting h
    (t/is= 0.0 ((deriv/derivative-fn m/sq) 1e110))          ;crazy
    (t/is= 1.999999999999999E110
      ((deriv/derivative-fn m/sq {::deriv/h 1e108}) 1e110)))) ;fixed by setting h

(defn gf
  [[a b]]
  (if-not b
    m/nan
    (+ (* (double a) b)
      (* 2 (m/sq a))
      (m/cube b))))

(ct/deftest gradient-fn-test
  (t/with-instrument `deriv/gradient-fn
    (t/is-spec-check deriv/gradient-fn))
  (t/with-instrument :all
    (t/is= [16.000000002236447 51.0000000062405] ((deriv/gradient-fn gf) [3.0 4.0])))) ;[16,51]

(ct/deftest jacobian-fn-test
  (t/with-instrument `deriv/jacobian-fn
    (t/is-spec-check deriv/jacobian-fn))
  (t/with-instrument :all
    (t/is= [[16.00000000745058 51.00000000745058] [96.00000001490116 72.0]] ;[[16,51][96,72]]
      ((deriv/jacobian-fn
         (fn [[a b]]
           (if-not b
             [m/nan m/nan]
             [(+ (* (double a) b)
                (* 2 (m/sq a))
                (m/cube b))
              (+ (* (m/sq a) (m/sq b)))])))
       [3.0 4.0]))))

(ct/deftest hessian-fn-test
  (t/with-instrument `deriv/hessian-fn
    (t/is-spec-check deriv/hessian-fn))
  (t/with-instrument :all
    ;;type 'joint-central' is the default
    (t/is= [[4.00000004674439 1.00000003833145]
            [1.00000003833145 23.999999854140697]]          ;[[4,1][1,24]]
      ((deriv/hessian-fn gf) [3.0 4.0]))
    (t/is= [[4.0000021094456315 0.9999977191910148]
            [0.9999977191910148 24.000001423060894]]        ;[[4,1][1,24]]
      ((deriv/hessian-fn gf {::deriv/type :central}) [3.0 4.0]))
    (t/is= [[3.9999908739700913 0.9999977201223373]
            [0.9999977201223373 24.000035122036934]]        ;[[4,1][1,24]]
      ((deriv/hessian-fn gf {::deriv/type :forward}) [3.0 4.0]))
    (t/is= [[4.000035812146962 0.9999977182596922]
            [0.9999977182596922 23.999990183860064]]        ;[[4,1][1,24]]
      ((deriv/hessian-fn gf {::deriv/type :backward}) [3.0 4.0]))))

;;;PARTIAL DERIVATIVE TESTS
(defn fxy
  [x y]
  (+ (* (double x) x)
    (* 2.0 y y)
    (* (double x) y)))

(ct/deftest partial-derivative-x-of-fxy-test
  (t/with-instrument `deriv/partial-derivative-x-of-fxy
    (t/is-spec-check deriv/partial-derivative-x-of-fxy))
  (t/with-instrument :all
    (t/is= 8.999999998593466 ((deriv/partial-derivative-x-of-fxy fxy) 3.0 3.0)))) ;9

(ct/deftest partial-derivative-y-of-fxy-test
  (t/with-instrument `deriv/partial-derivative-y-of-fxy
    (t/is-spec-check deriv/partial-derivative-y-of-fxy))
  (t/with-instrument :all
    (t/is= 15.000000001208491 ((deriv/partial-derivative-y-of-fxy fxy) 3.0 3.0)))) ;15

(ct/deftest second-partial-derivative-xx-of-fxy-test
  (t/with-instrument `deriv/second-partial-derivative-xx-of-fxy
    (t/is-spec-check deriv/second-partial-derivative-xx-of-fxy))
  (t/with-instrument :all
    (t/is= 2.000000023372195 ((deriv/second-partial-derivative-xx-of-fxy fxy) 3.0 3.0)))) ;2

(ct/deftest second-partial-derivative-yy-of-fxy-test
  (t/with-instrument `deriv/second-partial-derivative-yy-of-fxy
    (t/is-spec-check deriv/second-partial-derivative-yy-of-fxy))
  (t/with-instrument :all
    (t/is= 3.999999975690116 ((deriv/second-partial-derivative-yy-of-fxy fxy) 3.0 3.0)))) ;4

(ct/deftest second-partial-derivative-xy-of-fxy-test
  (t/with-instrument `deriv/second-partial-derivative-xy-of-fxy
    (t/is-spec-check deriv/second-partial-derivative-xy-of-fxy))
  (t/with-instrument :all
    (t/is= 1.0000000294496658 ((deriv/second-partial-derivative-xy-of-fxy fxy) 3.0 3.0)))) ;1

;;;IMPROVED DERIVATIVE FUNCTIONS TESTS

(ct/deftest richardson-derivative-fn-test
  ;; sin'(0) = cos(0) = 1
  (t/is-approx= 1.0 ((deriv/richardson-derivative-fn m/sin) 0.0) :tolerance 1e-10)
  ;; sin'(PI) = cos(PI) = -1
  (t/is-approx= -1.0 ((deriv/richardson-derivative-fn m/sin) m/PI) :tolerance 1e-8)
  ;; cos'(0) = -sin(0) = 0
  (t/is-approx= 0.0 ((deriv/richardson-derivative-fn m/cos) 0.0) :tolerance 1e-10)
  ;; Second derivative: sin''(0) = -sin(0) = 0
  (t/is-approx= 0.0
          ((deriv/richardson-derivative-fn m/sin {::deriv/derivative 2}) 0.0)
          :tolerance 1e-8)
  ;; Richardson with more levels should be accurate
  (let [true-deriv (m/cos 1.0)
        richardson-6 ((deriv/richardson-derivative-fn m/sin {::deriv/richardson-levels 6}) 1.0)]
    ;; With 6 levels of Richardson, we should get good accuracy
    (t/is-approx= true-deriv richardson-6 :tolerance 1e-8)))

(ct/deftest adaptive-derivative-fn-test
  ;; sin'(PI) = -1
  (t/is-approx= -1.0 ((deriv/adaptive-derivative-fn m/sin) m/PI) :tolerance 1e-7)
  ;; x^2 derivative at x=5 is 10
  (t/is-approx= 10.0 ((deriv/adaptive-derivative-fn m/sq) 5.0) :tolerance 1e-7)
  ;; With custom tolerance - note that tighter rel-tol gives better result
  (t/is-approx= (m/cos 2.0)
          ((deriv/adaptive-derivative-fn m/sin {::deriv/rel-tol 1e-12}) 2.0)
          :tolerance 1e-7))

(ct/deftest derivative-with-error-fn-test
  (let [result ((deriv/derivative-with-error-fn m/sin) m/PI)]
    ;; Value should be close to -1
    (t/is-approx= -1.0 (::deriv/value result) :tolerance 1e-7)
    ;; Error bound should be small and non-negative
    (t/is (m/non-? (::deriv/error-bound result)))
    (t/is (< (::deriv/error-bound result) 1e-6)))
  ;; Test with x^2
  (let [result ((deriv/derivative-with-error-fn m/sq) 3.0)]
    (t/is-approx= 6.0 (::deriv/value result) :tolerance 1e-7)))

;;;VECTOR CALCULUS TESTS

(ct/deftest directional-derivative-fn-test
  ;; f(x,y) = x^2 + y^2, grad = [2x, 2y]
  ;; Direction [1,0] at (3,4) -> 2*3 = 6
  (let [f (fn [[x y]] (+ (m/sq x) (m/sq y)))
        df-x (deriv/directional-derivative-fn f [1.0 0.0])]
    (t/is-approx= 6.0 (df-x [3.0 4.0]) :tolerance 1e-6))
  ;; Direction [0,1] at (3,4) -> 2*4 = 8
  (let [f (fn [[x y]] (+ (m/sq x) (m/sq y)))
        df-y (deriv/directional-derivative-fn f [0.0 1.0])]
    (t/is-approx= 8.0 (df-y [3.0 4.0]) :tolerance 1e-6))
  ;; Direction [1,1] (normalized) at (3,4) -> (6+8)/sqrt(2) = 14/sqrt(2)
  (let [f (fn [[x y]] (+ (m/sq x) (m/sq y)))
        df-diag (deriv/directional-derivative-fn f [1.0 1.0])]
    (t/is-approx= (/ 14.0 m/sqrt-two) (df-diag [3.0 4.0]) :tolerance 1e-5))
  ;; Zero direction should return NaN
  (let [f (fn [[x y]] (+ (m/sq x) (m/sq y)))
        df-zero (deriv/directional-derivative-fn f [0.0 0.0])]
    (t/is (m/nan? (df-zero [3.0 4.0])))))

(ct/deftest laplacian-fn-test
  ;; f(x,y) = x^2 + y^2, Laplacian = 2 + 2 = 4
  (let [f (fn [[x y]] (+ (m/sq x) (m/sq y)))
        lap (deriv/laplacian-fn f)]
    (t/is-approx= 4.0 (lap [1.0 2.0]) :tolerance 1e-5)
    (t/is-approx= 4.0 (lap [5.0 -3.0]) :tolerance 1e-5))
  ;; f(x,y,z) = x^2 + 2y^2 + 3z^2, Laplacian = 2 + 4 + 6 = 12
  (let [f (fn [[x y z]] (+ (m/sq x) (* 2 (m/sq y)) (* 3 (m/sq z))))
        lap (deriv/laplacian-fn f)]
    (t/is-approx= 12.0 (lap [1.0 1.0 1.0]) :tolerance 1e-5)))

(ct/deftest divergence-fn-test
  ;; F(x,y) = [x, y], div = 1 + 1 = 2
  (let [F (fn [[x y]] [x y])
        divF (deriv/divergence-fn F)]
    (t/is-approx= 2.0 (divF [3.0 4.0]) :tolerance 1e-6))
  ;; F(x,y) = [xy, y^2], div = y + 2y = 3y
  (let [F (fn [[x y]] [(* x y) (m/sq y)])
        divF (deriv/divergence-fn F)]
    (t/is-approx= 9.0 (divF [2.0 3.0]) :tolerance 1e-6)) ;; 3*3 = 9
  ;; F(x,y,z) = [x^2, y^2, z^2], div = 2x + 2y + 2z
  (let [F (fn [[x y z]] [(m/sq x) (m/sq y) (m/sq z)])
        divF (deriv/divergence-fn F)]
    (t/is-approx= 12.0 (divF [1.0 2.0 3.0]) :tolerance 1e-6))) ;; 2+4+6 = 12

(ct/deftest curl-fn-test
  ;; F(x,y,z) = [y, -x, 0], curl = [0, 0, -1-1] = [0, 0, -2]
  (let [F (fn [[x y _z]] [y (- x) 0.0])
        curlF (deriv/curl-fn F)
        result (curlF [1.0 2.0 3.0])]
    (t/is-approx= 0.0 (nth result 0) :tolerance 1e-6)
    (t/is-approx= 0.0 (nth result 1) :tolerance 1e-6)
    (t/is-approx= -2.0 (nth result 2) :tolerance 1e-6))
  ;; F(x,y,z) = [yz, xz, xy], curl = [x-x, y-y, z-z] = [0, 0, 0]
  (let [F (fn [[x y z]] [(* y z) (* x z) (* x y)])
        curlF (deriv/curl-fn F)
        result (curlF [1.0 2.0 3.0])]
    (t/is-approx= 0.0 (nth result 0) :tolerance 1e-5)
    (t/is-approx= 0.0 (nth result 1) :tolerance 1e-5)
    (t/is-approx= 0.0 (nth result 2) :tolerance 1e-5))
  ;; Non-3D input should return NaN
  (let [F (fn [v] v)
        curlF (deriv/curl-fn F)
        result (curlF [1.0 2.0])]
    (t/is (every? m/nan? result))))

;;;HIGHER-ORDER MIXED PARTIALS TEST

(ct/deftest mixed-partial-fn-test
  ;; f(x,y) = x^2 * y^3
  ;; df/dx = 2xy^3, d^2f/dx^2 = 2y^3
  ;; d^3f/(dx^2 dy) = 6y^2
  ;; At (1, 2): 6 * 4 = 24
  (let [f (fn [[x y]] (* (m/sq x) (m/cube y)))
        d3f (deriv/mixed-partial-fn f [2 1])]
    (t/is-approx= 24.0 (d3f [1.0 2.0]) :tolerance 0.5))
  ;; Just first derivative in x: df/dx = 2xy^3 at (1,2) = 2*1*8 = 16
  (let [f (fn [[x y]] (* (m/sq x) (m/cube y)))
        df-dx (deriv/mixed-partial-fn f [1 0])]
    (t/is-approx= 16.0 (df-dx [1.0 2.0]) :tolerance 1e-5))
  ;; Just first derivative in y: df/dy = 3x^2*y^2 at (1,2) = 3*1*4 = 12
  (let [f (fn [[x y]] (* (m/sq x) (m/cube y)))
        df-dy (deriv/mixed-partial-fn f [0 1])]
    (t/is-approx= 12.0 (df-dy [1.0 2.0]) :tolerance 1e-5)))

;;;SPARSE JACOBIAN AND HESSIAN TESTS

(ct/deftest sparse-jacobian-fn-test
  ;; F(x,y) = [x^2, y^2], diagonal Jacobian entries: [2x, 2y]
  (let [F (fn [[x y]] [(m/sq x) (m/sq y)])
        sJ (deriv/sparse-jacobian-fn F #{[0 0] [1 1]})
        result (sJ [3.0 4.0])]
    ;; Result should be [[0 0 6.0] [1 1 8.0]]
    (t/is (= 2 (count result)))
    (let [entry00 (first (filter #(= [0 0] (vec (take 2 %))) result))
          entry11 (first (filter #(= [1 1] (vec (take 2 %))) result))]
      (t/is-approx= 6.0 (nth entry00 2) :tolerance 1e-6)
      (t/is-approx= 8.0 (nth entry11 2) :tolerance 1e-6)))
  ;; Test off-diagonal entry
  (let [F (fn [[x y]] [(* x y) (+ x y)])
        sJ (deriv/sparse-jacobian-fn F #{[0 1]})
        result (sJ [2.0 3.0])
        ;; dF0/dy = x = 2
        entry01 (first result)]
    (t/is-approx= 2.0 (nth entry01 2) :tolerance 1e-6)))

(ct/deftest sparse-hessian-fn-test
  ;; f(x,y) = x^2 + y^2, Hessian diagonal = [2, 2]
  (let [f (fn [[x y]] (+ (m/sq x) (m/sq y)))
        sH (deriv/sparse-hessian-fn f #{[0 0] [1 1]})
        result (sH [3.0 4.0])]
    (t/is (= 2 (count result)))
    (let [entry00 (first (filter #(= [0 0] (vec (take 2 %))) result))
          entry11 (first (filter #(= [1 1] (vec (take 2 %))) result))]
      (t/is-approx= 2.0 (nth entry00 2) :tolerance 1e-5)
      (t/is-approx= 2.0 (nth entry11 2) :tolerance 1e-5)))
  ;; f(x,y) = xy, Hessian off-diagonal = 1
  (let [f (fn [[x y]] (* x y))
        sH (deriv/sparse-hessian-fn f #{[0 1]})
        result (sH [2.0 3.0])
        ;; d^2f/(dx dy) = 1
        entry01 (first result)]
    (t/is-approx= 1.0 (nth entry01 2) :tolerance 1e-5)))
