(ns provisdom.math.t-calculus
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.core :as m]
    [provisdom.math.calculus :as ca]
    [provisdom.math.bounds :as bo]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

(set! *warn-on-reflection* true)

(ost/instrument)

;;COMPARE AGAINST THE FOLLOWING
; Implements the adaptive quadrature described on page 511 of Numerical Analysis Kinkade et al.
; ## License
; Copyright (C) 2014 Daniel Aaron Phelps
; Distributed under the Eclipse Public License, the same as Clojure.

(defn- simpsons-estimate
  "Equation '8.5' page 509 - approximates the integral of f over [a b]."
  ^double
  [f ^double a ^double b ^double h]
  (* (/ h 3.0) (+ (f a) (* 4 (f (+ a h))) (f b))))

(defn- close-enough?
  "Finds if |a - b| < |error|."
  [^double a ^double b ^double error]
  (< (Math/abs (- a b)) (Math/abs error)))

(defn- insured-approximation
  "Equation 7 page 509 in Kinkade et al."
  ^double
  [^double S* ^double S** ^double S]
  (+ S* S** (* (/ 1.0 15.0) (+ S* S** (* -1.0 S)))))

(defn- adapt-quad-internal
  "Do not call this fn directly.  Start with adaptive-quadrature instead."
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
      :else (+ (adapt-quad-internal f delta eps n (inc k) sigma a h fa (f (+ a h)) fc S-left) ;From a to the midpoint
               ;;From the midpoint to b
               (adapt-quad-internal f delta eps n (inc k) sigma (+ a (* 2. h)) h fc (f (+ a (* 3.0 h))) fb S-right)))))

(defn- adaptive-quadrature-test
  "Approximates the definite integral of f over [a b] with an error less
  or equal than eps.  f is a real valued function of one real argument.
  The parameter n specifies how many recursive calls are allowed.  An
  exception is thrown before the n+1st recursive call."
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

(defn- univariate-integration-test
  "Univariate Integration using Romberg."
  [f lower-bound upper-bound]
  (let [max-eval 100]
    (if (== lower-bound upper-bound)
      0.0
      (adaptive-quadrature-test f lower-bound upper-bound 1e-6 max-eval))))

(comment
  (s/fdef univariate-integration-test
          :args (s/and
                  (s/cat :f (s/fspec :args (s/cat :x double?) :ret double?)
                         :lower-bound double?
                         :upper-bound double?)
                  #(> (:upper-bound %) (:lower-bound %)))
          :ret double?))

;;;INTEGRATION TESTS
(comment "returned functions don't seem to equate..."
         (deftest change-of-variable-test
           (is= {::ca/multiplicative-fn (fn [number] (let [s (m/sq number)] (m/div (inc s) (m/sq (m/one- s)))))
                 ::ca/converter-fn      (fn [number] (m/div number (m/one- (m/sq number))))
                 ::bo/interval          [-1.0 1.0]}
                (ca/change-of-variable [m/inf- m/inf+]))
           (is= {::ca/multiplicative-fn (constantly 1.0)
                 ::ca/converter-fn      identity
                 ::bo/interval          [3.0 4.0]}
                (ca/change-of-variable [3.0 4.0]))
           (is= {::ca/multiplicative-fn (fn [number] (m/div (m/sq number)))
                 ::ca/converter-fn      (fn [number] (+ 3.0 (m/div (m/one- number) number)))
                 ::bo/interval          [0.0 1.0]}
                (ca/change-of-variable [3.0 m/inf+]))
           (is= {::ca/multiplicative-fn (fn [number] (m/div (m/sq number)))
                 ::ca/converter-fn      (fn [number] (- 4.0 (m/div (m/one- number) number)))
                 ::bo/interval          [0.0 1.0]}
                (ca/change-of-variable [m/inf- 4.0]))))

(deftest integration-test
  ;;ordinary
  (is= 69.33333333333331 (ca/integration m/sq [2.0 6.0]))
  (is= 2.886579864025407E-15 (ca/integration m/cos [m/PI (* 5 m/PI)]))
  (is= -1.000000000000004 (ca/integration m/cos [m/PI (* 5.5 m/PI)]))
  (is= 3.3306690738754696E-16 (ca/integration m/cos [m/PI (* 6 m/PI)]))
  ;;change of variable
  ;!!!!this is getting a NaN value, presumably something to do with error = NaN
  ;!!!!need to stop the integration upon a NaN I think -- same as below -- why NaN?
  (is (instance? Exception (ca/integration m/cos [m/PI m/inf+])))
  (is= 0.19999999999999996 (ca/integration #(m/pow % -2.0) [5.0 m/inf+]))
  (is= 0.19999999999999996 (ca/integration #(m/pow % -2.0) [m/inf- -5.0]))
  (is= 1.7724538509055163 (ca/integration #(m/exp (- (m/sq %))) [m/inf- m/inf+]))
  ;;vector
  (is= [15.999999999999998 69.33333333333333]
       (ca/integration
         #(vector % (m/sq %))
         [2.0 6.0]))
  ;   with change of var
  (is= [1.7724538509055163 1.8128049541109543]
       (ca/integration
         #(vector (m/exp (- (m/sq %))) (m/exp (- (m/pow % 4))))
         [m/inf- m/inf+]))
  ;;matrix
  (is= [[15.999999999999998 69.33333333333334] [24.0 19.999999999999996]]
       (ca/integration
         #(vector [% (m/sq %)] [(+ 2.0 %) 5.0])
         [2.0 6.0]))
  ;   with change of var
  (is= [[6.400000000000001E-5 0.002666666666666666] [0.2 0.2]]
       (ca/integration
         #(vector [(m/pow % -6) (m/pow % -4)] [(m/pow % -2.0) (m/pow % -2.0)])
         [5.0 m/inf+])))

(deftest rectangular-integration-test
  (is= 1.0
       (ca/rectangular-integration
         (fn [[a b]] (+ (double (or a 0.0)) (or b 0.0)))
         [[0.0 1.0] [0.0 1.0]]))
  (is= 1.5
       (ca/rectangular-integration
         (fn [[a b c]] (+ (double (or a 0.0)) (or b 0.0) (or c 0.0)))
         [[0.0 1.0] [0.0 1.0] [0.0 1.0]]))
  (is= 2.0
       (ca/rectangular-integration
         (fn [[a b c d]] (+ (double (or a 0.0)) (or b 0.0) (or c 0.0) (or d 0.0)))
         [[0.0 1.0] [0.0 1.0] [0.0 1.0] [0.0 1.0]]))
  (is= 3.1415926535897944                                   ;3.141592653589793 PI
       (ca/rectangular-integration
         (fn [[a b]] (m/exp (- (+ (m/sq (or a 0.0)) (m/sq (or b 0.0))))))
         [[m/inf- m/inf+] [m/inf- m/inf+]]))
  (is= 0.1
       (ca/rectangular-integration
         (fn [[a b]] (m/pow (* (double (or a 0.0)) (or b 0.0)) -2.0))
         [[5.0 m/inf+] [1.0 2.0]]))
  (is= 0.01666666666666667
       (ca/rectangular-integration
         (fn [[a b]] (m/pow (* (double (or a 0.0)) (or b 0.0)) -2.0))
         [[3.0 4.0] [m/inf- -5.0]]))
  ;;vector
  (is= [32.0 64.0]
       (ca/rectangular-integration
         (fn [[a b]] [(or a 0.0) (* (double (or a 0.0)) (or b 0.0))])
         [[2.0 6.0] [1.0 3.0]]))
  ;   with change of var
  (is= [0.24705031697079533 0.24705031697079533]
       (ca/rectangular-integration
         (fn [[a b]] [(m/exp (- (+ (m/sq (or a 0.0)) (m/sq (or b 0.0)))))
                      (m/exp (- (+ (m/sq (or a 0.0)) (m/sq (or b 0.0)))))])
         [[m/inf- m/inf+] [1.0 3.0]]))
  ;;matrix
  (is= [[32.0 64.0] [32.0 40.00000000000001]]
       (ca/rectangular-integration
         (fn [[a b]] [[(or a 0.0) (* (double (or a 0.0)) (or b 0.0))] [(+ 2.0 (or b 0.0)) 5.0]])
         [[2.0 6.0] [1.0 3.0]]))
  ;   with change of var
  (is= [[0.06666666666666667 0.06666666666666667] [0.06666666666666667 0.06666666666666667]]
       (ca/rectangular-integration
         (fn [[a b]] [[(m/pow (* (double (or a 0.0)) (or b 0.0)) -2.0) (m/pow (* (double (or a 0.0)) (or b 0.0)) -2.0)]
                      [(m/pow (* (double (or a 0.0)) (or b 0.0)) -2.0) (m/pow (* (double (or a 0.0)) (or b 0.0)) -2.0)]])
         [[2.0 6.0] [m/inf- -5.0]])))

(deftest non-rectangular-2D-integration-test
  (is= 1.0
       (ca/non-rectangular-2D-integration
         (fn [outer inner]
           (+ outer (double inner)))
         [0.0 1.0]
         (fn [outer] [0.0 1.0])))
  (is= 192.0
       (ca/non-rectangular-2D-integration
         (fn [outer inner]
           (+ outer inner))
         [2.0 6.0]
         (fn [outer] [(+ outer 2.0) (+ outer 6.0)])))
  (is= 3.141592653589793                                    ;3.141592653589793 PI
       (ca/non-rectangular-2D-integration
         (fn [outer inner]
           (* (m/exp (- (m/sq outer))) (m/exp (- (m/sq inner)))))
         [m/inf- m/inf+]
         (fn [outer] [m/inf- m/inf+])))
  (is= [32.0 64.0]
       (ca/non-rectangular-2D-integration
         (fn [outer inner]
           (vector outer (* outer (double inner))))
         [2.0 6.0]
         (fn [outer] [1.0 3.0])))
  (is= [0.24705031697079533 0.24705031697079533]
       (ca/non-rectangular-2D-integration
         (fn [outer inner]
           [(* (m/exp (- (m/sq outer))) (m/exp (- (m/sq inner))))
            (* (m/exp (- (m/sq outer))) (m/exp (- (m/sq inner))))])
         [m/inf- m/inf+]
         (fn [outer] [1.0 3.0])))
  (is= [[32.0 64.0] [32.0 39.99999999999999]]
       (ca/non-rectangular-2D-integration
         (fn [outer inner]
           (vector [outer (* outer (double inner))] [(+ 2.0 inner) 5.0]))
         [2.0 6.0]
         (fn [outer] [1.0 3.0])))
  (is= [[0.06666666666666667 0.06666666666666667] [0.06666666666666667 0.06666666666666667]]
       (ca/non-rectangular-2D-integration
         (fn [outer inner]
           [[(m/pow (* (double outer) inner) -2.0) (m/pow (* (double outer) inner) -2.0)]
            [(m/pow (* (double outer) inner) -2.0) (m/pow (* (double outer) inner) -2.0)]])
         [2.0 6.0]
         (fn [outer] [m/inf- -5.0]))))

#_(deftest non-rectangular-3D-integration-test              ;too slow
  (is= 1.5
       (ca/non-rectangular-3D-integration
         (fn [outer middle inner]
           (+ (double outer) middle inner))
         [0.0 1.0]
         (fn [outer] [0.0 1.0])
         (fn [outer middle] [0.0 1.0]))))

#_(deftest non-rectangular-4D-integration-test              ;too slow
  (is= 2.0000000000000004
       (ca/non-rectangular-4D-integration
         (fn [outer outer-middle inner-middle inner]
           (+ (double outer) outer-middle inner-middle inner))
         [0.0 1.0]
         (fn [outer] [0.0 1.0])
         (fn [outer outer-middle] [0.0 1.0])
         (fn [outer outer-middle inner-middle] [0.0 1.0]))))

(defspec-test test-change-of-variable `ca/change-of-variable)
;(defspec-test test-integration `ca/integration) ;slow
;(defspec-test test-rectangular-integration `ca/rectangular-integration) slow
;(defspec-test test-integration-non-rectangular-2D `ca/integration-non-rectangular-2D) ;too slow
;(defspec-test test-integration-non-rectangular-3D `ca/integration-non-rectangular-3D) ;way too slow
;(defspec-test test-integration-non-rectangular-4D `ca/integration-non-rectangular-4D) ;way too slow

;;;DERIVATIVE TESTS
(defn der-f
  [number]
  (+ (m/exp number)
     (* 7 (m/pow number 6))
     (* 3 (m/pow number 3))
     number))

(deftest derivative-fn-test
  (is= 28922.598150033144 ((ca/derivative-fn der-f {::ca/derivative 0}) 4.0))
  ;;first derivative: 131624.413159103
  (is= 131624.41317399498 ((ca/derivative-fn der-f) 5.0))
  (is= 131624.41315653265 ((ca/derivative-fn der-f {::ca/h 1e-5}) 5.0))
  (is= 131624.41340682562 ((ca/derivative-fn der-f {::ca/h 1e-7}) 5.0))
  (is= 131624.4131885469 ((ca/derivative-fn der-f {::ca/accuracy 8}) 5.0))
  (is= 131624.41315944307 ((ca/derivative-fn der-f {::ca/type :forward}) 5.0))
  (is= 131624.4131885469 ((ca/derivative-fn der-f {::ca/type :backward}) 5.0))
  ;;second derivative: 131488.413159103
  (is= 131488.41360816732 ((ca/derivative-fn der-f {::ca/derivative 2}) 5.0))
  (is= 131488.41841029935 ((ca/derivative-fn der-f {::ca/derivative 2 ::ca/h 1e-6}) 5.0))
  (is= 131488.41389920563 ((ca/derivative-fn der-f {::ca/derivative 2 ::ca/h 1e-8}) 5.0))
  (is= 131488.41040674597 ((ca/derivative-fn der-f {::ca/derivative 2 ::ca/type :forward}) 5.0))
  ;;second deriv of log: -0.04
  (is= -0.03999999886872274 ((ca/derivative-fn m/log {::ca/derivative 2 ::ca/accuracy 2}) 5.0))
  (is= -0.039999989986938544 ((ca/derivative-fn m/log {::ca/derivative 2 ::ca/accuracy 8}) 5.0))
  ;;third deriv: 105166.413159103
  (is= 105166.41574213281 ((ca/derivative-fn der-f {::ca/derivative 3}) 5.0))
  (is= 105166.41574213281 ((ca/derivative-fn der-f {::ca/derivative 3 ::ca/h 1e-7}) 5.0))
  (is= 105166.13838262855 ((ca/derivative-fn der-f {::ca/derivative 3 ::ca/h 1e-9}) 5.0))
  (is= 105166.41574213281 ((ca/derivative-fn der-f {::ca/derivative 3 ::ca/accuracy 6}) 5.0))
  ;;fourth deriv: 63148.413159103
  (is= 63148.41448329389 ((ca/derivative-fn der-f {::ca/derivative 4}) 5.0))
  (is= 63148.41448329389 ((ca/derivative-fn der-f {::ca/derivative 4 ::ca/h 1e-7}) 5.0))
  (is= 63148.56000244617 ((ca/derivative-fn der-f {::ca/derivative 4 ::ca/h 1e-9}) 5.0))
  (is= 63148.41448329389 ((ca/derivative-fn der-f {::ca/derivative 4 ::ca/accuracy 4}) 5.0))
  (is= 63148.41448329389 ((ca/derivative-fn der-f {::ca/derivative 4 ::ca/accuracy 6}) 5.0))
  ;;fifth deriv: 25348.41315903
  (is= 25348.41462038457 ((ca/derivative-fn der-f {::ca/derivative 5}) 5.0))
  (is= 25348.537368699905 ((ca/derivative-fn der-f {::ca/derivative 5 ::ca/h 1e-8}) 5.0))
  (is= 25348.660635063425 ((ca/derivative-fn der-f {::ca/derivative 5 ::ca/h 1e-6}) 5.0))
  ;;sixth deriv: 5188.4131591
  (is= 5188.451371035215 ((ca/derivative-fn der-f {::ca/derivative 6}) 5.0))
  ;;seventh deriv: 148.413159103
  (is= 148.4122348483652 ((ca/derivative-fn der-f {::ca/derivative 7}) 5.0))
  ;;eighth deriv: 148.413159103
  (is= 148.37917386236595 ((ca/derivative-fn der-f {::ca/derivative 8}) 5.0))
  ;;example issues requiring 'h'
  (is= 2.0E8 ((ca/derivative-fn m/sq) 1e8))                 ;good
  (is= 1.92E9 ((ca/derivative-fn m/sq) 1e9))                ;bad
  (is= 3.2768E10 ((ca/derivative-fn m/sq) 1e10))            ;ugly
  (is= 0.0 ((ca/derivative-fn m/sq) 1e11))                  ;crazy
  (is= 2.0E11 ((ca/derivative-fn m/sq {::ca/h 1e8}) 1e11))  ;fixed by setting h
  (is= 0.0 ((ca/derivative-fn m/sq) 1e110))                 ;crazy
  (is= 1.999999999999999E110 ((ca/derivative-fn m/sq {::ca/h 1e108}) 1e110)) ;fixed by setting h
  )

(defn gf
  [[a b]]
  (if-not b
    m/nan
    (+ (* (double a) b) (* 2 (m/sq a)) (m/cube b))))

(deftest gradient-fn-test
  (is= [16.000000002236447 51.0000000062405]
       ((ca/gradient-fn gf) [3.0 4.0])))                    ;[16,51]

(deftest jacobian-fn-test
  (is= [[16.00000000745058 51.00000000745058] [96.00000001490116 72.0]] ;[[16,51][96,72]]
       ((ca/jacobian-fn
          (fn [[a b]]
            (if-not b
              [m/nan m/nan]
              [(+ (* (double a) b) (* 2 (m/sq a)) (m/cube b))
               (+ (* (m/sq a) (m/sq b)))])))
         [3.0 4.0])))

(deftest hessian-fn-test
  ;;type 'joint-central' is the default
  (is= [[4.00000004674439 1.00000003833145] [1.00000003833145 23.999999854140697]] ;[[4,1][1,24]]
       ((ca/hessian-fn gf) [3.0 4.0]))
  (is= [[4.0000021094456315 0.9999977191910148] [0.9999977191910148 24.000001423060894]] ;[[4,1][1,24]]
       ((ca/hessian-fn gf {::ca/type :central}) [3.0 4.0]))
  (is= [[3.9999908739700913 0.9999977201223373] [0.9999977201223373 24.000035122036934]] ;[[4,1][1,24]]
       ((ca/hessian-fn gf {::ca/type :forward}) [3.0 4.0]))
  (is= [[4.000035812146962 0.9999977182596922] [0.9999977182596922 23.999990183860064]] ;[[4,1][1,24]]
       ((ca/hessian-fn gf {::ca/type :backward}) [3.0 4.0])))

(defspec-test test-derivative-fn `ca/derivative-fn)
(defspec-test test-gradient-fn `ca/gradient-fn)
;(defspec-test test-jacobian-fn `ca/jacobian-fn) ;slow-ish
;(defspec-test test-hessian-fn `ca/hessian-fn) ;slow

;;;PARTIAL DERIVATIVE TESTS
(defn fxy
  [x y]
  (+ (* (double x) x) (* 2.0 y y) (* (double x) y)))

(deftest partial-derivative-x-of-fxy-test
  (is= 8.999999998593466 ((ca/partial-derivative-x-of-fxy fxy) 3.0 3.0))) ;9

(deftest partial-derivative-y-of-fxy-test
  (is= 15.000000001208491 ((ca/partial-derivative-y-of-fxy fxy) 3.0 3.0))) ;15

(deftest second-partial-derivative-xx-of-fxy-test
  (is= 2.000000023372195 ((ca/second-partial-derivative-xx-of-fxy fxy) 3.0 3.0))) ;2

(deftest second-partial-derivative-yy-of-fxy-test
  (is= 3.999999975690116 ((ca/second-partial-derivative-yy-of-fxy fxy) 3.0 3.0))) ;4

(deftest second-partial-derivative-xy-of-fxy-test
  (is= 1.0000000294496658 ((ca/second-partial-derivative-xy-of-fxy fxy) 3.0 3.0))) ;1

;(defspec-test test-partial-derivative-x-of-fxy `ca/partial-derivative-x-of-fxy) ;slow-ish
;(defspec-test test-partial-derivative-y-of-fxy `ca/partial-derivative-y-of-fxy) ;slow-ish
(defspec-test test-second-partial-derivative-xx-of-fxy `ca/second-partial-derivative-xx-of-fxy)
(defspec-test test-second-partial-derivative-yy-of-fxy `ca/second-partial-derivative-yy-of-fxy)
(defspec-test test-second-partial-derivative-xy-of-fxy `ca/second-partial-derivative-xy-of-fxy)

#_(ost/unstrument)