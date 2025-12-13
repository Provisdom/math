(ns provisdom.math.derivatives-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
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

(deftest derivative-fn-test
  (t/with-instrument `deriv/derivative-fn
    (t/is (t/spec-check deriv/derivative-fn)))
  (t/with-instrument (st/instrumentable-syms)
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

(deftest gradient-fn-test
  (t/with-instrument `deriv/gradient-fn
    (t/is (t/spec-check deriv/gradient-fn)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= [16.000000002236447 51.0000000062405] ((deriv/gradient-fn gf) [3.0 4.0])))) ;[16,51]

(deftest jacobian-fn-test
  (t/with-instrument `deriv/jacobian-fn
    (t/is (t/spec-check deriv/jacobian-fn)))
  (t/with-instrument (st/instrumentable-syms)
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

(deftest hessian-fn-test
  (t/with-instrument `deriv/hessian-fn
    (t/is (t/spec-check deriv/hessian-fn)))
  (t/with-instrument (st/instrumentable-syms)
    ;;type 'joint-central' is the default
    (t/is= [[4.00000004674439 1.00000003833145]
            [1.00000003833145 23.999999854140697]] ;[[4,1][1,24]]
      ((deriv/hessian-fn gf) [3.0 4.0]))
    (t/is= [[4.0000021094456315 0.9999977191910148]
            [0.9999977191910148 24.000001423060894]] ;[[4,1][1,24]]
      ((deriv/hessian-fn gf {::deriv/type :central}) [3.0 4.0]))
    (t/is= [[3.9999908739700913 0.9999977201223373]
            [0.9999977201223373 24.000035122036934]] ;[[4,1][1,24]]
      ((deriv/hessian-fn gf {::deriv/type :forward}) [3.0 4.0]))
    (t/is= [[4.000035812146962 0.9999977182596922]
            [0.9999977182596922 23.999990183860064]] ;[[4,1][1,24]]
      ((deriv/hessian-fn gf {::deriv/type :backward}) [3.0 4.0]))))

;;;PARTIAL DERIVATIVE TESTS
(defn fxy
  [x y]
  (+ (* (double x) x)
    (* 2.0 y y)
    (* (double x) y)))

(deftest partial-derivative-x-of-fxy-test
  (t/with-instrument `deriv/partial-derivative-x-of-fxy
    (t/is (t/spec-check deriv/partial-derivative-x-of-fxy)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= 8.999999998593466 ((deriv/partial-derivative-x-of-fxy fxy) 3.0 3.0))))  ;9

(deftest partial-derivative-y-of-fxy-test
  (t/with-instrument `deriv/partial-derivative-y-of-fxy
    (t/is (t/spec-check deriv/partial-derivative-y-of-fxy)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= 15.000000001208491 ((deriv/partial-derivative-y-of-fxy fxy) 3.0 3.0))))  ;15

(deftest second-partial-derivative-xx-of-fxy-test
  (t/with-instrument `deriv/second-partial-derivative-xx-of-fxy
    (t/is (t/spec-check deriv/second-partial-derivative-xx-of-fxy)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= 2.000000023372195 ((deriv/second-partial-derivative-xx-of-fxy fxy) 3.0 3.0)))) ;2

(deftest second-partial-derivative-yy-of-fxy-test
  (t/with-instrument `deriv/second-partial-derivative-yy-of-fxy
    (t/is (t/spec-check deriv/second-partial-derivative-yy-of-fxy)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= 3.999999975690116 ((deriv/second-partial-derivative-yy-of-fxy fxy) 3.0 3.0)))) ;4

(deftest second-partial-derivative-xy-of-fxy-test
  (t/with-instrument `deriv/second-partial-derivative-xy-of-fxy
    (t/is (t/spec-check deriv/second-partial-derivative-xy-of-fxy)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= 1.0000000294496658 ((deriv/second-partial-derivative-xy-of-fxy fxy) 3.0 3.0)))) ;1
