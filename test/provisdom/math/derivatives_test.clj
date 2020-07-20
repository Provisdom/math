(ns provisdom.math.derivatives-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.core :as m]
    [provisdom.math.derivatives :as derivatives]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;;70 SECONDS

(set! *warn-on-reflection* true)

(ost/instrument)

(defn der-f
  [number]
  (+ (m/exp number)
     (* 7 (m/pow number 6))
     (* 3 (m/pow number 3))
     number))

(deftest derivative-fn-test
  (is (spec-check derivatives/derivative-fn))
  (is= 28922.598150033144
       ((derivatives/derivative-fn der-f {::derivatives/derivative 0}) 4.0))
  ;;first derivative: 131624.413159103
  (is= 131624.41317399498 ((derivatives/derivative-fn der-f) 5.0))
  (is= 131624.41315653265
       ((derivatives/derivative-fn der-f {::derivatives/h 1e-5}) 5.0))
  (is= 131624.41340682562
       ((derivatives/derivative-fn der-f {::derivatives/h 1e-7}) 5.0))
  (is= 131624.4131885469
       ((derivatives/derivative-fn der-f {::derivatives/accuracy 8}) 5.0))
  (is= 131624.41315944307
       ((derivatives/derivative-fn der-f {::derivatives/type :forward}) 5.0))
  (is= 131624.4131885469
       ((derivatives/derivative-fn der-f {::derivatives/type :backward}) 5.0))
  ;;second derivative: 131488.413159103
  (is= 131488.41360816732
       ((derivatives/derivative-fn der-f {::derivatives/derivative 2}) 5.0))
  (is= 131488.41841029935
       ((derivatives/derivative-fn der-f {::derivatives/derivative 2
                                          ::derivatives/h          1e-6})
        5.0))
  (is= 131488.41389920563
       ((derivatives/derivative-fn der-f {::derivatives/derivative 2
                                          ::derivatives/h          1e-8})
        5.0))
  (is= 131488.41040674597
       ((derivatives/derivative-fn der-f {::derivatives/derivative 2
                                          ::derivatives/type       :forward})
        5.0))
  ;;second deriv of log: -0.04
  (is= -0.03999999886872274
       ((derivatives/derivative-fn m/log {::derivatives/derivative 2
                                          ::derivatives/accuracy   2})
        5.0))
  (is= -0.039999989986938544
       ((derivatives/derivative-fn m/log {::derivatives/derivative 2
                                          ::derivatives/accuracy   8})
        5.0))
  ;;third deriv: 105166.413159103
  (is= 105166.41574213281
       ((derivatives/derivative-fn der-f {::derivatives/derivative 3}) 5.0))
  (is= 105166.41574213281
       ((derivatives/derivative-fn der-f {::derivatives/derivative 3
                                          ::derivatives/h          1e-7})
        5.0))
  (is= 105166.13838262855
       ((derivatives/derivative-fn der-f {::derivatives/derivative 3
                                          ::derivatives/h          1e-9})
        5.0))
  (is= 105166.41574213281
       ((derivatives/derivative-fn der-f {::derivatives/derivative 3
                                          ::derivatives/accuracy   6})
        5.0))
  ;;fourth deriv: 63148.413159103
  (is= 63148.41448329389
       ((derivatives/derivative-fn der-f {::derivatives/derivative 4}) 5.0))
  (is= 63148.41448329389
       ((derivatives/derivative-fn der-f {::derivatives/derivative 4
                                          ::derivatives/h          1e-7})
        5.0))
  (is= 63148.56000244617
       ((derivatives/derivative-fn der-f {::derivatives/derivative 4
                                          ::derivatives/h          1e-9})
        5.0))
  (is= 63148.41448329389
       ((derivatives/derivative-fn der-f {::derivatives/derivative 4
                                          ::derivatives/accuracy   4})
        5.0))
  (is= 63148.41448329389
       ((derivatives/derivative-fn der-f {::derivatives/derivative 4
                                          ::derivatives/accuracy   6})
        5.0))
  ;;fifth deriv: 25348.41315903
  (is= 25348.41462038457
       ((derivatives/derivative-fn der-f {::derivatives/derivative 5}) 5.0))
  (is= 25348.537368699905
       ((derivatives/derivative-fn der-f {::derivatives/derivative 5
                                          ::derivatives/h          1e-8})
        5.0))
  (is= 25348.660635063425
       ((derivatives/derivative-fn der-f {::derivatives/derivative 5
                                          ::derivatives/h          1e-6})
        5.0))
  ;;sixth deriv: 5188.4131591
  (is= 5188.451371035215
       ((derivatives/derivative-fn der-f {::derivatives/derivative 6}) 5.0))
  ;;seventh deriv: 148.413159103
  (is= 148.4122348483652
       ((derivatives/derivative-fn der-f {::derivatives/derivative 7}) 5.0))
  ;;eighth deriv: 148.413159103
  (is= 148.37917386236595
       ((derivatives/derivative-fn der-f {::derivatives/derivative 8}) 5.0))
  ;;example issues requiring 'h'
  (is= 2.0E8 ((derivatives/derivative-fn m/sq) 1e8))        ;good
  (is= 1.92E9 ((derivatives/derivative-fn m/sq) 1e9))       ;bad
  (is= 3.2768E10 ((derivatives/derivative-fn m/sq) 1e10))   ;ugly
  (is= 0.0 ((derivatives/derivative-fn m/sq) 1e11))         ;crazy
  ;;fixed by setting h
  (is= 2.0E11
       ((derivatives/derivative-fn m/sq {::derivatives/h 1e8}) 1e11))
  (is= 0.0 ((derivatives/derivative-fn m/sq) 1e110))        ;crazy
  ;;fixed by setting h
  (is= 1.999999999999999E110
       ((derivatives/derivative-fn m/sq {::derivatives/h 1e108}) 1e110))
  )

(defn gf
  [[a b]]
  (if-not b
    m/nan
    (+ (* (double a) b)
       (* 2 (m/sq a))
       (m/cube b))))

(deftest gradient-fn-test
  (is (spec-check derivatives/gradient-fn))
  (is= [16.000000002236447 51.0000000062405]
       ((derivatives/gradient-fn gf) [3.0 4.0])))           ;[16,51]

(deftest jacobian-fn-test
  (is (spec-check derivatives/jacobian-fn {:coll-check-limit 10
                                           :coll-error-limit 10
                                           :fspec-iterations 10
                                           :recursion-limit  1
                                           :test-check       {:num-tests 50}}))
  ;;[[16,51][96,72]]
  (is= [[16.00000000745058 51.00000000745058] [96.00000001490116 72.0]]
       ((derivatives/jacobian-fn
          (fn [[a b]]
            (if-not b
              [m/nan m/nan]
              [(+ (* (double a) b)
                  (* 2 (m/sq a))
                  (m/cube b))
               (+ (* (m/sq a) (m/sq b)))])))
        [3.0 4.0])))

(deftest hessian-fn-test
  (is (spec-check derivatives/hessian-fn {:coll-check-limit 10
                                          :coll-error-limit 10
                                          :fspec-iterations 10
                                          :recursion-limit  1
                                          :test-check       {:num-tests 50}}))
  ;;type 'joint-central' is the default
  (is= [[4.00000004674439 1.00000003833145]
        [1.00000003833145 23.999999854140697]]              ;[[4,1][1,24]]
       ((derivatives/hessian-fn gf) [3.0 4.0]))
  (is= [[4.0000021094456315 0.9999977191910148]
        [0.9999977191910148 24.000001423060894]]            ;[[4,1][1,24]]
       ((derivatives/hessian-fn gf {::derivatives/type :central}) [3.0 4.0]))
  (is= [[3.9999908739700913 0.9999977201223373]
        [0.9999977201223373 24.000035122036934]]            ;[[4,1][1,24]]
       ((derivatives/hessian-fn gf {::derivatives/type :forward}) [3.0 4.0]))
  (is= [[4.000035812146962 0.9999977182596922]
        [0.9999977182596922 23.999990183860064]]            ;[[4,1][1,24]]
       ((derivatives/hessian-fn gf {::derivatives/type :backward}) [3.0 4.0])))

;;;PARTIAL DERIVATIVE TESTS
(defn fxy
  [x y]
  (+ (* (double x) x)
     (* 2.0 y y)
     (* (double x) y)))

(deftest partial-derivative-x-of-fxy-test
  (is (spec-check derivatives/partial-derivative-x-of-fxy
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 500}}))
  (is= 8.999999998593466
       ((derivatives/partial-derivative-x-of-fxy fxy) 3.0 3.0))) ;9

(deftest partial-derivative-y-of-fxy-test
  (is (spec-check derivatives/partial-derivative-y-of-fxy
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 500}}))
  (is= 15.000000001208491
       ((derivatives/partial-derivative-y-of-fxy fxy) 3.0 3.0))) ;15

(deftest second-partial-derivative-xx-of-fxy-test
  (is (spec-check derivatives/second-partial-derivative-xx-of-fxy
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 500}}))
  (is= 2.000000023372195
       ((derivatives/second-partial-derivative-xx-of-fxy fxy
                                                         ) 3.0 3.0))) ;2

(deftest second-partial-derivative-yy-of-fxy-test
  (is (spec-check derivatives/second-partial-derivative-yy-of-fxy
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 500}}))
  (is= 3.999999975690116
       ((derivatives/second-partial-derivative-yy-of-fxy fxy) 3.0 3.0))) ;4

(deftest second-partial-derivative-xy-of-fxy-test
  (is (spec-check derivatives/second-partial-derivative-xy-of-fxy
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 500}}))
  (is= 1.0000000294496658
       ((derivatives/second-partial-derivative-xy-of-fxy fxy) 3.0 3.0))) ;1

#_(ost/unstrument)