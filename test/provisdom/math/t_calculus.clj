(ns provisdom.math.t-calculus
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.core :as m]
            [provisdom.math.calculus :as ca]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]))

(ost/instrument)

(def der-f #(+ (m/exp %) (* 7 (m/pow % 6)) (* 3 (m/pow % 3)) %))

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
  ;;example issues
  (is= 2.0E8 ((ca/derivative-fn m/sq) 1e8))                 ;good
  (is= 1.92E9 ((ca/derivative-fn m/sq) 1e9))                ;bad
  (is= 3.2768E10 ((ca/derivative-fn m/sq) 1e10))            ;ugly
  (is= 0.0 ((ca/derivative-fn m/sq) 1e11))                  ;crazy
  (is= 2.0E11 ((ca/derivative-fn m/sq {::ca/h 1e8}) 1e11))  ;fixed
  (is= 0.0 ((ca/derivative-fn m/sq) 1e110))                 ;crazy
  (is= 1.999999999999999E110 ((ca/derivative-fn m/sq {::ca/h 1e108}) 1e110)) ;fixed
  )

(deftest calculus-test
  (derivative-fn-test))

(defspec-test test-derivative-fn `ca/derivative-fn)

#_(ost/unstrument)