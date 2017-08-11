(ns provisdom.math.t-calculus
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.core :as m]
            [provisdom.math.calculus :as ca]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]))

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
                  (s/cat :f (s/fspec :args (s/cat :x double?) :ret double?) :lower-bound double? :upper-bound double?)
                  #(> (:upper-bound %) (:lower-bound %)))
          :ret double?))

;;;TESTS

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

(defn gf [[a b]] (if-not b m/nan (+ (* a b) (* 2 (m/sq a)) (m/pow b 3))))

(deftest gradient-fn-test
  (is= [16.000000002236447 51.0000000062405] ((ca/gradient-fn gf) [3.0 4.0]))) ;[16,51]

(deftest jacobian-fn-test
  (is= [[16.00000000745058 51.00000000745058] [96.00000001490116 72.0]] ;[[16,51][96,72]]
       ((ca/jacobian-fn
          (fn [[a b]] (if-not b [m/nan m/nan] [(+ (* a b) (* 2 (m/sq a)) (m/cube b)) (+ (* (m/sq a) (m/sq b)))])))
         [3.0 4.0])))

(deftest hessian-fn-test
  ;;type 'joint-central' is the default
  (is= [[4.00000004674439 1.0000000205678816] [1.0000000205678816 23.999999854140697]] ;[[4,1][1,24]]
       ((ca/hessian-fn gf) [3.0 4.0]))
  (is= [[4.0000021094456315 1.0000089528039098] [1.0000089528039098 24.000001423060894]] ;[[4,1][1,24]]
       ((ca/hessian-fn gf {::ca/type :central}) [3.0 4.0]))
  (is= [[3.9999908739700913 1.0000201892107725] [1.0000201892107725 24.000035122036934]] ;[[4,1][1,24]]
       ((ca/hessian-fn gf {::ca/type :forward}) [3.0 4.0]))
  (is= [[4.000035812146962 1.0000201873481274] [1.0000201873481274 23.999990183860064]] ;[[4,1][1,24]]
       ((ca/hessian-fn gf {::ca/type :backward}) [3.0 4.0])))

(deftest derivative-tests
  (derivative-fn-test)
  (gradient-fn-test)
  (jacobian-fn-test)
  (hessian-fn-test))

(defspec-test test-derivative-fn `ca/derivative-fn)
(defspec-test test-gradient-fn `ca/gradient-fn)
;(defspec-test test-jacobian-fn `ca/jacobian-fn) ;slow-ish
;(defspec-test test-hessian-fn `ca/hessian-fn) ;slow

(defn fxy [x y] (+ (* x x) (* 2 y y) (* x y)))

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

(deftest partial-derivative-tests
  (partial-derivative-x-of-fxy-test)
  (partial-derivative-y-of-fxy-test)
  (second-partial-derivative-xx-of-fxy-test)
  (second-partial-derivative-yy-of-fxy-test)
  (second-partial-derivative-xy-of-fxy-test))

;(defspec-test test-partial-derivative-x-of-fxy `ca/partial-derivative-x-of-fxy) ;slow-ish
;(defspec-test test-partial-derivative-y-of-fxy `ca/partial-derivative-y-of-fxy) ;slow-ish
(defspec-test test-second-partial-derivative-xx-of-fxy `ca/second-partial-derivative-xx-of-fxy)
(defspec-test test-second-partial-derivative-yy-of-fxy `ca/second-partial-derivative-yy-of-fxy)
(defspec-test test-second-partial-derivative-xy-of-fxy `ca/second-partial-derivative-xy-of-fxy)

#_(ost/unstrument)