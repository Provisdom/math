(ns provisdom.math.integrals-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.core :as m]
    [provisdom.utility-belt.anomalies :as anomalies]
    [provisdom.math.integrals :as integrals]
    [provisdom.math.intervals :as intervals]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;;50 SECONDS -- haven't done much of this one yet

(set! *warn-on-reflection* true)

(ost/instrument)

;;COMPARE AGAINST THE FOLLOWING
; Implements the adaptive quadrature described on page 511 of
; Numerical Analysis Kinkade et al.
; ## License
; Copyright (C) 2014 Daniel Aaron Phelps
; Distributed under the Eclipse Public License, the same as Clojure.

(defn- simpsons-estimate
  "Equation '8.5' page 509 - approximates the integral of `f` over [`a` `b`]."
  ^double
  [f ^double a ^double b ^double h]
  (* (/ h 3.0)
     (+ (f a)
        (* 4 (f (+ a h)))
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
              ;;From a to the midpoint
              (adapt-quad-internal
                f delta eps n (inc k) sigma a h fa (f (+ a h)) fc S-left)
              ;;From the midpoint to b
              (adapt-quad-internal
                f delta eps n (inc k) sigma
                (+ a (* 2. h)) h fc (f (+ a (* 3.0 h))) fb S-right)))))

(defn- adaptive-quadrature-test
  "Approximates the definite integral of `f` over [`a` `b`] with an error less
  or equal than `eps`. `f is a real valued function of one real argument. The
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

(defn- univariate-integration-test
  "Univariate Integration using Romberg."
  [f lower-bound upper-bound]
  (let [max-eval 100]
    (if (== lower-bound upper-bound)
      0.0
      (adaptive-quadrature-test f lower-bound upper-bound 1e-6 max-eval))))

(comment
  (s/fdef univariate-integration-test
          :args (s/and (s/cat :f (s/fspec :args (s/cat :x double?)
                                          :ret double?)
                              :lower-bound double?
                              :upper-bound double?)
                       #(> (:upper-bound %) (:lower-bound %)))
          :ret double?))

;;;INTEGRATION TESTS
(s/def ::mul1
  (s/fspec :args (s/cat :number ::m/number)
           :ret ::m/number
           :fn (fn [{{number :number} :args
                     ret              :ret}]
                 (m/=== ret
                        (let [s (m/sq number)]
                          (m/div (inc s) (m/sq (m/one- s))))))))

(s/def ::con1
  (s/fspec :args (s/cat :number ::m/number)
           :ret ::m/number
           :fn (fn [{{number :number} :args
                     ret              :ret}]
                 (m/=== ret
                        (m/div number (m/one- (m/sq number)))))))

(s/def ::mul2
  (s/fspec :args (s/cat :number ::m/number)
           :ret #{1.0}))

(s/def ::con2
  (s/fspec :args (s/cat :number ::m/number)
           :ret ::m/number
           :fn (fn [{{number :number} :args
                     ret              :ret}]
                 (m/=== ret number))))

(s/def ::mul3
  (s/fspec :args (s/cat :number ::m/number)
           :ret ::m/number
           :fn (fn [{{number :number} :args
                     ret              :ret}]
                 (m/=== ret
                        (m/div (m/sq number))))))

(s/def ::con3
  (s/fspec :args (s/cat :number ::m/number)
           :ret ::m/number
           :fn (fn [{{number :number} :args
                     ret              :ret}]
                 (m/=== ret
                        (+ 3.0 (m/div (m/one- number) number))))))

(s/def ::con4
  (s/fspec :args (s/cat :number ::m/number)
           :ret ::m/number
           :fn (fn [{{number :number} :args
                     ret              :ret}]
                 (m/=== ret
                        (- 4.0 (m/div (m/one- number) number))))))

#_(deftest change-of-variable-test
  (is (spec-check integrals/change-of-variable))
  (let [cov (integrals/change-of-variable [m/inf- m/inf+])]
    (s/explain ::mul1 (::integrals/multiplicative-fn cov))
    (s/explain ::con1 (::integrals/converter-fn cov))
    (is= [-1.0 1.0] (::intervals/finite-interval cov)))
  (let [cov2 (integrals/change-of-variable [3.0 4.0])]
    (s/explain ::mul2 (::integrals/multiplicative-fn cov2))
    (s/explain ::con2 (::integrals/converter-fn cov2))
    (is= [3.0 4.0] (::intervals/finite-interval cov2)))
  (let [cov3 (integrals/change-of-variable [3.0 m/inf+])]
    (s/explain ::mul3 (::integrals/multiplicative-fn cov3))
    (s/explain ::con3 (::integrals/converter-fn cov3))
    (is= [0.0 1.0] (::intervals/finite-interval cov3)))
  (let [cov4 (integrals/change-of-variable [m/inf- 4.0])]
    (s/explain ::mul3 (::integrals/multiplicative-fn cov4))
    (s/explain ::con4 (::integrals/converter-fn cov4))
    (is= [0.0 1.0] (::intervals/finite-interval cov4))))

#_(deftest integration-test
  #_(is (spec-check integrals/integration
                    {:coll-check-limit 10
                     :coll-error-limit 10
                     :fspec-iterations 10
                     :recursion-limit  1
                     :test-check       {:num-tests 1}}))
  ;;ordinary
  (is= 69.33333333333331 (integrals/integration m/sq [2.0 6.0]))
  (is= 2.886579864025407E-15 (integrals/integration m/cos [m/PI (* 5 m/PI)]))
  (is= -1.000000000000004 (integrals/integration m/cos [m/PI (* 5.5 m/PI)]))
  (is= 3.3306690738754696E-16 (integrals/integration m/cos [m/PI (* 6 m/PI)]))

  ;;change of variable -- takes almost a minute to run this one
  ;!!!!this is getting a NaN value, presumably something to do with error = NaN
  ;!!!!need to stop the integration upon a NaN I think --
  ; same as below -- why NaN?
  (is= {::anomalies/message  "Error contains NaN. Value: -Infinity"
        ::anomalies/fn       #'provisdom.math.integrals/adaptive-quadrature
        ::anomalies/category ::anomalies/no-solve}
       (integrals/integration m/cos [m/PI m/inf+]))
  (is= 0.19999999999999996
       (integrals/integration #(m/pow % -2.0) [5.0 m/inf+]))
  (is= 0.19999999999999996
       (integrals/integration #(m/pow % -2.0) [m/inf- -5.0]))
  (is= 1.7724538509055163
       (integrals/integration #(m/exp (- (m/sq %))) [m/inf- m/inf+]))
  ;;vector
  (is= [15.999999999999998 69.33333333333333]
       (integrals/integration
         #(vector % (m/sq %))
         [2.0 6.0]))
  ;   with change of var
  (is= [1.7724538509055163 1.8128049541109543]
       (integrals/integration
         #(vector (m/exp (- (m/sq %))) (m/exp (- (m/pow % 4))))
         [m/inf- m/inf+]))
  ;;matrix
  (is= [[15.999999999999998 69.33333333333334] [24.0 19.999999999999996]]
       (integrals/integration
         #(vector [% (m/sq %)] [(+ 2.0 %) 5.0])
         [2.0 6.0]))
  ;   with change of var
  (is= [[6.400000000000001E-5 0.002666666666666666] [0.2 0.2]]
       (integrals/integration
         #(vector [(m/pow % -6) (m/pow % -4)] [(m/pow % -2.0) (m/pow % -2.0)])
         [5.0 m/inf+])))

#_(deftest rectangular-integration-test
  (is (spec-check integrals/rectangular-integration
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 1}}))
  #_(is= 1.0
       (integrals/rectangular-integration
         (fn [[a b]]
           (let [a (or a 0.0)
                 b (or b 0.0)]
             (+ (double a) b)))
         [[0.0 1.0] [0.0 1.0]]))
  #_(is= 1.5                                                ;pretty slow
         (integrals/rectangular-integration
           (fn [[a b c]]
             (let [a (or a 0.0)
                   b (or b 0.0)
                   c (or c 0.0)]
               (+ (double a) b c)))
           [[0.0 1.0] [0.0 1.0] [0.0 1.0]]))
  #_(is= 2.0                                                ;slow
         (integrals/rectangular-integration
           (fn [[a b c d]]
             (let [a (or a 0.0)
                   b (or b 0.0)
                   c (or c 0.0)
                   d (or d 0.0)]
               (+ (double a) b c d)))
           [[0.0 1.0] [0.0 1.0] [0.0 1.0] [0.0 1.0]]))
  ;;slow-ish 3.141592653589793 PI
  #_(is= 3.1415926535897944
         (integrals/rectangular-integration
           (fn [[a b]]
             (let [a (or a 0.0)
                   b (or b 0.0)]
               (m/exp (- (+ (m/sq a) (m/sq b))))))
           [[m/inf- m/inf+] [m/inf- m/inf+]]))
  #_(is= 0.1
       (integrals/rectangular-integration
         (fn [[a b]]
           (let [a (or a 0.0)
                 b (or b 0.0)]
             (m/pow (* (double a) b) -2.0)))
         [[5.0 m/inf+] [1.0 2.0]]))
  #_(is= 0.01666666666666667
       (integrals/rectangular-integration
         (fn [[a b]]
           (let [a (or a 0.0)
                 b (or b 0.0)]
             (m/pow (* (double a) b) -2.0)))
         [[3.0 4.0] [m/inf- -5.0]]))
  ;;vector
  #_(is= [32.0 64.0]
       (integrals/rectangular-integration
         (fn [[a b]]
           (let [a (or a 0.0)
                 b (or b 0.0)]
             [a (* (double a) b)]))
         [[2.0 6.0] [1.0 3.0]]))
  ;   with change of var
  #_(is= [0.24705031697079533 0.24705031697079533]
       (integrals/rectangular-integration
         (fn [[a b]]
           (let [a (or a 0.0)
                 b (or b 0.0)
                 c (m/exp (- (+ (m/sq a) (m/sq b))))]
             [c c]))
         [[m/inf- m/inf+] [1.0 3.0]]))
  ;;matrix
  #_(is= [[32.0 64.0] [32.0 40.00000000000001]]
       (integrals/rectangular-integration
         (fn [[a b]]
           (let [a (or a 0.0)
                 b (or b 0.0)]
             [[a (* (double a) b)] [(+ 2.0 b) 5.0]]))
         [[2.0 6.0] [1.0 3.0]]))
  ;   with change of var
  #_(is= [[0.06666666666666667 0.06666666666666667]
        [0.06666666666666667 0.06666666666666667]]
       (integrals/rectangular-integration
         (fn [[a b]]
           (let [a (or a 0.0)
                 b (or b 0.0)
                 c (m/pow (* (double a) b) -2.0)]
             [[c c] [c c]]))
         [[2.0 6.0] [m/inf- -5.0]])))

#_(deftest non-rectangular-2D-integration-test              ;too slow
    (is (spec-check integrals/non-rectangular-2D-integration
                    {:coll-check-limit 10
                     :coll-error-limit 10
                     :fspec-iterations 10
                     :recursion-limit  1
                     :test-check       {:num-tests 1}}))
    (is= 1.0
         (integrals/non-rectangular-2D-integration
           (fn [outer inner]
             (+ outer (double inner)))
           [0.0 1.0]
           (fn [outer]
             [0.0 1.0])))
    (is= 192.0
         (integrals/non-rectangular-2D-integration
           (fn [outer inner]
             (+ outer inner))
           [2.0 6.0]
           (fn [outer]
             [(+ outer 2.0) (+ outer 6.0)])))
    (is= 3.141592653589793                                  ;3.141592653589793 PI
         (integrals/non-rectangular-2D-integration
           (fn [outer inner]
             (* (m/exp (- (m/sq outer))) (m/exp (- (m/sq inner)))))
           [m/inf- m/inf+]
           (fn [outer]
             [m/inf- m/inf+])))
    (is= [32.0 64.0]
         (integrals/non-rectangular-2D-integration
           (fn [outer inner]
             (vector outer (* outer (double inner))))
           [2.0 6.0]
           (fn [outer]
             [1.0 3.0])))
    (is= [0.24705031697079533 0.24705031697079533]
         (integrals/non-rectangular-2D-integration
           (fn [outer inner]
             (let [val (* (m/exp (- (m/sq outer))) (m/exp (- (m/sq inner))))]
               [val val]))
           [m/inf- m/inf+]
           (fn [outer]
             [1.0 3.0])))
    (is= [[32.0 64.0] [32.0 39.99999999999999]]
         (integrals/non-rectangular-2D-integration
           (fn [outer inner]
             (vector [outer (* outer (double inner))] [(+ 2.0 inner) 5.0]))
           [2.0 6.0]
           (fn [outer]
             [1.0 3.0])))
    (is= [[0.06666666666666667 0.06666666666666667]
          [0.06666666666666667 0.06666666666666667]]
         (integrals/non-rectangular-2D-integration
           (fn [outer inner]
             (let [val (m/pow (* (double outer) inner) -2.0)]
               [[val val] [val val]]))
           [2.0 6.0]
           (fn [outer]
             [m/inf- -5.0]))))

#_(deftest non-rectangular-3D-integration-test              ;too slow
    (is (spec-check integrals/non-rectangular-3D-integration
                    {:coll-check-limit 10
                     :coll-error-limit 10
                     :fspec-iterations 10
                     :recursion-limit  1
                     :test-check       {:num-tests 1}}))
    (is= 1.5
         (integrals/non-rectangular-3D-integration
           (fn [outer middle inner]
             (+ (double outer) middle inner))
           [0.0 1.0]
           (fn [outer]
             [0.0 1.0])
           (fn [outer middle]
             [0.0 1.0]))))

#_(deftest non-rectangular-4D-integration-test              ;too slow
    (is (spec-check integrals/non-rectangular-4D-integration
                    {:coll-check-limit 10
                     :coll-error-limit 10
                     :fspec-iterations 10
                     :recursion-limit  1
                     :test-check       {:num-tests 1}}))
    (is= 2.0000000000000004
         (integrals/non-rectangular-4D-integration
           (fn [outer outer-middle inner-middle inner]
             (+ (double outer) outer-middle inner-middle inner))
           [0.0 1.0]
           (fn [outer]
             [0.0 1.0])
           (fn [outer outer-middle]
             [0.0 1.0])
           (fn [outer outer-middle inner-middle]
             [0.0 1.0]))))

#_(ost/unstrument)