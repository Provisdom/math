(ns provisdom.math.polynomials-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.polynomials :as polynomials]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;;20 SECONDS

(set! *warn-on-reflection* true)

(ost/instrument)

;;;CHEBYSHEV POLYNOMIALS
(deftest chebyshev-polynomial-fn-test
  (is (spec-check polynomials/chebyshev-polynomial-fn))
  (is= 1.0 ((polynomials/chebyshev-polynomial-fn 0) 1.0))
  (is= 1.0 ((polynomials/chebyshev-polynomial-fn 1) 1.0))
  (is= 1.0 ((polynomials/chebyshev-polynomial-fn 2) 1.0))
  (is= 1.0 ((polynomials/chebyshev-polynomial-fn 3) 1.0))
  (is= 1.0 ((polynomials/chebyshev-polynomial-fn 4) 1.0))
  (is= 1.0 ((polynomials/chebyshev-polynomial-fn 5) 1.0))
  (is= 1.0 ((polynomials/chebyshev-polynomial-fn 6) 1.0))
  (is= 1.0 ((polynomials/chebyshev-polynomial-fn 7) 1.0))
  (is= 1.0 ((polynomials/chebyshev-polynomial-fn 8) 1.0))
  (is= 1.0 ((polynomials/chebyshev-polynomial-fn 9) 1.0))
  (is= 1.0 ((polynomials/chebyshev-polynomial-fn 10) 1.0))
  (is= 1.0 ((polynomials/chebyshev-polynomial-fn 11) 1.0))
  (is= 1.0 ((polynomials/chebyshev-polynomial-fn 12) 1.0))
  (is= 1.0 ((polynomials/chebyshev-polynomial-fn 13) 1.0))
  (is= 2.0 ((polynomials/chebyshev-polynomial-fn 1) 2.0))
  (is= 17.0 ((polynomials/chebyshev-polynomial-fn 2) 3.0))
  (is= 244.0 ((polynomials/chebyshev-polynomial-fn 3) 4.0))
  (is= 4801.0 ((polynomials/chebyshev-polynomial-fn 4) 5.0))
  (is= 120126.0 ((polynomials/chebyshev-polynomial-fn 5) 6.0))
  (is= 3650401.0 ((polynomials/chebyshev-polynomial-fn 6) 7.0))
  (is= 1.30576328E8 ((polynomials/chebyshev-polynomial-fn 7) 8.0))
  (is= 5.374978561E9 ((polynomials/chebyshev-polynomial-fn 8) 9.0))
  (is= 2.5028308009E11 ((polynomials/chebyshev-polynomial-fn 9) 10.0))
  (is= 1.3007560326001E13 ((polynomials/chebyshev-polynomial-fn 10) 11.0))
  (is= 7.46411226303612E14 ((polynomials/chebyshev-polynomial-fn 11) 12.0))
  (is= 4.687309681236E16 ((polynomials/chebyshev-polynomial-fn 12) 13.0))
  (is= 1.0
       ((polynomials/chebyshev-polynomial-fn 0 {::polynomials/second-kind? true}) 1.0))
  (is= 2.0
       ((polynomials/chebyshev-polynomial-fn 1 {::polynomials/second-kind? true}) 1.0))
  (is= 3.0
       ((polynomials/chebyshev-polynomial-fn 2 {::polynomials/second-kind? true}) 1.0))
  (is= 4.0
       ((polynomials/chebyshev-polynomial-fn 3 {::polynomials/second-kind? true}) 1.0))
  (is= 5.0
       ((polynomials/chebyshev-polynomial-fn 4 {::polynomials/second-kind? true}) 1.0))
  (is= 6.0
       ((polynomials/chebyshev-polynomial-fn 5 {::polynomials/second-kind? true}) 1.0))
  (is= 7.0
       ((polynomials/chebyshev-polynomial-fn 6 {::polynomials/second-kind? true}) 1.0))
  (is= 8.0
       ((polynomials/chebyshev-polynomial-fn 7 {::polynomials/second-kind? true}) 1.0))
  (is= 9.0
       ((polynomials/chebyshev-polynomial-fn 8 {::polynomials/second-kind? true}) 1.0))
  (is= 10.0
       ((polynomials/chebyshev-polynomial-fn 9 {::polynomials/second-kind? true}) 1.0))
  (is= 11.0
       ((polynomials/chebyshev-polynomial-fn 10 {::polynomials/second-kind? true}) 1.0))
  (is= 12.0
       ((polynomials/chebyshev-polynomial-fn 11 {::polynomials/second-kind? true}) 1.0))
  (is= 13.0
       ((polynomials/chebyshev-polynomial-fn 12 {::polynomials/second-kind? true}) 1.0))
  (is= 14.0
       ((polynomials/chebyshev-polynomial-fn 13 {::polynomials/second-kind? true}) 1.0))
  (is= 4.0
       ((polynomials/chebyshev-polynomial-fn 1 {::polynomials/second-kind? true}) 2.0))
  (is= 35.0
       ((polynomials/chebyshev-polynomial-fn 2 {::polynomials/second-kind? true}) 3.0))
  (is= 496.0
       ((polynomials/chebyshev-polynomial-fn 3 {::polynomials/second-kind? true}) 4.0))
  (is= 9701.0
       ((polynomials/chebyshev-polynomial-fn 4 {::polynomials/second-kind? true}) 5.0))
  (is= 241956.0
       ((polynomials/chebyshev-polynomial-fn 5 {::polynomials/second-kind? true}) 6.0))
  (is= 7338631.0
       ((polynomials/chebyshev-polynomial-fn 6 {::polynomials/second-kind? true}) 7.0))
  (is= 2.62184896E8
       ((polynomials/chebyshev-polynomial-fn 7 {::polynomials/second-kind? true}) 8.0))
  (is= 1.0783446409E10
       ((polynomials/chebyshev-polynomial-fn 8 {::polynomials/second-kind? true}) 9.0))
  (is= 5.018270401E11
       ((polynomials/chebyshev-polynomial-fn 9 {::polynomials/second-kind? true}) 10.0))
  (is= 2.6069206375211E13
       ((polynomials/chebyshev-polynomial-fn 10 {::polynomials/second-kind? true}) 11.0))
  (is= 1.4954277353148E15
       ((polynomials/chebyshev-polynomial-fn 11 {::polynomials/second-kind? true}) 12.0))
  (is= 9.3885489910449904E16
       ((polynomials/chebyshev-polynomial-fn 12 {::polynomials/second-kind? true}) 13.0))
  (is= 41.76799999999999
       ((polynomials/chebyshev-polynomial-fn 3) 2.3))
  (is= -0.7307444539392003
       ((polynomials/chebyshev-polynomial-fn 13) 0.3))
  (is= 88.13599999999997
       ((polynomials/chebyshev-polynomial-fn 3 {::polynomials/second-kind? true}) 2.3))
  (is= -0.9454282973183997
       ((polynomials/chebyshev-polynomial-fn 13 {::polynomials/second-kind? true}) 0.3)))

(deftest chebyshev-derivative-fn-test
  (is (spec-check polynomials/chebyshev-derivative-fn))
  (is= 0.0 ((polynomials/chebyshev-derivative-fn 0 0) 1.0))
  (is= 0.0 ((polynomials/chebyshev-derivative-fn 0 1) 1.0))
  (is= 1.0 ((polynomials/chebyshev-derivative-fn 1 1) 1.0))
  (is= 0.0 ((polynomials/chebyshev-derivative-fn 1 2) 1.0))
  (is= 4.0 ((polynomials/chebyshev-derivative-fn 2 1) 1.0))
  (is= 4.0 ((polynomials/chebyshev-derivative-fn 2 2) 1.0))
  (is= 9.0 ((polynomials/chebyshev-derivative-fn 3 1) 1.0))
  (is= 24.0 ((polynomials/chebyshev-derivative-fn 3 2) 1.0))
  (is= 23.99999998070612 ((polynomials/chebyshev-derivative-fn 3 3) 1.0))
  (is= 2.0
       ((polynomials/chebyshev-derivative-fn 1 1 {::polynomials/second-kind? true}) 1.0))
  (is= 2.220446049250313E-9
       ((polynomials/chebyshev-derivative-fn 1 2 {::polynomials/second-kind? true}) 1.0))
  (is= 8.0
       ((polynomials/chebyshev-derivative-fn 2 1 {::polynomials/second-kind? true}) 1.0))
  (is= 8.00000000911183
       ((polynomials/chebyshev-derivative-fn 2 2 {::polynomials/second-kind? true}) 1.0))
  (is= 20.0
       ((polynomials/chebyshev-derivative-fn 3 1 {::polynomials/second-kind? true}) 1.0))
  (is= 48.00000002358473
       ((polynomials/chebyshev-derivative-fn 3 2 {::polynomials/second-kind? true}) 1.0))
  (is= 47.99999995697135
       ((polynomials/chebyshev-derivative-fn 3 3 {::polynomials/second-kind? true}) 1.0)))

(deftest chebyshev-polynomial-factors-to-regular-polynomial-factors-test
  (is (spec-check polynomials/chebyshev-polynomial-factors-to-regular-polynomial-factors))
  (is= [3.0 4.999999999810711]
       (polynomials/chebyshev-polynomial-factors-to-regular-polynomial-factors [3.0 5.0]))
  (is= [3.0 10.000000000065512]
       (polynomials/chebyshev-polynomial-factors-to-regular-polynomial-factors
         [3.0 5.0]
         {::polynomials/second-kind? true}))
  (is= [-1.0 -13.00000000020729 15.999999999349868 143.99999999969992]
       (polynomials/chebyshev-polynomial-factors-to-regular-polynomial-factors [3.0 5.0 4.0 6.0]))
  (is= [-1.0 -13.999999999902979 31.99999999980996 287.99999999939985]
       (polynomials/chebyshev-polynomial-factors-to-regular-polynomial-factors
         [3.0 5.0 4.0 6.0]
         {::polynomials/second-kind? true})))

;;;POLYNOMIAL SERIES
(deftest polynomial-fn-test
  (is (spec-check polynomials/polynomial-fn {:coll-check-limit 10
                                        :coll-error-limit      10
                                        :fspec-iterations      10
                                        :recursion-limit       1
                                        :test-check            {:num-tests 60}}))
  (is= [1.0 4.0 16.0 64.0] ((polynomials/polynomial-fn 3) 4.0))
  (is= [4.0 16.0 64.0]
       ((polynomials/polynomial-fn 3 {::polynomials/start-degree 1}) 4.0))
  (is= [1.0 4.0 31.0 244.0]
       ((polynomials/polynomial-fn 3 {::polynomials/chebyshev-kind 1}) 4.0))
  (is= [1.0 8.0 63.0 496.0]
       ((polynomials/polynomial-fn 3 {::polynomials/chebyshev-kind 2}) 4.0)))

(deftest polynomial-fns-test
  (is (spec-check polynomials/polynomial-fns {:coll-check-limit 10
                                         :coll-error-limit      10
                                         :fspec-iterations      10
                                         :recursion-limit       1
                                         :test-check            {:num-tests 50}}))
  (is= [1.0 4.0 16.0 64.0] (map #(% 4.0) (polynomials/polynomial-fns 3)))
  (is= [4.0 16.0 64.0]
       (map #(% 4.0) (polynomials/polynomial-fns 3 {::polynomials/start-degree 1})))
  (is= [1.0 4.0 31.0 244.0]
       (map #(% 4.0) (polynomials/polynomial-fns 3 {::polynomials/chebyshev-kind 1})))
  (is= [1.0 8.0 63.0 496.0]
       (map #(% 4.0) (polynomials/polynomial-fns 3 {::polynomials/chebyshev-kind 2}))))

(deftest polynomial-2D-count-test
  (is (spec-check polynomials/polynomial-2D-count))
  (is= 10 (polynomials/polynomial-2D-count 3))
  (is= 9 (polynomials/polynomial-2D-count 3 {::polynomials/start-degree 1})))

(deftest polynomial-2D-fn-by-degree-test
  (is (spec-check polynomials/polynomial-2D-fn-by-degree
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 400}}))
  (is= [1.0 2.0 4.0 4.0 8.0 16.0 8.0 16.0 32.0 64.0]
       ((polynomials/polynomial-2D-fn-by-degree 3) 4.0 2.0))
  (is= [2.0 4.0 4.0 8.0 16.0 8.0 16.0 32.0 64.0]
       ((polynomials/polynomial-2D-fn-by-degree 3 {::polynomials/start-degree 1}) 4.0 2.0))
  (is= [1.0 2.0 4.0 7.0 8.0 31.0 26.0 28.0 62.0 244.0]
       ((polynomials/polynomial-2D-fn-by-degree 3 {::polynomials/chebyshev-kind 1}) 4.0 2.0))
  (is= [1.0 4.0 8.0 15.0 32.0 63.0 56.0 120.0 252.0 496.0]
       ((polynomials/polynomial-2D-fn-by-degree 3 {::polynomials/chebyshev-kind 2}) 4.0 2.0)))

(deftest polynomial-2D-fn-by-basis-count-test
  (is (spec-check polynomials/polynomial-2D-fn-by-basis-count
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 200}}))
  (is= [1.0 2.0 4.0 4.0]
       ((polynomials/polynomial-2D-fn-by-basis-count 4) 4.0 2.0)))

(deftest polynomial-ND-fn-test
  ;slow because with degree 3 and a vector of length 6 will take about 6 seconds
  (is (spec-check polynomials/polynomial-ND-fn {:coll-check-limit 10
                                           :coll-error-limit      10
                                           :fspec-iterations      10
                                           :recursion-limit       1
                                           :test-check            {:num-tests 5}}))
  (is= [1.0 2.0 3.0 4.0 4.0 6.0 8.0 9.0 12.0 16.0 12.0 16.0 18.0 24.0 32.0 36.0
        48.0 36.0 48.0 64.0 72.0 96.0 144.0 144.0 192.0 288.0 576.0]
       ((polynomials/polynomial-ND-fn 2) [2.0 3.0 4.0])))

(deftest polynomial-ND-fn-without-cross-terms-test
  (is (spec-check polynomials/polynomial-ND-fn-without-cross-terms
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 30}}))
  (is= [1.0 2.0 3.0 4.0 4.0 9.0 16.0 8.0 27.0 64.0]
       ((polynomials/polynomial-ND-fn-without-cross-terms 3) [2.0 3.0 4.0])))

#_(ost/unstrument)