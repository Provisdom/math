(ns provisdom.math.polynomials-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :as t]
    [provisdom.math.polynomials :as poly]))
;;1 SECONDS
(set! *warn-on-reflection* true)

;;;CHEBYSHEV POLYNOMIALS
(deftest chebyshev-polynomial-fn-test
  (t/with-instrument `poly/chebyshev-polynomial-fn
    (t/is (t/spec-check poly/chebyshev-polynomial-fn)))
  (t/with-instrument :all
    (t/is= 1.0 ((poly/chebyshev-polynomial-fn 0) 1.0))
    (t/is= 1.0 ((poly/chebyshev-polynomial-fn 1) 1.0))
    (t/is= 1.0 ((poly/chebyshev-polynomial-fn 2) 1.0))
    (t/is= 1.0 ((poly/chebyshev-polynomial-fn 3) 1.0))
    (t/is= 1.0 ((poly/chebyshev-polynomial-fn 4) 1.0))
    (t/is= 1.0 ((poly/chebyshev-polynomial-fn 5) 1.0))
    (t/is= 1.0 ((poly/chebyshev-polynomial-fn 6) 1.0))
    (t/is= 1.0 ((poly/chebyshev-polynomial-fn 7) 1.0))
    (t/is= 1.0 ((poly/chebyshev-polynomial-fn 8) 1.0))
    (t/is= 1.0 ((poly/chebyshev-polynomial-fn 9) 1.0))
    (t/is= 1.0 ((poly/chebyshev-polynomial-fn 10) 1.0))
    (t/is= 1.0 ((poly/chebyshev-polynomial-fn 11) 1.0))
    (t/is= 1.0 ((poly/chebyshev-polynomial-fn 12) 1.0))
    (t/is= 1.0 ((poly/chebyshev-polynomial-fn 13) 1.0))
    (t/is= 2.0 ((poly/chebyshev-polynomial-fn 1) 2.0))
    (t/is= 17.0 ((poly/chebyshev-polynomial-fn 2) 3.0))
    (t/is= 244.0 ((poly/chebyshev-polynomial-fn 3) 4.0))
    (t/is= 4801.0 ((poly/chebyshev-polynomial-fn 4) 5.0))
    (t/is= 120126.0 ((poly/chebyshev-polynomial-fn 5) 6.0))
    (t/is= 3650401.0 ((poly/chebyshev-polynomial-fn 6) 7.0))
    (t/is= 1.30576328E8 ((poly/chebyshev-polynomial-fn 7) 8.0))
    (t/is= 5.374978561E9 ((poly/chebyshev-polynomial-fn 8) 9.0))
    (t/is= 2.5028308009E11 ((poly/chebyshev-polynomial-fn 9) 10.0))
    (t/is= 1.3007560326001E13 ((poly/chebyshev-polynomial-fn 10) 11.0))
    (t/is= 7.46411226303612E14 ((poly/chebyshev-polynomial-fn 11) 12.0))
    (t/is= 4.687309681236E16 ((poly/chebyshev-polynomial-fn 12) 13.0))
    (t/is= 1.0 ((poly/chebyshev-polynomial-fn 0 {::poly/second-kind? true}) 1.0))
    (t/is= 2.0 ((poly/chebyshev-polynomial-fn 1 {::poly/second-kind? true}) 1.0))
    (t/is= 3.0 ((poly/chebyshev-polynomial-fn 2 {::poly/second-kind? true}) 1.0))
    (t/is= 4.0 ((poly/chebyshev-polynomial-fn 3 {::poly/second-kind? true}) 1.0))
    (t/is= 5.0 ((poly/chebyshev-polynomial-fn 4 {::poly/second-kind? true}) 1.0))
    (t/is= 6.0 ((poly/chebyshev-polynomial-fn 5 {::poly/second-kind? true}) 1.0))
    (t/is= 7.0 ((poly/chebyshev-polynomial-fn 6 {::poly/second-kind? true}) 1.0))
    (t/is= 8.0 ((poly/chebyshev-polynomial-fn 7 {::poly/second-kind? true}) 1.0))
    (t/is= 9.0 ((poly/chebyshev-polynomial-fn 8 {::poly/second-kind? true}) 1.0))
    (t/is= 10.0 ((poly/chebyshev-polynomial-fn 9 {::poly/second-kind? true}) 1.0))
    (t/is= 11.0 ((poly/chebyshev-polynomial-fn 10 {::poly/second-kind? true}) 1.0))
    (t/is= 12.0 ((poly/chebyshev-polynomial-fn 11 {::poly/second-kind? true}) 1.0))
    (t/is= 13.0 ((poly/chebyshev-polynomial-fn 12 {::poly/second-kind? true}) 1.0))
    (t/is= 14.0 ((poly/chebyshev-polynomial-fn 13 {::poly/second-kind? true}) 1.0))
    (t/is= 4.0 ((poly/chebyshev-polynomial-fn 1 {::poly/second-kind? true}) 2.0))
    (t/is= 35.0 ((poly/chebyshev-polynomial-fn 2 {::poly/second-kind? true}) 3.0))
    (t/is= 496.0 ((poly/chebyshev-polynomial-fn 3 {::poly/second-kind? true}) 4.0))
    (t/is= 9701.0 ((poly/chebyshev-polynomial-fn 4 {::poly/second-kind? true}) 5.0))
    (t/is= 241956.0 ((poly/chebyshev-polynomial-fn 5 {::poly/second-kind? true}) 6.0))
    (t/is= 7338631.0 ((poly/chebyshev-polynomial-fn 6 {::poly/second-kind? true}) 7.0))
    (t/is= 2.62184896E8 ((poly/chebyshev-polynomial-fn 7 {::poly/second-kind? true}) 8.0))
    (t/is= 1.0783446409E10 ((poly/chebyshev-polynomial-fn 8 {::poly/second-kind? true}) 9.0))
    (t/is= 5.018270401E11 ((poly/chebyshev-polynomial-fn 9 {::poly/second-kind? true}) 10.0))
    (t/is= 2.6069206375211E13 ((poly/chebyshev-polynomial-fn 10 {::poly/second-kind? true}) 11.0))
    (t/is= 1.4954277353148E15 ((poly/chebyshev-polynomial-fn 11 {::poly/second-kind? true}) 12.0))
    (t/is= 9.3885489910449904E16
      ((poly/chebyshev-polynomial-fn 12 {::poly/second-kind? true}) 13.0))
    (t/is= 41.76799999999999 ((poly/chebyshev-polynomial-fn 3) 2.3))
    (t/is= -0.7307444539392003 ((poly/chebyshev-polynomial-fn 13) 0.3))
    (t/is= 88.13599999999997 ((poly/chebyshev-polynomial-fn 3 {::poly/second-kind? true}) 2.3))
    (t/is= -0.9454282973183997 ((poly/chebyshev-polynomial-fn 13 {::poly/second-kind? true}) 0.3))))

(deftest chebyshev-derivative-fn-test
  (t/with-instrument `poly/chebyshev-derivative-fn
    (t/is (t/spec-check poly/chebyshev-derivative-fn)))
  (t/with-instrument :all
    (t/is= 0.0 ((poly/chebyshev-derivative-fn 0 0) 1.0))
    (t/is= 0.0 ((poly/chebyshev-derivative-fn 0 1) 1.0))
    (t/is= 1.0 ((poly/chebyshev-derivative-fn 1 1) 1.0))
    (t/is= 0.0 ((poly/chebyshev-derivative-fn 1 2) 1.0))
    (t/is= 4.0 ((poly/chebyshev-derivative-fn 2 1) 1.0))
    (t/is= 4.0 ((poly/chebyshev-derivative-fn 2 2) 1.0))
    (t/is= 9.0 ((poly/chebyshev-derivative-fn 3 1) 1.0))
    (t/is= 24.0 ((poly/chebyshev-derivative-fn 3 2) 1.0))
    (t/is= 23.99999998070612 ((poly/chebyshev-derivative-fn 3 3) 1.0))
    (t/is= 2.0 ((poly/chebyshev-derivative-fn 1 1 {::poly/second-kind? true}) 1.0))
    (t/is= 2.220446049250313E-9 ((poly/chebyshev-derivative-fn 1 2 {::poly/second-kind? true}) 1.0))
    (t/is= 8.0 ((poly/chebyshev-derivative-fn 2 1 {::poly/second-kind? true}) 1.0))
    (t/is= 8.00000000911183 ((poly/chebyshev-derivative-fn 2 2 {::poly/second-kind? true}) 1.0))
    (t/is= 20.0 ((poly/chebyshev-derivative-fn 3 1 {::poly/second-kind? true}) 1.0))
    (t/is= 48.00000002358473 ((poly/chebyshev-derivative-fn 3 2 {::poly/second-kind? true}) 1.0))
    (t/is= 47.99999995697135 ((poly/chebyshev-derivative-fn 3 3 {::poly/second-kind? true}) 1.0))))

(deftest chebyshev-poly-factors-to-regular-poly-factors-test
  (t/with-instrument `poly/chebyshev-poly-factors-to-regular-poly-factors
    (t/is (t/spec-check poly/chebyshev-poly-factors-to-regular-poly-factors)))
  (t/with-instrument :all
    (t/is= [3.0 4.999999999810711] (poly/chebyshev-poly-factors-to-regular-poly-factors [3.0 5.0]))
    (t/is= [3.0 10.000000000065512]
      (poly/chebyshev-poly-factors-to-regular-poly-factors
        [3.0 5.0]
        {::poly/second-kind? true}))
    (t/is= [-1.0 -13.00000000020729 15.999999999349868 143.99999999969992]
      (poly/chebyshev-poly-factors-to-regular-poly-factors [3.0 5.0 4.0 6.0]))
    (t/is= [-1.0 -13.999999999902979 31.99999999980996 287.99999999939985]
      (poly/chebyshev-poly-factors-to-regular-poly-factors
        [3.0 5.0 4.0 6.0]
        {::poly/second-kind? true}))))

;;;POLYNOMIAL SERIES
(deftest polynomial-fn-test
  (t/with-instrument `poly/polynomial-fn
    (t/is (t/spec-check poly/polynomial-fn)))
  (t/with-instrument :all
    (t/is= [1.0 4.0 16.0 64.0] ((poly/polynomial-fn 3) 4.0))
    (t/is= [4.0 16.0 64.0] ((poly/polynomial-fn 3 {::poly/start-degree 1}) 4.0))
    (t/is= [1.0 4.0 31.0 244.0] ((poly/polynomial-fn 3 {::poly/chebyshev-kind 1}) 4.0))
    (t/is= [1.0 8.0 63.0 496.0] ((poly/polynomial-fn 3 {::poly/chebyshev-kind 2}) 4.0))))

(deftest polynomial-fns-test
  (t/with-instrument `poly/polynomial-fns
    (t/is (t/spec-check poly/polynomial-fns)))
  (t/with-instrument :all
    (t/is= [1.0 4.0 16.0 64.0] (map #(% 4.0) (poly/polynomial-fns 3)))
    (t/is= [4.0 16.0 64.0] (map #(% 4.0) (poly/polynomial-fns 3 {::poly/start-degree 1})))
    (t/is= [1.0 4.0 31.0 244.0] (map #(% 4.0) (poly/polynomial-fns 3 {::poly/chebyshev-kind 1})))
    (t/is= [1.0 8.0 63.0 496.0] (map #(% 4.0) (poly/polynomial-fns 3 {::poly/chebyshev-kind 2})))))

(deftest polynomial-2D-count-test
  (t/with-instrument `poly/polynomial-2D-count
    (t/is (t/spec-check poly/polynomial-2D-count)))
  (t/with-instrument :all
    (t/is= 10 (poly/polynomial-2D-count 3))
    (t/is= 9 (poly/polynomial-2D-count 3 {::poly/start-degree 1}))))

(deftest polynomial-2D-fn-by-degree-test
  (t/with-instrument `poly/polynomial-2D-fn-by-degree
    (t/is (t/spec-check poly/polynomial-2D-fn-by-degree)))
  (t/with-instrument :all
    (t/is= [1.0 2.0 4.0 4.0 8.0 16.0 8.0 16.0 32.0 64.0]
      ((poly/polynomial-2D-fn-by-degree 3) 4.0 2.0))
    (t/is= [2.0 4.0 4.0 8.0 16.0 8.0 16.0 32.0 64.0]
      ((poly/polynomial-2D-fn-by-degree 3 {::poly/start-degree 1}) 4.0 2.0))
    (t/is= [1.0 2.0 4.0 7.0 8.0 31.0 26.0 28.0 62.0 244.0]
      ((poly/polynomial-2D-fn-by-degree 3 {::poly/chebyshev-kind 1}) 4.0 2.0))
    (t/is= [1.0 4.0 8.0 15.0 32.0 63.0 56.0 120.0 252.0 496.0]
      ((poly/polynomial-2D-fn-by-degree 3 {::poly/chebyshev-kind 2}) 4.0 2.0))))

(deftest polynomial-2D-fn-by-basis-count-test
  (t/with-instrument `poly/polynomial-2D-fn-by-basis-count
    (t/is (t/spec-check poly/polynomial-2D-fn-by-basis-count)))
  (t/with-instrument :all
    (t/is= [1.0 2.0 4.0 4.0] ((poly/polynomial-2D-fn-by-basis-count 4) 4.0 2.0))))

(deftest polynomial-ND-fn-test
  (t/with-instrument `poly/polynomial-ND-fn
    (t/is (t/spec-check poly/polynomial-ND-fn)))
  (t/with-instrument :all
    (t/is= [1.0 2.0 3.0 4.0 4.0 6.0 8.0 9.0 12.0 16.0 12.0 16.0 18.0 24.0 32.0 36.0
          48.0 36.0 48.0 64.0 72.0 96.0 144.0 144.0 192.0 288.0 576.0]
      ((poly/polynomial-ND-fn 2) [2.0 3.0 4.0]))))
(deftest polynomial-ND-fn-without-cross-terms-test
  (t/with-instrument `poly/polynomial-ND-fn-without-cross-terms
    (t/is (t/spec-check poly/polynomial-ND-fn-without-cross-terms)))
  (t/with-instrument :all
    (t/is= [1.0 2.0 3.0 4.0 4.0 9.0 16.0 8.0 27.0 64.0]
      ((poly/polynomial-ND-fn-without-cross-terms 3) [2.0 3.0 4.0]))))
