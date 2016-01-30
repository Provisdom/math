(ns provisdom.test.math.series
  (:require [midje.sweet :refer :all]
            [criterium.core :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.series :refer :all]
            [provisdom.utility-belt.core :as co]
            [provisdom.math [core :as m]
             [combinatorics :as cm]]))

(facts "chebyshev"
       (fact "chebyshev-polynomial-fn"
             ((chebyshev-polynomial-fn 0) 1.0) => 1.0
             ((chebyshev-polynomial-fn 1) 1.0) => 1.0
             ((chebyshev-polynomial-fn 2) 1.0) => 1.0
             ((chebyshev-polynomial-fn 3) 1.0) => 1.0
             ((chebyshev-polynomial-fn 4) 1.0) => 1.0
             ((chebyshev-polynomial-fn 5) 1.0) => 1.0
             ((chebyshev-polynomial-fn 6) 1.0) => 1.0
             ((chebyshev-polynomial-fn 7) 1.0) => 1.0
             ((chebyshev-polynomial-fn 8) 1.0) => 1.0
             ((chebyshev-polynomial-fn 9) 1.0) => 1.0
             ((chebyshev-polynomial-fn 10) 1.0) => 1.0
             ((chebyshev-polynomial-fn 11) 1.0) => 1.0
             ((chebyshev-polynomial-fn 12) 1.0) => 1.0
             ((chebyshev-polynomial-fn 13) 1.0) => 1.0 
             ((chebyshev-polynomial-fn 0 true) 1.0) => 1.0
             ((chebyshev-polynomial-fn 1 true) 1.0) => 2.0
             ((chebyshev-polynomial-fn 2 true) 1.0) => 3.0
             ((chebyshev-polynomial-fn 3 true) 1.0) => 4.0
             ((chebyshev-polynomial-fn 4 true) 1.0) => 5.0
             ((chebyshev-polynomial-fn 5 true) 1.0) => 6.0
             ((chebyshev-polynomial-fn 6 true) 1.0) => 7.0
             ((chebyshev-polynomial-fn 7 true) 1.0) => 8.0
             ((chebyshev-polynomial-fn 8 true) 1.0) => 9.0
             ((chebyshev-polynomial-fn 9 true) 1.0) => 10.0
             ((chebyshev-polynomial-fn 10 true) 1.0) => 11.0
             ((chebyshev-polynomial-fn 11 true) 1.0) => 12.0
             ((chebyshev-polynomial-fn 3) 2.3) => 41.76799999999999
             ((chebyshev-polynomial-fn 13) 0.3) => -1.3655874457599992
             ((chebyshev-polynomial-fn 3 true) 2.3) => 88.13599999999997
             ((chebyshev-polynomial-fn 13 true) 0.3) => 4.358627839999997)
       (fact "chebyshev-derivative-fn"
             ((chebyshev-derivative-fn 0 0) 1.0) => (throws)
             ((chebyshev-derivative-fn 0 1) 1.0) => 0.0
             ((chebyshev-derivative-fn 1 1) 1.0) => 1.0
             ((chebyshev-derivative-fn 1 2) 1.0) => 0.0
             ((chebyshev-derivative-fn 2 1) 1.0) => 4.0
             ((chebyshev-derivative-fn 2 2) 1.0) => 4.0
             ((chebyshev-derivative-fn 3 1) 1.0) => 9.0
             ((chebyshev-derivative-fn 3 2) 1.0) => 24.0
             ((chebyshev-derivative-fn 3 3) 1.0) => 47.99999995697135
             ((chebyshev-derivative-fn 1 1 true) 1.0) => 2.0
             ((chebyshev-derivative-fn 1 2 true) 1.0) => 2.220446049250313E-9
             ((chebyshev-derivative-fn 2 1 true) 1.0) => 8.0
             ((chebyshev-derivative-fn 2 2 true) 1.0) => 8.00000000911183
             ((chebyshev-derivative-fn 3 1 true) 1.0) => 20.0
             ((chebyshev-derivative-fn 3 2 true) 1.0) => 48.00000002358473
             ((chebyshev-derivative-fn 3 3 true) 1.0) => 47.99999995697135)
       (fact "chebyshev-polynomial-factors-to-regular-polynomial-factors"
             (chebyshev-polynomial-factors-to-regular-polynomial-factors 
               [3.0 5.0]) => [3.0 4.999999999810711]
             (chebyshev-polynomial-factors-to-regular-polynomial-factors 
               [3.0 5.0] true) => [3.0 10.000000000065512]
             (chebyshev-polynomial-factors-to-regular-polynomial-factors 
               [3.0 5.0 4.0 6.0]) 
             => [-1.0 -13.00000000020729 15.999999999349868 143.99999999969992]
             (chebyshev-polynomial-factors-to-regular-polynomial-factors 
               [3.0 5.0 4.0 6.0] true) 
             => [-1.0 -13.999999999902979 31.99999999980996 
                 287.99999999939985]))

 (facts "series"
        (fact "polynomial"
             ((polynomial-fn 3) 4.0) => [1.0 4.0 16.0 64.0]
             ((polynomial-fn 3 :start-degree 1) 4.0) => [4.0 16.0 64.0]
             ((polynomial-fn 3 :chebyshev-kind 1) 4.0) => [1.0 4.0 31.0 244.0]
             ((polynomial-fn 3 :chebyshev-kind 2) 4.0) => [1.0 8.0 63.0 496.0]
             (map #(% 4.0) (polynomial-fns 3)) => [1.0 4.0 16.0 64.0]                        
             (map #(% 4.0) (polynomial-fns 3 :start-degree 1)) 
             => [4.0 16.0 64.0]
             (map #(% 4.0) (polynomial-fns 3 :chebyshev-kind 1)) 
             => [1.0 4.0 31.0 244.0]
             (map #(% 4.0) (polynomial-fns 3 :chebyshev-kind 2)) 
             => [1.0 8.0 63.0 496.0]
             (polynomial-2D-count 3) => 10
             (polynomial-2D-count 1 3) => 9
             ((polynomial-2D-fn 3) [4.0 2.0]) 
             => [1.0 2.0 4.0 4.0 8.0 16.0 8.0 16.0 32.0 64.0]
             ((polynomial-2D-fn 3 :start-degree 1) [4.0 2.0]) 
             => [2.0 4.0 4.0 8.0 16.0 8.0 16.0 32.0 64.0]
             ((polynomial-2D-fn 3 :chebyshev-kind 1) [4.0 2.0]) 
             => [1.0 2.0 4.0 7.0 8.0 31.0 26.0 28.0 62.0 244.0]
             ((polynomial-2D-fn 3 :chebyshev-kind 2) [4.0 2.0]) 
             => [1.0 4.0 8.0 15.0 32.0 63.0 56.0 120.0 252.0 496.0]
             ((polynomial-2D-fn 4 :by-count? true) [4.0 2.0]) 
             => [1.0 2.0 4.0 4.0]
             ((polynomial-ND-fn 3 3) [2.0 3.0 4.0]) 
             => [1.0 2.0 3.0 4.0 4.0 6.0 8.0 9.0 12.0 16.0 8.0 12.0 16.0 18.0 
                 24.0 32.0 27.0 36.0 48.0 64.0]
             ((polynomial-ND-fn-without-cross 3 3) [2.0 3.0 4.0]) 
             => [1.0 2.0 4.0 3.0 9.0 4.0 16.0])
        (fact "power-series: (a_n * x^n)"
              ((power-series-fn [11 12 13]) 2.0) => [11.0 24.0 52.0])
        (fact "power-series derivative"
              ((power-series-derivative-fn [11 12 13]) 2.0) => [0.0 12.0 52.0])
        (fact "power-series integral"
              ((power-series-integral-fn [11 12 13]) 2.0) 
              => [22.0 24.0 34.666666666666664])
        (fact "continued fraction a0 + (1 / (a1 + 1 / (a2 + 1 / (a3 + ..."
              (continued-fraction [1.0 3.0 6.0 8.0]) 
              => [1.0 -0.25 0.01 -1.96078431372549E-4])
        (fact "generalized continued fraction 
                  a0 + (b0 / (a1 + b1 / (a2 + b2 / (a3 + ..."
              (generalized-continued-fraction 
                [1.0 3.0 6.0 8.0] [2.0 3.0 2.0 6.0]) 
              => [1.0 0.6666666666666667 -0.09523809523809534 
                  0.003284072249589487]))
 
(facts "sum convergent series"
       (fact "normally returns series sum -- options to change stopping 
                 predicate, error predicate, and error message"
             (sum-convergent-series [1.02 3.05 7.05]) => 11.120000000000001
             (sum-convergent-series [1 3 7]) => 11
             (defn sin-series [x] (map #(* (/ (cm/factorial (inc (* 2 %)))) 
                                           x (m/pow (- (m/sq x)) %)) (range)))
             (sum-convergent-series (sin-series m/PI)) 
             => (test-roughly -1.4288601243858605E-15 1e-14)
             (sum-convergent-series (sin-series (* 1.5 m/PI))) 
             => -1.0000000000000047
             (sum-convergent-series (sin-series (* 2 m/PI))) 
             => (test-roughly -6.011088767600521E-15 1e-14)
             (sum-convergent-series (sin-series (* 2.5 m/PI))) 
             => 0.9999999999998882)
       (fact "can apply floating-point kahan sum for slightly greater 
                 accuracy"
             (sum-convergent-series [1.02 3.05 7.05] :kahan? true) => 11.12
             (sum-convergent-series (sin-series (* 2.5 m/PI)) :kahan? true) 
             => 0.9999999999998883))
