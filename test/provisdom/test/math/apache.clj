(ns provisdom.test.math.apache
  (:require [midje.sweet :refer :all]
            [criterium.core :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.apache :refer :all]
            [provisdom.math [core :as m]
             [matrix :as mx]]))

(fact "interpolation 2D"
      (def xs [2.0 4.0 5.0 6.0 7.0])
      (def ys [18.0 20.0 21.0 23.0 26.0 45.0])
      (def fxys (fn [x y] (+ (m/log y) x (* (m/exp (- x)) (+ y (m/cube y))))))
      (def fxy 
        (mx/matrix nil (fn [i j] (fxys (nth xs i) (nth ys j))) 
                   [(count xs) (count ys)]))
      ((:val-fn (interpolation-2D xs ys fxy true)) 3 20) => 707.8318163816091
      ((:valid-fn? (interpolation-2D xs ys fxy true)) 4.1 26.0) => true
      ((:valid-fn? (interpolation-2D xs ys fxy true)) 2.1 20.0) => false 
      ((:val-fn (interpolation-2D xs ys fxy false)) 3 20) => 496.6707017028581
      ((:valid-fn? (interpolation-2D xs ys fxy false)) 3 20) => true )

(fact "nonlinear least squares"
      (defn- constraints-fn [x] 
        (let [a (first x), b (second x)] 
          [(- a b 0.5) (- (m/sq a) (m/sq b) 0.5)]))
      (def ncons 2)
      (defn- jac [x]
        (let [a (first x), b (second x)] 
          [[1 -1][(* 2 a) (* 2 b)]]))
      (def guess [1.0 1.0])
      (nonlinear-least-squares constraints-fn ncons jac guess) 
      => {:errors [0.0 -6.801038283654748E-7], 
          :point [0.7500006801038284 0.25000068010382837]})
       
      