(ns provisdom.test.math.calculus
  (:require [midje.sweet :refer :all]
            [criterium.core :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.calculus :refer :all]
            [provisdom.math [core :as m]
             [matrix :as mx]]
            [provisdom.utility-belt.core :as co]))

(facts "integrate" 
       (fact "normally return the integral"
             (integrate #(* % %) [2.0 6.0]) => 69.33333333333331
             (integrate m/cos [m/PI (* 5 m/PI)]) => 2.886579864025407E-15
             (integrate m/cos [m/PI (* 5.5 m/PI)]) => -1.000000000000004
             (integrate m/cos [m/PI (* 6 m/PI)]) => 3.3306690738754696E-16)
       (fact "change of variable"
             (integrate m/cos [m/PI m/inf+]) => (throws)
             (integrate #(/ (m/sq %)) [5.0 m/inf+]) => 0.2
             (integrate #(/ (m/sq %)) [m/inf- -5.0]) => 0.2
             (integrate #(m/exp (- (m/sq %))) [m/inf- m/inf+]) 
             => (test-roughly 1.7724538509055163 1e-14))
       (fact "can integrate with a slow function that returns a vector or 
              a matrix"
             (integrate #(vector % (* % %)) [2.0 6.0]) 
             => [15.999999999999998 69.33333333333333]
             (integrate
               #(mx/compute-vector :apache-commons [% (* % %)]) [2.0 6.0])
             => (mx/apache-commons [15.999999999999998 69.33333333333333])
             (integrate 
               #(mx/matrix :apache-commons [[% (* % %)][(+ 2.0 %) 5.0]]) 
               [2.0 6.0]) 
             => (mx/apache-commons [[16.0 69.33333333333333] 
                                    [24.0 19.999999999999996]])
             (integrate 
               #(mx/compute-vector
                  :clatrix [(m/exp (- (m/sq %))) (m/exp (- (m/pow % 4)))]) 
               [m/inf- m/inf+]) 
             => (mx/clatrix [1.7724538509055159 1.8128049541109545])
             (integrate 
               #(mx/matrix 
                  :clatrix [[(m/pow % -6) (m/pow % -4)][(/ (m/sq %)) 
                                                        (/ (m/sq %))]])
               [5.0 m/inf+]) 
             => (mx/clatrix [[6.4E-5 0.0026666666666666666] [0.2 0.2]])))

(facts "multivariate integrate"
       (fact "block integrals"
             (integrate (fn [[a b]] (+ a b)) [[0.0 1.0][0.0 1.0]]) => 1.0
             (integrate (fn [[a b c]] (+ a b c)) [[0.0 1.0][0.0 1.0][0.0 1.0]]) 
             => 1.5 
             (integrate (fn [[a b c d]] (+ a b c d)) 
                        [[0.0 1.0][0.0 1.0][0.0 1.0][0.0 1.0]]) => 2.0 
             (integrate 
               (fn [[a b]] (* (m/exp (- (m/sq a))) (m/exp (- (m/sq b))))) 
               [[m/inf- m/inf+][m/inf- m/inf+]]) 
             => 3.1415926535897944 ;3.141592653589793 PI
             (integrate (fn [[a b]] (* (/ (m/sq a)) (/ (m/sq b)))) 
                        [[5.0 m/inf+][1.0 2.0]]) => 0.1
             (integrate (fn [[a b]] (* (/ (m/sq a)) (/ (m/sq b)))) 
                        [[3.0 4.0][m/inf- -5.0]]) => 0.016666666666666666
             (integrate 
               (fn [[a b]] 
                 (mx/compute-vector :apache-commons [a (* a b)]))
               [[2.0 6.0][1.0 3.0]]) => (mx/apache-commons [32 64])
             (integrate 
               (fn [[a b]] (mx/matrix :clatrix [[a (* a b)][(+ 2.0 b) 5.0]]))
               [[2.0 6.0][1.0 3.0]]) 
             => (mx/clatrix [[32.0 64.0] [32.0 40.00000000000001]])
             (integrate 
               (fn [[a b]] 
                 (mx/compute-vector
                   :clatrix 
                   [(* (m/exp (- (m/sq a))) (m/exp (- (m/sq b)))) 
                    (* (m/exp (- (m/sq a))) (m/exp (- (m/sq b))))]))
               [[m/inf- m/inf+][1.0 3.0]]) 
             => (mx/clatrix [0.24705031697079533 0.24705031697079533])
             (integrate 
               (fn [[a b]] 
                 (mx/matrix 
                   :clatrix 
                   [[(* (/ (m/sq a)) (/ (m/sq b))) 
                     (* (/ (m/sq a)) (/ (m/sq b)))]
                    [(* (/ (m/sq a)) (/ (m/sq b))) 
                     (* (/ (m/sq a)) (/ (m/sq b)))]]))
               [[2.0 6.0][m/inf- -5.0]]) 
             => (mx/clatrix [[0.06666666666666667 0.06666666666666667] 
                             [0.06666666666666667 0.06666666666666667]]))
       (fact "non-rectangular 2D" 
             (integrate-non-rectangular-2D 
               #(+ % %2) [0.0 1.0] (fn [e] [0.0 1.0])) => 1.0
             (integrate-non-rectangular-2D 
               #(+ % %2) [2.0 6.0] (fn [e] [(+ e 2.0) (+ e 6.0)])) => 192.0
             (integrate-non-rectangular-2D 
               #(* (m/exp (- (m/sq %))) (m/exp (- (m/sq %2)))) [m/inf- m/inf+] 
               (fn [e] [m/inf- m/inf+])) 
             => 3.141592653589793 ;3.141592653589793 PI
             (integrate-non-rectangular-2D
               #(mx/compute-vector :apache-commons [% (* % %2)]) [2.0 6.0]
               (fn [e] [1.0 3.0])) => (mx/apache-commons [32.0 64.0])
             (integrate-non-rectangular-2D 
               #(mx/matrix :clatrix [[% (* % %2)][(+ 2.0 %2) 5.0]]) 
               [2.0 6.0] (fn [e] [1.0 3.0])) 
             => (mx/clatrix [[32.0 64.0][32.0 40.00000000000001]])
             (integrate-non-rectangular-2D 
               (fn [a b] 
                 (mx/compute-vector
                   :clatrix 
                   [(* (m/exp (- (m/sq a))) (m/exp (- (m/sq b)))) 
                    (* (m/exp (- (m/sq a))) (m/exp (- (m/sq b))))]))
               [m/inf- m/inf+] (fn [e] [1.0 3.0])) 
             => (mx/clatrix [0.24705031697079535 0.24705031697079535])
             (integrate-non-rectangular-2D 
               (fn [a b] 
                 (mx/matrix 
                   :clatrix 
                   [[(* (/ (m/sq a)) (/ (m/sq b))) 
                     (* (/ (m/sq a)) (/ (m/sq b)))]
                    [(* (/ (m/sq a)) (/ (m/sq b))) 
                     (* (/ (m/sq a)) (/ (m/sq b)))]]))
               [2.0 6.0] (fn [e] [m/inf- -5.0])) 
             => (mx/clatrix [[0.06666666666666667 0.06666666666666667] 
                             [0.06666666666666667 0.06666666666666667]])) 
       (fact "non-rectangular 3D"
             (integrate-non-rectangular-3D 
               #(+ % %2 %3) [0.0 1.0] (fn [a] [0.0 1.0]) (fn [a b] [0.0 1.0])) 
             => 1.5)
       (fact "non-rectangular 4D"
             (integrate-non-rectangular-4D 
               #(+ % %2 %3 %4) [0.0 1.0] (fn [a] [0.0 1.0]) 
               (fn [a b] [0.0 1.0]) (fn [a b c] [0.0 1.0])) 
             => 2.0000000000000004))