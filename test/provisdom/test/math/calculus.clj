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
               #(mx/create-vector :apache-commons [% (* % %)]) [2.0 6.0]) 
             => (mx/apache-commons [15.999999999999998 69.33333333333333])
             (integrate 
               #(mx/matrix :apache-commons [[% (* % %)][(+ 2.0 %) 5.0]]) 
               [2.0 6.0]) 
             => (mx/apache-commons [[16.0 69.33333333333333] 
                                    [24.0 19.999999999999996]])
             (integrate 
               #(mx/create-vector 
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
                 (mx/create-vector :apache-commons [a (* a b)])) 
               [[2.0 6.0][1.0 3.0]]) => (mx/apache-commons [32 64])
             (integrate 
               (fn [[a b]] (mx/matrix :clatrix [[a (* a b)][(+ 2.0 b) 5.0]]))
               [[2.0 6.0][1.0 3.0]]) 
             => (mx/clatrix [[32.0 64.0] [32.0 40.00000000000001]])
             (integrate 
               (fn [[a b]] 
                 (mx/create-vector 
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
               #(mx/create-vector :apache-commons [% (* % %2)]) [2.0 6.0] 
               (fn [e] [1.0 3.0])) => (mx/apache-commons [32.0 64.0])
             (integrate-non-rectangular-2D 
               #(mx/matrix :clatrix [[% (* % %2)][(+ 2.0 %2) 5.0]]) 
               [2.0 6.0] (fn [e] [1.0 3.0])) 
             => (mx/clatrix [[32.0 64.0][32.0 40.00000000000001]])
             (integrate-non-rectangular-2D 
               (fn [a b] 
                 (mx/create-vector 
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

(def gf 
  #(let [a (first %), b (second %)] (+ (* a b) (* 2 (m/sq a)) (m/pow b 3))))

(facts "numerical gradient function"
       (fact "uses the central difference."
             ((gradient-fn gf) [3.0 4.0]) 
             => [16.000000002236447 51.0000000062405])) ;[16,51]

(facts "numerical jacobian function"
       (fact "uses the central difference."
             ((jacobian-fn 
                #(let [a (first %), b (second %)] 
                   [(+ (* a b) (* 2 (m/sq a)) (m/pow b 3)) 
                    (+ (* (m/sq a) (m/sq b)))])) 
               [3.0 4.0]) 
             => [[16.00000000745058 51.00000000745058] 
                 [96.00000001490116 72.0]])) ;[[16,51][96,72]]

(facts "numerical hessian function"
       (fact "uses the default joint central difference."
             ((hessian-fn gf) [3.0 4.0]) 
             => [[4.00000004674439 1.0000000472132342] 
                 [1.0000000472132342 23.999999854140697]] ;[[4,1][1,24]]
             ((hessian-fn gf :implementation :clatrix) [3.0 4.0]) 
             => (mx/clatrix 
                  [[4.00000004674439 1.0000000472132342] 
                   [1.0000000472132342 23.999999854140697]])) ;[[4,1][1,24]]
       (fact "uses separate central differences"
             ((hessian-fn gf :type :central :implementation :apache-commons) 
               [3.0 4.0]) 
             => (mx/apache-commons [[3.9999999756910256 1.000000011688826] 
                                    [1.000000011685188 23.99999996070983]]))
       (fact "uses forward/backward differences"
             ((hessian-fn gf :type :forward) 
               [3.0 4.0]) 
             => [[3.999999620427843 0.9999999406427378] 
                 [1.0000005090842023 23.99999985418981]]
             ((hessian-fn gf :type :backward) 
               [3.0 4.0]) 
             => [[3.9999990519863786 0.9999993721867213] 
                 [0.9999989458592609 23.999999569903594]])) ;[[4,1][1,24]]

(facts "partial derivatives of fxy"
       (def fxy (fn [x y] (+ (* x x) (* 2 y y) (* x y))))
       (fact "dx"
             ((partial-derivative-x-of-fxy fxy) 3.0 3.0) => 8.999999998593466)
       (fact "dy"
             ((partial-derivative-y-of-fxy fxy) 3.0 3.0) => 15.000000001208491)
       (fact "dxx"
             ((second-partial-derivative-xx-of-fxy fxy) 3.0 3.0) 
             => 2.000000023372195)
       (fact "dyy"
             ((second-partial-derivative-yy-of-fxy fxy) 3.0 3.0) 
             => 3.999999975690116)
       (fact "dxy"
             ((second-partial-derivative-xy-of-fxy fxy) 3.0 3.0) 
             => 1.0000000294496658))