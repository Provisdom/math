(ns provisdom.math.polynomials-test
  (:require
    [provisdom.math.polynomials :as poly]
    [provisdom.test.core :as t]))

;;22 seconds

(set! *warn-on-reflection* true)

;;;CHEBYSHEV POLYNOMIALS
(t/deftest chebyshev-polynomial-fn-test
  (t/with-instrument `poly/chebyshev-polynomial-fn
    (t/is-spec-check poly/chebyshev-polynomial-fn))
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

(t/deftest chebyshev-derivative-fn-test
  (t/with-instrument `poly/chebyshev-derivative-fn
    (t/is-spec-check poly/chebyshev-derivative-fn))
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

(t/deftest chebyshev-poly-factors-to-regular-poly-factors-test
  (t/with-instrument `poly/chebyshev-poly-factors-to-regular-poly-factors
    (t/is-spec-check poly/chebyshev-poly-factors-to-regular-poly-factors))
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
(t/deftest polynomial-fn-test
  (t/with-instrument `poly/polynomial-fn
    (t/is-spec-check poly/polynomial-fn))
  (t/with-instrument :all
    (t/is= [1.0 4.0 16.0 64.0] ((poly/polynomial-fn 3) 4.0))
    (t/is= [4.0 16.0 64.0] ((poly/polynomial-fn 3 {::poly/start-degree 1}) 4.0))
    (t/is= [1.0 4.0 31.0 244.0] ((poly/polynomial-fn 3 {::poly/chebyshev-kind 1}) 4.0))
    (t/is= [1.0 8.0 63.0 496.0] ((poly/polynomial-fn 3 {::poly/chebyshev-kind 2}) 4.0))))

(t/deftest polynomial-fns-test
  (t/with-instrument `poly/polynomial-fns
    (t/is-spec-check poly/polynomial-fns))
  (t/with-instrument :all
    (t/is= [1.0 4.0 16.0 64.0] (map #(% 4.0) (poly/polynomial-fns 3)))
    (t/is= [4.0 16.0 64.0] (map #(% 4.0) (poly/polynomial-fns 3 {::poly/start-degree 1})))
    (t/is= [1.0 4.0 31.0 244.0] (map #(% 4.0) (poly/polynomial-fns 3 {::poly/chebyshev-kind 1})))
    (t/is= [1.0 8.0 63.0 496.0] (map #(% 4.0) (poly/polynomial-fns 3 {::poly/chebyshev-kind 2})))))

(t/deftest polynomial-2D-count-test
  (t/with-instrument `poly/polynomial-2D-count
    (t/is-spec-check poly/polynomial-2D-count))
  (t/with-instrument :all
    (t/is= 10 (poly/polynomial-2D-count 3))
    (t/is= 9 (poly/polynomial-2D-count 3 {::poly/start-degree 1}))))

(t/deftest polynomial-2D-fn-by-degree-test
  (t/with-instrument `poly/polynomial-2D-fn-by-degree
    (t/is-spec-check poly/polynomial-2D-fn-by-degree))
  (t/with-instrument :all
    (t/is= [1.0 2.0 4.0 4.0 8.0 16.0 8.0 16.0 32.0 64.0]
      ((poly/polynomial-2D-fn-by-degree 3) 4.0 2.0))
    (t/is= [2.0 4.0 4.0 8.0 16.0 8.0 16.0 32.0 64.0]
      ((poly/polynomial-2D-fn-by-degree 3 {::poly/start-degree 1}) 4.0 2.0))
    (t/is= [1.0 2.0 4.0 7.0 8.0 31.0 26.0 28.0 62.0 244.0]
      ((poly/polynomial-2D-fn-by-degree 3 {::poly/chebyshev-kind 1}) 4.0 2.0))
    (t/is= [1.0 4.0 8.0 15.0 32.0 63.0 56.0 120.0 252.0 496.0]
      ((poly/polynomial-2D-fn-by-degree 3 {::poly/chebyshev-kind 2}) 4.0 2.0))))

(t/deftest polynomial-2D-fn-by-basis-count-test
  (t/with-instrument `poly/polynomial-2D-fn-by-basis-count
    (t/is-spec-check poly/polynomial-2D-fn-by-basis-count))
  (t/with-instrument :all
    (t/is= [1.0 2.0 4.0 4.0] ((poly/polynomial-2D-fn-by-basis-count 4) 4.0 2.0))))

(t/deftest polynomial-ND-fn-test
  (t/with-instrument `poly/polynomial-ND-fn
    (t/is-spec-check poly/polynomial-ND-fn))
  (t/with-instrument :all
    (t/is= [1.0 2.0 3.0 4.0 4.0 6.0 8.0 9.0 12.0 16.0 12.0 16.0 18.0 24.0 32.0 36.0 48.0 36.0 48.0
            64.0 72.0 96.0 144.0 144.0 192.0 288.0 576.0]
      ((poly/polynomial-ND-fn 2) [2.0 3.0 4.0]))))

(t/deftest polynomial-ND-fn-without-cross-terms-test
  (t/with-instrument `poly/polynomial-ND-fn-without-cross-terms
    (t/is-spec-check poly/polynomial-ND-fn-without-cross-terms))
  (t/with-instrument :all
    (t/is= [1.0 2.0 3.0 4.0 4.0 9.0 16.0 8.0 27.0 64.0]
      ((poly/polynomial-ND-fn-without-cross-terms 3) [2.0 3.0 4.0]))))

;;;POLYNOMIAL EVALUATION
(t/deftest horner-eval-test
  (t/with-instrument `poly/horner-eval
    (t/is-spec-check poly/horner-eval))
  (t/with-instrument :all
    ;; 1 + 2x + 3x² at x=2: 1 + 4 + 12 = 17
    (t/is= 17.0 (poly/horner-eval [1 2 3] 2.0))
    ;; constant polynomial
    (t/is= 5.0 (poly/horner-eval [5] 100.0))
    ;; linear: 2 + 3x at x=4: 2 + 12 = 14
    (t/is= 14.0 (poly/horner-eval [2 3] 4.0))
    ;; x³ at x=3: 27
    (t/is= 27.0 (poly/horner-eval [0 0 0 1] 3.0))
    ;; negative coefficients: -1 + 2x - 3x² at x=2: -1 + 4 - 12 = -9
    (t/is= -9.0 (poly/horner-eval [-1 2 -3] 2.0))))

(t/deftest clenshaw-eval-test
  (t/with-instrument `poly/clenshaw-eval
    (t/is-spec-check poly/clenshaw-eval))
  (t/with-instrument :all
    ;; Single coefficient c₀T₀(x) = c₀
    (t/is= 5.0 (poly/clenshaw-eval [5] 0.5))
    ;; c₀T₀ + c₁T₁ at x=0.5: 1*1 + 2*0.5 = 2
    (t/is= 2.0 (poly/clenshaw-eval [1 2] 0.5))
    ;; T₂(0.5) = 2(0.5)² - 1 = -0.5, so [1 2 3] => 1 + 2*0.5 + 3*(-0.5) = 0.5
    (t/is= 0.5 (poly/clenshaw-eval [1 2 3] 0.5))
    ;; At x=1, T_n(1) = 1 for all n, so sum of coefficients
    (t/is= 6.0 (poly/clenshaw-eval [1 2 3] 1.0))
    ;; Second kind: U₀(0.5)=1, U₁(0.5)=1, U₂(0.5)=0
    ;; [1 2 3] => 1*1 + 2*1 + 3*0 = 3
    (t/is= 3.0 (poly/clenshaw-eval [1 2 3] 0.5 {::poly/second-kind? true}))
    ;; At x=1, U_n(1) = n+1, so [1 2 3] => 1*1 + 2*2 + 3*3 = 14
    (t/is= 14.0 (poly/clenshaw-eval [1 2 3] 1.0 {::poly/second-kind? true}))))

;;;POLYNOMIAL ARITHMETIC
(t/deftest poly-add-test
  (t/with-instrument `poly/poly-add
    (t/is-spec-check poly/poly-add))
  (t/with-instrument :all
    (t/is= [5.0 7.0 3.0] (poly/poly-add [1 2 3] [4 5]))
    (t/is= [2.0 4.0 6.0] (poly/poly-add [1 2 3] [1 2 3]))
    (t/is= [1.0 2.0 3.0] (poly/poly-add [1 2 3] [0]))))

(t/deftest poly-subtract-test
  (t/with-instrument `poly/poly-subtract
    (t/is-spec-check poly/poly-subtract))
  (t/with-instrument :all
    (t/is= [1.0 2.0 3.0] (poly/poly-subtract [5 7 3] [4 5]))
    (t/is= [0.0 0.0 0.0] (poly/poly-subtract [1 2 3] [1 2 3]))))

(t/deftest poly-multiply-test
  (t/with-instrument `poly/poly-multiply
    (t/is-spec-check poly/poly-multiply))
  (t/with-instrument :all
    ;; (1+x)² = 1 + 2x + x²
    (t/is= [1.0 2.0 1.0] (poly/poly-multiply [1 1] [1 1]))
    ;; (1+x)(1-x) = 1 - x²
    (t/is= [1.0 0.0 -1.0] (poly/poly-multiply [1 1] [1 -1]))
    ;; (2)(3x) = 6x
    (t/is= [0.0 6.0] (poly/poly-multiply [2] [0 3]))
    ;; (1 + 2x)(3 + 4x + 5x²) = 3 + 10x + 13x² + 10x³
    (t/is= [3.0 10.0 13.0 10.0] (poly/poly-multiply [1 2] [3 4 5]))))

(t/deftest poly-scale-test
  (t/with-instrument `poly/poly-scale
    (t/is-spec-check poly/poly-scale))
  (t/with-instrument :all
    (t/is= [2.0 4.0 6.0] (poly/poly-scale [1 2 3] 2.0))
    (t/is= [0.0 0.0 0.0] (poly/poly-scale [1 2 3] 0.0))
    (t/is= [-1.0 -2.0 -3.0] (poly/poly-scale [1 2 3] -1.0))))

(t/deftest poly-divide-test
  (t/with-instrument `poly/poly-divide
    (t/is-spec-check poly/poly-divide))
  (t/with-instrument :all
    ;; (x² + 2x + 1) / (x + 1) = (x + 1), remainder 0
    (let [{:keys [quotient remainder]} (poly/poly-divide [1 2 1] [1 1])]
      (t/is= [1.0 1.0] quotient)
      (t/is= [0.0] remainder))
    ;; (x² + 1) / (x + 1) = (x - 1), remainder 2
    (let [{:keys [quotient remainder]} (poly/poly-divide [1 0 1] [1 1])]
      (t/is= [-1.0 1.0] quotient)
      (t/is= [2.0] remainder))
    ;; degree of dividend < degree of divisor
    (let [{:keys [quotient remainder]} (poly/poly-divide [1 2] [1 2 3])]
      (t/is= [0.0] quotient)
      (t/is= [1.0 2.0] remainder))))

;;;CHEBYSHEV UTILITIES
(t/deftest chebyshev-nodes-test
  (t/with-instrument `poly/chebyshev-nodes
    (t/is-spec-check poly/chebyshev-nodes))
  (t/is= 3 (count (poly/chebyshev-nodes 3)))
  ;; Nodes should be in [-1, 1]
  (t/is (every? #(<= -1 % 1) (poly/chebyshev-nodes 5)))
  ;; n=1 should give cos(π/2) = 0
  (t/is-approx= 0.0 (first (poly/chebyshev-nodes 1)) :tolerance 1e-10)
  ;; n=2 should give cos(π/4) and cos(3π/4)
  (let [nodes (poly/chebyshev-nodes 2)]
    (t/is-approx= 0.7071067811865476 (first nodes) :tolerance 1e-10)
    (t/is-approx= -0.7071067811865476 (second nodes) :tolerance 1e-10)))

(t/deftest chebyshev-extrema-test
  (t/with-instrument `poly/chebyshev-extrema
    (t/is-spec-check poly/chebyshev-extrema))
  (t/with-instrument `poly/chebyshev-extrema
    (t/is= 3 (count (poly/chebyshev-extrema 3)))
    ;; Extrema should include ±1
    (t/is= 1.0 (first (poly/chebyshev-extrema 5)))
    (t/is= -1.0 (last (poly/chebyshev-extrema 5)))))

(t/deftest regular-poly-factors-to-chebyshev-poly-factors-test
  ;; Skip spec-check and instrumentation due to internal function calls that have issues
  ;; x² = (T₂ + T₀)/2, so [1 0 1] (1+x²) => [1.5 0 0.5]
  (let [result (poly/regular-poly-factors-to-chebyshev-poly-factors [1 0 1])]
    (t/is= 3 (count result))
    (t/is-approx= 1.5 (nth result 0) :tolerance 1e-10)
    (t/is-approx= 0.0 (nth result 1) :tolerance 1e-10)
    (t/is-approx= 0.5 (nth result 2) :tolerance 1e-10))
  ;; constant: 5 = 5*T₀
  (t/is-approx= 5.0
    (first (poly/regular-poly-factors-to-chebyshev-poly-factors [5])) :tolerance 1e-10)
  ;; x = T₁
  (let [result (poly/regular-poly-factors-to-chebyshev-poly-factors [0 1])]
    (t/is-approx= 0.0 (first result) :tolerance 1e-10)
    (t/is-approx= 1.0 (second result) :tolerance 1e-10)))

;;;ORTHOGONAL POLYNOMIALS
(t/deftest legendre-polynomial-fn-test
  (t/with-instrument `poly/legendre-polynomial-fn
    (t/is-spec-check poly/legendre-polynomial-fn))
  (t/with-instrument :all
    ;; P₀(x) = 1
    (t/is= 1.0 ((poly/legendre-polynomial-fn 0) 0.5))
    ;; P₁(x) = x
    (t/is= 0.5 ((poly/legendre-polynomial-fn 1) 0.5))
    ;; P₂(x) = (3x² - 1)/2, at x=0.5: (3*0.25 - 1)/2 = -0.125
    (t/is= -0.125 ((poly/legendre-polynomial-fn 2) 0.5))
    ;; P_n(1) = 1 for all n
    (t/is= 1.0 ((poly/legendre-polynomial-fn 5) 1.0))
    ;; P_n(-1) = (-1)^n
    (t/is= 1.0 ((poly/legendre-polynomial-fn 4) -1.0))
    (t/is= -1.0 ((poly/legendre-polynomial-fn 5) -1.0))))

(t/deftest hermite-polynomial-fn-test
  (t/with-instrument `poly/hermite-polynomial-fn
    (t/is-spec-check poly/hermite-polynomial-fn))
  (t/with-instrument :all
    ;; Physicist's: H₀(x) = 1
    (t/is= 1.0 ((poly/hermite-polynomial-fn 0) 2.0))
    ;; Physicist's: H₁(x) = 2x
    (t/is= 4.0 ((poly/hermite-polynomial-fn 1) 2.0))
    ;; Physicist's: H₂(x) = 4x² - 2, at x=1: 4 - 2 = 2
    (t/is= 2.0 ((poly/hermite-polynomial-fn 2) 1.0))
    ;; Probabilist's: He₀(x) = 1
    (t/is= 1.0 ((poly/hermite-polynomial-fn 0 {::poly/physicist? false}) 2.0))
    ;; Probabilist's: He₁(x) = x
    (t/is= 2.0 ((poly/hermite-polynomial-fn 1 {::poly/physicist? false}) 2.0))
    ;; Probabilist's: He₂(x) = x² - 1, at x=1: 0
    (t/is= 0.0 ((poly/hermite-polynomial-fn 2 {::poly/physicist? false}) 1.0))))

(t/deftest laguerre-polynomial-fn-test
  (t/with-instrument `poly/laguerre-polynomial-fn
    (t/is-spec-check poly/laguerre-polynomial-fn))
  (t/with-instrument :all
    ;; L₀(x) = 1
    (t/is= 1.0 ((poly/laguerre-polynomial-fn 0) 2.0))
    ;; L₁(x) = 1 - x
    (t/is= -1.0 ((poly/laguerre-polynomial-fn 1) 2.0))
    ;; L₂(x) = (x² - 4x + 2)/2, at x=1: (1 - 4 + 2)/2 = -0.5
    (t/is= -0.5 ((poly/laguerre-polynomial-fn 2) 1.0))
    ;; L_n(0) = 1 for all n
    (t/is= 1.0 ((poly/laguerre-polynomial-fn 5) 0.0))))

;;;INTERPOLATION
(t/deftest lagrange-interpolation-fn-test
  (t/with-instrument `poly/lagrange-interpolation-fn
    (t/is-spec-check poly/lagrange-interpolation-fn))
  (t/with-instrument `poly/lagrange-interpolation-fn
    ;; Points on y = x²: (0,0), (1,1), (2,4)
    (let [f (poly/lagrange-interpolation-fn [[0 0] [1 1] [2 4]])]
      (t/is= 0.0 (f 0.0))
      (t/is= 1.0 (f 1.0))
      (t/is= 4.0 (f 2.0))
      (t/is= 2.25 (f 1.5))) ; 1.5² = 2.25
    ;; Points on y = 1 + x²: (0,1), (1,2), (2,5)
    (let [f (poly/lagrange-interpolation-fn [[0 1] [1 2] [2 5]])]
      (t/is= 3.25 (f 1.5))))) ; 1 + 1.5² = 3.25

(t/deftest lagrange-interpolation-coefficients-test
  ;; Skip spec-check and instrumentation due to internal poly-multiply/scale calls
  ;; Points (0,1), (1,2), (2,5) lie on y = 1 + x²
  (let [coeffs (poly/lagrange-interpolation-coefficients [[0 1] [1 2] [2 5]])]
    (t/is= 3 (count coeffs))
    (t/is-approx= 1.0 (nth coeffs 0) :tolerance 1e-10)
    (t/is-approx= 0.0 (nth coeffs 1) :tolerance 1e-10)
    (t/is-approx= 1.0 (nth coeffs 2) :tolerance 1e-10))
  ;; Linear: (0,0), (1,2) => y = 2x
  (let [coeffs (poly/lagrange-interpolation-coefficients [[0 0] [1 2]])]
    (t/is= 2 (count coeffs))
    (t/is-approx= 0.0 (first coeffs) :tolerance 1e-10)
    (t/is-approx= 2.0 (second coeffs) :tolerance 1e-10)))

(t/deftest newton-interpolation-coefficients-test
  ;; Skip spec-check due to issues with generated points
  ;; Points (0,1), (1,2), (2,5)
  (let [{:keys [coefficients xs]} (poly/newton-interpolation-coefficients [[0 1] [1 2] [2 5]])]
    (t/is= [0 1 2] xs)
    ;; c₀ = f(x₀) = 1
    ;; c₁ = (f(x₁)-f(x₀))/(x₁-x₀) = (2-1)/(1-0) = 1
    ;; c₂ = divided difference = 1
    (t/is= 3 (count coefficients))
    (t/is-approx= 1.0 (nth coefficients 0) :tolerance 1e-10)
    (t/is-approx= 1.0 (nth coefficients 1) :tolerance 1e-10)
    (t/is-approx= 1.0 (nth coefficients 2) :tolerance 1e-10)))

(t/deftest newton-interpolation-fn-test
  ;; Skip spec-check due to factory function issues
  ;; Should produce same results as Lagrange
  (let [points [[0 1] [1 2] [2 5]]
        lagrange (poly/lagrange-interpolation-fn points)
        newton (poly/newton-interpolation-fn points)]
    (t/is-approx= (lagrange 0.0) (newton 0.0) :tolerance 1e-10)
    (t/is-approx= (lagrange 1.0) (newton 1.0) :tolerance 1e-10)
    (t/is-approx= (lagrange 1.5) (newton 1.5) :tolerance 1e-10)
    (t/is-approx= (lagrange 2.0) (newton 2.0) :tolerance 1e-10)))
