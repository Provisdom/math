(ns provisdom.math.series-test
  (:require
    
    [clojure.test :refer :all]
    [provisdom.test.core :as t]
    [provisdom.math.combinatorics :as cm]
    [provisdom.math.core :as m]
    [provisdom.math.series :as series]))

;;1 SECONDS

(set! *warn-on-reflection* true)

(deftest power-series-fn-test
  (t/with-instrument `series/power-series-fn
    (t/is-spec-check series/power-series-fn))
  (t/with-instrument :all
    (t/is= [11.0 24.0 52.0] (vec ((series/power-series-fn [11 12 13]) 2.0)))))

(deftest power-series-derivative-fn-test
  (t/with-instrument `series/power-series-derivative-fn
    (t/is-spec-check series/power-series-derivative-fn))
  (t/with-instrument :all
    (t/is= [0.0 12.0 52.0] (vec ((series/power-series-derivative-fn [11 12 13]) 2.0)))))

(deftest power-series-integral-fn-test
  (t/with-instrument `series/power-series-integral-fn
    (t/is-spec-check series/power-series-integral-fn))
  (t/with-instrument :all
    (t/is= [22.0 24.0 34.666666666666664]
      (vec ((series/power-series-integral-fn [11 12 13]) 2.0)))))

(deftest continued-fraction-test
  (t/with-instrument `series/continued-fraction
    (t/is-spec-check series/continued-fraction))
  (t/with-instrument :all
    (t/is= [1.0 -0.25 0.01 -1.96078431372549E-4]
      (vec (series/continued-fraction [1.0 3.0 6.0 8.0])))))

(deftest generalized-continued-fraction-test
  (t/with-instrument `series/generalized-continued-fraction
    (t/is-spec-check series/generalized-continued-fraction))
  (t/with-instrument :all
    (t/is= [1.0 0.6666666666666667 -0.09523809523809534 0.003284072249589487]
      (vec (series/generalized-continued-fraction [1.0 3.0 6.0 8.0] [2.0 3.0 2.0 6.0])))))

(deftest multiplicative-continued-fraction-test
  (t/with-instrument `series/multiplicative-continued-fraction
    (t/is-spec-check series/multiplicative-continued-fraction))
  (t/with-instrument :all
    (t/is= [1.0 1.3333333333333333 0.986842105263158 1.0002580645161292]
      (vec (series/multiplicative-continued-fraction [1.0 3.0 6.0 8.0])))))

(deftest multiplicative-generalized-continued-fraction-test
  (t/with-instrument `series/multiplicative-generalized-continued-fraction
    (t/is-spec-check series/multiplicative-generalized-continued-fraction))
  (t/with-instrument :all
    (t/is= [1.0  1.6666666666666665 0.9428571428571427 1.00208986415882967]
      (vec (series/multiplicative-generalized-continued-fraction
             [1.0 3.0 6.0 8.0] [2.0 3.0 2.0 6.0])))))

;;;SUM CONVERGENT SERIES
(defn sin-series
  [x]
  (map #(* (/ (cm/factorial (inc (* 2 %))))
           x
           (m/pow (- (m/sq x)) %))
       (range)))

(deftest sum-convergent-series-test
  (t/with-instrument `series/sum-convergent-series
    (t/is-spec-check series/sum-convergent-series))
  (t/with-instrument :all
    (t/is= 11.12 (series/sum-convergent-series [1.02 3.05 7.05]))
    (t/is= 11.120000000000001
      (series/sum-convergent-series [1.02 3.05 7.05] {::series/kahan? false}))
    (t/is= 11.0 (series/sum-convergent-series [1 3 7]))
    (t/is= 3.5963967336368764E-16 (series/sum-convergent-series (sin-series m/PI)))
    (t/is= -1.0000000000000009 (series/sum-convergent-series (sin-series (* 1.5 m/PI))))
    (t/is= 1.529109857109388E-14 (series/sum-convergent-series (sin-series (* 2 m/PI))))
    (t/is= 0.9999999999999878 (series/sum-convergent-series (sin-series (* 2.5 m/PI))))
    (t/is= 9.918249052390073E-14 (series/sum-convergent-series (sin-series (* 3.0 m/PI))))
    (t/is= -1.0000000000005367 (series/sum-convergent-series (sin-series (* 3.5 m/PI))))
    (t/is= 0.9999999999999877
      (series/sum-convergent-series (sin-series (* 2.5 m/PI)) {::series/kahan? false}))))

(deftest multiplicative-sum-convergent-series-test
  (t/with-instrument `series/multiplicative-sum-convergent-series
    (t/is-spec-check series/multiplicative-sum-convergent-series))
  (t/with-instrument :all
    (t/is= 21.93255 (series/multiplicative-sum-convergent-series [1.02 3.05 7.05]))
    (t/is= 21.0 (series/multiplicative-sum-convergent-series [1 3 7]))))

;;;SERIES ACCELERATION
(deftest aitken-accelerate-test
  (t/with-instrument `series/aitken-accelerate
    (t/is-spec-check series/aitken-accelerate))
  (t/with-instrument :all
    ;; Test on alternating harmonic series partial sums
    (let [terms (map #(/ (m/pow -1 %) (inc %)) (range 20))
          partial-sums (reductions + terms)
          accelerated (vec (series/aitken-accelerate partial-sums))]
      ;; Accelerated should be closer to ln(2) ≈ 0.693147
      (t/is (< (m/abs (- (last accelerated) (m/log 2)))
              (m/abs (- (last (vec partial-sums)) (m/log 2))))))))

(deftest wynn-epsilon-test
  (t/with-instrument `series/wynn-epsilon
    (t/is-spec-check series/wynn-epsilon {:num-tests 20}))
  (t/with-instrument :all
    ;; Test on alternating harmonic series
    (let [terms (map #(/ (m/pow -1 %) (inc %)) (range 20))
          partial-sums (reductions + terms)
          accelerated (series/wynn-epsilon partial-sums)]
      (t/is (vector? accelerated))
      (t/is (pos? (count accelerated))))))

(deftest euler-transform-test
  (t/with-instrument `series/euler-transform
    (t/is-spec-check series/euler-transform))
  (t/with-instrument :all
    ;; Test produces non-empty output
    (let [terms (map #(/ 1.0 (inc %)) (range 10))
          transformed (vec (take 5 (series/euler-transform terms)))]
      (t/is (= 5 (count transformed))))))

(deftest richardson-extrapolate-test
  (t/with-instrument `series/richardson-extrapolate
    (t/is-spec-check series/richardson-extrapolate {:num-tests 20}))
  (t/with-instrument :all
    ;; Test with simple approximation sequence
    (let [approxs [2.0 1.5 1.25 1.125]
          refined (vec (series/richardson-extrapolate approxs))]
      (t/is (pos? (count refined))))))

;;;POWER SERIES OPERATIONS
(deftest cauchy-product-test
  (t/with-instrument `series/cauchy-product
    (t/is-spec-check series/cauchy-product {:num-tests 20}))
  (t/with-instrument :all
    ;; (1+x+x²)(1+x) = 1+2x+2x²+x³
    (t/is= [1.0 2.0 2.0 1.0] (series/cauchy-product [1 1 1] [1 1]))
    ;; (1+x)(1-x) = 1-x²
    (t/is= [1.0 0.0 -1.0] (series/cauchy-product [1 1] [1 -1]))
    ;; (1)(any) = any
    (t/is= [2.0 3.0 4.0] (series/cauchy-product [1] [2 3 4]))))

(deftest power-series-compose-test
  (t/with-instrument `series/power-series-compose
    (t/is-spec-check series/power-series-compose {:num-tests 10}))
  (t/with-instrument :all
    ;; f(x)=1+x+x², g(x)=2x → f(2x)=1+2x+4x²
    (t/is= [1.0 2.0 4.0] (series/power-series-compose [1 1 1] [0 2]))
    ;; f(x)=1+x, g(x)=x → f(x)=1+x
    (t/is= [1.0 1.0] (series/power-series-compose [1 1] [0 1]))))

(deftest power-series-inverse-test
  (t/with-instrument `series/power-series-inverse
    (t/is-spec-check series/power-series-inverse {:num-tests 10}))
  (t/with-instrument :all
    ;; Inverse of f(x)=x should be g(x)=x
    (let [result (series/power-series-inverse [0 1])]
      (t/is (vector? result))
      (t/is (m/roughly? 0.0 (first result) 1e-10))
      (t/is (m/roughly? 1.0 (second result) 1e-10)))
    ;; Invalid input: first coeff not 0
    (t/is (map? (series/power-series-inverse [1 1])))
    ;; Invalid input: second coeff is 0
    (t/is (map? (series/power-series-inverse [0 0 1])))))

(deftest radius-of-convergence-test
  (t/with-instrument `series/radius-of-convergence
    (t/is-spec-check series/radius-of-convergence {:num-tests 20}))
  (t/with-instrument :all
    ;; Geometric series [1,1,1,...] has R=1
    (let [result (series/radius-of-convergence [1 1 1 1 1 1 1 1 1 1])]
      (t/is= 1.0 (::series/ratio-estimate result))
      (t/is= 1.0 (::series/root-estimate result))
      (t/is= 1.0 (::series/combined-estimate result)))))

;;;PADÉ APPROXIMANTS
(deftest pade-approximant-test
  (t/with-instrument `series/pade-approximant
    (t/is-spec-check series/pade-approximant {:num-tests 20}))
  (t/with-instrument :all
    ;; [1/1] Padé for e^x ≈ (1+0.5x)/(1-0.5x)
    (let [e-coeffs [1 1 0.5 (/ 6) (/ 24)]
          pade (series/pade-approximant e-coeffs 1 1)]
      (t/is= [1.0 0.5] (::series/numerator pade))
      (t/is= [1.0 -0.5] (::series/denominator pade)))
    ;; [0/0] is just constant
    (let [pade (series/pade-approximant [5 1 2] 0 0)]
      (t/is= [5.0] (::series/numerator pade))
      (t/is= [1.0] (::series/denominator pade)))))

(deftest evaluate-pade-test
  (t/with-instrument `series/evaluate-pade
    (t/is-spec-check series/evaluate-pade {:num-tests 20}))
  (t/with-instrument :all
    (let [e-coeffs [1 1 0.5 (/ 6) (/ 24) (/ 120)]
          pade22 (series/pade-approximant e-coeffs 2 2)]
      ;; [2/2] Padé at x=1 should approximate e
      (t/is (m/roughly? m/E (series/evaluate-pade pade22 1.0) 0.01))
      ;; At x=0, should equal 1
      (t/is= 1.0 (series/evaluate-pade pade22 0.0)))))

;;;IMPROVED SUMMATION
(deftest neumaier-sum-test
  (t/with-instrument `series/neumaier-sum
    (t/is-spec-check series/neumaier-sum {:num-tests 20}))
  (t/with-instrument :all
    ;; Standard case
    (t/is= 6.0 (series/neumaier-sum [1 2 3]))
    ;; Case with wildly varying magnitudes (where Kahan fails)
    (t/is= 2.0 (series/neumaier-sum [1.0 1e100 1.0 -1e100]))
    ;; Empty sequence
    (t/is= 0.0 (series/neumaier-sum []))))

(deftest pairwise-sum-test
  (t/with-instrument `series/pairwise-sum
    (t/is-spec-check series/pairwise-sum {:num-tests 20}))
  (t/with-instrument :all
    ;; Sum of 1 to 1000 = 500500
    (t/is= 500500.0 (series/pairwise-sum (range 1 1001)))
    ;; Small sequence
    (t/is= 10.0 (series/pairwise-sum [1 2 3 4]))
    ;; Empty sequence
    (t/is= 0.0 (series/pairwise-sum []))))

(deftest sum-with-diagnostics-test
  (t/with-instrument `series/sum-with-diagnostics
    (t/is-spec-check series/sum-with-diagnostics {:num-tests 10}))
  (t/with-instrument :all
    ;; Geometric series converges to 2
    (let [terms (map #(m/pow 0.5 %) (range))
          result (series/sum-with-diagnostics terms)]
      (t/is (::series/converged? result))
      (t/is (m/roughly? 2.0 (::series/sum result) 1e-10))
      (t/is (pos? (::series/iterations result))))
    ;; Finite sequence
    (let [result (series/sum-with-diagnostics [1 2 3])]
      (t/is (::series/converged? result))
      (t/is= 6.0 (::series/sum result)))))
