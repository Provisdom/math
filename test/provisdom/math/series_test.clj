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
    (t/is (t/spec-check series/power-series-fn)))
  (t/with-instrument :all)
  (t/is= [11.0 24.0 52.0] (vec ((series/power-series-fn [11 12 13]) 2.0))))

(deftest power-series-derivative-fn-test
  (t/with-instrument `series/power-series-derivative-fn
    (t/is (t/spec-check series/power-series-derivative-fn)))
  (t/with-instrument :all)
  (t/is= [0.0 12.0 52.0] (vec ((series/power-series-derivative-fn [11 12 13]) 2.0))))

(deftest power-series-integral-fn-test
  (t/with-instrument `series/power-series-integral-fn
    (t/is (t/spec-check series/power-series-integral-fn)))
  (t/with-instrument :all)
  (t/is= [22.0 24.0 34.666666666666664] (vec ((series/power-series-integral-fn [11 12 13]) 2.0))))

(deftest continued-fraction-test
  (t/with-instrument `series/continued-fraction
    (t/is (t/spec-check series/continued-fraction)))
  (t/with-instrument :all)
  (t/is= [1.0 -0.25 0.01 -1.96078431372549E-4] (vec (series/continued-fraction [1.0 3.0 6.0 8.0]))))

(deftest generalized-continued-fraction-test
  (t/with-instrument `series/generalized-continued-fraction
    (t/is (t/spec-check series/generalized-continued-fraction)))
  (t/with-instrument :all)
  (t/is= [1.0 0.6666666666666667 -0.09523809523809534 0.003284072249589487]
    (vec (series/generalized-continued-fraction [1.0 3.0 6.0 8.0] [2.0 3.0 2.0 6.0]))))

(deftest multiplicative-continued-fraction-test
  (t/with-instrument `series/multiplicative-continued-fraction
    (t/is (t/spec-check series/multiplicative-continued-fraction)))
  (t/with-instrument :all
    (t/is= [1.0 1.3333333333333333 0.986842105263158 1.0002580645161292]
      (vec (series/multiplicative-continued-fraction [1.0 3.0 6.0 8.0])))))

(deftest multiplicative-generalized-continued-fraction-test
  (t/with-instrument `series/multiplicative-generalized-continued-fraction
    (t/is (t/spec-check series/multiplicative-generalized-continued-fraction)))
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
    (t/is (t/spec-check series/sum-convergent-series)))
  (t/with-instrument :all)
  (t/is= 11.12 (series/sum-convergent-series [1.02 3.05 7.05]))
  (t/is= 11.120000000000001 (series/sum-convergent-series [1.02 3.05 7.05] {::series/kahan? false}))
  (t/is= 11.0 (series/sum-convergent-series [1 3 7]))
  (t/is= 3.5963967336368764E-16 (series/sum-convergent-series (sin-series m/PI)))
  (t/is= -1.0000000000000009 (series/sum-convergent-series (sin-series (* 1.5 m/PI))))
  (t/is= 1.529109857109388E-14 (series/sum-convergent-series (sin-series (* 2 m/PI))))
  (t/is= 0.9999999999999878 (series/sum-convergent-series (sin-series (* 2.5 m/PI))))
  (t/is= 9.918249052390073E-14 (series/sum-convergent-series (sin-series (* 3.0 m/PI))))
  (t/is= -1.0000000000005367 (series/sum-convergent-series (sin-series (* 3.5 m/PI))))
  (t/is= 0.9999999999999877
    (series/sum-convergent-series (sin-series (* 2.5 m/PI)) {::series/kahan? false})))

(deftest multiplicative-sum-convergent-series-test
  (t/with-instrument `series/multiplicative-sum-convergent-series
    (t/is (t/spec-check series/multiplicative-sum-convergent-series)))
  (t/with-instrument :all
    (t/is= 21.93255 (series/multiplicative-sum-convergent-series [1.02 3.05 7.05]))
    (t/is= 21.0 (series/multiplicative-sum-convergent-series [1 3 7]))))
