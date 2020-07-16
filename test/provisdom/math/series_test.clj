(ns provisdom.math.series-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.core :as m]
    [provisdom.math.combinatorics :as cm]
    [provisdom.math.series :as series]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;;9 SECONDS

(set! *warn-on-reflection* true)

(ost/instrument)

(deftest power-series-fn-test
  (is (spec-check series/power-series-fn))
  (is= [11.0 24.0 52.0] ((series/power-series-fn [11 12 13]) 2.0)))

(deftest power-series-derivative-fn-test
  (is (spec-check series/power-series-derivative-fn))
  (is= [0.0 12.0 52.0] ((series/power-series-derivative-fn [11 12 13]) 2.0)))

(deftest power-series-integral-fn-test
  (is (spec-check series/power-series-integral-fn))
  (is= [22.0 24.0 34.666666666666664]
       ((series/power-series-integral-fn [11 12 13]) 2.0)))

(deftest continued-fraction-test
  (is (spec-check series/continued-fraction))
  (is= [1.0 -0.25 0.01 -1.96078431372549E-4]
       (series/continued-fraction [1.0 3.0 6.0 8.0])))

(deftest generalized-continued-fraction-test
  (is (spec-check series/generalized-continued-fraction))
  (is= [1.0 0.6666666666666667 -0.09523809523809534 0.003284072249589487]
       (series/generalized-continued-fraction [1.0 3.0 6.0 8.0] [2.0 3.0 2.0 6.0])))

;;;SUM CONVERGENT SERIES
(defn sin-series
  [x]
  (map #(* (/ (cm/factorial (inc (* 2 %))))
           x
           (m/pow (- (m/sq x)) %))
       (range)))

(deftest sum-convergent-series-test
  (is (spec-check series/sum-convergent-series))
  (is= 11.12 (series/sum-convergent-series [1.02 3.05 7.05]))
  (is= 11.120000000000001
       (series/sum-convergent-series [1.02 3.05 7.05] {::series/kahan? false}))
  (is= 11.0 (series/sum-convergent-series [1 3 7]))
  (is= 3.5963967336368764E-16 (series/sum-convergent-series (sin-series m/PI)))
  (is= -1.0000000000000009
       (series/sum-convergent-series (sin-series (* 1.5 m/PI))))
  (is= 1.529109857109388E-14
       (series/sum-convergent-series (sin-series (* 2 m/PI))))
  (is= 0.9999999999999878
       (series/sum-convergent-series (sin-series (* 2.5 m/PI))))
  (is= 9.918249052390073E-14
       (series/sum-convergent-series (sin-series (* 3.0 m/PI))))
  (is= -1.0000000000005367
       (series/sum-convergent-series (sin-series (* 3.5 m/PI))))
  (is= 0.9999999999999877
       (series/sum-convergent-series (sin-series (* 2.5 m/PI))
                                     {::series/kahan? false})))

#_(ost/unstrument)