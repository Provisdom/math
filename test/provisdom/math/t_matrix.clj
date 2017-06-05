(ns provisdom.math.t-matrix
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.matrix :as mx]
            [provisdom.math.core :as m]
            [clojure.spec.test.alpha :as sta]
            [orchestra.spec.test :as st]))

(st/instrument)

(deftest kahan-sum-test
  (is= m/inf+ (mx/kahan-sum [m/inf+ m/inf+]))
  (is (m/nan? (mx/kahan-sum [m/inf+ m/inf-])))
  (is= 17.340604306430002 (mx/kahan-sum '(-3.0 6.34060430643 14.0)))
  (is= 4950.0 (mx/kahan-sum (map double (range 1 100))))
  (is= 15550.883635269476 (mx/kahan-sum (map (partial * m/PI) (range 1 100))))
  (is= 15550.883635269474 (mx/esum (map (partial * m/PI) (range 1 100)))))

(deftest matrix-test
  (kahan-sum-test))

(defspec-test test-kahan-sum `mx/kahan-sum)

#_(st/unstrument)