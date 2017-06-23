(ns provisdom.math.t-matrix
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.matrix :as mx]
            [provisdom.math.core :as m]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]))

(ost/instrument)

(def se '((1.0 0.5) (2.0 4.0)))
(def ve (mx/matrix se))
(def se1D '(1.0 0.5))
(def ve1D (mx/matrix se1D))
(def se-row '((1.0 0.5)))
(def ve-row (mx/matrix se-row))
(def se-col '((1.0) (0.5)))
(def ve-col (mx/matrix se-col))
(def se-sym '((1.0 0.5) (0.5 2.0)))
(def ve-sym (mx/matrix se-sym))

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

(deftest mempty?-test
  (is (mx/empty-matrix? [[]]))
  (is-not (mx/empty-matrix? [[1]]))
  (is-not (mx/empty-matrix? [[] [2]]))
  (is-not (mx/empty-matrix? [[m/nan]]))
  (is-not (mx/empty-matrix? [[false]])))

(deftest diagonal?-test
  (is-not (mx/diagonal-matrix? se))
  (is (mx/diagonal-matrix? [[1.0 0.0] [0.0 2.0]])))

(deftest symmetric?-test
  (is-not (mx/symmetric? se))
  (is (mx/symmetric? se-sym)))

(deftest unit-diagonal?-test
  (is-not (mx/matrix-with-unit-diagonal? se))
  (is-not (mx/matrix-with-unit-diagonal? se-sym))
  (is (mx/matrix-with-unit-diagonal? (mx/identity-matrix 3))))

(deftest symmetric-with-unit-diagonal?-test
  (is-not (mx/symmetric-matrix-with-unit-diagonal? se))
  (is-not (mx/symmetric-matrix-with-unit-diagonal? se-sym))
  (is (mx/symmetric-matrix-with-unit-diagonal? (mx/identity-matrix 3))))

(deftest positive?-test
  (is-not (mx/positive-matrix? se))
  (is (mx/positive-matrix? se-sym))
  (is-not (mx/positive-matrix? '((1.0 -1.1) (-1.1 1.0))))
  (is-not (mx/positive-matrix? '((1.0 -1.0) (-1.0 1.0))))
  (is (mx/positive-matrix? '((1.0 -1.0) (-1.0 1.0)) 1e-32)))       ;accuracy too strict

(deftest positive-with-unit-diagonal?-test
  (is-not (mx/positive-matrix-with-unit-diagonal? se))
  (is-not (mx/positive-matrix-with-unit-diagonal? se-sym))
  (is-not (mx/positive-matrix-with-unit-diagonal? '((1.0 -1.1) (-1.1 1.0))))
  (is-not (mx/positive-matrix-with-unit-diagonal? '((1.0 -1.0) (-1.0 1.0))))
  (is (mx/positive-matrix-with-unit-diagonal? '((1.0 -1.0) (-1.0 1.0)) 1e-32))) ;accuracy too strict

(deftest non-negative?-test
  (is-not (mx/non-negative-matrix? se))
  (is (mx/non-negative-matrix? se-sym))
  (is-not (mx/non-negative-matrix? '((1.0 -1.1) (-1.1 1.0))))
  (is (mx/non-negative-matrix? '((1.0 -1.0) (-1.0 1.0))))
  (is-not (mx/non-negative-matrix? '((1.0 -1.0001) (-1.0001 1.0))))
  (is (mx/non-negative-matrix? '((1.0 -1.0001) (-1.0001 1.0)) 1e-3))) ;accuracy too lax

(deftest row-or-column-matrix?-test
  (is-not (mx/row-or-column-matrix? m/sq))
  (is-not (mx/row-or-column-matrix? se))
  (is-not (mx/row-or-column-matrix? ve))
  (is (mx/row-or-column-matrix? se-row))
  (is (mx/row-or-column-matrix? ve-row))
  (is (mx/row-or-column-matrix? se-col))
  (is (mx/row-or-column-matrix? ve-col))
  (is-not (mx/row-or-column-matrix? se1D))
  (is-not (mx/row-or-column-matrix? ve1D)))

(deftest size-symmetric-test
  (is= 1 (mx/size-symmetric 1))
  (is= nil (mx/size-symmetric 2))
  (is= 2 (mx/size-symmetric 3))
  (is= 3 (mx/size-symmetric 6)))

(deftest size-symmetric-with-unit-diagonal-test
  (is= 2 (mx/size-symmetric-with-unit-diagonal 1))
  (is= nil (mx/size-symmetric-with-unit-diagonal 2))
  (is= 3 (mx/size-symmetric-with-unit-diagonal 3))
  (is= 4 (mx/size-symmetric-with-unit-diagonal 6)))

(deftest ecount-symmetric-test
  (is= 1 (mx/ecount-symmetric 1))
  (is= 3 (mx/ecount-symmetric 2))
  (is= 6 (mx/ecount-symmetric 3))
  (is= 21 (mx/ecount-symmetric 6)))

(deftest ecount-symmetric-with-unit-diagonal-test
  (is= 0 (mx/ecount-symmetric-with-unit-diagonal 1))
  (is= 1 (mx/ecount-symmetric-with-unit-diagonal 2))
  (is= 3 (mx/ecount-symmetric-with-unit-diagonal 3))
  (is= 15 (mx/ecount-symmetric-with-unit-diagonal 6)))

(deftest to-vector-from-symmetric-test
  (is= [1.0 2.0 4.0] (mx/symmetric-matrix->vector se {::mx/by-row? false}))
  (is= [1.0 0.5 4.0] (mx/symmetric-matrix->vector ve))
  (is= [1.0 0.5 2.0] (mx/symmetric-matrix->vector se-sym {::mx/by-row? false}))
  (is= [1.0 0.5 2.0] (mx/symmetric-matrix->vector ve-sym))
  (is= nil (mx/symmetric-matrix->vector se1D))
  (is= [1.0] (mx/symmetric-matrix->vector se-row {::mx/by-row? false}))
  (is= [1.0 0.5] (mx/symmetric-matrix->vector se-row))
  (is= [1.0 0.5] (mx/symmetric-matrix->vector se-col {::mx/by-row? false}))
  (is= [1.0] (mx/symmetric-matrix->vector se-col)))

(deftest to-vector-from-symmetric-with-unit-diagonal-test
  (is= nil (mx/symmetric-matrix-with-unit-diagonal->vector #(+ 3 3) {::mx/by-row? false}))
  (is= [2.0] (mx/symmetric-matrix-with-unit-diagonal->vector se {::mx/by-row? false}))
  (is= [0.5] (mx/symmetric-matrix-with-unit-diagonal->vector ve))
  (is= [0.5] (mx/symmetric-matrix-with-unit-diagonal->vector se-sym {::mx/by-row? false}))
  (is= [0.5] (mx/symmetric-matrix-with-unit-diagonal->vector ve-sym))
  (is= nil (mx/symmetric-matrix-with-unit-diagonal->vector se1D))
  (is= [] (mx/symmetric-matrix-with-unit-diagonal->vector se-row {::mx/by-row? false}))
  (is= [0.5] (mx/symmetric-matrix-with-unit-diagonal->vector se-row))
  (is= [0.5] (mx/symmetric-matrix-with-unit-diagonal->vector se-col {::mx/by-row? false}))
  (is= [] (mx/symmetric-matrix-with-unit-diagonal->vector se-col)))

(deftest special-type-helpers-test
  (mempty?-test)
  (diagonal?-test)
  (symmetric?-test)
  (unit-diagonal?-test)
  (symmetric-with-unit-diagonal?-test)
  (positive?-test)
  (positive-with-unit-diagonal?-test)
  (non-negative?-test)
  (row-or-column-matrix?-test)
  (size-symmetric-test)
  (size-symmetric-with-unit-diagonal-test)
  (ecount-symmetric-test)
  (ecount-symmetric-with-unit-diagonal-test)
  (to-vector-from-symmetric-test)
  (to-vector-from-symmetric-with-unit-diagonal-test))

(defspec-test test-mempty? `mx/empty-matrix?)
(defspec-test test-diagonal? `mx/diagonal-matrix?)
(defspec-test test-symmetric? `mx/symmetric?)
(defspec-test test-unit-diagonal? `mx/matrix-with-unit-diagonal?)
(defspec-test test-symmetric-with-unit-diagonal? `mx/symmetric-matrix-with-unit-diagonal?)
(defspec-test test-positive? `mx/positive-matrix?)
(defspec-test test-positive-with-unit-diagonal? `mx/positive-matrix-with-unit-diagonal?)
(defspec-test test-non-negative? `mx/non-negative-matrix?)
(defspec-test test-row-or-column-matrix? `mx/row-or-column-matrix?)
(defspec-test test-size-symmetric `mx/size-symmetric)
(defspec-test test-size-symmetric-with-unit-diagonal `mx/size-symmetric-with-unit-diagonal)
(defspec-test test-ecount-symmetric `mx/ecount-symmetric)
(defspec-test test-ecount-symmetric-with-unit-diagonal `mx/ecount-symmetric-with-unit-diagonal)
(defspec-test test-to-vector-from-symmetric `mx/symmetric-matrix->vector)
(defspec-test test-to-vector-from-symmetric-with-unit-diagonal `mx/symmetric-matrix-with-unit-diagonal->vector)

#_(ost/unstrument)