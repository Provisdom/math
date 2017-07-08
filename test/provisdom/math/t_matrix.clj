(ns provisdom.math.t-matrix
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.matrix :as mx]
            [provisdom.math.core :as m]
            [provisdom.math.random2 :as random]
            [provisdom.math.clatrix :as decomp]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]))

(ost/instrument)

(deftest matrix?-test
  (is-not (mx/matrix? [0]))
  (is-not (mx/matrix? 0))
  (is-not (mx/matrix? [[[0.0] [0.0]]]))
  (is-not (mx/matrix? [[nil]]))
  (is-not (mx/matrix? [["A"]]))
  (is-not (mx/matrix? '((0))))
  (is (mx/matrix? [[0]]))
  (is (mx/matrix? [[m/nan]]))
  (is (mx/matrix? [[0.0] [0.0]])))

(deftest row-matrix?-test
  (is-not (mx/row-matrix? [0]))
  (is-not (mx/row-matrix? [[0.0] [0.0]]))
  (is-not (mx/row-matrix? [[nil]]))
  (is-not (mx/row-matrix? '((0))))
  (is (mx/row-matrix? [[0]]))
  (is (mx/row-matrix? [[0.0 0.0]]))
  (is (mx/row-matrix? [[m/nan]])))

(deftest column-matrix?-test
  (is-not (mx/column-matrix? [0]))
  (is-not (mx/column-matrix? [[0.0 0.0]]))
  (is-not (mx/column-matrix? [[nil]]))
  (is-not (mx/column-matrix? '((0))))
  (is (mx/column-matrix? [[0]]))
  (is (mx/column-matrix? [[0.0] [0.0]]))
  (is (mx/column-matrix? [[m/nan]])))

(deftest row-or-column-matrix?-test
  (is-not (mx/row-or-column-matrix? m/sq))
  (is-not (mx/row-or-column-matrix? [[1.0 0.5] [2.0 4.0]]))
  (is (mx/row-or-column-matrix? [[1.0 0.5]]))
  (is (mx/row-or-column-matrix? [[1.0] [0.5]]))
  (is-not (mx/row-or-column-matrix? [1.0 0.5]))
  (is-not (mx/row-or-column-matrix? 2.0)))

(deftest zero-matrix?-test
  (is-not (mx/zero-matrix? [0]))
  (is (mx/zero-matrix? [[0]]))
  (is (mx/zero-matrix? [[0.0]]))
  (is (mx/zero-matrix? [[0.0] [0.0]]))
  (is-not (mx/zero-matrix? [[0.0] [0.1]])))

(deftest empty-matrix?-test
  (is-not (mx/empty-matrix? []))
  (is (mx/empty-matrix? [[]]))
  (is-not (mx/empty-matrix? [[1]]))
  (is-not (mx/empty-matrix? [[] [2]]))
  (is-not (mx/empty-matrix? [[m/nan]]))
  (is-not (mx/empty-matrix? [[false]])))

(deftest square-matrix?-test
  (is-not (mx/square-matrix? []))
  (is (mx/square-matrix? [[]]))
  (is (mx/square-matrix? [[1]]))
  (is-not (mx/square-matrix? [[] [2]]))
  (is (mx/square-matrix? [[m/nan]]))
  (is-not (mx/square-matrix? [[1 2]])))

(deftest diagonal-matrix?-test
  (is-not (mx/diagonal-matrix? [[1.0 0.5] [2.0 4.0]]))
  (is (mx/diagonal-matrix? [[1.0 0.0] [0.0 2.0]])))

(deftest matrix-with-unit-diagonal?-test
  (is-not (mx/matrix-with-unit-diagonal? [[1.0 0.5] [2.0 4.0]]))
  (is-not (mx/matrix-with-unit-diagonal? [[1.0 0.5] [0.5 2.0]]))
  (is (mx/matrix-with-unit-diagonal? (mx/identity-matrix 3)))
  (is (mx/matrix-with-unit-diagonal? [[1.0 0.5] [0.5 1.0]])))

(deftest symmetric-matrix?-test
  (is-not (mx/symmetric-matrix? [[1.0 0.5] [2.0 4.0]]))
  (is (mx/symmetric-matrix? [[1.0 0.5] [0.5 2.0]])))

(deftest symmetric-matrix-with-unit-diagonal?-test
  (is-not (mx/symmetric-matrix-with-unit-diagonal? [[1.0 0.5] [2.0 4.0]]))
  (is-not (mx/symmetric-matrix-with-unit-diagonal? [[1.0 0.5] [0.5 2.0]]))
  (is (mx/symmetric-matrix-with-unit-diagonal? (mx/identity-matrix 3)))
  (is (mx/symmetric-matrix-with-unit-diagonal? [[1.0 0.5] [0.5 1.0]])))

(deftest type-tests
  (matrix?-test)
  (row-matrix?-test)
  (column-matrix?-test)
  (row-or-column-matrix?-test)
  (zero-matrix?-test)
  (empty-matrix?-test)
  (square-matrix?-test)
  (diagonal-matrix?-test)
  (matrix-with-unit-diagonal?-test)
  (symmetric-matrix?-test)
  (symmetric-matrix-with-unit-diagonal?-test))

(defspec-test test-matrix? `mx/matrix?)
(defspec-test test-row-matrix? `mx/row-matrix?)
(defspec-test test-column-matrix? `mx/column-matrix?)
(defspec-test test-row-or-column-matrix? `mx/row-or-column-matrix?)
(defspec-test test-zero-matrix? `mx/zero-matrix?)
(defspec-test test-empty-matrix? `mx/empty-matrix?)
(defspec-test test-square-matrix? `mx/square-matrix?)
(defspec-test test-diagonal-matrix? `mx/diagonal-matrix?)
(defspec-test test-matrix-with-unit-diagonal? `mx/matrix-with-unit-diagonal?)
(defspec-test test-symmetric-matrix? `mx/symmetric-matrix?)
(defspec-test test-symmetric-with-unit-diagonal? `mx/symmetric-matrix-with-unit-diagonal?)

(deftest to-matrix-test
  (is= [[1.0 0.5]] (mx/to-matrix [1.0 0.5] 1))
  (is= [[1.0 3.0 5.0] [2.0 4.0 6.0]] (mx/to-matrix [1.0 2.0 3.0 4.0 5.0 6.0] 2 {::mx/by-row? false}))
  (is= [[1.0 2.0 3.0] [4.0 5.0 6.0]] (mx/to-matrix [1.0 2.0 3.0 4.0 5.0 6.0] 2))
  (is= [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 0.0]] (mx/to-matrix [1.0 2.0 3.0 4.0 5.0 6.0 7.0] 2)))

(deftest constant-matrix-test
  (is= [[1.0 1.0] [1.0 1.0]] (mx/constant-matrix 2 2 1.0)))

(deftest compute-matrix-test)

(deftest identity-matrix-test)

(deftest row-matrix-test
  (is= [[1.0 0.5]] (mx/row-matrix [1.0 0.5]))
  (is= [[0.0 1.0]] (mx/row-matrix 2 #(double %)))
  (is= [[3.0 3.0]] (mx/row-matrix 2 (constantly 3.0))))

(deftest column-matrix-test
  (is= [[1.0] [0.5]] (mx/column-matrix [1.0 0.5]))
  (is= [[0.0] [1.0]] (mx/column-matrix 2 #(double %)))
  (is= [[3.0] [3.0]] (mx/column-matrix 2 (constantly 3.0))))

(deftest diagonal-matrix-test
  (is= [[1.0 0.0] [0.0 0.5]] (mx/diagonal-matrix [1.0 0.5]))
  (is= [[0.0 0.0] [0.0 1.0]] (mx/diagonal-matrix 2 #(double %)))
  (is= [[1.0 0.0] [0.0 3.0]] (mx/diagonal-matrix [1.0 3.0]))
  (is= [[3.0 0.0] [0.0 3.0]] (mx/diagonal-matrix 2 (constantly 3.0))))

(deftest triangular-matrix-test
  (is= [[1.0 2.0] [0.0 3.0]] (mx/triangular-matrix [1.0 2.0 3.0] {::mx/upper? true}))
  (is= [[1.0 0.0] [2.0 3.0]] (mx/triangular-matrix [1.0 2.0 3.0] {::mx/upper? false}))
  (is= [[7.0 0.0 0.0 0.0] [1.0 8.0 0.0 0.0] [2.0 4.0 9.0 0.0] [3.0 5.0 6.0 10.0]]
       (mx/triangular-matrix [7.0 8.0 9.0 10.0] [1.0 2.0 3.0 4.0 5.0 6.0] {::mx/upper? false ::mx/by-row? false}))
  (is= [[7.0 1.0 2.0 4.0] [0.0 8.0 3.0 5.0] [0.0 0.0 9.0 6.0] [0.0 0.0 0.0 10.0]]
       (mx/triangular-matrix [7.0 8.0 9.0 10.0] [1.0 2.0 3.0 4.0 5.0 6.0] {::mx/upper? true ::mx/by-row? false}))
  (is= [[7.0 0.0 0.0 0.0] [1.0 8.0 0.0 0.0] [2.0 3.0 9.0 0.0] [4.0 5.0 6.0 10.0]]
       (mx/triangular-matrix [7.0 8.0 9.0 10.0] [1.0 2.0 3.0 4.0 5.0 6.0] {::mx/upper? false}))
  (is= [[7.0 1.0 2.0 3.0] [0.0 8.0 4.0 5.0] [0.0 0.0 9.0 6.0] [0.0 0.0 0.0 10.0]]
       (mx/triangular-matrix [7.0 8.0 9.0 10.0] [1.0 2.0 3.0 4.0 5.0 6.0] {::mx/upper? true}))
  (is= [[1.0 0.0 0.0] [2.0 4.0 0.0] [3.0 5.0 6.0]]
       (mx/triangular-matrix [1.0 2.0 3.0 4.0 5.0 6.0] {::mx/upper? false ::mx/by-row? false}))
  (is= [[1.0 2.0 4.0] [0.0 3.0 5.0] [0.0 0.0 6.0]]
       (mx/triangular-matrix [1.0 2.0 3.0 4.0 5.0 6.0] {::mx/upper? true ::mx/by-row? false}))
  (is= [[1.0 0.0 0.0] [2.0 3.0 0.0] [4.0 5.0 6.0]]
       (mx/triangular-matrix [1.0 2.0 3.0 4.0 5.0 6.0] {::mx/upper? false}))
  (is= [[1.0 2.0 3.0] [0.0 4.0 5.0] [0.0 0.0 6.0]]
       (mx/triangular-matrix [1.0 2.0 3.0 4.0 5.0 6.0] {::mx/upper? true})))

(deftest symmetric-matrix-test
  (is= nil (mx/symmetric-matrix [1.0 2.0]))
  (is= [[1.0 2.0] [2.0 3.0]] (mx/symmetric-matrix [1.0 2.0 3.0]))
  (is= [[0.0 2.0] [2.0 3.0]] (mx/symmetric-matrix 2 #(double (+ %1 (* 2 %2)))))
  (is= [[0.0 1.0] [1.0 3.0]] (mx/symmetric-matrix 2 #(double (+ %1 (* 2 %2))) {::mx/by-row? false})))

(deftest symmetric-matrix-with-unit-diagonal-test
  (is= nil (mx/symmetric-matrix-with-unit-diagonal [1.0 2.0]))
  (is= [[1.0 1.0 2.0] [1.0 1.0 3.0] [2.0 3.0 1.0]] (mx/symmetric-matrix-with-unit-diagonal [1.0 2.0 3.0]))
  (is= [[1.0 2.0 4.0] [2.0 1.0 5.0] [4.0 5.0 1.0]]
       (mx/symmetric-matrix-with-unit-diagonal 3 #(double (+ %1 (* 2 %2)))))
  (is= [[1.0 1.0 2.0] [1.0 1.0 4.0] [2.0 4.0 1.0]]
       (mx/symmetric-matrix-with-unit-diagonal 3 #(double (+ %1 (* 2 %2))) {::mx/by-row? false})))

(deftest toeplitz-matrix-test                               ;also called [[diagonal-constant-matrix]]
  (is= [[1.0 2.0 3.0] [4.0 1.0 2.0] [5.0 4.0 1.0]] (mx/toeplitz-matrix [1.0 2.0 3.0] [1.0 4.0 5.0]))
  (is= [[1.0 2.0] [4.0 1.0] [5.0 4.0]] (mx/toeplitz-matrix [1.0 2.0] [1.0 4.0 5.0]))
  (is= [[1.0 2.0 3.0] [4.0 1.0 2.0]] (mx/toeplitz-matrix [1.0 2.0 3.0] [1.0 4.0])))

(deftest outer-product-test
  (is= [[1.0 2.0] [2.0 4.0]] (mx/outer-product [1.0 2.0]))
  (is= [[4.0 8.0 10.0] [8.0 16.0 20.0] [10.0 20.0 25.0]] (mx/outer-product [2.0 4.0 5.0]))
  (is= [[1.0 0.5] [0.5 0.25]] (mx/outer-product [1.0 0.5]))
  (is= [[4.0 6.0] [6.0 9.0]] (mx/outer-product [2.0 3.0])))

(deftest rnd-matrix!-test
  (random/bind-seed 0
    (is= [[]] (mx/rnd-matrix! 0 0)))
  (random/bind-seed 0
    (is= [[0.8833108082136426 0.026433771592597743] [0.10634669156721244 0.17386786595968284]]
         (mx/rnd-matrix! 2 2)))
  (random/bind-seed 0
    (is= [[0.8833108082136426 0.026433771592597743 0.10634669156721244]
          [0.17386786595968284 0.24568894884013137 0.39646797562881353]]
         (mx/rnd-matrix! 2 3))))

(deftest rnd-reflection-matrix!-test
  (random/bind-seed 0
    (is= [[]] (mx/rnd-reflection-matrix! 0)))
  (random/bind-seed 0
    (is= [[-1.0]] (mx/rnd-reflection-matrix! 1)))
  (random/bind-seed 0
    (is= [[-0.9982104970725829 -0.059798022827738856] [-0.059798022827738856 0.998210497072583]]
         (mx/rnd-reflection-matrix! 2))))

(deftest rnd-spectral-matrix!-test
  (random/bind-seed 0
    (is= [[]] (mx/rnd-spectral-matrix! [])))
  (random/bind-seed 0
    (is= [[2.0]] (mx/rnd-spectral-matrix! [2.0])))
  (random/bind-seed 0
    (is= [[1.7925482077721386 -0.9782452422074177] [-0.9782452422074177 2.2074517922278627]]
         (mx/rnd-spectral-matrix! [1.0 3.0]))))

(deftest rnd-positive-matrix!-test
  (random/bind-seed 0
    (is= [[]] (mx/rnd-positive-matrix! 0)))
  (random/bind-seed 0
    (is= [[0.8833108082136426]] (mx/rnd-positive-matrix! 1)))
  (random/bind-seed 0
    (is= [[0.6946098792362991 0.3550851337817903] [0.3550851337817903 0.21513470056994127]]
         (mx/rnd-positive-matrix! 2)))
  (random/bind-seed 0
    (is (decomp/positive-matrix? (mx/rnd-positive-matrix! 2)))))

(deftest constructor-tests
  (to-matrix-test)
  (constant-matrix-test)
  (compute-matrix-test)
  (identity-matrix-test)
  (row-matrix-test)
  (column-matrix-test)
  (diagonal-matrix-test)
  (triangular-matrix-test)
  (symmetric-matrix-test)
  (symmetric-matrix-with-unit-diagonal-test)
  (toeplitz-matrix-test)
  (outer-product-test)
  (rnd-matrix!-test)
  (rnd-reflection-matrix!-test)
  (rnd-spectral-matrix!-test)
  (rnd-positive-matrix!-test))

(defspec-test test-to-matrix `mx/to-matrix)
(defspec-test test-constant-matrix `mx/constant-matrix)
(defspec-test test-compute-matrix `mx/compute-matrix)
(defspec-test test-identity-matrix `mx/identity-matrix)
(defspec-test test-row-matrix `mx/row-matrix)
(defspec-test test-column-matrix `mx/column-matrix)
(defspec-test test-diagonal-matrix `mx/diagonal-matrix)
(defspec-test test-triangular-matrix `mx/triangular-matrix)
(defspec-test test-symmetric-matrix `mx/symmetric-matrix)
(defspec-test test-symmetric-matrix-with-unit-diagonal `mx/symmetric-matrix-with-unit-diagonal)
(defspec-test test-toeplitz-matrix `mx/toeplitz-matrix)
(defspec-test test-outer-product `mx/outer-product)
(defspec-test test-rnd-matrix! `mx/rnd-matrix!)
(defspec-test test-rnd-reflection-matrix! `mx/rnd-reflection-matrix!)
(defspec-test test-rnd-spectral-matrix! `mx/rnd-spectral-matrix!)
(defspec-test test-rnd-positive-matrix! `mx/rnd-positive-matrix!)

(def ve [[1.0 0.5] [2.0 4.0]])
(def ve-sym [[1.0 0.5] [0.5 1.0]])
(def ve1D [1.0 0.5])
(def ve-row [[1.0 0.5]])
(def ve-col [[1.0] [0.5]])

(deftest sparse->matrix-test
  (is= [[3.0 5.0] [6.0 7.0]] (mx/sparse->matrix [[0 0 3.0]] [[4.0 5.0] [6.0 7.0]]))
  (is= [[3.0 0.0] [0.0 0.0]] (mx/sparse->matrix [[0 0 3.0]] [2 2]))
  (is= [[3.0 5.0] [2.0 -1.0]] (mx/sparse->matrix [[0 0 3.0] [1 0 2.0] [0 1 5.0] [1 1 -1.0]] [2 2]))
  (is= nil (mx/sparse->matrix [[0 2 3.0]] [2 2])))

(deftest sparse->symmetric-matrix-test
  (is= [[3.0 2.0] [2.0 4.0]] (mx/sparse->symmetric-matrix [[0 0 3.0] [1 0 2.0]] [[1.0 2.0] [3.0 4.0]])))

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
  (is= [1.0 2.0 4.0] (mx/symmetric-matrix->vector [[1.0 0.5] [2.0 4.0]] {::mx/by-row? false}))
  (is= [1.0 0.5 4.0] (mx/symmetric-matrix->vector [[1.0 0.5] [2.0 4.0]]))
  (is= [1.0 0.5 2.0] (mx/symmetric-matrix->vector [[1.0 0.5] [0.5 1.0]] {::mx/by-row? false}))
  (is= [1.0 0.5 2.0] (mx/symmetric-matrix->vector [[1.0 0.5] [0.5 1.0]]))
  (is= nil (mx/symmetric-matrix->vector [1.0 0.5]))
  (is= [1.0] (mx/symmetric-matrix->vector [[1.0 0.5]] {::mx/by-row? false}))
  (is= [1.0 0.5] (mx/symmetric-matrix->vector [[1.0 0.5]]))
  (is= [1.0 0.5] (mx/symmetric-matrix->vector [[1.0] [0.5]] {::mx/by-row? false}))
  (is= [1.0] (mx/symmetric-matrix->vector [[1.0] [0.5]])))

(deftest to-vector-from-symmetric-with-unit-diagonal-test
  (is= nil (mx/symmetric-matrix-with-unit-diagonal->vector #(+ 3 3) {::mx/by-row? false}))
  (is= [2.0] (mx/symmetric-matrix-with-unit-diagonal->vector [[1.0 0.5] [2.0 4.0]] {::mx/by-row? false}))
  (is= [0.5] (mx/symmetric-matrix-with-unit-diagonal->vector [[1.0 0.5] [2.0 4.0]]))
  (is= [0.5] (mx/symmetric-matrix-with-unit-diagonal->vector [[1.0 0.5] [0.5 1.0]] {::mx/by-row? false}))
  (is= [0.5] (mx/symmetric-matrix-with-unit-diagonal->vector [[1.0 0.5] [0.5 1.0]]))
  (is= nil (mx/symmetric-matrix-with-unit-diagonal->vector [1.0 0.5]))
  (is= [] (mx/symmetric-matrix-with-unit-diagonal->vector [[1.0 0.5]] {::mx/by-row? false}))
  (is= [0.5] (mx/symmetric-matrix-with-unit-diagonal->vector [[1.0 0.5]]))
  (is= [0.5] (mx/symmetric-matrix-with-unit-diagonal->vector [[1.0] [0.5]] {::mx/by-row? false}))
  (is= [] (mx/symmetric-matrix-with-unit-diagonal->vector [[1.0] [0.5]])))

(deftest special-type-helpers-test
  (size-symmetric-test)
  (size-symmetric-with-unit-diagonal-test)
  (ecount-symmetric-test)
  (ecount-symmetric-with-unit-diagonal-test)
  (to-vector-from-symmetric-test)
  (to-vector-from-symmetric-with-unit-diagonal-test))

(defspec-test test-size-symmetric `mx/size-symmetric)
(defspec-test test-size-symmetric-with-unit-diagonal `mx/size-symmetric-with-unit-diagonal)
(defspec-test test-ecount-symmetric `mx/ecount-symmetric)
(defspec-test test-ecount-symmetric-with-unit-diagonal `mx/ecount-symmetric-with-unit-diagonal)
(defspec-test test-to-vector-from-symmetric `mx/symmetric-matrix->vector)
(defspec-test test-to-vector-from-symmetric-with-unit-diagonal `mx/symmetric-matrix-with-unit-diagonal->vector)


(deftest square-matrix-test
  (is= [[]] (mx/square-matrix [[]]))
  (is= [[1.0]] (mx/square-matrix [[1.0]]))
  (is= [[1.0]] (mx/square-matrix [[1.0][2.0][3.0]]))
  (is= [[1.0]] (mx/square-matrix [[1.0 2.0 3.0]]))
  (is= [[1.0 2.0][2.0 3.0]] (mx/square-matrix [[1.0 2.0][2.0 3.0][4.0 5.0]])))

(deftest symmetric-matrix-by-averaging-test
  (is= [[1.0 1.25] [1.25 4.0]] (mx/symmetric-matrix-by-averaging [[1.0 0.5] [2.0 4.0]])))

(deftest manipulation-tests
  (square-matrix-test)
  (symmetric-matrix-by-averaging-test))

(defspec-test test-square-matrix `mx/square-matrix)
(defspec-test test-symmetric-matrix-by-averaging `mx/symmetric-matrix-by-averaging)

#_(ost/unstrument)