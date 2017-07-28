(ns provisdom.math.t-matrix
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.matrix :as mx]
            [provisdom.math.core :as m]
            [provisdom.math.random2 :as random]
            [provisdom.math.tensor :as tensor]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]))

(ost/instrument)

(deftest matrix?-test
  (is-not (mx/matrix? [0]))
  (is-not (mx/matrix? [[] []]))
  (is (mx/matrix? [[]]))
  (is-not (mx/matrix? 0))
  (is-not (mx/matrix? [[[0.0] [0.0]]]))
  (is-not (mx/matrix? [[nil]]))
  (is-not (mx/matrix? [["A"]]))
  (is-not (mx/matrix? '((0))))
  (is (mx/matrix? [[0]]))
  (is (mx/matrix? [[m/nan]]))
  (is (mx/matrix? [[0.0] [0.0]])))

(deftest empty-matrix?-test
  (is-not (mx/empty-matrix? []))
  (is (mx/empty-matrix? [[]]))
  (is-not (mx/empty-matrix? [[1]]))
  (is-not (mx/empty-matrix? [[] [2]]))
  (is-not (mx/empty-matrix? [[m/nan]]))
  (is-not (mx/empty-matrix? [[false]])))

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

(deftest zero-matrix?-test
  (is-not (mx/zero-matrix? [0]))
  (is (mx/zero-matrix? [[0]]))
  (is (mx/zero-matrix? [[0.0]]))
  (is (mx/zero-matrix? [[0.0] [0.0]]))
  (is-not (mx/zero-matrix? [[0.0] [0.1]])))

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

(deftest upper-triangular-matrix?-test
  (is (mx/upper-triangular-matrix? [[]]))
  (is (mx/upper-triangular-matrix? [[1]]))
  (is-not (mx/upper-triangular-matrix? [[1 1]]))
  (is-not (mx/upper-triangular-matrix? [[1] [1]]))
  (is-not (mx/upper-triangular-matrix? [[1 1] [1 1]]))
  (is (mx/upper-triangular-matrix? [[1 0] [0 1]]))
  (is (mx/upper-triangular-matrix? [[1 1] [0 1]])))

(deftest lower-triangular-matrix?-test
  (is (mx/lower-triangular-matrix? [[]]))
  (is (mx/lower-triangular-matrix? [[1]]))
  (is-not (mx/lower-triangular-matrix? [[1 1]]))
  (is-not (mx/lower-triangular-matrix? [[1] [1]]))
  (is-not (mx/lower-triangular-matrix? [[1 1] [1 1]]))
  (is (mx/lower-triangular-matrix? [[1 0] [0 1]]))
  (is (mx/lower-triangular-matrix? [[1 0] [1 1]])))

(deftest symmetric-matrix?-test
  (is-not (mx/symmetric-matrix? [[1.0 0.5] [2.0 4.0]]))
  (is (mx/symmetric-matrix? [[1.0 0.5] [0.5 2.0]])))

(deftest type-tests
  (matrix?-test)
  (empty-matrix?-test)
  (row-matrix?-test)
  (column-matrix?-test)
  (zero-matrix?-test)
  (square-matrix?-test)
  (diagonal-matrix?-test)
  (upper-triangular-matrix?-test)
  (lower-triangular-matrix?-test)
  (symmetric-matrix?-test))

;(defspec-test test-matrix? `mx/matrix?) ;slowish
;(defspec-test test-empty-matrix? `mx/empty-matrix?) ;slowish
;(defspec-test test-row-matrix? `mx/row-matrix?) ;slowish
;(defspec-test test-column-matrix? `mx/column-matrix?) ;slowish
;(defspec-test test-zero-matrix? `mx/zero-matrix?) ;slowish
;(defspec-test test-square-matrix? `mx/square-matrix?) ;slowish
;(defspec-test test-diagonal-matrix? `mx/diagonal-matrix?) ;slowish
;(defspec-test test-upper-triangular-matrix? `mx/upper-triangular-matrix?) ;slowish
;(defspec-test test-lower-triangular-matrix? `mx/lower-triangular-matrix?) ;slowish
;(defspec-test test-symmetric-matrix? `mx/symmetric-matrix?) ;slowish

(deftest to-matrix-test
  (is= [[]] (mx/to-matrix [] 1))
  (is= [[]] (mx/to-matrix [1.0 0.5] 0))
  (is= [[1.0 0.5]] (mx/to-matrix [1.0 0.5] 1))
  (is= [[1.0] [0.5]] (mx/to-matrix [1.0 0.5] 2))
  (is= [[1.0] [0.5] [0.0]] (mx/to-matrix [1.0 0.5] 3))
  (is= [[1.0 3.0 5.0] [2.0 4.0 6.0]] (mx/to-matrix [1.0 2.0 3.0 4.0 5.0 6.0] 2 {::mx/by-row? false}))
  (is= [[1.0 2.0 3.0] [4.0 5.0 6.0]] (mx/to-matrix [1.0 2.0 3.0 4.0 5.0 6.0] 2))
  (is= [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 0.0]] (mx/to-matrix [1.0 2.0 3.0 4.0 5.0 6.0 7.0] 2)))

(deftest constant-matrix-test
  (is= [[]] (mx/constant-matrix 1 0 0.0))
  (is= [[]] (mx/constant-matrix 0 1 0.0))
  (is= [[1.0 1.0] [1.0 1.0]] (mx/constant-matrix 2 2 1.0)))

(deftest compute-matrix-test
  (is= [[]] (mx/compute-matrix 1 0 (fn [r c] (+ r c))))
  (is= [[]] (mx/compute-matrix 0 1 (fn [r c] (+ r c))))
  (is= [[0 1] [1 2]] (mx/compute-matrix 2 2 (fn [r c] (+ r c)))))

(deftest identity-matrix-test
  (is= [[]] (mx/identity-matrix 0))
  (is= [[1.0]] (mx/identity-matrix 1))
  (is= [[1.0 0.0] [0.0 1.0]] (mx/identity-matrix 2)))

(deftest row-matrix-test
  (is= [[]] (mx/row-matrix []))
  (is= [[1.0 0.5]] (mx/row-matrix [1.0 0.5]))
  (is= [[]] (mx/row-matrix 0 #(double %)))
  (is= [[0.0 1.0]] (mx/row-matrix 2 #(double %)))
  (is= [[3.0 3.0]] (mx/row-matrix 2 (constantly 3.0))))

(deftest column-matrix-test
  (is= [[]] (mx/column-matrix []))
  (is= [[1.0] [0.5]] (mx/column-matrix [1.0 0.5]))
  (is= [[]] (mx/column-matrix 0 #(double %)))
  (is= [[0.0] [1.0]] (mx/column-matrix 2 #(double %)))
  (is= [[3.0] [3.0]] (mx/column-matrix 2 (constantly 3.0))))

(deftest diagonal-matrix-test
  (is= [[]] (mx/diagonal-matrix []))
  (is= [[1.0 0.0] [0.0 0.5]] (mx/diagonal-matrix [1.0 0.5]))
  (is= [[]] (mx/diagonal-matrix 0 #(double %)))
  (is= [[0.0 0.0] [0.0 1.0]] (mx/diagonal-matrix 2 #(double %)))
  (is= [[1.0 0.0] [0.0 3.0]] (mx/diagonal-matrix [1.0 3.0]))
  (is= [[3.0 0.0] [0.0 3.0]] (mx/diagonal-matrix 2 (constantly 3.0))))

(deftest deserialize-upper-triangular-matrix-test
  (is= [[]] (mx/deserialize-upper-triangular-matrix []))
  (is= nil (mx/deserialize-upper-triangular-matrix [1.0 2.0]))
  (is= [[1.0 2.0] [0.0 3.0]] (mx/deserialize-upper-triangular-matrix [1.0 2.0 3.0]))
  (is= [[7.0 1.0 2.0 4.0] [0.0 8.0 3.0 5.0] [0.0 0.0 9.0 6.0] [0.0 0.0 0.0 10.0]]
       (mx/deserialize-upper-triangular-matrix [7.0 8.0 9.0 10.0] [1.0 2.0 3.0 4.0 5.0 6.0] {::mx/by-row? false}))
  (is= [[7.0 1.0 2.0 3.0] [0.0 8.0 4.0 5.0] [0.0 0.0 9.0 6.0] [0.0 0.0 0.0 10.0]]
       (mx/deserialize-upper-triangular-matrix [7.0 8.0 9.0 10.0] [1.0 2.0 3.0 4.0 5.0 6.0] {::mx/by-row? true}))
  (is= [[1.0 2.0 4.0] [0.0 3.0 5.0] [0.0 0.0 6.0]]
       (mx/deserialize-upper-triangular-matrix [1.0 2.0 3.0 4.0 5.0 6.0] {::mx/by-row? false}))
  (is= [[1.0 2.0 3.0] [0.0 4.0 5.0] [0.0 0.0 6.0]] (mx/deserialize-upper-triangular-matrix [1.0 2.0 3.0 4.0 5.0 6.0])))

(deftest deserialize-lower-triangular-matrix-test
  (is= [[]] (mx/deserialize-lower-triangular-matrix []))
  (is= nil (mx/deserialize-lower-triangular-matrix [1.0 2.0]))
  (is= [[1.0 0.0] [2.0 3.0]] (mx/deserialize-lower-triangular-matrix [1.0 2.0 3.0]))
  (is= [[7.0 0.0 0.0 0.0] [1.0 8.0 0.0 0.0] [2.0 4.0 9.0 0.0] [3.0 5.0 6.0 10.0]]
       (mx/deserialize-lower-triangular-matrix [7.0 8.0 9.0 10.0] [1.0 2.0 3.0 4.0 5.0 6.0] {::mx/by-row? false}))
  (is= [[7.0 0.0 0.0 0.0] [1.0 8.0 0.0 0.0] [2.0 3.0 9.0 0.0] [4.0 5.0 6.0 10.0]]
       (mx/deserialize-lower-triangular-matrix [7.0 8.0 9.0 10.0] [1.0 2.0 3.0 4.0 5.0 6.0] {::mx/by-row? true}))
  (is= [[1.0 0.0 0.0] [2.0 4.0 0.0] [3.0 5.0 6.0]]
       (mx/deserialize-lower-triangular-matrix [1.0 2.0 3.0 4.0 5.0 6.0] {::mx/by-row? false}))
  (is= [[1.0 0.0 0.0] [2.0 3.0 0.0] [4.0 5.0 6.0]] (mx/deserialize-lower-triangular-matrix [1.0 2.0 3.0 4.0 5.0 6.0])))

(deftest deserialize-symmetric-matrix-test
  (is= [[]] (mx/deserialize-symmetric-matrix []))
  (is= nil (mx/deserialize-symmetric-matrix [1.0 2.0]))
  (is= [[1.0 2.0] [2.0 3.0]] (mx/deserialize-symmetric-matrix [1.0 2.0 3.0])))

;;also called [[diagonal-constant-matrix]]
(deftest toeplitz-matrix-test
  (is= [[]] (mx/diagonal-constant-matrix [] []))
  (is= [[1]] (mx/diagonal-constant-matrix [1] [1]))
  (is= [[1.0 2.0 3.0] [4.0 1.0 2.0] [5.0 4.0 1.0]] (mx/toeplitz-matrix [1.0 2.0 3.0] [1.0 4.0 5.0]))
  (is= [[1.0 2.0] [4.0 1.0] [5.0 4.0]] (mx/toeplitz-matrix [1.0 2.0] [1.0 4.0 5.0]))
  (is= [[1.0 2.0 3.0] [4.0 1.0 2.0]] (mx/toeplitz-matrix [1.0 2.0 3.0] [1.0 4.0])))

(deftest outer-product-test
  (is= [[]] (mx/outer-product []))
  (is= [[9.0]] (mx/outer-product [3]))
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

(deftest sparse->matrix-test
  (is= [[4.0 5.0] [6.0 7.0]] (mx/sparse->matrix [] [[4.0 5.0] [6.0 7.0]]))
  (is= [[3.0 5.0] [6.0 7.0]] (mx/sparse->matrix [[0 0 3.0]] [[4.0 5.0] [6.0 7.0]]))
  (is= [[3.0 0.0] [0.0 0.0]] (mx/sparse->matrix [[0 0 3.0]] (mx/constant-matrix 2 2 0.0)))
  (is= [[3.0 5.0] [2.0 -1.0]]
       (mx/sparse->matrix [[0 0 3.0] [1 0 2.0] [0 1 5.0] [1 1 -1.0]] (mx/constant-matrix 2 2 0.0)))
  (is= [[0.0 0.0] [0.0 0.0]] (mx/sparse->matrix [[0 2 3.0]] (mx/constant-matrix 2 2 0.0))))

(deftest sparse->symmetric-matrix-test
  (is= [[4.0 5.0] [5.0 7.0]] (mx/sparse->symmetric-matrix [] [[4.0 5.0] [5.0 7.0]]))
  (is= [[3.0 2.0] [2.0 4.0]] (mx/sparse->symmetric-matrix [[0 0 3.0] [1 0 2.0]] [[1.0 2.0] [2.0 4.0]])))

(deftest constructor-tests
  (to-matrix-test)
  (constant-matrix-test)
  (compute-matrix-test)
  (identity-matrix-test)
  (row-matrix-test)
  (column-matrix-test)
  (diagonal-matrix-test)
  (deserialize-upper-triangular-matrix-test)
  (deserialize-lower-triangular-matrix-test)
  (deserialize-symmetric-matrix-test)
  (toeplitz-matrix-test)
  (outer-product-test)
  (rnd-matrix!-test)
  (rnd-reflection-matrix!-test)
  (rnd-spectral-matrix!-test)
  (sparse->matrix-test)
  (sparse->symmetric-matrix-test))

(comment "about 30 seconds total for these"
         (defspec-test test-to-matrix `mx/to-matrix)
         (defspec-test test-constant-matrix `mx/constant-matrix)
         (defspec-test test-compute-matrix `mx/compute-matrix)
         (defspec-test test-identity-matrix `mx/identity-matrix)
         (defspec-test test-row-matrix `mx/row-matrix)
         (defspec-test test-column-matrix `mx/column-matrix)
         (defspec-test test-diagonal-matrix `mx/diagonal-matrix)
         (defspec-test test-deserialize-upper-triangular-matrix `mx/deserialize-upper-triangular-matrix)
         (defspec-test test-deserialize-lower-triangular-matrix `mx/deserialize-lower-triangular-matrix)
         (defspec-test test-deserialize-symmetric-matrix `mx/deserialize-symmetric-matrix)
         (defspec-test test-toeplitz-matrix `mx/toeplitz-matrix)
         (defspec-test test-outer-product `mx/outer-product)
         (defspec-test test-rnd-matrix! `mx/rnd-matrix!)
         (defspec-test test-rnd-reflection-matrix! `mx/rnd-reflection-matrix!)
         (defspec-test test-rnd-spectral-matrix! `mx/rnd-spectral-matrix!)
         (defspec-test test-sparse->matrix `mx/sparse->matrix)
         (defspec-test test-sparse->symmetric-matrix `mx/sparse->symmetric-matrix)
         )

(deftest rows-test
  (is= 0 (mx/rows [[]]))
  (is= 1 (mx/rows [[1.0]]))
  (is= 1 (mx/rows [[1.0 2.0]]))
  (is= 2 (mx/rows [[1.0] [2.0]]))
  (is= 2 (mx/rows [[1.0 0.5] [2.0 4.0]])))

(deftest columns-test
  (is= 0 (mx/columns [[]]))
  (is= 1 (mx/columns [[1.0]]))
  (is= 2 (mx/columns [[1.0 2.0]]))
  (is= 1 (mx/columns [[1.0] [2.0]]))
  (is= 2 (mx/columns [[1.0 0.5] [2.0 4.0]])))

(deftest get-row-test
  (is= [1.0] (mx/get-row [[1.0]] 0))
  (is= [2.0] (mx/get-row [[1.0] [2.0]] 1))
  (is= [1.0 0.5] (mx/get-row [[1.0 0.5] [2.0 4.0]] 0)))

(deftest get-column-test
  (is= [1.0] (mx/get-column [[1.0]] 0))
  (is= [2.0] (mx/get-column [[1.0 2.0]] 1))
  (is= [1.0 2.0] (mx/get-column [[1.0 0.5] [2.0 4.0]] 0)))

(deftest diagonal-test
  (is= [] (mx/diagonal [[]]))
  (is= [1] (mx/diagonal [[1]]))
  (is= [1.0 4.0] (mx/diagonal [[1.0 0.5] [2.0 4.0]]))
  (is= [1.0] (mx/diagonal [[1.0 0.5]]))
  (is= [1.0] (mx/diagonal [[1.0] [0.5]]))
  (is= [0.5] (mx/diagonal [[1.0 0.5] [2.0 4.0]] 1))
  (is= [] (mx/diagonal [[1.0 0.5] [2.0 4.0]] 2))
  (is= [2.0] (mx/diagonal [[1.0 0.5] [2.0 4.0]] -1))
  (is= [] (mx/diagonal [[1.0 0.5] [2.0 4.0]] -2)))

(deftest serialize-symmetric-or-triangular-matrix-test
  (is= [] (mx/serialize-symmetric-or-triangular-matrix [[]]))
  (is= [3] (mx/serialize-symmetric-or-triangular-matrix [[3]]))
  (is= [1.0 2.0 4.0] (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5] [2.0 4.0]] {::mx/by-row? false}))
  (is= [1.0 0.5 4.0] (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5] [2.0 4.0]]))
  (is= [1.0 0.5 1.0] (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5] [0.5 1.0]] {::mx/by-row? false}))
  (is= [1.0 0.5 1.0] (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5] [0.5 1.0]]))
  (is= [1.0] (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5]] {::mx/by-row? false}))
  (is= [1.0 0.5] (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5]]))
  (is= [1.0 0.5] (mx/serialize-symmetric-or-triangular-matrix [[1.0] [0.5]] {::mx/by-row? false}))
  (is= [1.0] (mx/serialize-symmetric-or-triangular-matrix [[1.0] [0.5]])))

(deftest size-of-symmetric-or-triangular-matrix-test
  (is= 1 (mx/size-of-symmetric-or-triangular-matrix 1))
  (is (m/nan? (mx/size-of-symmetric-or-triangular-matrix 2)))
  (is= 2 (mx/size-of-symmetric-or-triangular-matrix 3))
  (is= 3 (mx/size-of-symmetric-or-triangular-matrix 6)))

(deftest size-of-symmetric-or-triangular-matrix-without-diagonal-test
  (is= 2 (mx/size-of-symmetric-or-triangular-matrix-without-diagonal 1))
  (is (m/nan? (mx/size-of-symmetric-or-triangular-matrix-without-diagonal 2)))
  (is= 3 (mx/size-of-symmetric-or-triangular-matrix-without-diagonal 3))
  (is= 4 (mx/size-of-symmetric-or-triangular-matrix-without-diagonal 6)))

(deftest ecount-of-symmetric-or-triangular-matrix-test
  (is= 1 (mx/ecount-of-symmetric-or-triangular-matrix 1))
  (is= 3 (mx/ecount-of-symmetric-or-triangular-matrix 2))
  (is= 6 (mx/ecount-of-symmetric-or-triangular-matrix 3))
  (is= 21 (mx/ecount-of-symmetric-or-triangular-matrix 6)))

(deftest ecount-of-symmetric-or-triangular-matrix-without-diagonal-test
  (is= 0 (mx/ecount-of-symmetric-or-triangular-matrix-without-diagonal 1))
  (is= 1 (mx/ecount-of-symmetric-or-triangular-matrix-without-diagonal 2))
  (is= 3 (mx/ecount-of-symmetric-or-triangular-matrix-without-diagonal 3))
  (is= 15 (mx/ecount-of-symmetric-or-triangular-matrix-without-diagonal 6)))

(deftest trace-test
  (is= 0.0 (mx/trace [[]]))
  (is= 1 (mx/trace [[1]]))
  (is= 5.0 (mx/trace [[1.0 0.5] [2.0 4.0]])))

(deftest get-slices-as-matrix-test
  (is= [[]] (mx/get-slices-as-matrix [[]] {::mx/row-indices 0}))
  (is= [[1.0 0.5]] (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices 0}))
  (is= [[1.0 0.5] [2.0 4.0]] (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices [0 1]}))
  (is= [[1.0] [2.0]] (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/column-indices 0}))
  (is= [[1.0 0.5] [2.0 4.0]] (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/column-indices [0 1]}))
  (is= [[2.0 4.0]] (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/exception-row-indices 0}))
  (is= [[]] (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/exception-row-indices [0 1]}))
  (is= [[0.5] [4.0]] (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/exception-column-indices 0}))
  (is= [[]] (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/exception-column-indices [0 1]}))
  (is= [[]] (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices 0 ::mx/exception-row-indices 0}))
  (is= [[]] (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices [0] ::mx/exception-row-indices 0}))
  (is= [[]] (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices 0 ::mx/exception-row-indices [0]}))
  (is= [[1.0]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/exception-row-indices 1 ::mx/exception-column-indices 1}))
  (is= [[1.0]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices 0 ::mx/column-indices 0})))

(deftest filter-by-row-test
  (is= [[]] (mx/filter-by-row #(< (apply + (tensor/emap m/sq %)) 2.0) [[]]))
  (is= [[1.0 0.5]] (mx/filter-by-row #(< (apply + (tensor/emap m/sq %)) 2.0) [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0 0.5]] (mx/filter-by-row #(< (apply + (tensor/emap m/sq %)) 2.0) [[1.0 0.5]]))
  (is= [[1.0] [0.5]] (mx/filter-by-row #(< (apply + (tensor/emap m/sq %)) 2.0) [[1.0] [0.5]]))
  (is= [[1.0 0.5]] (mx/filter-by-row #(< (apply + (tensor/emap m/sq %)) 2.0) [[1.0 0.5] [2.0 4.0]])))

(deftest filter-by-column-test
  (is= [[]] (mx/filter-by-column #(< (apply + (tensor/emap m/sq %)) 6.0) [[]]))
  (is= [[1.0] [2.0]] (mx/filter-by-column #(< (apply + (tensor/emap m/sq %)) 6.0) [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0 0.5]] (mx/filter-by-column #(< (apply + (tensor/emap m/sq %)) 6.0) [[1.0 0.5]]))
  (is= [[1.0] [0.5]] (mx/filter-by-column #(< (apply + (tensor/emap m/sq %)) 6.0) [[1.0] [0.5]]))
  (is= [[1.0] [2.0]] (mx/filter-by-column #(< (apply + (tensor/emap m/sq %)) 6.0) [[1.0 0.5] [2.0 4.0]])))

(deftest filter-symmetric-matrix-test
  (is= [[]] (mx/filter-symmetric-matrix #(< (apply + (tensor/emap m/sq %)) 2.0) [[]]))
  (is= [[1.0]] (mx/filter-symmetric-matrix #(< (apply + (tensor/emap m/sq %)) 2.0) [[1.0 0.5] [0.5 4.0]])))

(def s [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0] [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]])
(deftest matrix-partition-test
  (is= {::mx/bottom-left  [[9.0 10.0] [13.0 14.0]]
        ::mx/bottom-right [[11.0 12.0] [15.0 16.0]]
        ::mx/top-left     [[1.0 2.0] [5.0 6.0]]
        ::mx/top-right    [[3.0 4.0] [7.0 8.0]]}
       (mx/matrix-partition s 2 2))
  (is= {::mx/bottom-left  [[5.0] [9.0] [13.0]]
        ::mx/bottom-right [[6.0 7.0 8.0] [10.0 11.0 12.0] [14.0 15.0 16.0]]
        ::mx/top-left     [[1.0]]
        ::mx/top-right    [[2.0 3.0 4.0]]}
       (mx/matrix-partition s 1 1))
  (is= {::mx/bottom-left  [[1.0 2.0 3.0] [5.0 6.0 7.0] [9.0 10.0 11.0] [13.0 14.0 15.0]]
        ::mx/bottom-right [[4.0] [8.0] [12.0] [16.0]]
        ::mx/top-left     [[]]
        ::mx/top-right    [[]]}
       (mx/matrix-partition s 0 3))
  (is= {::mx/bottom-left  [[]]
        ::mx/bottom-right [[13.0 14.0 15.0 16.0]]
        ::mx/top-left     [[]]
        ::mx/top-right    [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0] [9.0 10.0 11.0 12.0]]}
       (mx/matrix-partition s 3 0))
  (is= {::mx/bottom-left  [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0] [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]]
        ::mx/bottom-right [[]]
        ::mx/top-left     [[]]
        ::mx/top-right    [[]]}
       (mx/matrix-partition s 0 4))
  (is= {::mx/bottom-left  [[]]
        ::mx/bottom-right [[]]
        ::mx/top-left     [[]]
        ::mx/top-right    [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0] [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]]}
       (mx/matrix-partition s 4 0))
  (is= {::mx/bottom-left  [[]]
        ::mx/bottom-right [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0] [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]]
        ::mx/top-left     [[]]
        ::mx/top-right    [[]]}
       (mx/matrix-partition s 0 0))
  (is= {::mx/bottom-left  [[]]
        ::mx/bottom-right [[]]
        ::mx/top-left     [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0] [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]]
        ::mx/top-right    [[]]}
       (mx/matrix-partition s 4 4)))

(deftest some-kv-test
  (is= 0.5 (mx/some-kv (fn [row column number] (> (+ row column) number)) [[1.0 0.5] [2.0 4.0]]))
  (is= 0.5 (mx/some-kv (fn [row column number] (> (+ row column) number)) [[1.0 0.5] [2.0 4.0]] {::mx/by-row false})))

(deftest ereduce-kv-test
  (is= 29.9
       (mx/ereduce-kv (fn [tot r c n1 n2 n3] (+ tot r c n1 n2 n3))
                      3.4
                      [[1.0 0.5] [2.0 4.0]]
                      [[1.0 0.5] [2.0 4.0]]
                      [[1.0 0.5] [2.0 4.0]]))
  (is= 22.4 (mx/ereduce-kv (fn [tot r c n1 n2] (+ tot r c n1 n2)) 3.4 [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
  (is= 14.9 (mx/ereduce-kv (fn [tot r c n] (+ tot r c n)) 3.4 [[1.0 0.5] [2.0 4.0]]))
  (is= 5.9 (mx/ereduce-kv (fn [tot r c n] (+ tot r c n)) 3.4 [[1.0 0.5]]))
  (is= 5.9 (mx/ereduce-kv (fn [tot r c n] (+ tot r c n)) 3.4 [[1.0] [0.5]]))
  (is= 7.4 (mx/ereduce-kv (fn [tot r c n1 n2] (+ tot r c n1 n2)) 3.4 [[1.0 0.5]] [[1.0 0.5]])))

(deftest matrix->sparse-test
  (is= [] (mx/matrix->sparse [[]]))
  (is= [[1 0 1.0]] (mx/matrix->sparse [[0.0 0.0] [1.0 0.0]]))
  (is= [[0 0 1.0] [0 1 0.5] [1 0 2.0] [1 1 4.0]] (mx/matrix->sparse [[1.0 0.5] [2.0 4.0]]))
  (is= [[0 0 1.0] [0 1 0.5] [1 0 2.0]] (mx/matrix->sparse [[1.0 0.5] [2.0 4.0]] #(< % 2.1)))
  (is= [] (mx/matrix->sparse [[1.0 0.5] [2.0 4.0]] neg?))
  (is= [[1 0 0.5]] (mx/matrix->sparse [[1.0] [0.5]] #(< % 0.7))))

(deftest symmetric-matrix->sparse-test
  (is= [] (mx/symmetric-matrix->sparse [[]]))
  (is= [[0 1 1.0]] (mx/symmetric-matrix->sparse [[0.0 1.0] [1.0 0.0]]))
  (is= [[0 0 1.0] [0 1 0.5] [1 1 4.0]] (mx/symmetric-matrix->sparse [[1.0 0.5] [0.5 4.0]]))
  (is= [[0 0 1.0] [0 1 0.5]] (mx/symmetric-matrix->sparse [[1.0 0.5] [0.5 4.0]] #(< % 2.1))))

(deftest info-tests
  (rows-test)
  (columns-test)
  (get-row-test)
  (get-column-test)
  (diagonal-test)
  (serialize-symmetric-or-triangular-matrix-test)
  (size-of-symmetric-or-triangular-matrix-test)
  (size-of-symmetric-or-triangular-matrix-without-diagonal-test)
  (ecount-of-symmetric-or-triangular-matrix-test)
  (ecount-of-symmetric-or-triangular-matrix-without-diagonal-test)
  (trace-test)
  (get-slices-as-matrix-test)
  (filter-by-row-test)
  (filter-by-column-test)
  (filter-symmetric-matrix-test)
  (matrix-partition-test)
  (some-kv-test)
  (ereduce-kv-test)
  (matrix->sparse-test)
  (symmetric-matrix->sparse-test))

(defspec-test test-rows `mx/rows)
(defspec-test test-columns `mx/columns)
(defspec-test test-get-row `mx/get-row)
(defspec-test test-get-column `mx/get-column)
(defspec-test test-diagonal `mx/diagonal)
(defspec-test test-serialize-symmetric-or-triangular-matrix `mx/serialize-symmetric-or-triangular-matrix)
(defspec-test test-size-of-symmetric-or-triangular-matrix `mx/size-of-symmetric-or-triangular-matrix)
(defspec-test test-ecount-of-symmetric-or-triangular-matrix `mx/ecount-of-symmetric-or-triangular-matrix)
(defspec-test test-trace `mx/trace)
(defspec-test test-get-slices-as-matrix `mx/get-slices-as-matrix)
(defspec-test test-filter-by-row 'mx/filter-by-row)
(defspec-test test-filter-by-column 'mx/filter-by-column)
(defspec-test test-filter-symmetric-matrix 'mx/filter-symmetric-matrix)
(defspec-test test-matrix-partition `mx/matrix-partition)
(defspec-test test-some-kv `mx/some-kv)
;(defspec-test test-ereduce-kv `mx/ereduce-kv) ;too general to spec-test
(defspec-test test-matrix->sparse 'mx/matrix->sparse)
(defspec-test test-symmetric-matrix->sparse 'mx/symmetric-matrix->sparse)

(deftest transpose-test
  (is= [[]] (mx/transpose [[]]))
  (is= [[1]] (mx/transpose [[1]]))
  (is= [[1.0 2.0] [0.5 4.0]] (mx/transpose [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0 0.5] [2.0 4.0]] (mx/transpose (mx/transpose [[1.0 0.5] [2.0 4.0]])))
  (is= [[1.0 0.5]] (mx/transpose [[1.0] [0.5]]))
  (is= [[1.0] [0.5]] (mx/transpose [[1.0 0.5]])))

(deftest assoc-row-test
  (is= [[8.0 9.0]] (mx/assoc-row [[]] 0 [8.0 9.0]))
  (is= nil (mx/assoc-row [[1]] 0 [8.0 9.0]))
  (is= [[2]] (mx/assoc-row [[1]] 0 [2]))
  (is= [[1] [2]] (mx/assoc-row [[1]] 1 [2]))
  (is= nil (mx/assoc-row [[1]] 2 [2]))
  (is= [[8.0 9.0] [2.0 4.0]] (mx/assoc-row [[1.0 0.5] [2.0 4.0]] 0 [8.0 9.0])))

(deftest assoc-column-test
  (is= [[8.0] [9.0]] (mx/assoc-column [[]] 0 [8.0 9.0]))
  (is= nil (mx/assoc-column [[1]] 0 [8.0 9.0]))
  (is= [[2]] (mx/assoc-column [[1]] 0 [2]))
  (is= [[1 2]] (mx/assoc-column [[1]] 1 [2]))
  (is= nil (mx/assoc-column [[1]] 2 [2]))
  (is= [[8.0 0.5] [9.0 4.0]] (mx/assoc-column [[1.0 0.5] [2.0 4.0]] 0 [8.0 9.0])))

(deftest assoc-diagonal-test
  (is= [[8.0 0.0] [0.0 9.0]] (mx/assoc-diagonal [[]] [8.0 9.0]))
  (is= nil (mx/assoc-diagonal [[1]] [8.0 9.0]))
  (is= [[2]] (mx/assoc-diagonal [[1]] [2]))
  (is= [[8.0 0.5] [2.0 9.0]] (mx/assoc-diagonal [[1.0 0.5] [2.0 4.0]] [8.0 9.0])))

(deftest insert-row-test
  (is= [[8.0 9.0]] (mx/insert-row [[]] 0 [8.0 9.0]))
  (is= nil (mx/insert-row [[1]] 0 [8.0 9.0]))
  (is= [[2] [1]] (mx/insert-row [[1]] 0 [2]))
  (is= [[1] [2]] (mx/insert-row [[1]] 1 [2]))
  (is= nil (mx/insert-row [[1]] 2 [2]))
  (is= [[8.0 9.0] [1.0 0.5] [2.0 4.0]] (mx/insert-row [[1.0 0.5] [2.0 4.0]] 0 [8.0 9.0]))
  (is= [[1.0 0.5] [8.0 9.0] [2.0 4.0]] (mx/insert-row [[1.0 0.5] [2.0 4.0]] 1 [8.0 9.0])))

(deftest insert-column-test
  (is= [[8.0] [9.0]] (mx/insert-column [[]] 0 [8.0 9.0]))
  (is= nil (mx/insert-column [[1]] 0 [8.0 9.0]))
  (is= [[2 1]] (mx/insert-column [[1]] 0 [2]))
  (is= [[1 2]] (mx/insert-column [[1]] 1 [2]))
  (is= nil (mx/insert-column [[1]] 2 [2]))
  (is= [[8.0 1.0 0.5] [9.0 2.0 4.0]] (mx/insert-column [[1.0 0.5] [2.0 4.0]] 0 [8.0 9.0])))

(deftest update-row-test
  (is= nil (mx/update-row [[]] 0 (fn [column number] (+ column number 1))))
  (is= [[2]] (mx/update-row [[1]] 0 (fn [column number] (+ column number 1))))
  (is= nil (mx/update-row [[1]] 1 (fn [column number] (+ column number 1))))
  (is= [[2.0 2.5] [2.0 4.0]] (mx/update-row [[1.0 0.5] [2.0 4.0]] 0 (fn [column number] (+ column number 1)))))

(deftest update-column-test
  (is= nil (mx/update-column [[]] 0 (fn [row number] (+ row number 1))))
  (is= [[2]] (mx/update-column [[1]] 0 (fn [row number] (+ row number 1))))
  (is= nil (mx/update-column [[1]] 1 (fn [row number] (+ row number 1))))
  (is= [[2.0 0.5] [4.0 4.0]] (mx/update-column [[1.0 0.5] [2.0 4.0]] 0 (fn [row number] (+ row number 1)))))

(deftest update-diagonal-test
  (is= [[]] (mx/update-diagonal [[]] (fn [row number] (+ row number 1))))
  (is= [[2]] (mx/update-diagonal [[1]] (fn [row number] (+ row number 1))))
  (is= [[2.0 0.5] [2.0 6.0]] (mx/update-diagonal [[1.0 0.5] [2.0 4.0]] (fn [row number] (+ row number 1)))))

(deftest concat-rows-test
  (is= [[]] (mx/concat-rows [[]] [[]]))
  (is= nil (mx/concat-rows [[]] [[1]]))
  (is= [[1.0 0.5] [2.0 4.0] [1.0 0.5] [2.0 4.0]] (mx/concat-rows [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0] [2.0]] (mx/concat-rows [[1.0]] [[2.0]]))
  (is= [[1.0 0.5] [1.0 0.5] [2.0 4.0]] (mx/concat-rows [[1.0 0.5]] [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0 0.5] [2.0 4.0] [1.0 0.5]] (mx/concat-rows [[1.0 0.5] [2.0 4.0]] [[1.0 0.5]]))
  (is= [[1.0 0.5] [1.0 0.5]] (mx/concat-rows [[1.0 0.5]] [[1.0 0.5]]))
  (is= [[1.0 0.5] [1.0 0.5] [1.0 0.5]] (mx/concat-rows [[1.0 0.5]] [[1.0 0.5]] [[1.0 0.5]])))

(deftest concat-columns-test
  (is= [[]] (mx/concat-columns [[]] [[]]))
  (is= nil (mx/concat-columns [[]] [[1]]))
  (is= [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0]] (mx/concat-columns [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0 2.0]] (mx/concat-columns [[1.0]] [[2.0]]))
  (is= [[1.0 1.0 0.5] [0.5 2.0 4.0]] (mx/concat-columns [[1.0] [0.5]] [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0 1.0] [0.5 0.5]] (mx/concat-columns [[1.0] [0.5]] [[1.0] [0.5]]))
  (is= [[1.0 1.0 1.0] [0.5 0.5 0.5]] (mx/concat-columns [[1.0] [0.5]] [[1.0] [0.5]] [[1.0] [0.5]])))

(deftest merge-matrices-test
  (is= [[]]
       (mx/merge-matrices {::mx/top-left     [[]]
                           ::mx/top-right    [[]]
                           ::mx/bottom-left  [[]]
                           ::mx/bottom-right [[]]}))
  (is= nil
       (mx/merge-matrices {::mx/top-left     [[]]
                           ::mx/top-right    [[]]
                           ::mx/bottom-left  [[]]
                           ::mx/bottom-right [[1]]}))
  (is= [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0] [1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0]]
       (mx/merge-matrices {::mx/top-left     [[1.0 0.5] [2.0 4.0]]
                           ::mx/top-right    [[1.0 0.5] [2.0 4.0]]
                           ::mx/bottom-left  [[1.0 0.5] [2.0 4.0]]
                           ::mx/bottom-right [[1.0 0.5] [2.0 4.0]]}))
  (is= [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0] [1.0 0.5 1.0 0.5]]
       (mx/merge-matrices {::mx/top-left     [[1.0 0.5] [2.0 4.0]]
                           ::mx/top-right    [[1.0 0.5] [2.0 4.0]]
                           ::mx/bottom-left  [[1.0 0.5]]
                           ::mx/bottom-right [[1.0 0.5]]})))

(deftest replace-submatrix-test
  (is= [[]] (mx/replace-submatrix [[]] [[]] 1 0))
  (is= [[0.0 0.0] [1.0 0.5] [2.0 4.0]] (mx/replace-submatrix [[]] [[1.0 0.5] [2.0 4.0]] 1 0))
  (is= [[1.0 0.5] [2.0 4.0]] (mx/replace-submatrix [[1.0 0.5] [2.0 4.0]] [[]] 1 0))
  (is= [[0.0 1.0 2.0] [1.0 0.5 5.0] [2.0 4.0 8.0]]
       (mx/replace-submatrix [[0.0 1.0 2.0] [3.0 4.0 5.0] [6.0 7.0 8.0]] [[1.0 0.5] [2.0 4.0]] 1 0))
  (is= [[0.0 1.0 0.5] [3.0 2.0 4.0] [6.0 7.0 8.0]]
       (mx/replace-submatrix [[0.0 1.0 2.0] [3.0 4.0 5.0] [6.0 7.0 8.0]] [[1.0 0.5] [2.0 4.0]] 0 1))
  (is= [[0.0 1.0 2.0] [3.0 4.0 5.0] [1.0 0.5 8.0] [2.0 4.0 0.0]]
       (mx/replace-submatrix [[0.0 1.0 2.0] [3.0 4.0 5.0] [6.0 7.0 8.0]] [[1.0 0.5] [2.0 4.0]] 2 0))
  (is= [[0.0 0.0 0.0 1.0 0.5] [0.0 1.0 2.0 2.0 4.0] [3.0 4.0 5.0 0.0 0.0] [6.0 7.0 8.0 0.0 0.0]]
       (mx/replace-submatrix [[0.0 1.0 2.0] [3.0 4.0 5.0] [6.0 7.0 8.0]] [[1.0 0.5] [2.0 4.0]] -1 3)))

(deftest permute-matrix-test
  (is= [[1.0 0.5] [2.0 4.0]] (mx/permute-matrix [[1.0 0.5] [2.0 4.0]] {}))
  (is= [[2.0 4.0] [1.0 0.5]] (mx/permute-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices [1 0]}))
  (is= [[2.0 4.0]] (mx/permute-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices [1]}))
  (is= [[]] (mx/permute-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices []}))
  (is= [[1.0 0.5]] (mx/permute-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices [0 2]}))
  (is= [[0.5 1.0] [4.0 2.0]] (mx/permute-matrix [[1.0 0.5] [2.0 4.0]] {::mx/column-indices [1 0]}))
  (is= [[4.0 2.0] [0.5 1.0]]
       (mx/permute-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices [1 0] ::mx/column-indices [1 0]}))
  (is= (mx/identity-matrix 2)
       (mx/permute-matrix (mx/identity-matrix 2) {::mx/row-indices [1 0] ::mx/column-indices [1 0]})))

(deftest square-matrix-by-trimming-test
  (is= [[]] (mx/square-matrix-by-trimming [[]]))
  (is= [[1.0]] (mx/square-matrix-by-trimming [[1.0]]))
  (is= [[1.0]] (mx/square-matrix-by-trimming [[1.0] [2.0] [3.0]]))
  (is= [[1.0]] (mx/square-matrix-by-trimming [[1.0 2.0 3.0]]))
  (is= [[1.0 2.0] [2.0 3.0]] (mx/square-matrix-by-trimming [[1.0 2.0] [2.0 3.0] [4.0 5.0]])))

(deftest symmetric-matrix-by-averaging-test
  (is= [[]] (mx/symmetric-matrix-by-averaging [[]]))
  (is= [[3]] (mx/symmetric-matrix-by-averaging [[3]]))
  (is= [[1.0 1.25] [1.25 4.0]] (mx/symmetric-matrix-by-averaging [[1.0 0.5] [2.0 4.0]])))

(deftest manipulation-tests
  (transpose-test)
  (assoc-row-test)
  (assoc-column-test)
  (assoc-diagonal-test)
  (insert-row-test)
  (insert-column-test)
  (update-row-test)
  (update-column-test)
  (update-diagonal-test)
  (concat-rows-test)
  (concat-columns-test)
  (merge-matrices-test)
  (replace-submatrix-test)
  (permute-matrix-test)
  (square-matrix-by-trimming-test)
  (symmetric-matrix-by-averaging-test))

(defspec-test test-transpose 'mx/transpose)
(defspec-test test-assoc-row 'mx/assoc-row)
(defspec-test test-assoc-column 'mx/assoc-column)
(defspec-test test-assoc-diagonal 'mx/assoc-diagonal)
(defspec-test test-insert-row 'mx/insert-row)
(defspec-test test-insert-column 'mx/insert-column)
(defspec-test test-update-row 'mx/update-row)
(defspec-test test-update-column 'mx/update-column)
(defspec-test test-update-diagonal 'mx/update-diagonal)
(defspec-test test-concat-rows 'mx/concat-rows)
(defspec-test test-concat-columns 'mx/concat-columns)
(defspec-test test-merge-matrices 'mx/merge-matrices)
(defspec-test test-replace-submatrix 'mx/replace-submatrix)
(defspec-test test-permute-matrix 'mx/permute-matrix)
(defspec-test test-square-matrix-by-trimming `mx/square-matrix-by-trimming)
(defspec-test test-symmetric-matrix-by-averaging `mx/symmetric-matrix-by-averaging)

(deftest mx*-test
  (is= [[]] (mx/mx* [[]]))
  (is= [[1]] (mx/mx* [[1]]))
  (is= [[]] (mx/mx* [[]] [[]]))
  (is= [[3.0] [3.0]] (mx/mx* [[1 1 1] [1 1 1]] [[1] [1] [1]]))
  (is= [[2.0 2.5] [10.0 17.0]] (mx/mx* [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
  (is= [[7.0 11.0] [44.0 73.0]] (mx/mx* [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]])))

(deftest kronecker-product-test
  (is= [[]] (mx/kronecker-product))
  (is= [[]] (mx/kronecker-product [[]]))
  (is= [[1]] (mx/kronecker-product [[1]]))
  (is= [[]] (mx/kronecker-product [[]] [[1]]))
  (is= [[1.0 0.5 0.5 0.25] [2.0 4.0 1.0 2.0] [2.0 1.0 4.0 2.0] [4.0 8.0 8.0 16.0]]
       (mx/kronecker-product [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0 0.5 0.5 0.25 0.5 0.25 0.25 0.125] [2.0 4.0 1.0 2.0 1.0 2.0 0.5 1.0]
        [2.0 1.0 4.0 2.0 1.0 0.5 2.0 1.0] [4.0 8.0 8.0 16.0 2.0 4.0 4.0 8.0]
        [2.0 1.0 1.0 0.5 4.0 2.0 2.0 1.0] [4.0 8.0 2.0 4.0 8.0 16.0 4.0 8.0]
        [4.0 2.0 8.0 4.0 8.0 4.0 16.0 8.0] [8.0 16.0 16.0 32.0 16.0 32.0 32.0 64.0]]
       (mx/kronecker-product [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0 0.5 0.5 0.25]] (mx/kronecker-product [[1.0 0.5]] [[1.0 0.5]]))
  (is= [[1.0 0.5 0.5 0.25 0.5 0.25 0.25 0.125]] (mx/kronecker-product [[1.0 0.5]] [[1.0 0.5]] [[1.0 0.5]])))

(deftest math-tests
  (mx*-test)
  (kronecker-product-test))

(defspec-test test-mx* 'mx/mx*)
(defspec-test test-kronecker-product 'mx/kronecker-product)

(deftest round-roughly-zero-rows-test
  (is= [[]] (mx/round-roughly-zero-rows [[]] 1e-6))
  (is= [[1.0 0.5] [2.0 4.0]] (mx/round-roughly-zero-rows [[1.0 0.5] [2.0 4.0]] 1e-6))
  (is= [[0.0 0.0] [1.0 1.0E-17]] (mx/round-roughly-zero-rows [[1e-13 1e-8] [1.0 1e-17]] 1e-6)))

(deftest round-roughly-zero-columns-test
  (is= [[]] (mx/round-roughly-zero-columns [[]] 1e-6))
  (is= [[1.0 0.5] [2.0 4.0]] (mx/round-roughly-zero-columns [[1.0 0.5] [2.0 4.0]] 1e-6))
  (is= [[1.0E-13 0.0] [1.0 0.0]] (mx/round-roughly-zero-columns [[1e-13 1e-8] [1.0 1e-17]] 1e-6)))

(deftest rounding-tests
  (round-roughly-zero-rows-test)
  (round-roughly-zero-columns-test))

(defspec-test test-round-roughly-zero-rows 'mx/round-roughly-zero-rows)
(defspec-test test-round-roughly-zero-columns 'mx/round-roughly-zero-columns)

#_(ost/unstrument)