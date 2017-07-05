(ns provisdom.math.t-matrix
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.matrix :as mx]
            [provisdom.math.core :as m]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]))

(ost/instrument)

(def ve [[1.0 0.5] [2.0 4.0]])
(def ve-sym [[1.0 0.5] [0.5 1.0]])
(def ve1D [1.0 0.5])
(def ve-row [[1.0 0.5]])
(def ve-col [[1.0] [0.5]])

(deftest matrix?-test)
(deftest row-matrix?-test)
(deftest column-matrix?-test)

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

(deftest zero-matrix?-test)

(deftest empty-matrix?-test
  (is (mx/empty-matrix? [[]]))
  (is-not (mx/empty-matrix? [[1]]))
  (is-not (mx/empty-matrix? [[] [2]]))
  (is-not (mx/empty-matrix? [[m/nan]]))
  (is-not (mx/empty-matrix? [[false]])))

(deftest square-matrix?-test)

(deftest diagonal-matrix?-test
  (is-not (mx/diagonal-matrix? ve))
  (is (mx/diagonal-matrix? [[1.0 0.0] [0.0 2.0]])))

(deftest matrix-with-unit-diagonal?-test
  (is-not (mx/matrix-with-unit-diagonal? ve))
  (is-not (mx/matrix-with-unit-diagonal? ve-sym))
  (is (mx/matrix-with-unit-diagonal? (mx/identity-matrix 3))))

(deftest symmetric-matrix?-test
  (is-not (mx/symmetric-matrix? ve))
  (is (mx/symmetric-matrix? ve-sym)))

(deftest symmetric-matrix-with-unit-diagonal?-test
  (is-not (mx/symmetric-matrix-with-unit-diagonal? ve))
  (is-not (mx/symmetric-matrix-with-unit-diagonal? ve-sym))
  (is (mx/symmetric-matrix-with-unit-diagonal? (mx/identity-matrix 3))))

(deftest positive-matrix?-test
  (is-not (mx/positive-matrix? ve))
  (is (mx/positive-matrix? ve-sym))
  (is-not (mx/positive-matrix? '((1.0 -1.1) (-1.1 1.0))))
  (is-not (mx/positive-matrix? '((1.0 -1.0) (-1.0 1.0))))
  (is (mx/positive-matrix? '((1.0 -1.0) (-1.0 1.0)) 1e-32)))       ;accuracy too strict

(deftest positive-matrix-with-unit-diagonal?-test
  (is-not (mx/positive-matrix-with-unit-diagonal? se))
  (is-not (mx/positive-matrix-with-unit-diagonal? se-sym))
  (is-not (mx/positive-matrix-with-unit-diagonal? '((1.0 -1.1) (-1.1 1.0))))
  (is-not (mx/positive-matrix-with-unit-diagonal? '((1.0 -1.0) (-1.0 1.0))))
  (is (mx/positive-matrix-with-unit-diagonal? '((1.0 -1.0) (-1.0 1.0)) 1e-32))) ;accuracy too strict

(deftest non-negative-matrix?-test
  (is-not (mx/non-negative-matrix? se))
  (is (mx/non-negative-matrix? se-sym))
  (is-not (mx/non-negative-matrix? '((1.0 -1.1) (-1.1 1.0))))
  (is (mx/non-negative-matrix? '((1.0 -1.0) (-1.0 1.0))))
  (is-not (mx/non-negative-matrix? '((1.0 -1.0001) (-1.0001 1.0))))
  (is (mx/non-negative-matrix? '((1.0 -1.0001) (-1.0001 1.0)) 1e-3))) ;accuracy too lax

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
  (symmetric-matrix-with-unit-diagonal?-test)
  (positive-matrix?-test)
  (positive-matrix-with-unit-diagonal?-test)
  (non-negative-matrix?-test))

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
(defspec-test test-positive-matrix? `mx/positive-matrix?)
(defspec-test test-positive-matrix-with-unit-diagonal? `mx/positive-matrix-with-unit-diagonal?)
(defspec-test test-non-negative-matrix? `mx/non-negative-matrix?)

(deftest to-matrix-test)
(deftest constant-matrix-test)
(deftest compute-matrix-test)
(deftest identity-matrix-test)
(deftest square-matrix-test)
(deftest row-matrix-test)
(deftest column-matrix-test)
(deftest diagonal-matrix-test)
(deftest triangular-matrix-test)
(deftest symmetric-matrix-test)
(deftest symmetric-matrix-with-unit-diagonal-test)
(deftest symmetric-matrix-by-averaging-test)
(deftest toeplitz-matrix-test)
(deftest outer-product-test)
(deftest sparse->matrix-test)
(deftest sparse->symmetric-matrix-test)
(deftest rnd-matrix-test)
(deftest rnd-reflection-matrix-test)
(deftest rnd-spectral-matrix-test)
(deftest rnd-positive-matrix-test)

(facts "matrix constructors"
       (fact "constant"
             (constant-matrix [2 2] 1.0) => [[1.0 1.0] [1.0 1.0]]
             (constant-matrix :clatrix [2 2] 1.0)
             => (clatrix [[1.0 1.0] [1.0 1.0]])
             (constant-matrix :apache-commons [2 2] 1.0)
             => (apache-commons '((1.0, 1.0) (1.0, 1.0))))
       (fact "to-matrix"
             (to-matrix se1D se1D 1 true) => [[1.0 0.5]]
             (to-matrix ve1D ve1D 1 true) => [[1.0 0.5]]
             (to-matrix ap1D ap1D 1 true) => (apache-commons
                                               [[1.0 0.5]])
             (to-matrix cl1D cl1D 1 true) => (clatrix [[1.0 0.5]])
             (to-matrix [1.0 2.0 3.0 4.0 5.0 6.0] 2 false)
             => [[1.0 3.0 5.0] [2.0 4.0 6.0]]
             (to-matrix [1.0 2.0 3.0 4.0 5.0 6.0] 2 true)
             => [[1.0 2.0 3.0] [4.0 5.0 6.0]]
             (to-matrix [1.0 2.0 3.0 4.0 5.0 6.0 7.0] 2 true)
             => (throws)
             (to-matrix :clatrix [1.0 2.0 3.0 4.0 5.0 6.0] 2 true)
             => (clatrix [[1.0 2.0 3.0] [4.0 5.0 6.0]])
             (to-matrix
               :apache-commons [1.0 2.0 3.0 4.0 5.0 6.0] 2 true)
             => (apache-commons '((1.0 2.0 3.0) (4.0 5.0 6.0))))
       (fact "square"
             (square-matrix [[1.0 2.0 3.0]]) => [[1.0]]
             (square-matrix 2 #(double (+ % %2))) => [[0.0 1.0] [1.0 2.0]]
             (square-matrix 2 3.0) => [[3.0 3.0] [3.0 3.0]]
             (square-matrix :clatrix 2 #(+ % %2)) => (clatrix [[0.0 1.0]
                                                               [1.0 2.0]])
             (square-matrix :apache-commons 2 #(+ % %2))
             => (apache-commons '((0.0 1.0) (1.0 2.0))))
       (fact "column"
             (column-matrix se1D se1D) => [[1.0] [0.5]]
             (column-matrix ve1D ve1D) => [[1.0] [0.5]]
             (column-matrix ap1D ap1D) => (apache-commons [[1.0] [0.5]])
             (column-matrix cl1D cl1D) => (clatrix [[1.0] [0.5]])
             (column-matrix nil 2 #(double %)) => [[0.0] [1.0]]
             (column-matrix nil 2 3.0) => [[3.0] [3.0]]
             (column-matrix :clatrix 2 #(+ % 0.0)) => (clatrix [[0.0] [1.0]])
             (column-matrix :apache-commons 2 #(+ % 0.0)) => (apache-commons
                                                               '((0.0) (1.0))))
       (fact "row"
             (row-matrix se1D se1D) => [[1.0 0.5]]
             (row-matrix ve1D ve1D) => [[1.0 0.5]]
             (row-matrix ap1D ap1D) => (apache-commons [[1.0 0.5]])
             (row-matrix cl1D ap1D) => (clatrix [[1.0 0.5]])
             (row-matrix cl1D cl1D) => (clatrix [[1.0 0.5]])
             (row-matrix nil 2 #(double %)) => [[0.0 1.0]]
             (row-matrix nil 2 3.0) => [[3.0 3.0]]
             (row-matrix :clatrix 2 #(+ % 0.0)) => (clatrix [[0.0 1.0]])
             (row-matrix :apache-commons 2 #(+ % 0.0)) => (apache-commons
                                                            '((0.0 1.0))))
       (fact "diagonal"
             (diagonal-matrix se1D se1D) => [[1.0 0.0] [0.0 0.5]]
             (diagonal-matrix ve1D ve1D) => [[1.0 0.0] [0.0 0.5]]
             (diagonal-matrix ap1D ap1D) => (apache-commons [[1.0 0.0]
                                                             [0.0 0.5]])
             (diagonal-matrix cl1D cl1D) => (clatrix [[1.0 0.0] [0.0 0.5]])
             (diagonal-matrix nil 2 #(double %)) => [[0.0 0.0] [0.0 1.0]]
             (diagonal-matrix nil [1.0 3.0]) => [[1.0 0.0] [0.0 3.0]]
             (diagonal-matrix nil 2 3.0) => [[3.0 0.0] [0.0 3.0]]
             (diagonal-matrix :clatrix 2 #(+ % 0.0)) => (clatrix [[0.0 0.0]
                                                                  [0.0 1.0]])
             (diagonal-matrix :apache-commons 2 #(+ % 0.0))
             => (apache-commons '((0.0 0.0) (0.0 1.0))))
       (fact "triangular"
             (triangular-matrix [1.0 2.0 3.0] true) => [[1.0 2.0] [0.0 3.0]]
             (triangular-matrix [1.0 2.0 3.0] false) => [[1.0 0.0] [2.0 3.0]]
             (triangular-matrix :clatrix [1.0 2.0 3.0] false)
             => (clatrix [[1.0 0.0] [2.0 3.0]])
             (triangular-matrix nil [4.0 5.0 6.0] [1.0 2.0 3.0] false)
             => [[4.0 0.0 0.0] [1.0 5.0 0.0] [2.0 3.0 6.0]])
       (fact "symmetric"
             (symmetric-matrix nil [1.0 2.0]) => (throws)
             (symmetric-matrix [1.0 2.0 3.0]) => [[1.0 2.0] [2.0 3.0]]
             (symmetric-matrix #(double (+ % (* 2 %2))) 2 true) => [[0.0 2.0]
                                                                    [2.0 3.0]]
             (symmetric-matrix nil #(double (+ % (* 2 %2))) 2 false)
             => [[0.0 1.0] [1.0 3.0]]
             (symmetric-matrix :clatrix #(+ % (* 2 %2)) 2 true)
             => (clatrix [[0.0 2.0] [2.0 3.0]])
             (symmetric-matrix :apache-commons [1.0 2.0 3.0])
             => (apache-commons '((1.0 2.0) (2.0 3.0))))
       (fact "symmetric with unit diagonal"
             (symmetric-matrix-with-unit-diagonal nil [1.0 2.0]) => throws
             (symmetric-matrix-with-unit-diagonal [1.0 2.0 3.0])
             => [[1.0 1.0 2.0] [1.0 1.0 3.0] [2.0 3.0 1.0]]
             (symmetric-matrix-with-unit-diagonal
               nil 3 #(double (+ % (* 2 %2))) true)
             => [[1.0 2.0 4.0] [2.0 1.0 5.0] [4.0 5.0 1.0]]
             (symmetric-matrix-with-unit-diagonal
               nil 3 #(double (+ % (* 2 %2))) false)
             => [[1.0 1.0 2.0] [1.0 1.0 4.0] [2.0 4.0 1.0]]
             (symmetric-matrix-with-unit-diagonal
               :clatrix 3 #(+ % (* 2 %2)) true)
             => (clatrix [[1.0 2.0 4.0] [2.0 1.0 5.0] [4.0 5.0 1.0]])
             (symmetric-matrix-with-unit-diagonal
               :apache-commons [1.0 2.0 3.0])
             => (apache-commons '((1.0 1.0 2.0) (1.0 1.0 3.0) (2.0 3.0 1.0))))
       (fact "symmetric matrix by averaging"
             (symmetric-matrix-by-averaging se1D) => (throws)
             (symmetric-matrix-by-averaging se) => [[1.0 1.25] [1.25 4.0]]
             (symmetric-matrix-by-averaging ve) => [[1.0 1.25] [1.25 4.0]]
             (symmetric-matrix-by-averaging cl) => [[1.0 1.25] [1.25 4.0]]
             (symmetric-matrix-by-averaging ap) => (apache-commons
                                                     [[1.0 1.25] [1.25 4.0]]))
       (fact "toeplitz"
             (toeplitz-matrix [1.0 2.0 3.0] [1.0 4.0 5.0])
             => [[1.0 2.0 3.0] [4.0 1.0 2.0] [5.0 4.0 1.0]]
             (toeplitz-matrix [1.0 2.0 3.0] [2.0 4.0 5.0]) => (throws)
             (toeplitz-matrix [1.0 2.0] [1.0 4.0 5.0]) => (throws)
             (toeplitz-matrix [1.0 2.0 3.0] [1.0 4.0]) => (throws)
             (toeplitz-matrix :clatrix [1.0 2.0 3.0] [1.0 4.0 5.0])
             => (clatrix [[1.0 2.0 3.0] [4.0 1.0 2.0] [5.0 4.0 1.0]])
             (toeplitz-matrix :apache-commons [1.0 2.0 3.0] [1.0 4.0 5.0])
             => (apache-commons [[1.0 2.0 3.0] [4.0 1.0 2.0] [5.0 4.0 1.0]]))
       (fact "outer product"
             (outer-product 3) => 9
             (outer-product [1.0 2.0]) => [[1.0 2.0] [2.0 4.0]]
             (outer-product :clatrix [1.0 2.0])
             => (clatrix [[1.0 2.0] [2.0 4.0]])
             (outer-product :apache-commons [2.0 4.0 5.0])
             => (apache-commons [[4.0 8.0 10.0] [8.0 16.0 20.0]
                                 [10.0 20.0 25.0]])
             (outer-product nil cl1D) => [[1.0 0.5] [0.5 0.25]]
             (outer-product nil cl-row) => (throws)
             (outer-product nil m/sq [2.0 3.0]) => [[16.0 36.0] [36.0 81.0]]
             (outer-product nil #(+ % %2) [2.0 3.0] ap)
             => [[5.0 6.5] [8.0 13.0]]
             (outer-product nil #(+ % %2 %3 %4) [2.0 3.0] cl ap ve)
             => [[7.0 7.5] [12.0 21.0]]
             (outer-product nil #(+ % %2 %3 %4 %5) [2.0 3.0] cl ap ve se)
             => [[8.0 8.0] [14.0 25.0]])
       (fact "sparse"
             (sparse->matrix [[0 0 3.0]] [[4.0 5.0] [6.0 7.0]])
             => [[3.0 5.0] [6.0 7.0]]
             (sparse->matrix :clatrix [[0 0 3.0]] [[4.0 5.0] [6.0 7.0]])
             => (clatrix [[3.0 5.0] [6.0 7.0]])
             (sparse->matrix [[0 0 3.0]] [2 2]) => [[3.0 0.0] [0.0 0.0]]
             (sparse->matrix [[0 0 3.0]] [2 2]) => [[3.0 0.0] [0.0 0.0]]
             (sparse->matrix [[0 0 3.0] [1 0 2.0] [0 1 5.0] [1 1 -1.0]] [2 2])
             => [[3.0 5.0] [2.0 -1.0]]
             (sparse->matrix [[0 2 3.0]] [2 2]) => (throws)
             (sparse->matrix
               :clatrix [[0 0 3.0] [1 0 2.0] [0 1 5.0] [1 1 -1.0]] [2 2])
             => (clatrix [[3.0 5.0] [2.0 -1.0]])
             (sparse->matrix
               :apache-commons [[0 0 3.0] [1 0 2.0] [0 1 5.0] [1 1 -1.0]]
               [2 2]) => (apache-commons [[3.0 5.0] [2.0 -1.0]])
             (sparse->symmetric-matrix
               :clatrix [[0 0 3.0] [1 0 2.0] [0 1 5.0] [1 1 -1.0]] 2)
             => (clatrix [[3.0 5.0] [5.0 -1.0]])
             (sparse->symmetric-matrix
               :clatrix [[0 0 3.0] [1 0 2.0]] [[1.0 2.0] [3.0 4.0]])
             => (clatrix [[3.0 2.0] [2.0 4.0]]))
       (fact "rnd-matrix"
             (first (rnd-matrix :clatrix 2 2 test-rnd-lazy))
             => (clatrix [[0.8335762378570932 0.11249249636232017]
                          [0.8502406979201282 0.7495670044667]])
             (first (rnd-matrix 2 3 test-rnd-lazy))
             => [[0.8335762378570932 0.11249249636232017 0.8502406979201282]
                 [0.7495670044667 0.4764463905206542 0.9509334914632213]])
       (fact "reflection"
             (first (rnd-reflection-matrix 2 test-rnd-lazy))
             => [[-0.9642275848105564 -0.265075771602011]
                 [-0.265075771602011 0.9642275848105563]]
             (first (rnd-reflection-matrix :apache-commons 2 test-rnd-lazy))
             => (apache-commons [[-0.9642275848105564 -0.265075771602011]
                                 [-0.265075771602011 0.9642275848105563]]))
       (fact "spectral"
             (first (rnd-spectral-matrix [1.0 3.0] test-rnd-lazy))
             => [[1.296772920438264 0.710965311791703]
                 [0.710965311791703 2.703227079561737]]
             (first (rnd-spectral-matrix :apache-commons [1.0 3.0]
                                         test-rnd-lazy))
             => (apache-commons [[1.296772920438264 0.710965311791703]
                                 [0.710965311791703 2.703227079561737]]))
       (fact "positive"
             (first (rnd-positive-matrix 2 test-rnd-lazy))
             => [[0.11268006837251504 0.011628411054483414]
                 [0.011628411054483414 0.833388665846898]]
             (first (rnd-positive-matrix :clatrix 2 test-rnd-lazy))
             => (clatrix [[0.11268006837251504 0.011628411054483414]
                          [0.011628411054483414 0.833388665846898]])))





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

#_(ost/unstrument)