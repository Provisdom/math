(ns provisdom.math.matrix-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.matrix :as mx]
    [provisdom.math.core :as m]
    [provisdom.math.random :as random]
    [provisdom.math.tensor :as tensor]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;;189 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

;;;TYPES
(deftest matrix?-test
  (is (spec-check mx/matrix?))
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
  (is (mx/matrix? [[m/inf+]]))
  (is (mx/matrix? [[0.0] [0.0]])))

(deftest matrix-num?-test
  (is (spec-check mx/matrix-num?))
  (is-not (mx/matrix-num? [0]))
  (is-not (mx/matrix-num? [[] []]))
  (is (mx/matrix-num? [[]]))
  (is-not (mx/matrix-num? 0))
  (is-not (mx/matrix-num? [[[0.0] [0.0]]]))
  (is-not (mx/matrix-num? [[nil]]))
  (is-not (mx/matrix-num? [["A"]]))
  (is-not (mx/matrix-num? '((0))))
  (is (mx/matrix-num? [[0]]))
  (is-not (mx/matrix-num? [[m/nan]]))
  (is (mx/matrix-num? [[m/inf+]]))
  (is (mx/matrix-num? [[0.0] [0.0]])))

(deftest matrix-finite?-test
  (is (spec-check mx/matrix-finite?))
  (is-not (mx/matrix-finite? [0]))
  (is-not (mx/matrix-finite? [[] []]))
  (is (mx/matrix-finite? [[]]))
  (is-not (mx/matrix-finite? 0))
  (is-not (mx/matrix-finite? [[[0.0] [0.0]]]))
  (is-not (mx/matrix-finite? [[nil]]))
  (is-not (mx/matrix-finite? [["A"]]))
  (is-not (mx/matrix-finite? '((0))))
  (is (mx/matrix-finite? [[0]]))
  (is-not (mx/matrix-finite? [[m/nan]]))
  (is-not (mx/matrix-finite? [[m/inf+]]))
  (is (mx/matrix-finite? [[0.0] [0.0]])))

(deftest matrix-finite-non-?-test
  (is (spec-check mx/matrix-finite-non-?))
  (is-not (mx/matrix-finite-non-? [0]))
  (is-not (mx/matrix-finite-non-? [[] []]))
  (is (mx/matrix-finite-non-? [[]]))
  (is-not (mx/matrix-finite-non-? 0))
  (is-not (mx/matrix-finite-non-? [[[0.0] [0.0]]]))
  (is-not (mx/matrix-finite-non-? [[nil]]))
  (is-not (mx/matrix-finite-non-? [["A"]]))
  (is-not (mx/matrix-finite-non-? '((0))))
  (is (mx/matrix-finite-non-? [[0]]))
  (is-not (mx/matrix-finite-non-? [[m/nan]]))
  (is-not (mx/matrix-finite-non-? [[m/inf+]]))
  (is (mx/matrix-finite-non-? [[0.0] [0.0]])))

(deftest matrix-prob?-test
  (is (spec-check mx/matrix-prob?))
  (is-not (mx/matrix-prob? [0]))
  (is-not (mx/matrix-prob? [[] []]))
  (is (mx/matrix-prob? [[]]))
  (is-not (mx/matrix-prob? 0))
  (is-not (mx/matrix-prob? [[[0.0] [0.0]]]))
  (is-not (mx/matrix-prob? [[nil]]))
  (is-not (mx/matrix-prob? [["A"]]))
  (is-not (mx/matrix-prob? '((0))))
  (is (mx/matrix-prob? [[0]]))
  (is-not (mx/matrix-prob? [[m/nan]]))
  (is-not (mx/matrix-prob? [[m/inf+]]))
  (is (mx/matrix-prob? [[0.0] [0.0]])))

(deftest empty-matrix?-test
  (is (spec-check mx/empty-matrix?))
  (is-not (mx/empty-matrix? []))
  (is (mx/empty-matrix? [[]]))
  (is-not (mx/empty-matrix? [[1]]))
  (is-not (mx/empty-matrix? [[] [2]]))
  (is-not (mx/empty-matrix? [[m/nan]]))
  (is-not (mx/empty-matrix? [[false]])))

(deftest row-matrix?-test
  (is (spec-check mx/row-matrix?))
  (is-not (mx/row-matrix? [0]))
  (is-not (mx/row-matrix? [[0.0] [0.0]]))
  (is-not (mx/row-matrix? [[nil]]))
  (is-not (mx/row-matrix? '((0))))
  (is (mx/row-matrix? [[0]]))
  (is (mx/row-matrix? [[0.0 0.0]]))
  (is (mx/row-matrix? [[m/nan]])))

(deftest column-matrix?-test
  (is (spec-check mx/column-matrix?))
  (is-not (mx/column-matrix? [0]))
  (is-not (mx/column-matrix? [[0.0 0.0]]))
  (is-not (mx/column-matrix? [[nil]]))
  (is-not (mx/column-matrix? '((0))))
  (is (mx/column-matrix? [[0]]))
  (is (mx/column-matrix? [[0.0] [0.0]]))
  (is (mx/column-matrix? [[m/nan]])))

(deftest zero-matrix?-test
  (is (spec-check mx/zero-matrix?))
  (is-not (mx/zero-matrix? [0]))
  (is (mx/zero-matrix? [[0]]))
  (is (mx/zero-matrix? [[0.0]]))
  (is (mx/zero-matrix? [[0.0] [0.0]]))
  (is-not (mx/zero-matrix? [[0.0] [0.1]])))

(deftest square-matrix?-test
  (is (spec-check mx/square-matrix?))
  (is-not (mx/square-matrix? []))
  (is (mx/square-matrix? [[]]))
  (is (mx/square-matrix? [[1]]))
  (is-not (mx/square-matrix? [[] [2]]))
  (is (mx/square-matrix? [[m/nan]]))
  (is-not (mx/square-matrix? [[1 2]])))

(deftest diagonal-matrix?-test
  (is (spec-check mx/diagonal-matrix?))
  (is-not (mx/diagonal-matrix? [[1.0 0.5] [2.0 4.0]]))
  (is (mx/diagonal-matrix? [[1.0 0.0] [0.0 2.0]])))

(deftest upper-triangular-matrix?-test
  (is (spec-check mx/upper-triangular-matrix?))
  (is (mx/upper-triangular-matrix? [[]]))
  (is (mx/upper-triangular-matrix? [[1]]))
  (is-not (mx/upper-triangular-matrix? [[1 1]]))
  (is-not (mx/upper-triangular-matrix? [[1] [1]]))
  (is-not (mx/upper-triangular-matrix? [[1 1] [1 1]]))
  (is (mx/upper-triangular-matrix? [[1 0] [0 1]]))
  (is (mx/upper-triangular-matrix? [[1 1] [0 1]])))

(deftest lower-triangular-matrix?-test
  (is (spec-check mx/lower-triangular-matrix?))
  (is (mx/lower-triangular-matrix? [[]]))
  (is (mx/lower-triangular-matrix? [[1]]))
  (is-not (mx/lower-triangular-matrix? [[1 1]]))
  (is-not (mx/lower-triangular-matrix? [[1] [1]]))
  (is-not (mx/lower-triangular-matrix? [[1 1] [1 1]]))
  (is (mx/lower-triangular-matrix? [[1 0] [0 1]]))
  (is (mx/lower-triangular-matrix? [[1 0] [1 1]])))

(deftest symmetric-matrix?-test
  (is (spec-check mx/symmetric-matrix?))
  (is-not (mx/symmetric-matrix? [[1.0 0.5] [2.0 4.0]]))
  (is (mx/symmetric-matrix? [[1.0 0.5] [0.5 2.0]])))

;;;CONSTRUCTORS
(deftest to-matrix-test
  (is (spec-check mx/to-matrix))
  (is= [[]] (mx/to-matrix [] 1))
  (is= [[]] (mx/to-matrix [1.0 0.5] 0))
  (is= [[1.0 0.5]] (mx/to-matrix [1.0 0.5] 1))
  (is= [[1.0] [0.5]] (mx/to-matrix [1.0 0.5] 2))
  (is= [[1.0] [0.5] [0.0]] (mx/to-matrix [1.0 0.5] 3))
  (is= [[1.0 3.0 5.0] [2.0 4.0 6.0]]
       (mx/to-matrix [1.0 2.0 3.0 4.0 5.0 6.0] 2 {::mx/by-row? false}))
  (is= [[1.0 2.0 3.0] [4.0 5.0 6.0]]
       (mx/to-matrix [1.0 2.0 3.0 4.0 5.0 6.0] 2))
  (is= [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 0.0]]
       (mx/to-matrix [1.0 2.0 3.0 4.0 5.0 6.0 7.0] 2)))

(deftest constant-matrix-test
  (is (spec-check mx/constant-matrix))
  (is= [[]] (mx/constant-matrix 1 0 0.0))
  (is= [[]] (mx/constant-matrix 0 1 0.0))
  (is= [[1.0 1.0] [1.0 1.0]] (mx/constant-matrix 2 2 1.0)))

(deftest compute-matrix-test
  (is (spec-check mx/compute-matrix))
  (is= [[]]
       (mx/compute-matrix 1 0 (fn [r c]
                                (+ r c))))
  (is= [[]]
       (mx/compute-matrix 0 1 (fn [r c]
                                (+ r c))))
  (is= [[0 1] [1 2]]
       (mx/compute-matrix 2 2 (fn [r c]
                                (+ r c)))))

(deftest identity-matrix-test
  (is (spec-check mx/identity-matrix))
  (is= [[]] (mx/identity-matrix 0))
  (is= [[1.0]] (mx/identity-matrix 1))
  (is= [[1.0 0.0] [0.0 1.0]] (mx/identity-matrix 2)))

(deftest row-matrix-test
  (is (spec-check mx/row-matrix))
  (is= [[]] (mx/row-matrix []))
  (is= [[1.0 0.5]] (mx/row-matrix [1.0 0.5]))
  (is= [[]] (mx/row-matrix 0 double))
  (is= [[0.0 1.0]] (mx/row-matrix 2 double))
  (is= [[3.0 3.0]] (mx/row-matrix 2 (constantly 3.0))))

(deftest column-matrix-test
  (is (spec-check mx/column-matrix))
  (is= [[]] (mx/column-matrix []))
  (is= [[1.0] [0.5]] (mx/column-matrix [1.0 0.5]))
  (is= [[]] (mx/column-matrix 0 double))
  (is= [[0.0] [1.0]] (mx/column-matrix 2 double))
  (is= [[3.0] [3.0]] (mx/column-matrix 2 (constantly 3.0))))

(deftest diagonal-matrix-test
  (is (spec-check mx/diagonal-matrix))
  (is= [[]] (mx/diagonal-matrix []))
  (is= [[1.0 0.0] [0.0 0.5]] (mx/diagonal-matrix [1.0 0.5]))
  (is= [[]] (mx/diagonal-matrix 0 double))
  (is= [[0.0 0.0] [0.0 1.0]] (mx/diagonal-matrix 2 double))
  (is= [[1.0 0.0] [0.0 3.0]] (mx/diagonal-matrix [1.0 3.0]))
  (is= [[3.0 0.0] [0.0 3.0]] (mx/diagonal-matrix 2 (constantly 3.0))))

(deftest deserialize-upper-triangular-matrix-test
  (is (spec-check mx/deserialize-upper-triangular-matrix))
  (is= [[]] (mx/deserialize-upper-triangular-matrix []))
  (is= nil (mx/deserialize-upper-triangular-matrix [1.0 2.0]))
  (is= [[1.0 2.0] [0.0 3.0]]
       (mx/deserialize-upper-triangular-matrix [1.0 2.0 3.0]))
  (is= [[7.0 1.0 2.0 4.0] [0.0 8.0 3.0 5.0] [0.0 0.0 9.0 6.0] [0.0 0.0 0.0 10.0]]
       (mx/deserialize-upper-triangular-matrix
         [7.0 8.0 9.0 10.0] [1.0 2.0 3.0 4.0 5.0 6.0]
         {::mx/by-row? false}))
  (is= [[7.0 1.0 2.0 3.0] [0.0 8.0 4.0 5.0] [0.0 0.0 9.0 6.0] [0.0 0.0 0.0 10.0]]
       (mx/deserialize-upper-triangular-matrix
         [7.0 8.0 9.0 10.0] [1.0 2.0 3.0 4.0 5.0 6.0]
         {::mx/by-row? true}))
  (is= [[1.0 2.0 4.0] [0.0 3.0 5.0] [0.0 0.0 6.0]]
       (mx/deserialize-upper-triangular-matrix
         [1.0 2.0 3.0 4.0 5.0 6.0]
         {::mx/by-row? false}))
  (is= [[1.0 2.0 3.0] [0.0 4.0 5.0] [0.0 0.0 6.0]]
       (mx/deserialize-upper-triangular-matrix [1.0 2.0 3.0 4.0 5.0 6.0])))

(deftest deserialize-lower-triangular-matrix-test
  (is (spec-check mx/deserialize-lower-triangular-matrix))
  (is= [[]] (mx/deserialize-lower-triangular-matrix []))
  (is= nil (mx/deserialize-lower-triangular-matrix [1.0 2.0]))
  (is= [[1.0 0.0] [2.0 3.0]]
       (mx/deserialize-lower-triangular-matrix [1.0 2.0 3.0]))
  (is= [[7.0 0.0 0.0 0.0] [1.0 8.0 0.0 0.0] [2.0 4.0 9.0 0.0] [3.0 5.0 6.0 10.0]]
       (mx/deserialize-lower-triangular-matrix
         [7.0 8.0 9.0 10.0] [1.0 2.0 3.0 4.0 5.0 6.0]
         {::mx/by-row? false}))
  (is= [[7.0 0.0 0.0 0.0] [1.0 8.0 0.0 0.0] [2.0 3.0 9.0 0.0] [4.0 5.0 6.0 10.0]]
       (mx/deserialize-lower-triangular-matrix
         [7.0 8.0 9.0 10.0] [1.0 2.0 3.0 4.0 5.0 6.0]
         {::mx/by-row? true}))
  (is= [[1.0 0.0 0.0] [2.0 4.0 0.0] [3.0 5.0 6.0]]
       (mx/deserialize-lower-triangular-matrix
         [1.0 2.0 3.0 4.0 5.0 6.0]
         {::mx/by-row? false}))
  (is= [[1.0 0.0 0.0] [2.0 3.0 0.0] [4.0 5.0 6.0]]
       (mx/deserialize-lower-triangular-matrix [1.0 2.0 3.0 4.0 5.0 6.0])))

(deftest deserialize-symmetric-matrix-test
  (is (spec-check mx/deserialize-symmetric-matrix))
  (is= [[]] (mx/deserialize-symmetric-matrix []))
  (is= nil (mx/deserialize-symmetric-matrix [1.0 2.0]))
  (is= [[1.0 2.0] [2.0 3.0]] (mx/deserialize-symmetric-matrix [1.0 2.0 3.0])))

;;also called [[diagonal-constant-matrix]]
(deftest toeplitz-matrix-test
  (is (spec-check mx/toeplitz-matrix))
  (is= [[]] (mx/diagonal-constant-matrix [] []))
  (is= [[1]] (mx/diagonal-constant-matrix [1] [1]))
  (is= [[1.0 2.0 3.0] [4.0 1.0 2.0] [5.0 4.0 1.0]]
       (mx/toeplitz-matrix [1.0 2.0 3.0] [1.0 4.0 5.0]))
  (is= [[1.0 2.0] [4.0 1.0] [5.0 4.0]]
       (mx/toeplitz-matrix [1.0 2.0] [1.0 4.0 5.0]))
  (is= [[1.0 2.0 3.0] [4.0 1.0 2.0]]
       (mx/toeplitz-matrix [1.0 2.0 3.0] [1.0 4.0])))

(deftest outer-product-test
  (is (spec-check mx/outer-product))
  (is= [[]] (mx/outer-product []))
  (is= [[9.0]] (mx/outer-product [3]))
  (is= [[1.0 2.0] [2.0 4.0]] (mx/outer-product [1.0 2.0]))
  (is= [[4.0 8.0 10.0] [8.0 16.0 20.0] [10.0 20.0 25.0]]
       (mx/outer-product [2.0 4.0 5.0]))
  (is= [[1.0 0.5] [0.5 0.25]] (mx/outer-product [1.0 0.5]))
  (is= [[4.0 6.0] [6.0 9.0]] (mx/outer-product [2.0 3.0])))

(deftest rnd-matrix!-test
  (is (spec-check mx/rnd-matrix!))
  (random/bind-seed 0
                    (is= [[]] (mx/rnd-matrix! 0 0)))
  (random/bind-seed 0
                    (is= [[0.8833108082136426 0.026433771592597743]
                          [0.10634669156721244 0.17386786595968284]]
                         (mx/rnd-matrix! 2 2)))
  (random/bind-seed 0
                    (is= [[0.8833108082136426 0.026433771592597743 0.10634669156721244]
                          [0.17386786595968284 0.24568894884013137 0.39646797562881353]]
                         (mx/rnd-matrix! 2 3))))

(deftest rnd-reflection-matrix!-test
  (is (spec-check mx/rnd-reflection-matrix!))
  (random/bind-seed 0
                    (is= [[]] (mx/rnd-reflection-matrix! 0)))
  (random/bind-seed 0
                    (is= [[-1.0]] (mx/rnd-reflection-matrix! 1)))
  (random/bind-seed 0
                    (is= [[-0.9982104970725829 -0.059798022827738856]
                          [-0.059798022827738856 0.998210497072583]]
                         (mx/rnd-reflection-matrix! 2))))

(deftest rnd-spectral-matrix!-test
  (is (spec-check mx/rnd-spectral-matrix!
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 300}}))
  (random/bind-seed 0
                    (is= [[]] (mx/rnd-spectral-matrix! [])))
  (random/bind-seed 0
                    (is= [[2.0]] (mx/rnd-spectral-matrix! [2.0])))
  (random/bind-seed 0
                    (is= [[1.7925482077721386 -0.9782452422074177]
                          [-0.9782452422074177 2.2074517922278627]]
                         (mx/rnd-spectral-matrix! [1.0 3.0]))))

(deftest sparse->matrix-test
  (is (spec-check mx/sparse->matrix))
  (is= [[4.0 5.0] [6.0 7.0]] (mx/sparse->matrix [] [[4.0 5.0] [6.0 7.0]]))
  (is= [[3.0 5.0] [6.0 7.0]]
       (mx/sparse->matrix [[0 0 3.0]] [[4.0 5.0] [6.0 7.0]]))
  (is= [[3.0 0.0] [0.0 0.0]]
       (mx/sparse->matrix [[0 0 3.0]] (mx/constant-matrix 2 2 0.0)))
  (is= [[3.0 5.0] [2.0 -1.0]]
       (mx/sparse->matrix [[0 0 3.0] [1 0 2.0] [0 1 5.0] [1 1 -1.0]]
                          (mx/constant-matrix 2 2 0.0)))
  (is= [[0.0 0.0] [0.0 0.0]]
       (mx/sparse->matrix [[0 2 3.0]] (mx/constant-matrix 2 2 0.0))))

(deftest sparse->symmetric-matrix-test
  (is (spec-check mx/sparse->symmetric-matrix))
  (is= [[4.0 5.0] [5.0 7.0]]
       (mx/sparse->symmetric-matrix [] [[4.0 5.0] [5.0 7.0]]))
  (is= [[3.0 2.0] [2.0 4.0]]
       (mx/sparse->symmetric-matrix [[0 0 3.0] [1 0 2.0]]
                                    [[1.0 2.0] [2.0 4.0]])))

;;;INFO
(deftest rows-test
  (is (spec-check mx/rows))
  (is= 0 (mx/rows [[]]))
  (is= 1 (mx/rows [[1.0]]))
  (is= 1 (mx/rows [[1.0 2.0]]))
  (is= 2 (mx/rows [[1.0] [2.0]]))
  (is= 2 (mx/rows [[1.0 0.5] [2.0 4.0]])))

(deftest columns-test
  (is (spec-check mx/columns))
  (is= 0 (mx/columns [[]]))
  (is= 1 (mx/columns [[1.0]]))
  (is= 2 (mx/columns [[1.0 2.0]]))
  (is= 1 (mx/columns [[1.0] [2.0]]))
  (is= 2 (mx/columns [[1.0 0.5] [2.0 4.0]])))

(deftest get-row-test
  (is (spec-check mx/get-row))
  (is= [1.0] (mx/get-row [[1.0]] 0))
  (is= [2.0] (mx/get-row [[1.0] [2.0]] 1))
  (is= [1.0 0.5] (mx/get-row [[1.0 0.5] [2.0 4.0]] 0)))

(deftest get-column-test
  (is (spec-check mx/get-column))
  (is= [1.0] (mx/get-column [[1.0]] 0))
  (is= [2.0] (mx/get-column [[1.0 2.0]] 1))
  (is= [1.0 2.0] (mx/get-column [[1.0 0.5] [2.0 4.0]] 0)))

(deftest diagonal-test
  (is (spec-check mx/diagonal))
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
  (is (spec-check mx/serialize-symmetric-or-triangular-matrix))
  (is= [] (mx/serialize-symmetric-or-triangular-matrix [[]]))
  (is= [3] (mx/serialize-symmetric-or-triangular-matrix [[3]]))
  (is= [1.0 2.0 4.0]
       (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5] [2.0 4.0]]
                                                    {::mx/by-row? false}))
  (is= [1.0 0.5 4.0]
       (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5] [2.0 4.0]]))
  (is= [1.0 0.5 1.0]
       (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5] [0.5 1.0]]
                                                    {::mx/by-row? false}))
  (is= [1.0 0.5 1.0]
       (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5] [0.5 1.0]]))
  (is= [1.0]
       (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5]]
                                                    {::mx/by-row? false}))
  (is= [1.0 0.5] (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5]]))
  (is= [1.0 0.5]
       (mx/serialize-symmetric-or-triangular-matrix [[1.0] [0.5]]
                                                    {::mx/by-row? false}))
  (is= [1.0] (mx/serialize-symmetric-or-triangular-matrix [[1.0] [0.5]])))

(deftest size-of-symmetric-or-triangular-matrix-test
  (is (spec-check mx/size-of-symmetric-or-triangular-matrix))
  (is= 1 (mx/size-of-symmetric-or-triangular-matrix 1))
  (is (m/nan? (mx/size-of-symmetric-or-triangular-matrix 2)))
  (is= 2 (mx/size-of-symmetric-or-triangular-matrix 3))
  (is= 3 (mx/size-of-symmetric-or-triangular-matrix 6)))

(deftest size-of-symmetric-or-triangular-matrix-without-diagonal-test
  (is (spec-check mx/size-of-symmetric-or-triangular-matrix-without-diagonal))
  (is= 2 (mx/size-of-symmetric-or-triangular-matrix-without-diagonal 1))
  (is (m/nan? (mx/size-of-symmetric-or-triangular-matrix-without-diagonal 2)))
  (is= 3 (mx/size-of-symmetric-or-triangular-matrix-without-diagonal 3))
  (is= 4 (mx/size-of-symmetric-or-triangular-matrix-without-diagonal 6)))

(deftest ecount-of-symmetric-or-triangular-matrix-test
  (is (spec-check mx/ecount-of-symmetric-or-triangular-matrix))
  (is= 1 (mx/ecount-of-symmetric-or-triangular-matrix 1))
  (is= 3 (mx/ecount-of-symmetric-or-triangular-matrix 2))
  (is= 6 (mx/ecount-of-symmetric-or-triangular-matrix 3))
  (is= 21 (mx/ecount-of-symmetric-or-triangular-matrix 6)))

(deftest ecount-of-symmetric-or-triangular-matrix-without-diagonal-test
  (is (spec-check mx/ecount-of-symmetric-or-triangular-matrix-without-diagonal))
  (is= 0 (mx/ecount-of-symmetric-or-triangular-matrix-without-diagonal 1))
  (is= 1 (mx/ecount-of-symmetric-or-triangular-matrix-without-diagonal 2))
  (is= 3 (mx/ecount-of-symmetric-or-triangular-matrix-without-diagonal 3))
  (is= 15 (mx/ecount-of-symmetric-or-triangular-matrix-without-diagonal 6)))

(deftest trace-test
  (is (spec-check mx/trace))
  (is= 0.0 (mx/trace [[]]))
  (is= 1.0 (mx/trace [[1]]))
  (is= 5.0 (mx/trace [[1.0 0.5] [2.0 4.0]])))

(deftest get-slices-as-matrix-test
  (is (spec-check mx/get-slices-as-matrix))
  (is= [[]] (mx/get-slices-as-matrix [[]] {::mx/row-indices 0}))
  (is= [[1.0 0.5]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices 0}))
  (is= [[1.0 0.5] [2.0 4.0]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices [0 1]}))
  (is= [[2.0 4.0] [1.0 0.5]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices [1 0]}))
  (is= [[1.0] [2.0]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/column-indices 0}))
  (is= [[1.0 0.5] [2.0 4.0]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]]
                                {::mx/column-indices [0 1]}))
  (is= [[0.5 1.0] [4.0 2.0]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]]
                                {::mx/column-indices [1 0]}))
  (is= [[2.0 4.0]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]]
                                {::mx/exception-row-indices 0}))
  (is= [[]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]]
                                {::mx/exception-row-indices [0 1]}))
  (is= [[0.5] [4.0]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]]
                                {::mx/exception-column-indices 0}))
  (is= [[]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]]
                                {::mx/exception-column-indices [0 1]}))
  (is= [[]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]]
                                {::mx/row-indices           0
                                 ::mx/exception-row-indices 0}))
  (is= [[]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]]
                                {::mx/row-indices           [0]
                                 ::mx/exception-row-indices 0}))
  (is= [[]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]]
                                {::mx/row-indices           0
                                 ::mx/exception-row-indices [0]}))
  (is= [[1.0]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]]
                                {::mx/exception-row-indices    1
                                 ::mx/exception-column-indices 1}))
  (is= [[1.0]]
       (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]]
                                {::mx/row-indices    0
                                 ::mx/column-indices 0})))

(deftest filter-by-row-test
  (is (spec-check mx/filter-by-row))
  (is= [[]]
       (mx/filter-by-row (fn [row-v]
                           (< (apply + (tensor/emap m/sq row-v)) 2.0))
                         [[]]))
  (is= [[1.0 0.5]]
       (mx/filter-by-row (fn [row-v]
                           (< (apply + (tensor/emap m/sq row-v)) 2.0))
                         [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0 0.5]]
       (mx/filter-by-row (fn [row-v]
                           (< (apply + (tensor/emap m/sq row-v)) 2.0))
                         [[1.0 0.5]]))
  (is= [[1.0] [0.5]]
       (mx/filter-by-row (fn [row-v]
                           (< (apply + (tensor/emap m/sq row-v)) 2.0))
                         [[1.0] [0.5]]))
  (is= [[1.0 0.5]]
       (mx/filter-by-row (fn [row-v]
                           (< (apply + (tensor/emap m/sq row-v)) 2.0))
                         [[1.0 0.5] [2.0 4.0]])))

(deftest filter-by-column-test
  (is (spec-check mx/filter-by-column))
  (is= [[]]
       (mx/filter-by-column (fn [column-v]
                              (< (apply + (tensor/emap m/sq column-v)) 6.0))
                            [[]]))
  (is= [[1.0] [2.0]]
       (mx/filter-by-column (fn [column-v]
                              (< (apply + (tensor/emap m/sq column-v)) 6.0))
                            [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0 0.5]]
       (mx/filter-by-column (fn [column-v]
                              (< (apply + (tensor/emap m/sq column-v)) 6.0))
                            [[1.0 0.5]]))
  (is= [[1.0] [0.5]]
       (mx/filter-by-column (fn [column-v]
                              (< (apply + (tensor/emap m/sq column-v)) 6.0))
                            [[1.0] [0.5]]))
  (is= [[1.0] [2.0]]
       (mx/filter-by-column (fn [column-v]
                              (< (apply + (tensor/emap m/sq column-v)) 6.0))
                            [[1.0 0.5] [2.0 4.0]])))

(deftest filter-symmetric-matrix-test
  (is (spec-check mx/filter-symmetric-matrix))
  (is= [[]]
       (mx/filter-symmetric-matrix (fn [v]
                                     (< (apply + (tensor/emap m/sq v)) 2.0))
                                   [[]]))
  (is= [[1.0]]
       (mx/filter-symmetric-matrix (fn [v]
                                     (< (apply + (tensor/emap m/sq v)) 2.0))
                                   [[1.0 0.5] [0.5 4.0]])))

(def s
  [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
   [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]])

(deftest matrix-partition-test
  (is (spec-check mx/matrix-partition))
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
  (is= {::mx/bottom-left  [[1.0 2.0 3.0] [5.0 6.0 7.0]
                           [9.0 10.0 11.0] [13.0 14.0 15.0]]
        ::mx/bottom-right [[4.0] [8.0] [12.0] [16.0]]
        ::mx/top-left     [[]]
        ::mx/top-right    [[]]}
       (mx/matrix-partition s 0 3))
  (is= {::mx/bottom-left  [[]]
        ::mx/bottom-right [[13.0 14.0 15.0 16.0]]
        ::mx/top-left     [[]]
        ::mx/top-right    [[1.0 2.0 3.0 4.0]
                           [5.0 6.0 7.0 8.0]
                           [9.0 10.0 11.0 12.0]]}
       (mx/matrix-partition s 3 0))
  (is= {::mx/bottom-left  [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                           [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]]
        ::mx/bottom-right [[]]
        ::mx/top-left     [[]]
        ::mx/top-right    [[]]}
       (mx/matrix-partition s 0 4))
  (is= {::mx/bottom-left  [[]]
        ::mx/bottom-right [[]]
        ::mx/top-left     [[]]
        ::mx/top-right    [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                           [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]]}
       (mx/matrix-partition s 4 0))
  (is= {::mx/bottom-left  [[]]
        ::mx/bottom-right [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                           [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]]
        ::mx/top-left     [[]]
        ::mx/top-right    [[]]}
       (mx/matrix-partition s 0 0))
  (is= {::mx/bottom-left  [[]]
        ::mx/bottom-right [[]]
        ::mx/top-left     [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                           [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]]
        ::mx/top-right    [[]]}
       (mx/matrix-partition s 4 4)))

(deftest some-kv-test
  (is (spec-check mx/some-kv))
  (is= 0.5
       (mx/some-kv (fn [row column number]
                     (> (+ row column) number))
                   [[1.0 0.5] [2.0 4.0]]))
  (is= 0.5
       (mx/some-kv (fn [row column number]
                     (> (+ row column) number))
                   [[1.0 0.5] [2.0 4.0]]
                   {::mx/by-row false})))

(deftest ereduce-kv-test
  (is (spec-check mx/ereduce-kv {:coll-check-limit 10
                                 :coll-error-limit 10
                                 :fspec-iterations 10
                                 :recursion-limit  1
                                 :test-check       {:num-tests 300}}))
  (is= 29.9
       (mx/ereduce-kv (fn [tot r c n1 n2 n3]
                        (when (number? tot)
                          (+ tot r c n1 n2 n3)))
                      3.4
                      [[1.0 0.5] [2.0 4.0]]
                      [[1.0 0.5] [2.0 4.0]]
                      [[1.0 0.5] [2.0 4.0]]))
  (is= 22.4
       (mx/ereduce-kv (fn [tot r c n1 n2]
                        (when (number? tot)
                          (+ tot r c n1 n2)))
                      3.4
                      [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
  (is= 14.9
       (mx/ereduce-kv (fn [tot r c n]
                        (when (number? tot)
                          (+ tot r c n)))
                      3.4
                      [[1.0 0.5] [2.0 4.0]]))
  (is= 5.9
       (mx/ereduce-kv (fn [tot r c n]
                        (when (number? tot)
                          (+ tot r c n)))
                      3.4
                      [[1.0 0.5]]))
  (is= 5.9
       (mx/ereduce-kv (fn [tot r c n]
                        (when (number? tot)
                          (+ tot r c n)))
                      3.4
                      [[1.0] [0.5]]))
  (is= 7.4
       (mx/ereduce-kv (fn [tot r c n1 n2]
                        (when (number? tot)
                          (+ tot r c n1 n2)))
                      3.4
                      [[1.0 0.5]] [[1.0 0.5]])))

(deftest matrix->sparse-test
  (is (spec-check mx/matrix->sparse))
  (is= [] (mx/matrix->sparse [[]]))
  (is= [[1 0 1.0]] (mx/matrix->sparse [[0.0 0.0] [1.0 0.0]]))
  (is= [[0 0 1.0] [0 1 0.5] [1 0 2.0] [1 1 4.0]]
       (mx/matrix->sparse [[1.0 0.5] [2.0 4.0]]))
  (is= [[0 0 1.0] [0 1 0.5] [1 0 2.0]]
       (mx/matrix->sparse [[1.0 0.5] [2.0 4.0]] #(< % 2.1)))
  (is= [] (mx/matrix->sparse [[1.0 0.5] [2.0 4.0]] neg?))
  (is= [[1 0 0.5]] (mx/matrix->sparse [[1.0] [0.5]] #(< % 0.7))))

(deftest symmetric-matrix->sparse-test
  (is (spec-check mx/symmetric-matrix->sparse
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 500}}))
  (is= [] (mx/symmetric-matrix->sparse [[]]))
  (is= [[0 1 1.0]] (mx/symmetric-matrix->sparse [[0.0 1.0] [1.0 0.0]]))
  (is= [[0 0 1.0] [0 1 0.5] [1 1 4.0]]
       (mx/symmetric-matrix->sparse [[1.0 0.5] [0.5 4.0]]))
  (is= [[0 0 1.0] [0 1 0.5]]
       (mx/symmetric-matrix->sparse [[1.0 0.5] [0.5 4.0]] #(< % 2.1))))

;;;MANIPULATION
(deftest transpose-test
  (is (spec-check mx/transpose))
  (is= [[]] (mx/transpose [[]]))
  (is= [[1]] (mx/transpose [[1]]))
  (is= [[1.0 2.0] [0.5 4.0]] (mx/transpose [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0 0.5] [2.0 4.0]]
       (mx/transpose (mx/transpose [[1.0 0.5] [2.0 4.0]])))
  (is= [[1.0 0.5]] (mx/transpose [[1.0] [0.5]]))
  (is= [[1.0] [0.5]] (mx/transpose [[1.0 0.5]])))

(deftest assoc-row-test
  (is (spec-check mx/assoc-row))
  (is= [[8.0 9.0]] (mx/assoc-row [[]] 0 [8.0 9.0]))
  (is= nil (mx/assoc-row [[1]] 0 [8.0 9.0]))
  (is= [[2]] (mx/assoc-row [[1]] 0 [2]))
  (is= [[1] [2]] (mx/assoc-row [[1]] 1 [2]))
  (is= nil (mx/assoc-row [[1]] 2 [2]))
  (is= [[8.0 9.0] [2.0 4.0]] (mx/assoc-row [[1.0 0.5] [2.0 4.0]] 0 [8.0 9.0])))

(deftest assoc-column-test
  (is (spec-check mx/assoc-column))
  (is= [[8.0] [9.0]] (mx/assoc-column [[]] 0 [8.0 9.0]))
  (is= nil (mx/assoc-column [[1]] 0 [8.0 9.0]))
  (is= [[2]] (mx/assoc-column [[1]] 0 [2]))
  (is= [[1 2]] (mx/assoc-column [[1]] 1 [2]))
  (is= nil (mx/assoc-column [[1]] 2 [2]))
  (is= [[8.0 0.5] [9.0 4.0]]
       (mx/assoc-column [[1.0 0.5] [2.0 4.0]] 0 [8.0 9.0])))

(deftest assoc-diagonal-test
  (is (spec-check mx/assoc-diagonal))
  (is= [[8.0 0.0] [0.0 9.0]] (mx/assoc-diagonal [[]] [8.0 9.0]))
  (is= nil (mx/assoc-diagonal [[1]] [8.0 9.0]))
  (is= [[2]] (mx/assoc-diagonal [[1]] [2]))
  (is= [[8.0 0.5] [2.0 9.0]]
       (mx/assoc-diagonal [[1.0 0.5] [2.0 4.0]] [8.0 9.0])))

(deftest insert-row-test
  (is (spec-check mx/insert-row))
  (is= [[8.0 9.0]] (mx/insert-row [[]] 0 [8.0 9.0]))
  (is= nil (mx/insert-row [[1]] 0 [8.0 9.0]))
  (is= [[2] [1]] (mx/insert-row [[1]] 0 [2]))
  (is= [[1] [2]] (mx/insert-row [[1]] 1 [2]))
  (is= nil (mx/insert-row [[1]] 2 [2]))
  (is= [[8.0 9.0] [1.0 0.5] [2.0 4.0]]
       (mx/insert-row [[1.0 0.5] [2.0 4.0]] 0 [8.0 9.0]))
  (is= [[1.0 0.5] [8.0 9.0] [2.0 4.0]]
       (mx/insert-row [[1.0 0.5] [2.0 4.0]] 1 [8.0 9.0])))

(deftest insert-column-test
  (is (spec-check mx/insert-column))
  (is= [[8.0] [9.0]] (mx/insert-column [[]] 0 [8.0 9.0]))
  (is= nil (mx/insert-column [[1]] 0 [8.0 9.0]))
  (is= [[2 1]] (mx/insert-column [[1]] 0 [2]))
  (is= [[1 2]] (mx/insert-column [[1]] 1 [2]))
  (is= nil (mx/insert-column [[1]] 2 [2]))
  (is= [[8.0 1.0 0.5] [9.0 2.0 4.0]]
       (mx/insert-column [[1.0 0.5] [2.0 4.0]] 0 [8.0 9.0])))

(deftest update-row-test
  (is (spec-check mx/update-row))
  (is= nil
       (mx/update-row [[]] 0 (fn [column number]
                               (+ column number 1))))
  (is= [[2]]
       (mx/update-row [[1]] 0 (fn [column number]
                                (+ column number 1))))
  (is= nil
       (mx/update-row [[1]] 1 (fn [column number]
                                (+ column number 1))))
  (is= [[2.0 2.5] [2.0 4.0]]
       (mx/update-row [[1.0 0.5] [2.0 4.0]] 0 (fn [column number]
                                                (+ column number 1)))))

(deftest update-column-test
  (is (spec-check mx/update-column))
  (is= nil
       (mx/update-column [[]] 0 (fn [row number]
                                  (+ row number 1))))
  (is= [[2]]
       (mx/update-column [[1]] 0 (fn [row number]
                                   (+ row number 1))))
  (is= nil
       (mx/update-column [[1]] 1 (fn [row number]
                                   (+ row number 1))))
  (is= [[2.0 0.5] [4.0 4.0]]
       (mx/update-column [[1.0 0.5] [2.0 4.0]] 0 (fn [row number]
                                                   (+ row number 1)))))

(deftest update-diagonal-test
  (is (spec-check mx/update-diagonal))
  (is= [[]]
       (mx/update-diagonal [[]] (fn [row number]
                                  (+ row number 1))))
  (is= [[2]]
       (mx/update-diagonal [[1]] (fn [row number]
                                   (+ row number 1))))
  (is= [[2.0 0.5] [2.0 6.0]]
       (mx/update-diagonal [[1.0 0.5] [2.0 4.0]] (fn [row number]
                                                   (+ row number 1)))))

(deftest concat-rows-test
  (is (spec-check mx/concat-rows))
  (is= [[]] (mx/concat-rows [[]] [[]]))
  (is= nil (mx/concat-rows [[]] [[1]]))
  (is= [[1.0 0.5] [2.0 4.0] [1.0 0.5] [2.0 4.0]]
       (mx/concat-rows [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0] [2.0]] (mx/concat-rows [[1.0]] [[2.0]]))
  (is= [[1.0 0.5] [1.0 0.5] [2.0 4.0]]
       (mx/concat-rows [[1.0 0.5]] [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0 0.5] [2.0 4.0] [1.0 0.5]]
       (mx/concat-rows [[1.0 0.5] [2.0 4.0]] [[1.0 0.5]]))
  (is= [[1.0 0.5] [1.0 0.5]] (mx/concat-rows [[1.0 0.5]] [[1.0 0.5]]))
  (is= [[1.0 0.5] [1.0 0.5] [1.0 0.5]]
       (mx/concat-rows [[1.0 0.5]] [[1.0 0.5]] [[1.0 0.5]])))

(deftest concat-columns-test
  (is (spec-check mx/concat-columns))
  (is= [[]] (mx/concat-columns [[]] [[]]))
  (is= nil (mx/concat-columns [[]] [[1]]))
  (is= [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0]]
       (mx/concat-columns [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0 2.0]] (mx/concat-columns [[1.0]] [[2.0]]))
  (is= [[1.0 1.0 0.5] [0.5 2.0 4.0]]
       (mx/concat-columns [[1.0] [0.5]] [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0 1.0] [0.5 0.5]] (mx/concat-columns [[1.0] [0.5]] [[1.0] [0.5]]))
  (is= [[1.0 1.0 1.0] [0.5 0.5 0.5]]
       (mx/concat-columns [[1.0] [0.5]] [[1.0] [0.5]] [[1.0] [0.5]])))

(deftest merge-matrices-test
  (is (spec-check mx/merge-matrices))
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
  (is (spec-check mx/replace-submatrix))
  (is= [[]] (mx/replace-submatrix [[]] [[]] 1 0))
  (is= [[0.0 0.0] [1.0 0.5] [2.0 4.0]]
       (mx/replace-submatrix [[]] [[1.0 0.5] [2.0 4.0]] 1 0))
  (is= [[1.0 0.5] [2.0 4.0]]
       (mx/replace-submatrix [[1.0 0.5] [2.0 4.0]] [[]] 1 0))
  (is= [[0.0 1.0 2.0] [1.0 0.5 5.0] [2.0 4.0 8.0]]
       (mx/replace-submatrix [[0.0 1.0 2.0] [3.0 4.0 5.0] [6.0 7.0 8.0]]
                             [[1.0 0.5] [2.0 4.0]]
                             1
                             0))
  (is= [[0.0 1.0 0.5] [3.0 2.0 4.0] [6.0 7.0 8.0]]
       (mx/replace-submatrix [[0.0 1.0 2.0] [3.0 4.0 5.0] [6.0 7.0 8.0]]
                             [[1.0 0.5] [2.0 4.0]]
                             0
                             1))
  (is= [[0.0 1.0 2.0] [3.0 4.0 5.0] [1.0 0.5 8.0] [2.0 4.0 0.0]]
       (mx/replace-submatrix [[0.0 1.0 2.0] [3.0 4.0 5.0] [6.0 7.0 8.0]]
                             [[1.0 0.5] [2.0 4.0]]
                             2
                             0))
  (is= [[0.0 0.0 0.0 1.0 0.5]
        [0.0 1.0 2.0 2.0 4.0]
        [3.0 4.0 5.0 0.0 0.0]
        [6.0 7.0 8.0 0.0 0.0]]
       (mx/replace-submatrix [[0.0 1.0 2.0] [3.0 4.0 5.0] [6.0 7.0 8.0]]
                             [[1.0 0.5] [2.0 4.0]]
                             -1
                             3)))

(deftest symmetric-matrix-by-averaging-test
  (is (spec-check mx/symmetric-matrix-by-averaging))
  (is= [[]] (mx/symmetric-matrix-by-averaging [[]]))
  (is= [[3]] (mx/symmetric-matrix-by-averaging [[3]]))
  (is= [[1.0 1.25] [1.25 4.0]]
       (mx/symmetric-matrix-by-averaging [[1.0 0.5] [2.0 4.0]])))

;;;MATH
(deftest mx*-test
  (is (spec-check mx/mx*))
  (is= [[]] (mx/mx* [[]]))
  (is= [[1]] (mx/mx* [[1]]))
  (is= [[]] (mx/mx* [[]] [[]]))
  (is= [[3.0] [3.0]] (mx/mx* [[1 1 1] [1 1 1]] [[1] [1] [1]]))
  (is= [[2.0 2.5] [10.0 17.0]]
       (mx/mx* [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
  (is= [[7.0 11.0] [44.0 73.0]]
       (mx/mx* [[1.0 0.5] [2.0 4.0]]
               [[1.0 0.5] [2.0 4.0]]
               [[1.0 0.5] [2.0 4.0]])))

(deftest kronecker-product-test
  (is (spec-check mx/kronecker-product {:coll-check-limit 10
                                        :coll-error-limit 10
                                        :fspec-iterations 10
                                        :recursion-limit  1
                                        :test-check       {:num-tests 25}}))
  (is= [[]] (mx/kronecker-product))
  (is= [[]] (mx/kronecker-product [[]]))
  (is= [[1]] (mx/kronecker-product [[1]]))
  (is= [[]] (mx/kronecker-product [[]] [[1]]))
  (is= [[1.0 0.5 0.5 0.25]
        [2.0 4.0 1.0 2.0]
        [2.0 1.0 4.0 2.0]
        [4.0 8.0 8.0 16.0]]
       (mx/kronecker-product [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0 0.5 0.5 0.25 0.5 0.25 0.25 0.125]
        [2.0 4.0 1.0 2.0 1.0 2.0 0.5 1.0]
        [2.0 1.0 4.0 2.0 1.0 0.5 2.0 1.0]
        [4.0 8.0 8.0 16.0 2.0 4.0 4.0 8.0]
        [2.0 1.0 1.0 0.5 4.0 2.0 2.0 1.0]
        [4.0 8.0 2.0 4.0 8.0 16.0 4.0 8.0]
        [4.0 2.0 8.0 4.0 8.0 4.0 16.0 8.0]
        [8.0 16.0 16.0 32.0 16.0 32.0 32.0 64.0]]
       (mx/kronecker-product [[1.0 0.5] [2.0 4.0]]
                             [[1.0 0.5] [2.0 4.0]]
                             [[1.0 0.5] [2.0 4.0]]))
  (is= [[1.0 0.5 0.5 0.25]] (mx/kronecker-product [[1.0 0.5]] [[1.0 0.5]]))
  (is= [[1.0 0.5 0.5 0.25 0.5 0.25 0.25 0.125]]
       (mx/kronecker-product [[1.0 0.5]] [[1.0 0.5]] [[1.0 0.5]])))

;;;ROUNDING
(deftest round-roughly-zero-rows-test
  (is (spec-check mx/round-roughly-zero-rows))
  (is= [[]] (mx/round-roughly-zero-rows [[]] 1e-6))
  (is= [[1.0 0.5] [2.0 4.0]]
       (mx/round-roughly-zero-rows [[1.0 0.5] [2.0 4.0]] 1e-6))
  (is= [[0.0 0.0] [1.0 1.0E-17]]
       (mx/round-roughly-zero-rows [[1e-13 1e-8] [1.0 1e-17]] 1e-6)))

(deftest round-roughly-zero-columns-test
  (is (spec-check mx/round-roughly-zero-columns))
  (is= [[]] (mx/round-roughly-zero-columns [[]] 1e-6))
  (is= [[1.0 0.5] [2.0 4.0]]
       (mx/round-roughly-zero-columns [[1.0 0.5] [2.0 4.0]] 1e-6))
  (is= [[1.0E-13 0.0] [1.0 0.0]]
       (mx/round-roughly-zero-columns [[1e-13 1e-8] [1.0 1e-17]] 1e-6)))

#_(ost/unstrument)