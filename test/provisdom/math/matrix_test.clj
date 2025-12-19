(ns provisdom.math.matrix-test
  (:require
    [clojure.test :refer :all]
    [provisdom.math.core :as m]
    [provisdom.math.matrix :as mx]
    [provisdom.math.random :as random]
    [provisdom.math.tensor :as tensor]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anom]))

;;64 seconds

(set! *warn-on-reflection* true)

;;;TYPES
(deftest matrix?-test
  (t/with-instrument `mx/matrix?
    (t/is-spec-check mx/matrix?))
  (t/with-instrument :all
    (t/is-not (mx/matrix? [0]))
    (t/is-not (mx/matrix? [[] []]))
    (t/is (mx/matrix? [[]]))
    (t/is-not (mx/matrix? 0))
    (t/is-not (mx/matrix? [[[0.0] [0.0]]]))
    (t/is-not (mx/matrix? [[nil]]))
    (t/is-not (mx/matrix? [["A"]]))
    (t/is-not (mx/matrix? '((0))))
    (t/is (mx/matrix? [[0]]))
    (t/is (mx/matrix? [[m/nan]]))
    (t/is (mx/matrix? [[m/inf+]]))
    (t/is (mx/matrix? [[0.0] [0.0]]))))

(deftest matrix-num?-test
  (t/with-instrument `mx/matrix-num?
    (t/is-spec-check mx/matrix-num?))
  (t/with-instrument :all
    (t/is-not (mx/matrix-num? [0]))
    (t/is-not (mx/matrix-num? [[] []]))
    (t/is (mx/matrix-num? [[]]))
    (t/is-not (mx/matrix-num? 0))
    (t/is-not (mx/matrix-num? [[[0.0] [0.0]]]))
    (t/is-not (mx/matrix-num? [[nil]]))
    (t/is-not (mx/matrix-num? [["A"]]))
    (t/is-not (mx/matrix-num? '((0))))
    (t/is (mx/matrix-num? [[0]]))
    (t/is-not (mx/matrix-num? [[m/nan]]))
    (t/is (mx/matrix-num? [[m/inf+]]))
    (t/is (mx/matrix-num? [[0.0] [0.0]]))))

(deftest matrix-finite?-test
  (t/with-instrument `mx/matrix-finite?
    (t/is-spec-check mx/matrix-finite?))
  (t/with-instrument :all
    (t/is-not (mx/matrix-finite? [0]))
    (t/is-not (mx/matrix-finite? [[] []]))
    (t/is (mx/matrix-finite? [[]]))
    (t/is-not (mx/matrix-finite? 0))
    (t/is-not (mx/matrix-finite? [[[0.0] [0.0]]]))
    (t/is-not (mx/matrix-finite? [[nil]]))
    (t/is-not (mx/matrix-finite? [["A"]]))
    (t/is-not (mx/matrix-finite? '((0))))
    (t/is (mx/matrix-finite? [[0]]))
    (t/is-not (mx/matrix-finite? [[m/nan]]))
    (t/is-not (mx/matrix-finite? [[m/inf+]]))
    (t/is (mx/matrix-finite? [[0.0] [0.0]]))))

(deftest matrix-finite-non-?-test
  (t/with-instrument `mx/matrix-finite-non-?
    (t/is-spec-check mx/matrix-finite-non-?))
  (t/with-instrument :all
    (t/is-not (mx/matrix-finite-non-? [0]))
    (t/is-not (mx/matrix-finite-non-? [[] []]))
    (t/is (mx/matrix-finite-non-? [[]]))
    (t/is-not (mx/matrix-finite-non-? 0))
    (t/is-not (mx/matrix-finite-non-? [[[0.0] [0.0]]]))
    (t/is-not (mx/matrix-finite-non-? [[nil]]))
    (t/is-not (mx/matrix-finite-non-? [["A"]]))
    (t/is-not (mx/matrix-finite-non-? '((0))))
    (t/is (mx/matrix-finite-non-? [[0]]))
    (t/is-not (mx/matrix-finite-non-? [[m/nan]]))
    (t/is-not (mx/matrix-finite-non-? [[m/inf+]]))
    (t/is (mx/matrix-finite-non-? [[0.0] [0.0]]))))

(deftest matrix-prob?-test
  (t/with-instrument `mx/matrix-prob?
    (t/is-spec-check mx/matrix-prob?))
  (t/with-instrument :all
    (t/is-not (mx/matrix-prob? [0]))
    (t/is-not (mx/matrix-prob? [[] []]))
    (t/is (mx/matrix-prob? [[]]))
    (t/is-not (mx/matrix-prob? 0))
    (t/is-not (mx/matrix-prob? [[[0.0] [0.0]]]))
    (t/is-not (mx/matrix-prob? [[nil]]))
    (t/is-not (mx/matrix-prob? [["A"]]))
    (t/is-not (mx/matrix-prob? '((0))))
    (t/is (mx/matrix-prob? [[0]]))
    (t/is-not (mx/matrix-prob? [[m/nan]]))
    (t/is-not (mx/matrix-prob? [[m/inf+]]))
    (t/is (mx/matrix-prob? [[0.0] [0.0]]))))

(deftest empty-matrix?-test
  (t/with-instrument `mx/empty-matrix?
    (t/is-spec-check mx/empty-matrix?))
  (t/with-instrument :all
    (t/is-not (mx/empty-matrix? []))
    (t/is (mx/empty-matrix? [[]]))
    (t/is-not (mx/empty-matrix? [[1]]))
    (t/is-not (mx/empty-matrix? [[] [2]]))
    (t/is-not (mx/empty-matrix? [[m/nan]]))
    (t/is-not (mx/empty-matrix? [[false]]))))

(deftest row-matrix?-test
  (t/with-instrument `mx/row-matrix?
    (t/is-spec-check mx/row-matrix?))
  (t/with-instrument :all
    (t/is-not (mx/row-matrix? [0]))
    (t/is-not (mx/row-matrix? [[0.0] [0.0]]))
    (t/is-not (mx/row-matrix? [[nil]]))
    (t/is-not (mx/row-matrix? '((0))))
    (t/is (mx/row-matrix? [[]]))
    (t/is (mx/row-matrix? [[0]]))
    (t/is (mx/row-matrix? [[0.0 0.0]]))
    (t/is (mx/row-matrix? [[m/nan]]))))

(deftest column-matrix?-test
  (t/with-instrument `mx/column-matrix?
    (t/is-spec-check mx/column-matrix?))
  (t/with-instrument :all
    (t/is-not (mx/column-matrix? [0]))
    (t/is-not (mx/column-matrix? [[0.0 0.0]]))
    (t/is-not (mx/column-matrix? [[nil]]))
    (t/is-not (mx/column-matrix? '((0))))
    (t/is (mx/column-matrix? [[]]))
    (t/is (mx/column-matrix? [[0]]))
    (t/is (mx/column-matrix? [[0.0] [0.0]]))
    (t/is (mx/column-matrix? [[m/nan]]))))

(deftest zero-matrix?-test
  (t/with-instrument `mx/zero-matrix?
    (t/is-spec-check mx/zero-matrix?))
  (t/with-instrument :all
    (t/is-not (mx/zero-matrix? [0]))
    (t/is (mx/zero-matrix? [[0]]))
    (t/is (mx/zero-matrix? [[0.0]]))
    (t/is (mx/zero-matrix? [[0.0] [0.0]]))
    (t/is-not (mx/zero-matrix? [[0.0] [0.1]]))))

(deftest square-matrix?-test
  (t/with-instrument `mx/square-matrix?
    (t/is-spec-check mx/square-matrix?))
  (t/with-instrument :all
    (t/is-not (mx/square-matrix? []))
    (t/is (mx/square-matrix? [[]]))
    (t/is (mx/square-matrix? [[1]]))
    (t/is-not (mx/square-matrix? [[] [2]]))
    (t/is (mx/square-matrix? [[m/nan]]))
    (t/is-not (mx/square-matrix? [[1 2]]))))

(deftest diagonal-matrix?-test
  (t/with-instrument `mx/diagonal-matrix?
    (t/is-spec-check mx/diagonal-matrix?))
  (t/with-instrument :all
    (t/is-not (mx/diagonal-matrix? [[1.0 0.5] [2.0 4.0]]))
    (t/is (mx/diagonal-matrix? [[1.0 0.0] [0.0 2.0]]))))

(deftest upper-triangular-matrix?-test
  (t/with-instrument `mx/upper-triangular-matrix?
    (t/is-spec-check mx/upper-triangular-matrix?))
  (t/with-instrument :all
    (t/is (mx/upper-triangular-matrix? [[]]))
    (t/is (mx/upper-triangular-matrix? [[1]]))
    (t/is-not (mx/upper-triangular-matrix? [[1 1]]))
    (t/is-not (mx/upper-triangular-matrix? [[1] [1]]))
    (t/is-not (mx/upper-triangular-matrix? [[1 1] [1 1]]))
    (t/is (mx/upper-triangular-matrix? [[1 0] [0 1]]))
    (t/is (mx/upper-triangular-matrix? [[1 1] [0 1]]))))

(deftest lower-triangular-matrix?-test
  (t/with-instrument `mx/lower-triangular-matrix?
    (t/is-spec-check mx/lower-triangular-matrix?))
  (t/with-instrument :all
    (t/is (mx/lower-triangular-matrix? [[]]))
    (t/is (mx/lower-triangular-matrix? [[1]]))
    (t/is-not (mx/lower-triangular-matrix? [[1 1]]))
    (t/is-not (mx/lower-triangular-matrix? [[1] [1]]))
    (t/is-not (mx/lower-triangular-matrix? [[1 1] [1 1]]))
    (t/is (mx/lower-triangular-matrix? [[1 0] [0 1]]))
    (t/is (mx/lower-triangular-matrix? [[1 0] [1 1]]))))

(deftest symmetric-matrix?-test
  (t/with-instrument `mx/symmetric-matrix?
    (t/is-spec-check mx/symmetric-matrix?))
  (t/with-instrument :all
    (t/is-not (mx/symmetric-matrix? [[1.0 0.5] [2.0 4.0]]))
    (t/is (mx/symmetric-matrix? [[1.0 0.5] [0.5 2.0]]))))

;;;CONSTRUCTORS
(deftest to-matrix-test
  (t/with-instrument `mx/to-matrix
    (t/is-spec-check mx/to-matrix))
  (t/with-instrument :all
    (t/is= [[]] (mx/to-matrix [] 1))
    (t/is= [[]] (mx/to-matrix [1.0 0.5] 0))
    (t/is= [[1.0 0.5]] (mx/to-matrix [1.0 0.5] 1))
    (t/is= [[1.0] [0.5]] (mx/to-matrix [1.0 0.5] 2))
    (t/is= [[1.0] [0.5] [0.0]] (mx/to-matrix [1.0 0.5] 3))
    (t/is= [[1.0 3.0 5.0] [2.0 4.0 6.0]]
      (mx/to-matrix [1.0 2.0 3.0 4.0 5.0 6.0] 2 {::mx/by-row? false}))
    (t/is= [[1.0 2.0 3.0] [4.0 5.0 6.0]]
      (mx/to-matrix [1.0 2.0 3.0 4.0 5.0 6.0] 2))
    (t/is= [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 0.0]]
      (mx/to-matrix [1.0 2.0 3.0 4.0 5.0 6.0 7.0] 2))))

(deftest constant-matrix-test
  (t/with-instrument `mx/constant-matrix
    (t/is-spec-check mx/constant-matrix))
  (t/with-instrument :all
    (t/is= [[]] (mx/constant-matrix 1 0 0.0))
    (t/is= [[]] (mx/constant-matrix 0 1 0.0))
    (t/is= [[1.0 1.0] [1.0 1.0]] (mx/constant-matrix 2 2 1.0))))

(deftest compute-matrix-test
  (t/with-instrument `mx/compute-matrix
    (t/is-spec-check mx/compute-matrix))
  (t/with-instrument :all
    (t/is= [[]]
      (mx/compute-matrix 1 0 (fn [r c]
                               (+ r c))))
    (t/is= [[]]
      (mx/compute-matrix 0 1 (fn [r c]
                               (+ r c))))
    (t/is= [[0 1] [1 2]]
      (mx/compute-matrix 2 2 (fn [r c]
                               (+ r c))))))

(deftest identity-matrix-test
  (t/with-instrument `mx/identity-matrix
    (t/is-spec-check mx/identity-matrix))
  (t/with-instrument :all
    (t/is= [[]] (mx/identity-matrix 0))
    (t/is= [[1.0]] (mx/identity-matrix 1))
    (t/is= [[1.0 0.0] [0.0 1.0]] (mx/identity-matrix 2))))

(deftest row-matrix-test
  (t/with-instrument `mx/row-matrix
    (t/is-spec-check mx/row-matrix))
  (t/with-instrument :all
    (t/is= [[]] (mx/row-matrix []))
    (t/is= [[1.0 0.5]] (mx/row-matrix [1.0 0.5]))
    (t/is= [[]] (mx/row-matrix 0 double))
    (t/is= [[0.0 1.0]] (mx/row-matrix 2 double))
    (t/is= [[3.0 3.0]] (mx/row-matrix 2 (constantly 3.0)))))

(deftest column-matrix-test
  (t/with-instrument `mx/column-matrix
    (t/is-spec-check mx/column-matrix))
  (t/with-instrument :all
    (t/is= [[]] (mx/column-matrix []))
    (t/is= [[1.0] [0.5]] (mx/column-matrix [1.0 0.5]))
    (t/is= [[]] (mx/column-matrix 0 double))
    (t/is= [[0.0] [1.0]] (mx/column-matrix 2 double))
    (t/is= [[3.0] [3.0]] (mx/column-matrix 2 (constantly 3.0)))))

(deftest diagonal-matrix-test
  (t/with-instrument `mx/diagonal-matrix
    (t/is-spec-check mx/diagonal-matrix))
  (t/with-instrument :all
    (t/is= [[]] (mx/diagonal-matrix []))
    (t/is= [[1.0 0.0] [0.0 0.5]] (mx/diagonal-matrix [1.0 0.5]))
    (t/is= [[]] (mx/diagonal-matrix 0 double))
    (t/is= [[0.0 0.0] [0.0 1.0]] (mx/diagonal-matrix 2 double))
    (t/is= [[1.0 0.0] [0.0 3.0]] (mx/diagonal-matrix [1.0 3.0]))
    (t/is= [[3.0 0.0] [0.0 3.0]] (mx/diagonal-matrix 2 (constantly 3.0)))))

(deftest deserialize-upper-triangular-matrix-test
  (t/with-instrument `mx/deserialize-upper-triangular-matrix
    (t/is-spec-check mx/deserialize-upper-triangular-matrix))
  (t/with-instrument :all
    (t/is= [[]] (mx/deserialize-upper-triangular-matrix []))
    (t/is= nil (mx/deserialize-upper-triangular-matrix [1.0 2.0]))
    (t/is= [[1.0 2.0] [0.0 3.0]] (mx/deserialize-upper-triangular-matrix [1.0 2.0 3.0]))
    (t/is= [[7.0 1.0 2.0 4.0] [0.0 8.0 3.0 5.0] [0.0 0.0 9.0 6.0] [0.0 0.0 0.0 10.0]]
      (mx/deserialize-upper-triangular-matrix
        [7.0 8.0 9.0 10.0] [1.0 2.0 3.0 4.0 5.0 6.0]
        {::mx/by-row? false}))
    (t/is= [[7.0 1.0 2.0 3.0] [0.0 8.0 4.0 5.0] [0.0 0.0 9.0 6.0] [0.0 0.0 0.0 10.0]]
      (mx/deserialize-upper-triangular-matrix
        [7.0 8.0 9.0 10.0] [1.0 2.0 3.0 4.0 5.0 6.0]
        {::mx/by-row? true}))
    (t/is= [[1.0 2.0 4.0] [0.0 3.0 5.0] [0.0 0.0 6.0]]
      (mx/deserialize-upper-triangular-matrix
        [1.0 2.0 3.0 4.0 5.0 6.0]
        {::mx/by-row? false}))
    (t/is= [[1.0 2.0 3.0] [0.0 4.0 5.0] [0.0 0.0 6.0]]
      (mx/deserialize-upper-triangular-matrix [1.0 2.0 3.0 4.0 5.0 6.0]))))

(deftest deserialize-lower-triangular-matrix-test
  (t/with-instrument `mx/deserialize-lower-triangular-matrix
    (t/is-spec-check mx/deserialize-lower-triangular-matrix))
  (t/with-instrument :all
    (t/is= [[]] (mx/deserialize-lower-triangular-matrix []))
    (t/is= nil (mx/deserialize-lower-triangular-matrix [1.0 2.0]))
    (t/is= [[1.0 0.0] [2.0 3.0]]
      (mx/deserialize-lower-triangular-matrix [1.0 2.0 3.0]))
    (t/is= [[7.0 0.0 0.0 0.0] [1.0 8.0 0.0 0.0] [2.0 4.0 9.0 0.0] [3.0 5.0 6.0 10.0]]
      (mx/deserialize-lower-triangular-matrix
        [7.0 8.0 9.0 10.0] [1.0 2.0 3.0 4.0 5.0 6.0]
        {::mx/by-row? false}))
    (t/is= [[7.0 0.0 0.0 0.0] [1.0 8.0 0.0 0.0] [2.0 3.0 9.0 0.0] [4.0 5.0 6.0 10.0]]
      (mx/deserialize-lower-triangular-matrix
        [7.0 8.0 9.0 10.0] [1.0 2.0 3.0 4.0 5.0 6.0]
        {::mx/by-row? true}))
    (t/is= [[1.0 0.0 0.0] [2.0 4.0 0.0] [3.0 5.0 6.0]]
      (mx/deserialize-lower-triangular-matrix
        [1.0 2.0 3.0 4.0 5.0 6.0]
        {::mx/by-row? false}))
    (t/is= [[1.0 0.0 0.0] [2.0 3.0 0.0] [4.0 5.0 6.0]]
      (mx/deserialize-lower-triangular-matrix [1.0 2.0 3.0 4.0 5.0 6.0]))))

(deftest deserialize-symmetric-matrix-test
  (t/with-instrument `mx/deserialize-symmetric-matrix
    (t/is-spec-check mx/deserialize-symmetric-matrix))
  (t/with-instrument :all
    (t/is= [[]] (mx/deserialize-symmetric-matrix []))
    (t/is= nil (mx/deserialize-symmetric-matrix [1.0 2.0]))
    (t/is= [[1.0 2.0] [2.0 3.0]]
      (mx/deserialize-symmetric-matrix [1.0 2.0 3.0]))))

;;also called [[diagonal-constant-matrix]]
(deftest toeplitz-matrix-test
  (t/with-instrument `mx/toeplitz-matrix
    (t/is-spec-check mx/toeplitz-matrix))
  (t/with-instrument :all
    (t/is= [[]] (mx/diagonal-constant-matrix [] []))
    (t/is= [[1]] (mx/diagonal-constant-matrix [1] [1]))
    (t/is= [[1.0 2.0 3.0] [4.0 1.0 2.0] [5.0 4.0 1.0]]
      (mx/toeplitz-matrix [1.0 2.0 3.0] [1.0 4.0 5.0]))
    (t/is= [[1.0 2.0] [4.0 1.0] [5.0 4.0]] (mx/toeplitz-matrix [1.0 2.0] [1.0 4.0 5.0]))
    (t/is= [[1.0 2.0 3.0] [4.0 1.0 2.0]] (mx/toeplitz-matrix [1.0 2.0 3.0] [1.0 4.0]))))

(deftest outer-product-test
  (t/with-instrument `mx/outer-product
    (t/is-spec-check mx/outer-product))
  (t/with-instrument :all
    (t/is= [[]] (mx/outer-product []))
    (t/is= [[9.0]] (mx/outer-product [3]))
    (t/is= [[1.0 2.0] [2.0 4.0]] (mx/outer-product [1.0 2.0]))
    (t/is= [[4.0 8.0 10.0] [8.0 16.0 20.0] [10.0 20.0 25.0]] (mx/outer-product [2.0 4.0 5.0]))
    (t/is= [[1.0 0.5] [0.5 0.25]] (mx/outer-product [1.0 0.5]))
    (t/is= [[4.0 6.0] [6.0 9.0]] (mx/outer-product [2.0 3.0]))))

(deftest rnd-matrix!-test
  (t/with-instrument `mx/rnd-matrix!
    (t/is-spec-check mx/rnd-matrix!))
  (t/with-instrument :all
    (random/bind-seed 0
      (t/is= [[]] (mx/rnd-matrix! 0 0)))
    (random/bind-seed 0
      (t/is= [[0.2961287401299688 0.8622994122994543]
              [0.07868284113948965 0.548683671433349]]
        (mx/rnd-matrix! 2 2)))
    (random/bind-seed 0
      (t/is= [[0.2961287401299688 0.8622994122994543 0.07868284113948965]
              [0.548683671433349 0.11620266042486127 0.5772125043785624]]
        (mx/rnd-matrix! 2 3)))))

(deftest rnd-reflection-matrix!-test
  (t/with-instrument `mx/rnd-reflection-matrix!
    (t/is-spec-check mx/rnd-reflection-matrix!))
  (t/with-instrument :all
    (random/bind-seed 0
      (t/is= [[]] (mx/rnd-reflection-matrix! 0)))
    (random/bind-seed 0
      (t/is= [[-1.0]] (mx/rnd-reflection-matrix! 1)))
    (random/bind-seed 0
      (t/is= [[0.7890118105552666 -0.6143780292330612]
              [-0.6143780292330612 -0.7890118105552668]]
        (mx/rnd-reflection-matrix! 2)))))

(deftest rnd-spectral-matrix!-test
  (t/with-instrument `mx/rnd-spectral-matrix!
    (t/is-spec-check mx/rnd-spectral-matrix!))
  (t/with-instrument :all
    (random/bind-seed 0
      (t/is= [[]] (mx/rnd-spectral-matrix! [])))
    (random/bind-seed 0
      (t/is= [[2.0]] (mx/rnd-spectral-matrix! [2.0])))
    (random/bind-seed 0
      (t/is= [[2.9987560086952176 0.04986416644446639]
              [0.04986416644446639 1.0012439913047837]]
        (mx/rnd-spectral-matrix! [1.0 3.0])))))

(deftest sparse->matrix-test
  (t/with-instrument `mx/sparse->matrix
    (t/is-spec-check mx/sparse->matrix))
  (t/with-instrument :all
    (t/is= [[4.0 5.0] [6.0 7.0]] (mx/sparse->matrix [] [[4.0 5.0] [6.0 7.0]]))
    (t/is= [[3.0 5.0] [6.0 7.0]] (mx/sparse->matrix [[0 0 3.0]] [[4.0 5.0] [6.0 7.0]]))
    (t/is= [[3.0 0.0] [0.0 0.0]] (mx/sparse->matrix [[0 0 3.0]] (mx/constant-matrix 2 2 0.0)))
    (t/is= [[3.0 5.0] [2.0 -1.0]]
      (mx/sparse->matrix [[0 0 3.0] [1 0 2.0] [0 1 5.0] [1 1 -1.0]] (mx/constant-matrix 2 2 0.0)))
    (t/is= [[0.0 0.0] [0.0 0.0]] (mx/sparse->matrix [[0 2 3.0]] (mx/constant-matrix 2 2 0.0)))))

(deftest sparse->symmetric-matrix-test
  (t/with-instrument `mx/sparse->symmetric-matrix
    (t/is-spec-check mx/sparse->symmetric-matrix))
  (t/with-instrument :all
    (t/is= [[4.0 5.0] [5.0 7.0]] (mx/sparse->symmetric-matrix [] [[4.0 5.0] [5.0 7.0]]))
    (t/is= [[3.0 2.0] [2.0 4.0]]
      (mx/sparse->symmetric-matrix [[0 0 3.0] [1 0 2.0]] [[1.0 2.0] [2.0 4.0]]))))

;;;INFO
(deftest rows-test
  (t/with-instrument `mx/rows
    (t/is-spec-check mx/rows))
  (t/with-instrument :all
    (t/is= 0 (mx/rows [[]]))
    (t/is= 1 (mx/rows [[1.0]]))
    (t/is= 1 (mx/rows [[1.0 2.0]]))
    (t/is= 2 (mx/rows [[1.0] [2.0]]))
    (t/is= 2 (mx/rows [[1.0 0.5] [2.0 4.0]]))))

(deftest columns-test
  (t/with-instrument `mx/columns
    (t/is-spec-check mx/columns))
  (t/with-instrument :all
    (t/is= 0 (mx/columns [[]]))
    (t/is= 1 (mx/columns [[1.0]]))
    (t/is= 2 (mx/columns [[1.0 2.0]]))
    (t/is= 1 (mx/columns [[1.0] [2.0]]))
    (t/is= 2 (mx/columns [[1.0 0.5] [2.0 4.0]]))))

(deftest get-row-test
  (t/with-instrument `mx/get-row
    (t/is-spec-check mx/get-row))
  (t/with-instrument :all
    (t/is= [1.0] (mx/get-row [[1.0]] 0))
    (t/is= [2.0] (mx/get-row [[1.0] [2.0]] 1))
    (t/is= [1.0 0.5] (mx/get-row [[1.0 0.5] [2.0 4.0]] 0))))

(deftest get-column-test
  (t/with-instrument `mx/get-column
    (t/is-spec-check mx/get-column))
  (t/with-instrument :all
    (t/is= [1.0] (mx/get-column [[1.0]] 0))
    (t/is= [2.0] (mx/get-column [[1.0 2.0]] 1))
    (t/is= [1.0 2.0] (mx/get-column [[1.0 0.5] [2.0 4.0]] 0))))

(deftest diagonal-test
  (t/with-instrument `mx/diagonal
    (t/is-spec-check mx/diagonal))
  (t/with-instrument :all
    (t/is= [] (mx/diagonal [[]]))
    (t/is= [1] (mx/diagonal [[1]]))
    (t/is= [1.0 4.0] (mx/diagonal [[1.0 0.5] [2.0 4.0]]))
    (t/is= [1.0] (mx/diagonal [[1.0 0.5]]))
    (t/is= [1.0] (mx/diagonal [[1.0] [0.5]]))
    (t/is= [0.5] (mx/diagonal [[1.0 0.5] [2.0 4.0]] 1))
    (t/is= [] (mx/diagonal [[1.0 0.5] [2.0 4.0]] 2))
    (t/is= [2.0] (mx/diagonal [[1.0 0.5] [2.0 4.0]] -1))
    (t/is= [] (mx/diagonal [[1.0 0.5] [2.0 4.0]] -2))))

(deftest serialize-symmetric-or-triangular-matrix-test
  (t/with-instrument `mx/serialize-symmetric-or-triangular-matrix
    (t/is-spec-check mx/serialize-symmetric-or-triangular-matrix))
  (t/with-instrument :all
    (t/is= [] (mx/serialize-symmetric-or-triangular-matrix [[]]))
    (t/is= [3] (mx/serialize-symmetric-or-triangular-matrix [[3]]))
    (t/is= [1.0 2.0 4.0]
      (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5] [2.0 4.0]] {::mx/by-row? false}))
    (t/is= [1.0 0.5 4.0]
      (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5] [2.0 4.0]]))
    (t/is= [1.0 0.5 1.0]
      (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5] [0.5 1.0]] {::mx/by-row? false}))
    (t/is= [1.0 0.5 1.0]
      (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5] [0.5 1.0]]))
    (t/is= [1.0]
      (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5]] {::mx/by-row? false}))
    (t/is= [1.0 0.5] (mx/serialize-symmetric-or-triangular-matrix [[1.0 0.5]]))
    (t/is= [1.0 0.5]
      (mx/serialize-symmetric-or-triangular-matrix [[1.0] [0.5]] {::mx/by-row? false}))
    (t/is= [1.0] (mx/serialize-symmetric-or-triangular-matrix [[1.0] [0.5]]))))

(deftest size-of-symmetric-or-triangular-matrix-test
  (t/with-instrument `mx/size-of-symmetric-or-triangular-matrix
    (t/is-spec-check mx/size-of-symmetric-or-triangular-matrix))
  (t/with-instrument :all
    (t/is= 1 (mx/size-of-symmetric-or-triangular-matrix 1))
    (t/is (m/nan? (mx/size-of-symmetric-or-triangular-matrix 2)))
    (t/is= 2 (mx/size-of-symmetric-or-triangular-matrix 3))
    (t/is= 3 (mx/size-of-symmetric-or-triangular-matrix 6))))

(deftest size-of-symmetric-or-triangular-matrix-without-diag-test
  (t/with-instrument `mx/size-of-symmetric-or-triangular-matrix-without-diag
    (t/is-spec-check mx/size-of-symmetric-or-triangular-matrix-without-diag))
  (t/with-instrument :all
    (t/is= 2 (mx/size-of-symmetric-or-triangular-matrix-without-diag 1))
    (t/is (m/nan? (mx/size-of-symmetric-or-triangular-matrix-without-diag 2)))
    (t/is= 3 (mx/size-of-symmetric-or-triangular-matrix-without-diag 3))
    (t/is= 4 (mx/size-of-symmetric-or-triangular-matrix-without-diag 6))))

(deftest ecount-of-symmetric-or-triangular-matrix-test
  (t/with-instrument `mx/ecount-of-symmetric-or-triangular-matrix
    (t/is-spec-check mx/ecount-of-symmetric-or-triangular-matrix))
  (t/with-instrument :all
    (t/is= 1 (mx/ecount-of-symmetric-or-triangular-matrix 1))
    (t/is= 3 (mx/ecount-of-symmetric-or-triangular-matrix 2))
    (t/is= 6 (mx/ecount-of-symmetric-or-triangular-matrix 3))
    (t/is= 21 (mx/ecount-of-symmetric-or-triangular-matrix 6))))

(deftest ecount-of-symmetric-or-triangular-matrix-without-diag-test
  (t/with-instrument `mx/ecount-of-symmetric-or-triangular-matrix-without-diag
    (t/is-spec-check mx/ecount-of-symmetric-or-triangular-matrix-without-diag))
  (t/with-instrument :all
    (t/is= 0 (mx/ecount-of-symmetric-or-triangular-matrix-without-diag 1))
    (t/is= 1 (mx/ecount-of-symmetric-or-triangular-matrix-without-diag 2))
    (t/is= 3 (mx/ecount-of-symmetric-or-triangular-matrix-without-diag 3))
    (t/is= 15 (mx/ecount-of-symmetric-or-triangular-matrix-without-diag 6))))

(deftest trace-test
  (t/with-instrument `mx/trace
    (t/is-spec-check mx/trace))
  (t/with-instrument :all
    (t/is= 0.0 (mx/trace [[]]))
    (t/is= 1.0 (mx/trace [[1]]))
    (t/is= 5.0 (mx/trace [[1.0 2.0] [3.0 4.0]]))
    (t/is= 5.0 (mx/trace [[1.0 0.5] [2.0 4.0]]))
    (t/is= 6.0 (mx/trace [[1.0 0.0 0.0] [0.0 2.0 0.0] [0.0 0.0 3.0]]))))

(deftest get-slices-as-matrix-test
  (t/with-instrument `mx/get-slices-as-matrix
    (t/is-spec-check mx/get-slices-as-matrix))
  (t/with-instrument :all
    (t/is= [[]] (mx/get-slices-as-matrix [[]] {::mx/row-indices 0}))
    (t/is= [[1.0 0.5]] (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices 0}))
    (t/is= [[1.0 0.5] [2.0 4.0]]
      (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices [0 1]}))
    (t/is= [[2.0 4.0] [1.0 0.5]]
      (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/row-indices [1 0]}))
    (t/is= [[1.0] [2.0]] (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/column-indices 0}))
    (t/is= [[1.0 0.5] [2.0 4.0]]
      (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/column-indices [0 1]}))
    (t/is= [[0.5 1.0] [4.0 2.0]]
      (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/column-indices [1 0]}))
    (t/is= [[2.0 4.0]]
      (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/exception-row-indices 0}))
    (t/is= [[]]
      (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/exception-row-indices [0 1]}))
    (t/is= [[0.5] [4.0]]
      (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/exception-column-indices 0}))
    (t/is= [[]]
      (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]] {::mx/exception-column-indices [0 1]}))
    (t/is= [[]]
      (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]]
        {::mx/row-indices           0
         ::mx/exception-row-indices 0}))
    (t/is= [[]]
      (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]]
        {::mx/row-indices           [0]
         ::mx/exception-row-indices 0}))
    (t/is= [[]]
      (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]]
        {::mx/row-indices           0
         ::mx/exception-row-indices [0]}))
    (t/is= [[1.0]]
      (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]]
        {::mx/exception-row-indices    1
         ::mx/exception-column-indices 1}))
    (t/is= [[1.0]]
      (mx/get-slices-as-matrix [[1.0 0.5] [2.0 4.0]]
        {::mx/row-indices    0
         ::mx/column-indices 0}))))

(deftest filter-by-row-test
  (t/with-instrument `mx/filter-by-row
    (t/is-spec-check mx/filter-by-row))
  (t/with-instrument :all
    (t/is= [[]]
      (mx/filter-by-row (fn [row-v]
                          (< (apply + (tensor/emap m/sq row-v)) 2.0))
        [[]]))
    (t/is= [[1.0 0.5]]
      (mx/filter-by-row (fn [row-v]
                          (< (apply + (tensor/emap m/sq row-v)) 2.0))
        [[1.0 0.5] [2.0 4.0]]))
    (t/is= [[1.0 0.5]]
      (mx/filter-by-row (fn [row-v]
                          (< (apply + (tensor/emap m/sq row-v)) 2.0))
        [[1.0 0.5]]))
    (t/is= [[1.0] [0.5]]
      (mx/filter-by-row (fn [row-v]
                          (< (apply + (tensor/emap m/sq row-v)) 2.0))
        [[1.0] [0.5]]))
    (t/is= [[1.0 0.5]]
      (mx/filter-by-row (fn [row-v]
                          (< (apply + (tensor/emap m/sq row-v)) 2.0))
        [[1.0 0.5] [2.0 4.0]]))))

(deftest filter-by-column-test
  (t/with-instrument `mx/filter-by-column
    (t/is-spec-check mx/filter-by-column))
  (t/with-instrument :all
    (t/is= [[]]
      (mx/filter-by-column (fn [column-v]
                             (< (apply + (tensor/emap m/sq column-v)) 6.0))
        [[]]))
    (t/is= [[1.0] [2.0]]
      (mx/filter-by-column (fn [column-v]
                             (< (apply + (tensor/emap m/sq column-v)) 6.0))
        [[1.0 0.5] [2.0 4.0]]))
    (t/is= [[1.0 0.5]]
      (mx/filter-by-column (fn [column-v]
                             (< (apply + (tensor/emap m/sq column-v)) 6.0))
        [[1.0 0.5]]))
    (t/is= [[1.0] [0.5]]
      (mx/filter-by-column (fn [column-v]
                             (< (apply + (tensor/emap m/sq column-v)) 6.0))
        [[1.0] [0.5]]))
    (t/is= [[1.0] [2.0]]
      (mx/filter-by-column (fn [column-v]
                             (< (apply + (tensor/emap m/sq column-v)) 6.0))
        [[1.0 0.5] [2.0 4.0]]))))

(deftest filter-symmetric-matrix-test
  (t/with-instrument `mx/filter-symmetric-matrix
    (t/is-spec-check mx/filter-symmetric-matrix))
  (t/with-instrument :all
    (t/is= [[]]
      (mx/filter-symmetric-matrix (fn [v]
                                    (< (apply + (tensor/emap m/sq v)) 2.0))
        [[]]))
    (t/is= [[1.0]]
      (mx/filter-symmetric-matrix (fn [v]
                                    (< (apply + (tensor/emap m/sq v)) 2.0))
        [[1.0 0.5] [0.5 4.0]]))))

(def s
  [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
   [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]])

(deftest matrix-partition-test
  (t/with-instrument `mx/matrix-partition
    (t/is-spec-check mx/matrix-partition))
  (t/with-instrument :all
    (t/is= {::mx/bottom-left  [[9.0 10.0] [13.0 14.0]]
            ::mx/bottom-right [[11.0 12.0] [15.0 16.0]]
            ::mx/top-left     [[1.0 2.0] [5.0 6.0]]
            ::mx/top-right    [[3.0 4.0] [7.0 8.0]]}
      (mx/matrix-partition s 2 2))
    (t/is= {::mx/bottom-left  [[5.0] [9.0] [13.0]]
            ::mx/bottom-right [[6.0 7.0 8.0] [10.0 11.0 12.0] [14.0 15.0 16.0]]
            ::mx/top-left     [[1.0]]
            ::mx/top-right    [[2.0 3.0 4.0]]}
      (mx/matrix-partition s 1 1))
    (t/is= {::mx/bottom-left  [[1.0 2.0 3.0] [5.0 6.0 7.0]
                               [9.0 10.0 11.0] [13.0 14.0 15.0]]
            ::mx/bottom-right [[4.0] [8.0] [12.0] [16.0]]
            ::mx/top-left     [[]]
            ::mx/top-right    [[]]}
      (mx/matrix-partition s 0 3))
    (t/is= {::mx/bottom-left  [[]]
            ::mx/bottom-right [[13.0 14.0 15.0 16.0]]
            ::mx/top-left     [[]]
            ::mx/top-right    [[1.0 2.0 3.0 4.0]
                               [5.0 6.0 7.0 8.0]
                               [9.0 10.0 11.0 12.0]]}
      (mx/matrix-partition s 3 0))
    (t/is= {::mx/bottom-left  [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                               [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]]
            ::mx/bottom-right [[]]
            ::mx/top-left     [[]]
            ::mx/top-right    [[]]}
      (mx/matrix-partition s 0 4))
    (t/is= {::mx/bottom-left  [[]]
            ::mx/bottom-right [[]]
            ::mx/top-left     [[]]
            ::mx/top-right    [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                               [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]]}
      (mx/matrix-partition s 4 0))
    (t/is= {::mx/bottom-left  [[]]
            ::mx/bottom-right [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                               [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]]
            ::mx/top-left     [[]]
            ::mx/top-right    [[]]}
      (mx/matrix-partition s 0 0))
    (t/is= {::mx/bottom-left  [[]]
            ::mx/bottom-right [[]]
            ::mx/top-left     [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                               [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]]
            ::mx/top-right    [[]]}
      (mx/matrix-partition s 4 4))))

(deftest some-kv-test
  (t/with-instrument `mx/some-kv
    (t/is-spec-check mx/some-kv))
  (t/with-instrument :all
    (t/is= 0.5
      (mx/some-kv (fn [row column number]
                    (> (+ row column) number))
        [[1.0 0.5] [2.0 4.0]]))
    (t/is= 0.5
      (mx/some-kv (fn [row column number]
                    (> (+ row column) number))
        [[1.0 0.5] [2.0 4.0]]
        {::mx/by-row false}))))

(deftest ereduce-kv-test
  (t/with-instrument `mx/ereduce-kv
    (t/is-spec-check mx/ereduce-kv))
  (t/with-instrument :all
    (t/is= 29.9
      (mx/ereduce-kv (fn [tot r c n1 n2 n3]
                       (when (number? tot)
                         (+ tot r c n1 n2 n3)))
        3.4
        [[1.0 0.5] [2.0 4.0]]
        [[1.0 0.5] [2.0 4.0]]
        [[1.0 0.5] [2.0 4.0]]))
    (t/is= 22.4
      (mx/ereduce-kv (fn [tot r c n1 n2]
                       (when (number? tot)
                         (+ tot r c n1 n2)))
        3.4
        [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
    (t/is= 14.9
      (mx/ereduce-kv (fn [tot r c n]
                       (when (number? tot)
                         (+ tot r c n)))
        3.4
        [[1.0 0.5] [2.0 4.0]]))
    (t/is= 5.9
      (mx/ereduce-kv (fn [tot r c n]
                       (when (number? tot)
                         (+ tot r c n)))
        3.4
        [[1.0 0.5]]))
    (t/is= 5.9
      (mx/ereduce-kv (fn [tot r c n]
                       (when (number? tot)
                         (+ tot r c n)))
        3.4
        [[1.0] [0.5]]))
    (t/is= 7.4
      (mx/ereduce-kv (fn [tot r c n1 n2]
                       (when (number? tot)
                         (+ tot r c n1 n2)))
        3.4
        [[1.0 0.5]] [[1.0 0.5]]))))

(deftest matrix->sparse-test
  (t/with-instrument `mx/matrix->sparse
    (t/is-spec-check mx/matrix->sparse))
  (t/with-instrument :all
    (t/is= [] (mx/matrix->sparse [[]]))
    (t/is= [[1 0 1.0]] (mx/matrix->sparse [[0.0 0.0] [1.0 0.0]]))
    (t/is= [[0 0 1.0] [0 1 0.5] [1 0 2.0] [1 1 4.0]] (mx/matrix->sparse [[1.0 0.5] [2.0 4.0]]))
    (t/is= [[0 0 1.0] [0 1 0.5] [1 0 2.0]] (mx/matrix->sparse [[1.0 0.5] [2.0 4.0]] #(< % 2.1)))
    (t/is= [] (mx/matrix->sparse [[1.0 0.5] [2.0 4.0]] neg?))
    (t/is= [[1 0 0.5]] (mx/matrix->sparse [[1.0] [0.5]] #(< % 0.7)))))

(deftest symmetric-matrix->sparse-test
  (t/with-instrument `mx/symmetric-matrix->sparse
    (t/is-spec-check mx/symmetric-matrix->sparse))
  (t/with-instrument :all
    (t/is= [] (mx/symmetric-matrix->sparse [[]]))
    (t/is= [[0 1 1.0]] (mx/symmetric-matrix->sparse [[0.0 1.0] [1.0 0.0]]))
    (t/is= [[0 0 1.0] [0 1 0.5] [1 1 4.0]] (mx/symmetric-matrix->sparse [[1.0 0.5] [0.5 4.0]]))
    (t/is= [[0 0 1.0] [0 1 0.5]] (mx/symmetric-matrix->sparse [[1.0 0.5] [0.5 4.0]] #(< % 2.1)))))

;;;MANIPULATION
(deftest transpose-test
  (t/with-instrument `mx/transpose
    (t/is-spec-check mx/transpose))
  (t/with-instrument :all
    (t/is= [[]] (mx/transpose [[]]))
    (t/is= [[1]] (mx/transpose [[1]]))
    (t/is= [[1.0 2.0] [0.5 4.0]] (mx/transpose [[1.0 0.5] [2.0 4.0]]))
    (t/is= [[1.0 0.5] [2.0 4.0]] (mx/transpose (mx/transpose [[1.0 0.5] [2.0 4.0]])))
    (t/is= [[1.0 0.5]] (mx/transpose [[1.0] [0.5]]))
    (t/is= [[1.0] [0.5]] (mx/transpose [[1.0 0.5]]))))

(deftest assoc-row-test
  (t/with-instrument `mx/assoc-row
    (t/is-spec-check mx/assoc-row))
  (t/with-instrument :all
    (t/is= [[8.0 9.0]] (mx/assoc-row [[]] 0 [8.0 9.0]))
    (t/is= ::anom/incorrect (::anom/category (mx/assoc-row [[1]] 0 [8.0 9.0])))
    (t/is= [[2]] (mx/assoc-row [[1]] 0 [2]))
    (t/is= [[1] [2]] (mx/assoc-row [[1]] 1 [2]))
    (t/is= ::anom/incorrect (::anom/category (mx/assoc-row [[1]] 2 [2])))
    (t/is= [[8.0 9.0] [2.0 4.0]] (mx/assoc-row [[1.0 0.5] [2.0 4.0]] 0 [8.0 9.0]))))

(deftest assoc-column-test
  (t/with-instrument `mx/assoc-column
    (t/is-spec-check mx/assoc-column))
  (t/with-instrument :all
    (t/is= [[8.0] [9.0]] (mx/assoc-column [[]] 0 [8.0 9.0]))
    (t/is= ::anom/incorrect (::anom/category (mx/assoc-column [[1]] 0 [8.0 9.0])))
    (t/is= [[2]] (mx/assoc-column [[1]] 0 [2]))
    (t/is= [[1 2]] (mx/assoc-column [[1]] 1 [2]))
    (t/is= ::anom/incorrect (::anom/category (mx/assoc-column [[1]] 2 [2])))
    (t/is= [[8.0 0.5] [9.0 4.0]] (mx/assoc-column [[1.0 0.5] [2.0 4.0]] 0 [8.0 9.0]))))

(deftest assoc-diagonal-test
  (t/with-instrument `mx/assoc-diagonal
    (t/is-spec-check mx/assoc-diagonal))
  (t/with-instrument :all
    (t/is= [[8.0 0.0] [0.0 9.0]] (mx/assoc-diagonal [[]] [8.0 9.0]))
    (t/is= ::anom/incorrect (::anom/category (mx/assoc-diagonal [[1]] [8.0 9.0])))
    (t/is= [[2]] (mx/assoc-diagonal [[1]] [2]))
    (t/is= [[8.0 0.5] [2.0 9.0]] (mx/assoc-diagonal [[1.0 0.5] [2.0 4.0]] [8.0 9.0]))))

(deftest insert-row-test
  (t/with-instrument `mx/insert-row
    (t/is-spec-check mx/insert-row))
  (t/with-instrument :all
    (t/is= [[8.0 9.0]] (mx/insert-row [[]] 0 [8.0 9.0]))
    (t/is= ::anom/incorrect (::anom/category (mx/insert-row [[1]] 0 [8.0 9.0])))
    (t/is= [[2] [1]] (mx/insert-row [[1]] 0 [2]))
    (t/is= [[1] [2]] (mx/insert-row [[1]] 1 [2]))
    (t/is= ::anom/incorrect (::anom/category (mx/insert-row [[1]] 2 [2])))
    (t/is= [[8.0 9.0] [1.0 0.5] [2.0 4.0]] (mx/insert-row [[1.0 0.5] [2.0 4.0]] 0 [8.0 9.0]))
    (t/is= [[1.0 0.5] [8.0 9.0] [2.0 4.0]] (mx/insert-row [[1.0 0.5] [2.0 4.0]] 1 [8.0 9.0]))))

(deftest insert-column-test
  (t/with-instrument `mx/insert-column
    (t/is-spec-check mx/insert-column))
  (t/with-instrument :all
    (t/is= [[8.0] [9.0]] (mx/insert-column [[]] 0 [8.0 9.0]))
    (t/is= ::anom/incorrect (::anom/category (mx/insert-column [[1]] 0 [8.0 9.0])))
    (t/is= [[2 1]] (mx/insert-column [[1]] 0 [2]))
    (t/is= [[1 2]] (mx/insert-column [[1]] 1 [2]))
    (t/is= ::anom/incorrect (::anom/category (mx/insert-column [[1]] 2 [2])))
    (t/is= [[8.0 1.0 0.5] [9.0 2.0 4.0]] (mx/insert-column [[1.0 0.5] [2.0 4.0]] 0 [8.0 9.0]))))

(deftest update-row-test
  (t/with-instrument `mx/update-row
    (t/is-spec-check mx/update-row))
  (t/with-instrument :all
    (t/is= nil
      (mx/update-row [[]] 0 (fn [column number]
                              (+ column number 1))))
    (t/is= [[2]]
      (mx/update-row [[1]] 0 (fn [column number]
                               (+ column number 1))))
    (t/is= nil
      (mx/update-row [[1]] 1 (fn [column number]
                               (+ column number 1))))
    (t/is= [[2.0 2.5] [2.0 4.0]]
      (mx/update-row [[1.0 0.5] [2.0 4.0]] 0 (fn [column number]
                                               (+ column number 1))))))

(deftest update-column-test
  (t/with-instrument `mx/update-column
    (t/is-spec-check mx/update-column))
  (t/with-instrument :all
    (t/is= nil
      (mx/update-column [[]] 0 (fn [row number]
                                 (+ row number 1))))
    (t/is= [[2]]
      (mx/update-column [[1]] 0 (fn [row number]
                                  (+ row number 1))))
    (t/is= nil
      (mx/update-column [[1]] 1 (fn [row number]
                                  (+ row number 1))))
    (t/is= [[2.0 0.5] [4.0 4.0]]
      (mx/update-column [[1.0 0.5] [2.0 4.0]] 0 (fn [row number]
                                                  (+ row number 1))))))

(deftest update-diagonal-test
  (t/with-instrument `mx/update-diagonal
    (t/is-spec-check mx/update-diagonal))
  (t/with-instrument :all
    (t/is= [[]]
      (mx/update-diagonal [[]] (fn [row number]
                                 (+ row number 1))))
    (t/is= [[2]]
      (mx/update-diagonal [[1]] (fn [row number]
                                  (+ row number 1))))
    (t/is= [[2.0 0.5] [2.0 6.0]]
      (mx/update-diagonal [[1.0 0.5] [2.0 4.0]] (fn [row number]
                                                  (+ row number 1))))))

(deftest remove-row-test
  (t/with-instrument `mx/remove-row
    (t/is-spec-check mx/remove-row))
  (t/with-instrument :all
    (t/is= [[]] (mx/remove-row [[]] 0))
    (t/is= [[]] (mx/remove-row [[1]] 0))
    (t/is= [[1]] (mx/remove-row [[1]] 1))
    (t/is= [[3 4]] (mx/remove-row [[1 2] [3 4]] 0))
    (t/is= [[1 2]] (mx/remove-row [[1 2] [3 4]] 1))
    (t/is= [[1 2] [3 4]] (mx/remove-row [[1 2] [3 4]] 2))
    (t/is= [[1 2] [5 6]] (mx/remove-row [[1 2] [3 4] [5 6]] 1))))

(deftest remove-column-test
  (t/with-instrument `mx/remove-column
    (t/is-spec-check mx/remove-column))
  (t/with-instrument :all
    (t/is= [[]] (mx/remove-column [[]] 0))
    (t/is= [[]] (mx/remove-column [[1]] 0))
    (t/is= [[1]] (mx/remove-column [[1]] 1))
    (t/is= [[2] [4]] (mx/remove-column [[1 2] [3 4]] 0))
    (t/is= [[1] [3]] (mx/remove-column [[1 2] [3 4]] 1))
    (t/is= [[1 2] [3 4]] (mx/remove-column [[1 2] [3 4]] 2))
    (t/is= [[1 3] [4 6]] (mx/remove-column [[1 2 3] [4 5 6]] 1))))

(deftest concat-rows-test
  (t/with-instrument `mx/concat-rows
    (t/is-spec-check mx/concat-rows))
  (t/with-instrument :all
    (t/is= [[]] (mx/concat-rows [[]] [[]]))
    (t/is= nil (mx/concat-rows [[]] [[1]]))
    (t/is= [[1.0 0.5] [2.0 4.0] [1.0 0.5] [2.0 4.0]]
      (mx/concat-rows [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
    (t/is= [[1.0] [2.0]] (mx/concat-rows [[1.0]] [[2.0]]))
    (t/is= [[1.0 0.5] [1.0 0.5] [2.0 4.0]] (mx/concat-rows [[1.0 0.5]] [[1.0 0.5] [2.0 4.0]]))
    (t/is= [[1.0 0.5] [2.0 4.0] [1.0 0.5]] (mx/concat-rows [[1.0 0.5] [2.0 4.0]] [[1.0 0.5]]))
    (t/is= [[1.0 0.5] [1.0 0.5]] (mx/concat-rows [[1.0 0.5]] [[1.0 0.5]]))
    (t/is= [[1.0 0.5] [1.0 0.5] [1.0 0.5]] (mx/concat-rows [[1.0 0.5]] [[1.0 0.5]] [[1.0 0.5]]))))

(deftest concat-columns-test
  (t/with-instrument `mx/concat-columns
    (t/is-spec-check mx/concat-columns))
  (t/with-instrument :all
    (t/is= [[]] (mx/concat-columns [[]] [[]]))
    (t/is= nil (mx/concat-columns [[]] [[1]]))
    (t/is= [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0]]
      (mx/concat-columns [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
    (t/is= [[1.0 2.0]] (mx/concat-columns [[1.0]] [[2.0]]))
    (t/is= [[1.0 1.0 0.5] [0.5 2.0 4.0]] (mx/concat-columns [[1.0] [0.5]] [[1.0 0.5] [2.0 4.0]]))
    (t/is= [[1.0 1.0] [0.5 0.5]] (mx/concat-columns [[1.0] [0.5]] [[1.0] [0.5]]))
    (t/is= [[1.0 1.0 1.0] [0.5 0.5 0.5]]
      (mx/concat-columns [[1.0] [0.5]] [[1.0] [0.5]] [[1.0] [0.5]]))))

(deftest merge-matrices-test
  (t/with-instrument `mx/merge-matrices
    (t/is-spec-check mx/merge-matrices))
  (t/with-instrument :all
    (t/is= [[]]
      (mx/merge-matrices {::mx/top-left     [[]]
                          ::mx/top-right    [[]]
                          ::mx/bottom-left  [[]]
                          ::mx/bottom-right [[]]}))
    (t/is= nil
      (mx/merge-matrices {::mx/top-left     [[]]
                          ::mx/top-right    [[]]
                          ::mx/bottom-left  [[]]
                          ::mx/bottom-right [[1]]}))
    (t/is=
      [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0] [1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0]]
      (mx/merge-matrices {::mx/top-left     [[1.0 0.5] [2.0 4.0]]
                          ::mx/top-right    [[1.0 0.5] [2.0 4.0]]
                          ::mx/bottom-left  [[1.0 0.5] [2.0 4.0]]
                          ::mx/bottom-right [[1.0 0.5] [2.0 4.0]]}))
    (t/is= [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0] [1.0 0.5 1.0 0.5]]
      (mx/merge-matrices {::mx/top-left     [[1.0 0.5] [2.0 4.0]]
                          ::mx/top-right    [[1.0 0.5] [2.0 4.0]]
                          ::mx/bottom-left  [[1.0 0.5]]
                          ::mx/bottom-right [[1.0 0.5]]}))))

(deftest replace-submatrix-test
  (t/with-instrument `mx/replace-submatrix
    (t/is-spec-check mx/replace-submatrix))
  (t/with-instrument :all
    (t/is= [[]] (mx/replace-submatrix [[]] [[]] 1 0))
    (t/is= [[0.0 0.0] [1.0 0.5] [2.0 4.0]]
      (mx/replace-submatrix [[]] [[1.0 0.5] [2.0 4.0]] 1 0))
    (t/is= [[1.0 0.5] [2.0 4.0]]
      (mx/replace-submatrix [[1.0 0.5] [2.0 4.0]] [[]] 1 0))
    (t/is= [[0.0 1.0 2.0] [1.0 0.5 5.0] [2.0 4.0 8.0]]
      (mx/replace-submatrix [[0.0 1.0 2.0] [3.0 4.0 5.0] [6.0 7.0 8.0]]
        [[1.0 0.5] [2.0 4.0]]
        1
        0))
    (t/is= [[0.0 1.0 0.5] [3.0 2.0 4.0] [6.0 7.0 8.0]]
      (mx/replace-submatrix [[0.0 1.0 2.0] [3.0 4.0 5.0] [6.0 7.0 8.0]]
        [[1.0 0.5] [2.0 4.0]]
        0
        1))
    (t/is= [[0.0 1.0 2.0] [3.0 4.0 5.0] [1.0 0.5 8.0] [2.0 4.0 0.0]]
      (mx/replace-submatrix [[0.0 1.0 2.0] [3.0 4.0 5.0] [6.0 7.0 8.0]]
        [[1.0 0.5] [2.0 4.0]]
        2
        0))
    (t/is= [[0.0 0.0 0.0 1.0 0.5]
            [0.0 1.0 2.0 2.0 4.0]
            [3.0 4.0 5.0 0.0 0.0]
            [6.0 7.0 8.0 0.0 0.0]]
      (mx/replace-submatrix [[0.0 1.0 2.0] [3.0 4.0 5.0] [6.0 7.0 8.0]]
        [[1.0 0.5] [2.0 4.0]]
        -1
        3))))

(deftest symmetric-matrix-by-averaging-test
  (t/with-instrument `mx/symmetric-matrix-by-averaging
    (t/is-spec-check mx/symmetric-matrix-by-averaging))
  (t/with-instrument :all
    (t/is= [[]] (mx/symmetric-matrix-by-averaging [[]]))
    (t/is= [[3]] (mx/symmetric-matrix-by-averaging [[3]]))
    (t/is= [[1.0 1.25] [1.25 4.0]] (mx/symmetric-matrix-by-averaging [[1.0 0.5] [2.0 4.0]]))))

;;;MATH
(deftest mx*-test
  (t/with-instrument `mx/mx*
    (t/is-spec-check mx/mx*))
  (t/with-instrument :all
    (t/is= [[]] (mx/mx* [[]]))
    (t/is= [[1]] (mx/mx* [[1]]))
    (t/is= [[]] (mx/mx* [[]] [[]]))
    (t/is= [[3.0] [3.0]] (mx/mx* [[1 1 1] [1 1 1]] [[1] [1] [1]]))
    (t/is= [[2.0 2.5] [10.0 17.0]] (mx/mx* [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
    (t/is= [[7.0 11.0] [44.0 73.0]]
      (mx/mx* [[1.0 0.5] [2.0 4.0]]
        [[1.0 0.5] [2.0 4.0]]
        [[1.0 0.5] [2.0 4.0]]))))

(deftest kronecker-product-test
  (t/with-instrument `mx/kronecker-product
    (t/is-spec-check mx/kronecker-product))
  (t/with-instrument :all
    (t/is= [[]] (mx/kronecker-product))
    (t/is= [[]] (mx/kronecker-product [[]]))
    (t/is= [[1]] (mx/kronecker-product [[1]]))
    (t/is= [[]] (mx/kronecker-product [[]] [[1]]))
    (t/is= [[1.0 0.5 0.5 0.25]
            [2.0 4.0 1.0 2.0]
            [2.0 1.0 4.0 2.0]
            [4.0 8.0 8.0 16.0]]
      (mx/kronecker-product [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
    (t/is= [[1.0 0.5 0.5 0.25 0.5 0.25 0.25 0.125]
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
    (t/is= [[1.0 0.5 0.5 0.25]] (mx/kronecker-product [[1.0 0.5]] [[1.0 0.5]]))
    (t/is= [[1.0 0.5 0.5 0.25 0.5 0.25 0.25 0.125]]
      (mx/kronecker-product [[1.0 0.5]] [[1.0 0.5]] [[1.0 0.5]]))))

;;;ROUNDING
(deftest round-roughly-zero-rows-test
  (t/with-instrument `mx/round-roughly-zero-rows
    (t/is-spec-check mx/round-roughly-zero-rows))
  (t/with-instrument :all
    (t/is= [[]] (mx/round-roughly-zero-rows [[]] 1e-6))
    (t/is= [[1.0 0.5] [2.0 4.0]] (mx/round-roughly-zero-rows [[1.0 0.5] [2.0 4.0]] 1e-6))
    (t/is= [[0.0 0.0] [1.0 1.0E-17]] (mx/round-roughly-zero-rows [[1e-13 1e-8] [1.0 1e-17]] 1e-6))))

(deftest round-roughly-zero-columns-test
  (t/with-instrument `mx/round-roughly-zero-columns
    (t/is-spec-check mx/round-roughly-zero-columns))
  (t/with-instrument :all
    (t/is= [[]] (mx/round-roughly-zero-columns [[]] 1e-6))
    (t/is= [[1.0 0.5] [2.0 4.0]] (mx/round-roughly-zero-columns [[1.0 0.5] [2.0 4.0]] 1e-6))
    (t/is= [[1.0E-13 0.0] [1.0 0.0]]
      (mx/round-roughly-zero-columns [[1e-13 1e-8] [1.0 1e-17]] 1e-6))))
