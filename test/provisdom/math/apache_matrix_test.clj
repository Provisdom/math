(ns provisdom.math.apache-matrix-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.apache-matrix :as apache-mx]
    [provisdom.math.core :as m]
    [provisdom.math.matrix :as mx]
    [provisdom.math.random :as random]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;;;210 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

;;;TYPES
(deftest apache-matrix?-test
  (is (spec-check apache-mx/apache-matrix))
  (is (apache-mx/apache-matrix? (apache-mx/apache-matrix [[]])))
  (is (apache-mx/apache-matrix? (apache-mx/apache-matrix [[1]])))
  (is (apache-mx/apache-matrix? (apache-mx/apache-matrix [[1 2] [3 4]])))
  (is-not (apache-mx/apache-matrix? "A"))
  (is-not (apache-mx/apache-matrix? [[1 2] [3 4]])))

(deftest empty-apache-matrix?-test
  (is (spec-check apache-mx/empty-apache-matrix?))
  (is-not (apache-mx/empty-apache-matrix? []))
  (is (apache-mx/empty-apache-matrix? (apache-mx/apache-matrix [[]])))
  (is-not (apache-mx/empty-apache-matrix? (apache-mx/apache-matrix [[1]])))
  (is-not (apache-mx/empty-apache-matrix? [[] [2]]))
  (is-not (apache-mx/empty-apache-matrix? (apache-mx/apache-matrix [[m/nan]]))))

(deftest apache-matrix-finite?-test
  (is (spec-check apache-mx/apache-matrix-finite?))
  (is (apache-mx/apache-matrix-finite? (apache-mx/apache-matrix [[]])))
  (is (apache-mx/apache-matrix-finite? (apache-mx/apache-matrix [[1]])))
  (is-not (apache-mx/apache-matrix-finite? (apache-mx/apache-matrix [[m/inf+]])))
  (is-not (apache-mx/apache-matrix-finite?
            (apache-mx/apache-matrix [[1 2] [3 m/inf+]])))
  (is-not (apache-mx/apache-matrix-finite? "A"))
  (is-not (apache-mx/apache-matrix-finite? [[1 2] [3 4]])))

(deftest square-apache-matrix?-test
  (is (spec-check apache-mx/square-apache-matrix?))
  (is (apache-mx/square-apache-matrix? (apache-mx/apache-matrix [[]])))
  (is (apache-mx/square-apache-matrix? (apache-mx/apache-matrix [[1]])))
  (is-not (apache-mx/square-apache-matrix? (apache-mx/apache-matrix [[1 1]])))
  (is-not (apache-mx/square-apache-matrix? (apache-mx/apache-matrix [[1] [1]])))
  (is (apache-mx/square-apache-matrix?
        (apache-mx/apache-matrix [[1 1] [1 1]]))))

(deftest diagonal-apache-matrix?-test
  (is (spec-check apache-mx/diagonal-apache-matrix?))
  (is (apache-mx/diagonal-apache-matrix? (apache-mx/apache-matrix [[]])))
  (is (apache-mx/diagonal-apache-matrix? (apache-mx/apache-matrix [[1]])))
  (is-not (apache-mx/diagonal-apache-matrix? (apache-mx/apache-matrix [[1 1]])))
  (is-not (apache-mx/diagonal-apache-matrix?
            (apache-mx/apache-matrix [[1] [1]])))
  (is-not (apache-mx/diagonal-apache-matrix?
            (apache-mx/apache-matrix [[1 1] [1 1]])))
  (is (apache-mx/diagonal-apache-matrix?
        (apache-mx/apache-matrix [[1 0] [0 1]]))))

(deftest upper-triangular-apache-matrix?-test
  (is (spec-check apache-mx/upper-triangular-apache-matrix?))
  (is (apache-mx/upper-triangular-apache-matrix?
        (apache-mx/apache-matrix [[]])))
  (is (apache-mx/upper-triangular-apache-matrix?
        (apache-mx/apache-matrix [[1]])))
  (is-not (apache-mx/upper-triangular-apache-matrix?
            (apache-mx/apache-matrix [[1 1]])))
  (is-not (apache-mx/upper-triangular-apache-matrix?
            (apache-mx/apache-matrix [[1] [1]])))
  (is-not (apache-mx/upper-triangular-apache-matrix?
            (apache-mx/apache-matrix [[1 1] [1 1]])))
  (is (apache-mx/upper-triangular-apache-matrix?
        (apache-mx/apache-matrix [[1 0] [0 1]])))
  (is (apache-mx/upper-triangular-apache-matrix?
        (apache-mx/apache-matrix [[1 1] [0 1]]))))

(deftest lower-triangular-apache-matrix?-test
  (is (spec-check apache-mx/lower-triangular-apache-matrix?))
  (is (apache-mx/lower-triangular-apache-matrix?
        (apache-mx/apache-matrix [[]])))
  (is (apache-mx/lower-triangular-apache-matrix?
        (apache-mx/apache-matrix [[1]])))
  (is-not (apache-mx/lower-triangular-apache-matrix?
            (apache-mx/apache-matrix [[1 1]])))
  (is-not (apache-mx/lower-triangular-apache-matrix?
            (apache-mx/apache-matrix [[1] [1]])))
  (is-not (apache-mx/lower-triangular-apache-matrix?
            (apache-mx/apache-matrix [[1 1] [1 1]])))
  (is (apache-mx/lower-triangular-apache-matrix?
        (apache-mx/apache-matrix [[1 0] [0 1]])))
  (is (apache-mx/lower-triangular-apache-matrix?
        (apache-mx/apache-matrix [[1 0] [1 1]]))))

(deftest symmetric-apache-matrix?-test
  (is (spec-check apache-mx/symmetric-apache-matrix?))
  (is (apache-mx/symmetric-apache-matrix? (apache-mx/apache-matrix [[]])))
  (is (apache-mx/symmetric-apache-matrix? (apache-mx/apache-matrix [[1]])))
  (is-not (apache-mx/symmetric-apache-matrix?
            (apache-mx/apache-matrix [[1 1]])))             ;?
  (is-not (apache-mx/symmetric-apache-matrix?
            (apache-mx/apache-matrix [[1] [1]])))
  (is (apache-mx/symmetric-apache-matrix?
        (apache-mx/apache-matrix [[1 1] [1 1]])))
  (is (apache-mx/symmetric-apache-matrix?
        (apache-mx/apache-matrix [[1 0] [0 1]])))
  (is-not (apache-mx/symmetric-apache-matrix?
            (apache-mx/apache-matrix [[1 0] [1 1]]))))

(deftest positive-semidefinite-apache-matrix-finite?-test
  (is (spec-check apache-mx/positive-semidefinite-apache-matrix-finite?))
  (is (apache-mx/positive-semidefinite-apache-matrix-finite?
        (apache-mx/apache-matrix [[]])
        m/dbl-close))
  (is (apache-mx/positive-semidefinite-apache-matrix-finite?
        (apache-mx/apache-matrix [[0.0]])
        m/dbl-close))
  (is (apache-mx/positive-semidefinite-apache-matrix-finite?
        (apache-mx/apache-matrix [[1.0]])
        m/dbl-close))
  (is (apache-mx/positive-semidefinite-apache-matrix-finite?
        (apache-mx/apache-matrix [[0.0 0.0] [0.0 0.0]])
        m/dbl-close))
  (is-not (apache-mx/positive-semidefinite-apache-matrix-finite?
            (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
            m/dbl-close))
  (is (apache-mx/positive-semidefinite-apache-matrix-finite?
        (apache-mx/apache-matrix [[1.0 0.5] [0.5 2.0]])
        m/dbl-close))
  (is-not (apache-mx/positive-semidefinite-apache-matrix-finite?
            (apache-mx/apache-matrix [[1.0 -1.1] [-1.1 1.0]])
            m/dbl-close))
  (is (apache-mx/positive-semidefinite-apache-matrix-finite?
        (apache-mx/apache-matrix [[1.0 -1.0] [-1.0 1.0]])
        m/dbl-close))
  (is (apache-mx/positive-semidefinite-apache-matrix-finite?
        (apache-mx/apache-matrix [[1.0 (m/next-down -1.0)] [(m/next-down -1.0) 1.0]])
        m/dbl-close))
  (is-not (apache-mx/positive-semidefinite-apache-matrix-finite?
            (apache-mx/apache-matrix [[1.0 (+ -1.0 -1.0E-14)] [(+ -1.0 -1.0E-14) 1.0]])
            m/dbl-close))
  (is (apache-mx/positive-semidefinite-apache-matrix-finite?
        (apache-mx/apache-matrix [[1.0 (+ -1.0 -1.0E-14)] [(+ -1.0 -1.0E-14) 1.0]])
        m/sgl-close)))

(deftest positive-definite-apache-matrix-finite?-test
  (is (spec-check apache-mx/positive-definite-apache-matrix-finite?))
  (is (apache-mx/positive-definite-apache-matrix-finite?
        (apache-mx/apache-matrix [[]])
        m/sgl-close))
  (is-not (apache-mx/positive-definite-apache-matrix-finite?
            (apache-mx/apache-matrix [[0.0]])
            m/sgl-close))
  (is (apache-mx/positive-definite-apache-matrix-finite?
        (apache-mx/apache-matrix [[1.0]])
        m/sgl-close))
  (is-not (apache-mx/positive-definite-apache-matrix-finite?
            (apache-mx/apache-matrix [[0.0 0.0] [0.0 0.0]])
            m/sgl-close))
  (is-not (apache-mx/positive-definite-apache-matrix-finite?
            (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
            m/sgl-close))
  (is (apache-mx/positive-definite-apache-matrix-finite?
        (apache-mx/apache-matrix [[1.0 0.5] [0.5 1.0]])
        m/sgl-close))
  (is-not (apache-mx/positive-definite-apache-matrix-finite?
            (apache-mx/apache-matrix [[1.0 -1.1] [-1.1 1.0]])
            m/sgl-close))
  (is-not (apache-mx/positive-definite-apache-matrix-finite?
            (apache-mx/apache-matrix [[1.0 -1.0] [-1.0 1.0]])
            m/sgl-close))
  (is (apache-mx/positive-definite-apache-matrix-finite?
        (apache-mx/apache-matrix [[1.0 -1.0] [-1.0 1.0]])
        1e-40))
  (is-not (apache-mx/positive-definite-apache-matrix-finite?
            (apache-mx/apache-matrix [[(m/next-up 1.0) -1.0] [-1.0 (m/next-up 1.0)]])
            m/sgl-close))
  (is-not (apache-mx/positive-definite-apache-matrix-finite?
            (apache-mx/apache-matrix [[(inc 1.0E-14) -1.0] [-1.0 (inc 1.0E-14)]])
            m/sgl-close))
  (is (apache-mx/positive-definite-apache-matrix-finite?
        (apache-mx/apache-matrix [[(inc 1.0E-14) -1.0] [-1.0 (inc 1.0E-14)]])
        m/dbl-close)))

(deftest correlation-apache-matrix?-test
  (is (spec-check apache-mx/correlation-apache-matrix?))
  (is (apache-mx/correlation-apache-matrix?
        (apache-mx/apache-matrix [[]])
        m/sgl-close))
  (is (apache-mx/correlation-apache-matrix?
        (apache-mx/apache-matrix [[1.0]])
        m/sgl-close))
  (is (apache-mx/correlation-apache-matrix?
        (apache-mx/apache-matrix [[1.0 0.2] [0.2 1.0]])
        m/sgl-close))
  (is-not (apache-mx/correlation-apache-matrix?
            (apache-mx/apache-matrix [[0.2]])
            m/sgl-close))
  (is-not (apache-mx/correlation-apache-matrix?
            (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
            m/sgl-close))
  (is-not (apache-mx/correlation-apache-matrix?
            (apache-mx/apache-matrix [[1.0 0.5] [0.5 2.0]])
            m/sgl-close))
  (is-not (apache-mx/correlation-apache-matrix?
            (apache-mx/apache-matrix [[1.0 -1.1] [-1.1 1.0]])
            m/sgl-close))
  (is-not (apache-mx/correlation-apache-matrix?
            (apache-mx/apache-matrix [[1.0 -1.0] [-1.0 1.0]])
            m/sgl-close))
  (is-not (apache-mx/correlation-apache-matrix?
            (apache-mx/apache-matrix [[(m/next-up 1.0) -1.0] [-1.0 (m/next-up 1.0)]])
            m/sgl-close))
  (is-not (apache-mx/correlation-apache-matrix?
            (apache-mx/apache-matrix [[1.0 -1.0] [-1.0 1.0]])
            m/sgl-close))
  (is (apache-mx/correlation-apache-matrix?
        (apache-mx/apache-matrix [[1.0 -1.0] [-1.0 1.0]])
        1e-40)))

;;;CONSTRUCTORS
(deftest apache-matrix-&-apache-matrix->matrix-test
  (is (spec-check apache-mx/apache-matrix))
  (is (spec-check apache-mx/apache-matrix->matrix))
  (is= [[]] (apache-mx/apache-matrix->matrix (apache-mx/apache-matrix [[]])))
  (is= [[1.0]]
       (apache-mx/apache-matrix->matrix (apache-mx/apache-matrix [[1.0]])))
  (is= [[1.0 2.0]]
       (apache-mx/apache-matrix->matrix (apache-mx/apache-matrix [[1.0 2.0]])))
  (is= [[1.0] [2.0]]
       (apache-mx/apache-matrix->matrix
         (apache-mx/apache-matrix [[1.0] [2.0]])))
  (is= [[1.0 0.5] [2.0 4.0]]
       (apache-mx/apache-matrix->matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]]))))

(deftest positive-semidefinite-apache-matrix-finite-by-squaring-test
  (is (spec-check apache-mx/positive-semidefinite-apache-matrix-finite-by-squaring
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 150}}))
  (is= (apache-mx/apache-matrix [[]])
       (apache-mx/positive-semidefinite-apache-matrix-finite-by-squaring
         (apache-mx/apache-matrix [[]])))
  (is= (apache-mx/apache-matrix [[4]])
       (apache-mx/positive-semidefinite-apache-matrix-finite-by-squaring
         (apache-mx/apache-matrix [[2]])))
  (is= (apache-mx/apache-matrix [[5.0 11.0] [11.0 25.0]])
       (apache-mx/positive-semidefinite-apache-matrix-finite-by-squaring
         (apache-mx/apache-matrix [[1 2] [3 4]]))))

(deftest positive-definite-apache-matrix-finite-by-squaring-test
  (is (spec-check apache-mx/positive-definite-apache-matrix-finite-by-squaring
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 150}}))
  (is= (apache-mx/apache-matrix [[]])
       (apache-mx/positive-definite-apache-matrix-finite-by-squaring
         (apache-mx/apache-matrix [[]])))
  (is= (apache-mx/apache-matrix [[4]])
       (apache-mx/positive-definite-apache-matrix-finite-by-squaring
         (apache-mx/apache-matrix [[2]])))
  (is= [[1.0E-4 3.0E-17] [3.0E-17 1.0000000000008E-4]]
       (apache-mx/apache-matrix->matrix
         (apache-mx/positive-definite-apache-matrix-finite-by-squaring
           (apache-mx/apache-matrix [[0 0] [3 4]])))))

(deftest correlation-apache-matrix-by-squaring-test
  (is (spec-check apache-mx/correlation-apache-matrix-by-squaring
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 150}}))
  (is= (apache-mx/apache-matrix [[]])
       (apache-mx/correlation-apache-matrix-by-squaring
         (apache-mx/apache-matrix [[]])))
  (is= (apache-mx/apache-matrix [[1.0]])
       (apache-mx/correlation-apache-matrix-by-squaring
         (apache-mx/apache-matrix [[2]])))
  (is= [[1.0 0.9838699100999075] [0.9838699100999075 1.0]]
       (apache-mx/apache-matrix->matrix
         (apache-mx/correlation-apache-matrix-by-squaring
           (apache-mx/apache-matrix [[1 2] [3 4]]))))
  (is= [[1.0 2.9999999999988E-13] [2.9999999999988E-13 1.0]]
       (apache-mx/apache-matrix->matrix
         (apache-mx/correlation-apache-matrix-by-squaring
           (apache-mx/apache-matrix [[0 0] [3 4]])))))

(deftest rnd-positive-definite-apache-matrix-finite!-test
  (is (spec-check apache-mx/rnd-positive-definite-apache-matrix-finite!
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 250}}))
  (random/bind-seed 0
    (is= (apache-mx/apache-matrix [[]])
         (apache-mx/rnd-positive-definite-apache-matrix-finite! 0)))
  (random/bind-seed 0
    (is= (apache-mx/apache-matrix [[0.8833108082136426]])
         (apache-mx/rnd-positive-definite-apache-matrix-finite! 1)))
  (random/bind-seed 0
    (is= (apache-mx/apache-matrix [[0.6946098792362991 0.3550851337817903]
                                   [0.3550851337817903 0.21513470056994127]])
         (apache-mx/rnd-positive-definite-apache-matrix-finite! 2))))

(deftest rnd-correlation-apache-matrix!-test
  (is (spec-check apache-mx/rnd-correlation-apache-matrix!
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 250}}))
  (random/bind-seed 0
    (is= (apache-mx/apache-matrix [[]])
         (apache-mx/rnd-correlation-apache-matrix! 0)))
  (random/bind-seed 0
    (is= (apache-mx/apache-matrix [[1.0]])
         (apache-mx/rnd-correlation-apache-matrix! 1)))
  (random/bind-seed 0
    (is= [[1.0 0.9185584128047] [0.9185584128047 1.0]]
         (apache-mx/apache-matrix->matrix
           (apache-mx/rnd-correlation-apache-matrix! 2)))))

;;;INFO
(deftest rows-test
  (is (spec-check apache-mx/rows))
  (is= 0 (apache-mx/rows (apache-mx/apache-matrix [[]])))
  (is= 1 (apache-mx/rows (apache-mx/apache-matrix [[1.0]])))
  (is= 1 (apache-mx/rows (apache-mx/apache-matrix [[1.0 2.0]])))
  (is= 2 (apache-mx/rows (apache-mx/apache-matrix [[1.0] [2.0]])))
  (is= 2 (apache-mx/rows (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]]))))

(deftest columns-test
  (is (spec-check apache-mx/columns))
  (is= 0 (apache-mx/columns (apache-mx/apache-matrix [[]])))
  (is= 1 (apache-mx/columns (apache-mx/apache-matrix [[1.0]])))
  (is= 2 (apache-mx/columns (apache-mx/apache-matrix [[1.0 2.0]])))
  (is= 1 (apache-mx/columns (apache-mx/apache-matrix [[1.0] [2.0]])))
  (is= 2 (apache-mx/columns (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]]))))

(deftest get-entry-test
  (is (spec-check apache-mx/get-entry))
  (is= 1.0 (apache-mx/get-entry (apache-mx/apache-matrix [[1.0]]) 0 0))
  (is= 2.0 (apache-mx/get-entry (apache-mx/apache-matrix [[1.0 2.0]]) 0 1))
  (is= 4.0
       (apache-mx/get-entry (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
                            1
                            1)))

(deftest get-row-test
  (is (spec-check apache-mx/get-row))
  (is= [1.0] (apache-mx/get-row (apache-mx/apache-matrix [[1.0]]) 0))
  (is= [2.0] (apache-mx/get-row (apache-mx/apache-matrix [[1.0] [2.0]]) 1))
  (is= [1.0 0.5]
       (apache-mx/get-row (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
                          0)))

(deftest get-column-test
  (is (spec-check apache-mx/get-column))
  (is= [1.0] (apache-mx/get-column (apache-mx/apache-matrix [[1.0]]) 0))
  (is= [2.0] (apache-mx/get-column (apache-mx/apache-matrix [[1.0 2.0]]) 1))
  (is= [1.0 2.0]
       (apache-mx/get-column
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         0)))

(deftest diagonal-test
  (is (spec-check apache-mx/diagonal))
  (is= [] (apache-mx/diagonal (apache-mx/apache-matrix [[]])))
  (is= [1.0] (apache-mx/diagonal (apache-mx/apache-matrix [[1.0]])))
  (is= [1.0] (apache-mx/diagonal (apache-mx/apache-matrix [[1.0 2.0]])))
  (is= [1.0] (apache-mx/diagonal (apache-mx/apache-matrix [[1.0] [2.0]])))
  (is= [1.0 4.0]
       (apache-mx/diagonal (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]]))))

(deftest trace-test
  (is (spec-check apache-mx/trace))
  (is= 0.0 (apache-mx/trace (apache-mx/apache-matrix [[]])))
  (is= 1.0 (apache-mx/trace (apache-mx/apache-matrix [[1]])))
  (is= 5.0 (apache-mx/trace (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]]))))

(deftest get-slices-as-matrix-test
  (is (spec-check apache-mx/get-slices-as-matrix))
  (is= (apache-mx/apache-matrix [[1.0 0.5]])
       (apache-mx/get-slices-as-matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         {::mx/row-indices 0}))
  (is= (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
       (apache-mx/get-slices-as-matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         {::mx/row-indices [0 1]}))
  (is= (apache-mx/apache-matrix [[2.0 4.0] [1.0 0.5]])
       (apache-mx/get-slices-as-matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         {::mx/row-indices [1 0]}))
  (is= (apache-mx/apache-matrix [[1.0] [2.0]])
       (apache-mx/get-slices-as-matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         {::mx/column-indices 0}))
  (is= (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
       (apache-mx/get-slices-as-matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         {::mx/column-indices [0 1]}))
  (is= (apache-mx/apache-matrix [[0.5 1.0] [4.0 2.0]])
       (apache-mx/get-slices-as-matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         {::mx/column-indices [1 0]}))
  (is= (apache-mx/apache-matrix [[2.0 4.0]])
       (apache-mx/get-slices-as-matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         {::mx/exception-row-indices 0}))
  (is= (apache-mx/apache-matrix [[]])
       (apache-mx/get-slices-as-matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         {::mx/exception-row-indices [0 1]}))
  (is= (apache-mx/apache-matrix [[0.5] [4.0]])
       (apache-mx/get-slices-as-matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         {::mx/exception-column-indices 0}))
  (is= (apache-mx/apache-matrix [[]])
       (apache-mx/get-slices-as-matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         {::mx/exception-column-indices [0 1]}))
  (is= (apache-mx/apache-matrix [[]])
       (apache-mx/get-slices-as-matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         {::mx/row-indices 0 ::mx/exception-row-indices 0}))
  (is= (apache-mx/apache-matrix [[]])
       (apache-mx/get-slices-as-matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         {::mx/row-indices [0] ::mx/exception-row-indices 0}))
  (is= (apache-mx/apache-matrix [[]])
       (apache-mx/get-slices-as-matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         {::mx/row-indices 0 ::mx/exception-row-indices [0]}))
  (is= (apache-mx/apache-matrix [[1.0]])
       (apache-mx/get-slices-as-matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         {::mx/exception-row-indices 1 ::mx/exception-column-indices 1}))
  (is= (apache-mx/apache-matrix [[1.0]])
       (apache-mx/get-slices-as-matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         {::mx/row-indices 0 ::mx/column-indices 0})))

(def s
  (apache-mx/apache-matrix [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                            [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]]))

(deftest matrix-partition-test
  (is (spec-check apache-mx/matrix-partition))
  (is= {::apache-mx/bottom-left  (apache-mx/apache-matrix [[9.0 10.0] [13.0 14.0]])
        ::apache-mx/bottom-right (apache-mx/apache-matrix [[11.0 12.0] [15.0 16.0]])
        ::apache-mx/top-left     (apache-mx/apache-matrix [[1.0 2.0] [5.0 6.0]])
        ::apache-mx/top-right    (apache-mx/apache-matrix [[3.0 4.0] [7.0 8.0]])}
       (apache-mx/matrix-partition s 2 2))
  (is= {::apache-mx/bottom-left  (apache-mx/apache-matrix [[5.0] [9.0] [13.0]])
        ::apache-mx/bottom-right (apache-mx/apache-matrix
                                   [[6.0 7.0 8.0] [10.0 11.0 12.0] [14.0 15.0 16.0]])
        ::apache-mx/top-left     (apache-mx/apache-matrix [[1.0]])
        ::apache-mx/top-right    (apache-mx/apache-matrix [[2.0 3.0 4.0]])}
       (apache-mx/matrix-partition s 1 1))
  (is= {::apache-mx/bottom-left  (apache-mx/apache-matrix
                                   [[1.0 2.0 3.0] [5.0 6.0 7.0]
                                    [9.0 10.0 11.0] [13.0 14.0 15.0]])
        ::apache-mx/bottom-right (apache-mx/apache-matrix
                                   [[4.0] [8.0] [12.0] [16.0]])
        ::apache-mx/top-left     (apache-mx/apache-matrix [[]])
        ::apache-mx/top-right    (apache-mx/apache-matrix [[]])}
       (apache-mx/matrix-partition s 0 3))
  (is= {::apache-mx/bottom-left  (apache-mx/apache-matrix [[]])
        ::apache-mx/bottom-right (apache-mx/apache-matrix [[13.0 14.0 15.0 16.0]])
        ::apache-mx/top-left     (apache-mx/apache-matrix [[]])
        ::apache-mx/top-right    (apache-mx/apache-matrix
                                   [[1.0 2.0 3.0 4.0]
                                    [5.0 6.0 7.0 8.0]
                                    [9.0 10.0 11.0 12.0]])}
       (apache-mx/matrix-partition s 3 0))
  (is= {::apache-mx/bottom-left  (apache-mx/apache-matrix
                                   [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                                    [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]])
        ::apache-mx/bottom-right (apache-mx/apache-matrix [[]])
        ::apache-mx/top-left     (apache-mx/apache-matrix [[]])
        ::apache-mx/top-right    (apache-mx/apache-matrix [[]])}
       (apache-mx/matrix-partition s 0 4))
  (is= {::apache-mx/bottom-left  (apache-mx/apache-matrix [[]])
        ::apache-mx/bottom-right (apache-mx/apache-matrix [[]])
        ::apache-mx/top-left     (apache-mx/apache-matrix [[]])
        ::apache-mx/top-right    (apache-mx/apache-matrix
                                   [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                                    [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]])}
       (apache-mx/matrix-partition s 4 0))
  (is= {::apache-mx/bottom-left  (apache-mx/apache-matrix [[]])
        ::apache-mx/bottom-right (apache-mx/apache-matrix
                                   [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                                    [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]])
        ::apache-mx/top-left     (apache-mx/apache-matrix [[]])
        ::apache-mx/top-right    (apache-mx/apache-matrix [[]])}
       (apache-mx/matrix-partition s 0 0))
  (is= {::apache-mx/bottom-left  (apache-mx/apache-matrix [[]])
        ::apache-mx/bottom-right (apache-mx/apache-matrix [[]])
        ::apache-mx/top-left     (apache-mx/apache-matrix
                                   [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                                    [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]])
        ::apache-mx/top-right    (apache-mx/apache-matrix [[]])}
       (apache-mx/matrix-partition s 4 4)))

(deftest some-kv-test
  (is (spec-check apache-mx/some-kv))
  (is= 0.5
       (apache-mx/some-kv (fn [row column number]
                            (> (+ row column) number))
                          (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])))
  (is= 0.6
       (apache-mx/some-kv
         (fn [row column number]
           (> (+ row column) number))
         (apache-mx/apache-matrix [[1.0 0.5] [0.6 4.0]])
         {::mx/by-row? false})))

;;;MANIPULATION
(deftest transpose-test
  (is (spec-check apache-mx/transpose))
  (is= (apache-mx/apache-matrix [[]])
       (apache-mx/transpose (apache-mx/apache-matrix [[]])))
  (is= (apache-mx/apache-matrix [[1.0]])
       (apache-mx/transpose (apache-mx/apache-matrix [[1.0]])))
  (is= (apache-mx/apache-matrix [[1.0] [2.0]])
       (apache-mx/transpose (apache-mx/apache-matrix [[1.0 2.0]])))
  (is= (apache-mx/apache-matrix [[1.0 2.0]])
       (apache-mx/transpose (apache-mx/apache-matrix [[1.0] [2.0]])))
  (is= (apache-mx/apache-matrix [[1.0 2.0] [0.5 4.0]])
       (apache-mx/transpose (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]]))))

(deftest assoc-entry!-test
  (is (spec-check apache-mx/assoc-entry!))
  (is= (apache-mx/apache-matrix [[1.0]])
       (let [a (apache-mx/apache-matrix [[0.0]])]
         (apache-mx/assoc-entry! a 0 0 1.0)
         a))
  (is= (apache-mx/apache-matrix [[1 2] [8 4]])
       (let [a (apache-mx/apache-matrix [[1 2] [3 4]])]
         (apache-mx/assoc-entry! a 1 0 8)
         a)))

(deftest assoc-diagonal!-test
  (is (spec-check apache-mx/assoc-diagonal!))
  (is= (apache-mx/apache-matrix [[2.0]])
       (let [a (apache-mx/apache-matrix [[0.0]])]
         (apache-mx/assoc-diagonal! a [2.0])
         a))
  (is= (apache-mx/apache-matrix [[5 2] [3 6]])
       (let [a (apache-mx/apache-matrix [[1 2] [3 4]])]
         (apache-mx/assoc-diagonal! a [5 6])
         a)))

(deftest symmetric-apache-matrix-by-averaging!-test
  (is (spec-check apache-mx/symmetric-apache-matrix-by-averaging!))
  (is= (apache-mx/apache-matrix [[0.0]])
       (let [a (apache-mx/apache-matrix [[0.0]])]
         (apache-mx/symmetric-apache-matrix-by-averaging! a)
         a))

  (is= (apache-mx/apache-matrix [[1.0 2.5] [2.5 4.0]])
       (let [a (apache-mx/apache-matrix [[1 2] [3 4]])]
         (apache-mx/symmetric-apache-matrix-by-averaging! a)
         a)))

(deftest concat-rows-test
  (is (spec-check apache-mx/concat-rows))
  (is= (apache-mx/apache-matrix [[]])
       (apache-mx/concat-rows (apache-mx/apache-matrix [[]])
                              (apache-mx/apache-matrix [[]])))
  (is= nil
       (apache-mx/concat-rows (apache-mx/apache-matrix [[]])
                              (apache-mx/apache-matrix [[1]])))
  (is= (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0] [1.0 0.5] [2.0 4.0]])
       (apache-mx/concat-rows (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
                              (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])))
  (is= (apache-mx/apache-matrix [[1.0] [2.0]])
       (apache-mx/concat-rows (apache-mx/apache-matrix [[1.0]])
                              (apache-mx/apache-matrix [[2.0]])))
  (is= (apache-mx/apache-matrix [[1.0 0.5] [1.0 0.5] [2.0 4.0]])
       (apache-mx/concat-rows (apache-mx/apache-matrix [[1.0 0.5]])
                              (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])))
  (is= (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0] [1.0 0.5]])
       (apache-mx/concat-rows (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
                              (apache-mx/apache-matrix [[1.0 0.5]])))
  (is= (apache-mx/apache-matrix [[1.0 0.5] [1.0 0.5]])
       (apache-mx/concat-rows (apache-mx/apache-matrix [[1.0 0.5]])
                              (apache-mx/apache-matrix [[1.0 0.5]])))
  (is= (apache-mx/apache-matrix [[1.0 0.5] [1.0 0.5] [1.0 0.5]])
       (apache-mx/concat-rows (apache-mx/apache-matrix [[1.0 0.5]])
                              (apache-mx/apache-matrix [[1.0 0.5]])
                              (apache-mx/apache-matrix [[1.0 0.5]]))))

(deftest concat-columns-test
  (is (spec-check apache-mx/concat-columns))
  (is= (apache-mx/apache-matrix [[]])
       (apache-mx/concat-columns (apache-mx/apache-matrix [[]])
                                 (apache-mx/apache-matrix [[]])))
  (is= nil
       (apache-mx/concat-columns (apache-mx/apache-matrix [[]])
                                 (apache-mx/apache-matrix [[1]])))
  (is= (apache-mx/apache-matrix [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0]])
       (apache-mx/concat-columns (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
                                 (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])))
  (is= (apache-mx/apache-matrix [[1.0 2.0]])
       (apache-mx/concat-columns (apache-mx/apache-matrix [[1.0]])
                                 (apache-mx/apache-matrix [[2.0]])))
  (is= (apache-mx/apache-matrix [[1.0 1.0 0.5] [0.5 2.0 4.0]])
       (apache-mx/concat-columns (apache-mx/apache-matrix [[1.0] [0.5]])
                                 (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])))
  (is= (apache-mx/apache-matrix [[1.0 1.0] [0.5 0.5]])
       (apache-mx/concat-columns (apache-mx/apache-matrix [[1.0] [0.5]])
                                 (apache-mx/apache-matrix [[1.0] [0.5]])))
  (is= (apache-mx/apache-matrix [[1.0 1.0 1.0] [0.5 0.5 0.5]])
       (apache-mx/concat-columns (apache-mx/apache-matrix [[1.0] [0.5]])
                                 (apache-mx/apache-matrix [[1.0] [0.5]])
                                 (apache-mx/apache-matrix [[1.0] [0.5]]))))

(deftest correlation-apache-matrix->covariance-apache-matrix-test
  (is (spec-check apache-mx/correlation-apache-matrix->covariance-apache-matrix
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 150}}))
  (is= [[]]
       (apache-mx/apache-matrix->matrix
         (apache-mx/correlation-apache-matrix->covariance-apache-matrix
           (apache-mx/apache-matrix [[]])
           [])))
  (is= [[2.9999999999999996]]
       (apache-mx/apache-matrix->matrix
         (apache-mx/correlation-apache-matrix->covariance-apache-matrix
           (apache-mx/apache-matrix [[1.0]])
           [3.0])))
  (is= [[2.9999999999999996 1.161895003862225] [1.161895003862225 5.000000000000001]]
       (apache-mx/apache-matrix->matrix
         (apache-mx/correlation-apache-matrix->covariance-apache-matrix
           (apache-mx/apache-matrix [[1.0 0.3] [0.3 1.0]])
           [3.0 5.0]))))

(deftest covariance-apache-matrix->correlation-apache-matrix-test
  (is (spec-check apache-mx/covariance-apache-matrix->correlation-apache-matrix
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 150}}))
  (is= [[]]
       (apache-mx/apache-matrix->matrix
         (apache-mx/covariance-apache-matrix->correlation-apache-matrix
           (apache-mx/apache-matrix [[]]))))
  (is= [[1.0]]
       (apache-mx/apache-matrix->matrix
         (apache-mx/covariance-apache-matrix->correlation-apache-matrix
           (apache-mx/apache-matrix [[3.0]]))))
  (is= [[1.0 0.232379000772445] [0.232379000772445 1.0]]
       (apache-mx/apache-matrix->matrix
         (apache-mx/covariance-apache-matrix->correlation-apache-matrix
           (apache-mx/apache-matrix [[3.0 0.9] [0.9 5.0]])))))

;;;MATH
(deftest ===-test
  (is (spec-check apache-mx/===))
  (is (apache-mx/=== (apache-mx/apache-matrix [[1.0 0.5] [2.0 m/nan]])
                     (apache-mx/apache-matrix [[1.0 0.5] [2.0 m/nan]])))
  (is (apache-mx/=== (apache-mx/apache-matrix [[1.0 0.5] [2.0 m/nan]])
                     (apache-mx/apache-matrix [[1.0 0.5] [2.0 m/nan]])
                     (apache-mx/apache-matrix [[1.0 0.5] [2.0 m/nan]]))))

(deftest mx*-test
  (is (spec-check apache-mx/mx*))
  (is= (apache-mx/apache-matrix [[]])
       (apache-mx/mx* (apache-mx/apache-matrix [[]])
                      (apache-mx/apache-matrix [[]])))
  (is= (apache-mx/apache-matrix [[6.0]])
       (apache-mx/mx* (apache-mx/apache-matrix [[2.0]])
                      (apache-mx/apache-matrix [[3.0]])))
  (is= nil
       (apache-mx/mx* (apache-mx/apache-matrix [[2.0]])
                      (apache-mx/apache-matrix [[3.0] [4.0]])))
  (is= (apache-mx/apache-matrix [[26.0]])
       (apache-mx/mx* (apache-mx/apache-matrix [[2.0 4.0]])
                      (apache-mx/apache-matrix [[3.0] [5.0]])))
  (is= (apache-mx/apache-matrix [[19.0 22.0] [43.0 50.0]])
       (apache-mx/mx* (apache-mx/apache-matrix [[1.0 2.0] [3.0 4.0]])
                      (apache-mx/apache-matrix [[5.0 6.0] [7.0 8.0]])))
  (is= (apache-mx/apache-matrix [[67.0 78.0] [201.0 234.0]])
       (apache-mx/mx* (apache-mx/apache-matrix [[1.0] [3.0]])
                      (apache-mx/apache-matrix [[5.0 6.0]])
                      (apache-mx/apache-matrix [[5.0 6.0] [7.0 8.0]]))))

(deftest add-test
  (is (spec-check apache-mx/add))
  (is= (apache-mx/apache-matrix [[]])
       (apache-mx/add (apache-mx/apache-matrix [[]]) (apache-mx/apache-matrix [[]])))
  (is= nil
       (apache-mx/add (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
                      (apache-mx/apache-matrix [[1.0 0.5]])))
  (is= (apache-mx/apache-matrix [[2.0 1.0] [4.0 8.0]])
       (apache-mx/add (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
                      (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])))
  (is= (apache-mx/apache-matrix [[2.0 1.0]])
       (apache-mx/add (apache-mx/apache-matrix [[1.0 0.5]])
                      (apache-mx/apache-matrix [[1.0 0.5]])))
  (is= (apache-mx/apache-matrix [[1.0 2.0]])
       (apache-mx/add (apache-mx/apache-matrix [[1.0 2.0]]))))

(deftest subtract-test
  (is (spec-check apache-mx/subtract))
  (is= (apache-mx/apache-matrix [[]])
       (apache-mx/subtract (apache-mx/apache-matrix [[]])
                           (apache-mx/apache-matrix [[]])))
  (is= nil
       (apache-mx/subtract (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
                           (apache-mx/apache-matrix [[1.0 0.5]])))
  (is= (apache-mx/apache-matrix [[0.0 0.0] [0.0 0.0]])
       (apache-mx/subtract (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
                           (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])))
  (is= (apache-mx/apache-matrix [[0.0 0.0]])
       (apache-mx/subtract (apache-mx/apache-matrix [[1.0 0.5]])
                           (apache-mx/apache-matrix [[1.0 0.5]])))
  (is= (apache-mx/apache-matrix [[-1.0 -0.5]])
       (apache-mx/subtract (apache-mx/apache-matrix [[1.0 0.5]])
                           (apache-mx/apache-matrix [[1.0 0.5]])
                           (apache-mx/apache-matrix [[1.0 0.5]]))))

;;;DECOMPOSITION
(deftest inverse-test
  (is (spec-check apache-mx/inverse))
  (is= (apache-mx/apache-matrix [[]])
       (apache-mx/inverse (apache-mx/apache-matrix [[]])))
  (is= (apache-mx/apache-matrix [[2.0]])
       (apache-mx/inverse (apache-mx/apache-matrix [[0.5]])))
  (is= [[-2.0000000000000004 1.0000000000000004] [1.5 -0.5000000000000003]]
       (apache-mx/apache-matrix->matrix
         (apache-mx/inverse (apache-mx/apache-matrix [[1 2] [3 4]]))))
  (is= nil (apache-mx/inverse (apache-mx/apache-matrix [[1 2] [1 2]]))))

(deftest lu-decomposition-with-determinant-and-inverse-test
  (is (spec-check apache-mx/lu-decomposition-with-determinant-and-inverse))
  (is= {::apache-mx/L              (apache-mx/apache-matrix [[1.0]])
        ::apache-mx/U              (apache-mx/apache-matrix [[4.0]])
        ::apache-mx/LU-permutation (apache-mx/apache-matrix [[1.0]])
        ::apache-mx/determinant    4.0
        ::apache-mx/inverse        (apache-mx/apache-matrix [[0.25]])}
       (apache-mx/lu-decomposition-with-determinant-and-inverse
         (apache-mx/apache-matrix [[4.0]])))
  (is= {::apache-mx/L              (apache-mx/apache-matrix [[1.0 0.0] [0.5 1.0]])
        ::apache-mx/U              (apache-mx/apache-matrix [[2.0 4.0] [0.0 -1.5]])
        ::apache-mx/LU-permutation (apache-mx/apache-matrix [[0.0 1.0] [1.0 0.0]])
        ::apache-mx/determinant    3.0
        ::apache-mx/inverse        (apache-mx/apache-matrix
                                     [[1.3333333333333333 -0.16666666666666663]
                                      [-0.6666666666666666 0.3333333333333333]])}
       (apache-mx/lu-decomposition-with-determinant-and-inverse
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]]))))

(deftest lu-decomposition-with-determinant-test
  (is (spec-check apache-mx/lu-decomposition-with-determinant))
  (is= {::apache-mx/L              (apache-mx/apache-matrix [[1.0]])
        ::apache-mx/U              (apache-mx/apache-matrix [[2.0]])
        ::apache-mx/LU-permutation (apache-mx/apache-matrix [[1.0]])
        ::apache-mx/determinant    2.0}
       (apache-mx/lu-decomposition-with-determinant
         (apache-mx/apache-matrix [[2.0]])))
  (is= {::apache-mx/L              (apache-mx/apache-matrix [[1.0 0.0] [0.0 1.0]])
        ::apache-mx/U              (apache-mx/apache-matrix [[1.0 0.0] [0.0 1.0]])
        ::apache-mx/LU-permutation (apache-mx/apache-matrix [[1.0 0.0] [0.0 1.0]])
        ::apache-mx/determinant    1.0}
       (apache-mx/lu-decomposition-with-determinant
         (apache-mx/apache-matrix [[1 0] [0 1]])))
  (is= {::apache-mx/L              (apache-mx/apache-matrix [[1.0 0.0] [0.5 1.0]])
        ::apache-mx/U              (apache-mx/apache-matrix [[2.0 4.0] [0.0 -1.5]])
        ::apache-mx/LU-permutation (apache-mx/apache-matrix [[0.0 1.0] [1.0 0.0]])
        ::apache-mx/determinant    3.0}
       (apache-mx/lu-decomposition-with-determinant
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]]))))

(deftest eigen-decomposition-test
  (is (spec-check apache-mx/eigen-decomposition))
  (is= {::apache-mx/eigenvectorsT      (apache-mx/apache-matrix [[]])
        ::apache-mx/eigenvalues-matrix (apache-mx/apache-matrix [[]])
        ::apache-mx/eigenvalues        []
        ::apache-mx/eigenvectors       (apache-mx/apache-matrix [[]])}
       (apache-mx/eigen-decomposition (apache-mx/apache-matrix [[]])))
  (is= {::apache-mx/eigenvectorsT      (apache-mx/apache-matrix [[1]])
        ::apache-mx/eigenvalues-matrix (apache-mx/apache-matrix [[1]])
        ::apache-mx/eigenvalues        [1.0]
        ::apache-mx/eigenvectors       (apache-mx/apache-matrix [[1]])}
       (apache-mx/eigen-decomposition (apache-mx/apache-matrix [[1]])))
  (is= {::apache-mx/eigenvectorsT      (apache-mx/apache-matrix
                                         [[-0.8553908861324309 -0.16211892282756657]
                                          [0.5179830421177656 -1.0708848574604801]])
        ::apache-mx/eigenvalues-matrix (apache-mx/apache-matrix
                                         [[0.6972243622680055 0.0] [0.0 4.302775637731996]])
        ::apache-mx/eigenvalues        [0.6972243622680055 4.302775637731996]
        ::apache-mx/eigenvectors       (apache-mx/apache-matrix
                                         [[-0.8553908861324309 0.5179830421177656]
                                          [-0.16211892282756657 -1.0708848574604801]])}
       (apache-mx/eigen-decomposition (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])))
  (is= {::apache-mx/eigenvectorsT      (apache-mx/apache-matrix [[1.0]])
        ::apache-mx/eigenvalues-matrix (apache-mx/apache-matrix [[2.0]])
        ::apache-mx/eigenvalues        [2.0]
        ::apache-mx/eigenvectors       (apache-mx/apache-matrix [[1.0]])}
       (apache-mx/eigen-decomposition (apache-mx/apache-matrix [[2.0]])))
  (is= {::apache-mx/eigenvectorsT      (apache-mx/apache-matrix
                                         [[1.0 0.0] [0.0 1.0]])
        ::apache-mx/eigenvalues-matrix (apache-mx/apache-matrix
                                         [[1e100 0.0] [0.0 1e100]])
        ::apache-mx/eigenvalues        [1e100 1e100]
        ::apache-mx/eigenvectors       (apache-mx/apache-matrix
                                         [[1.0 0.0] [0.0 1.0]])}
       (apache-mx/eigen-decomposition (apache-mx/apache-matrix
                                        [[1e100 0.0] [0.0 1e100]]))))

(deftest cholesky-decomposition-test
  (is (spec-check apache-mx/cholesky-decomposition
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 1}}))
  (is= {::apache-mx/cholesky-L  (apache-mx/apache-matrix [[]])
        ::apache-mx/cholesky-LT (apache-mx/apache-matrix [[]])}
       (apache-mx/cholesky-decomposition (apache-mx/apache-matrix [[]])))
  (is= {::apache-mx/cholesky-L  (apache-mx/apache-matrix [[2.0]])
        ::apache-mx/cholesky-LT (apache-mx/apache-matrix [[2.0]])}
       (apache-mx/cholesky-decomposition (apache-mx/apache-matrix [[4.0]])))
  (is= {::apache-mx/cholesky-L  (apache-mx/apache-matrix
                                  [[1.0 0.0] [0.5 1.6583123951777]])
        ::apache-mx/cholesky-LT (apache-mx/apache-matrix
                                  [[1.0 0.5] [0.0 1.6583123951777]])}
       (apache-mx/cholesky-decomposition
         (apache-mx/apache-matrix [[1.0 0.5] [0.5 3.0]]))))

(deftest rectangular-cholesky-decomposition-test
  (is (spec-check apache-mx/rectangular-cholesky-decomposition
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 1}}))
  (is= {::apache-mx/rectangular-root (apache-mx/apache-matrix [[]])
        ::apache-mx/rank             0}
       (apache-mx/rectangular-cholesky-decomposition
         (apache-mx/apache-matrix [[]])
         1e-4))
  (is= {::apache-mx/rectangular-root (apache-mx/apache-matrix
                                       [[0.2886751345948129 0.9574271077563381]
                                        [1.7320508075688772 0.0]])
        ::apache-mx/rank             2}
       (apache-mx/rectangular-cholesky-decomposition
         (apache-mx/apache-matrix [[1.0 0.5] [0.5 3.0]])
         1e-4))
  (is= {::apache-mx/rectangular-root (apache-mx/apache-matrix
                                       [[0.2886751345948129 0.9574271077563381]
                                        [1.7320508075688772 0.0]
                                        [5.773502691896259E-10 8.703882797784892E-10]])
        ::apache-mx/rank             2}
       (apache-mx/rectangular-cholesky-decomposition
         (apache-mx/apache-matrix [[1.0 0.5 1e-9] [0.5 3.0 1e-9] [1e-9 1e-9 1e-6]])
         1e-4))
  (is= {::apache-mx/rectangular-root (apache-mx/apache-matrix
                                       [[0.28392676259431254]
                                        [0.17035605755658673]])
        ::apache-mx/rank             1}
       (apache-mx/rectangular-cholesky-decomposition
         (apache-mx/apache-matrix [[0.08061440651728713 0.048368643910372044]
                                   [0.048368643910372044 0.029021186346223526]])
         1e-14)))

(deftest sv-decomposition-test
  (is (spec-check apache-mx/sv-decomposition))
  (is= {::apache-mx/svd-left        (apache-mx/apache-matrix [[]])
        ::apache-mx/singular-values (apache-mx/apache-matrix [[]])
        ::apache-mx/svd-right       (apache-mx/apache-matrix [[]])
        ::apache-mx/rank            0}
       (apache-mx/sv-decomposition (apache-mx/apache-matrix [[]])))
  (is= {::apache-mx/svd-left        (apache-mx/apache-matrix [[1.0]])
        ::apache-mx/singular-values (apache-mx/apache-matrix [[3.0]])
        ::apache-mx/svd-right       (apache-mx/apache-matrix [[1.0]])
        ::apache-mx/rank            1}
       (apache-mx/sv-decomposition (apache-mx/apache-matrix [[3.0]])))
  (is= {::apache-mx/svd-left        (apache-mx/apache-matrix
                                      [[-0.6000000000000001] [-0.8]])
        ::apache-mx/singular-values (apache-mx/apache-matrix [[5.0]])
        ::apache-mx/svd-right       (apache-mx/apache-matrix [[-1.0]])
        ::apache-mx/rank            1}
       (apache-mx/sv-decomposition (apache-mx/apache-matrix [[3.0] [4.0]])))
  (is= {::apache-mx/svd-left        (apache-mx/apache-matrix
                                      [[0.4899250213574801 0.871764574554319]
                                       [0.8717645745543189 -0.4899250213574801]])
        ::apache-mx/singular-values (apache-mx/apache-matrix
                                      [[7.343420458864693 0.0]
                                       [0.0 0.27235264699921685]])
        ::apache-mx/svd-right       (apache-mx/apache-matrix
                                      [[0.6082871552778867 0.7937170381968226]
                                       [-0.7937170381968226 0.6082871552778867]])
        ::apache-mx/rank            2}
       (apache-mx/sv-decomposition
         (apache-mx/apache-matrix [[2.0 3.0] [4.0 5.0]])))
  (is= {::apache-mx/svd-left        (apache-mx/apache-matrix
                                      [[0.20027709794089957 0.9797392939146471]
                                       [0.979739293914647 -0.2002770979408996]])
        ::apache-mx/singular-values (apache-mx/apache-matrix
                                      [[4.562639046204302 0.0] [0.0 0.6575142082509742]])
        ::apache-mx/svd-right       (apache-mx/apache-matrix
                                      [[0.47335668324824287 0.8808708477547789]
                                       [0.8808708477547789 -0.47335668324824287]])
        ::apache-mx/rank            2}
       (apache-mx/sv-decomposition (apache-mx/apache-matrix
                                     [[1.0 0.5] [2.0 4.0]])))
  (is= {::apache-mx/svd-left        (apache-mx/apache-matrix [[-1.0]])
        ::apache-mx/singular-values (apache-mx/apache-matrix
                                      [[1.118033988749895]])
        ::apache-mx/svd-right       (apache-mx/apache-matrix
                                      [[-0.8944271909999157 -0.4472135954999579]])
        ::apache-mx/rank            1}
       (apache-mx/sv-decomposition (apache-mx/apache-matrix [[1.0 0.5]])))
  (is= {::apache-mx/svd-left        (apache-mx/apache-matrix
                                      [[-0.8944271909999157] [-0.4472135954999579]])
        ::apache-mx/singular-values (apache-mx/apache-matrix
                                      [[1.118033988749895]])
        ::apache-mx/svd-right       (apache-mx/apache-matrix [[-1.0]])
        ::apache-mx/rank            1}
       (apache-mx/sv-decomposition (apache-mx/apache-matrix [[1.0] [0.5]]))))

(deftest condition-test
  (is (spec-check apache-mx/condition))
  (is (m/nan? (apache-mx/condition (apache-mx/apache-matrix [[]]))))
  (is= 1.0 (apache-mx/condition (apache-mx/apache-matrix [[2.0]])))
  (is= 2.0
       (apache-mx/condition (apache-mx/apache-matrix [[1.0 0.0] [0.0 2.0]])))
  (is= 4.0
       (apache-mx/condition (apache-mx/apache-matrix [[1.0 0.0] [0.0 4.0]]))))

(deftest qr-decomposition-with-linear-least-squares-and-error-matrix-test
  (is (spec-check apache-mx/qr-decomposition-with-linear-least-squares-and-error-matrix))
  (is= {::apache-mx/Q            (apache-mx/apache-matrix [[]])
        ::apache-mx/R            (apache-mx/apache-matrix [[]])
        ::apache-mx/LLS-solution (apache-mx/apache-matrix [[]])
        ::apache-mx/error        (apache-mx/apache-matrix [[]])}
       (apache-mx/qr-decomposition-with-linear-least-squares-and-error-matrix
         (apache-mx/apache-matrix [[]]) (apache-mx/apache-matrix [[]])))
  (is= {::apache-mx/Q            (apache-mx/apache-matrix
                                   [[-0.44721359549995787 0.8944271909999159]
                                    [-0.8944271909999157 -0.447213595499958]])
        ::apache-mx/R            (apache-mx/apache-matrix
                                   [[-2.23606797749979 -3.801315561749642]
                                    [0.0 -1.341640786499874]])
        ::apache-mx/LLS-solution (apache-mx/apache-matrix
                                   [[7.833333333333333] [-1.6666666666666665]])
        ::apache-mx/error        (apache-mx/apache-matrix
                                   [[1.8055555555555545 -0.944444444444444]
                                    [-0.944444444444444 0.5555555555555554]])}
       (apache-mx/qr-decomposition-with-linear-least-squares-and-error-matrix
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         (apache-mx/apache-matrix [[7.0] [9.0]])))
  (is= {::apache-mx/Q            (apache-mx/apache-matrix
                                   [[-0.8574929257125441 0.5144957554275266]
                                    [-0.5144957554275265 -0.8574929257125442]])
        ::apache-mx/R            (apache-mx/apache-matrix
                                   [[-1.16619037896906 -0.4973458969132756 -0.6345447650272829]
                                    [0.0 -0.051449575542752646 -0.6688444820557842]])
        ::apache-mx/LLS-solution (apache-mx/apache-matrix
                                   [[-24.999999999999996] [80.0] [0.0]])
        ::apache-mx/error        nil}
       (apache-mx/qr-decomposition-with-linear-least-squares-and-error-matrix
         (apache-mx/apache-matrix [[1.0 0.4 0.2] [0.6 0.3 0.9]])
         (apache-mx/apache-matrix [[7.0] [9.0]])))
  (is= {::apache-mx/Q            (apache-mx/apache-matrix
                                   [[-0.894427190999916 -0.4472135954999579]
                                    [-0.4472135954999579 0.8944271909999159]])
        ::apache-mx/R            (apache-mx/apache-matrix
                                   [[-1.118033988749895] [0.0]])
        ::apache-mx/LLS-solution (apache-mx/apache-matrix [[9.200000000000003]])
        ::apache-mx/error        (apache-mx/apache-matrix [[0.7999999999999996]])}
       (apache-mx/qr-decomposition-with-linear-least-squares-and-error-matrix
         (apache-mx/apache-matrix [[1.0] [0.5]])
         (apache-mx/apache-matrix [[7.0] [9.0]]))))

(deftest qr-decomposition-with-linear-least-squares-test
  (is (spec-check apache-mx/qr-decomposition-with-linear-least-squares))
  (is= {::apache-mx/Q            (apache-mx/apache-matrix [[]])
        ::apache-mx/R            (apache-mx/apache-matrix [[]])
        ::apache-mx/LLS-solution (apache-mx/apache-matrix [[]])}
       (apache-mx/qr-decomposition-with-linear-least-squares
         (apache-mx/apache-matrix [[]]) (apache-mx/apache-matrix [[]])))
  (is= {::apache-mx/Q            (apache-mx/apache-matrix [[-1.0]])
        ::apache-mx/R            (apache-mx/apache-matrix [[-1.0 -0.5]])
        ::apache-mx/LLS-solution (apache-mx/apache-matrix
                                   [[7.0 9.0] [0.0 0.0]])}
       (apache-mx/qr-decomposition-with-linear-least-squares
         (apache-mx/apache-matrix [[1.0 0.5]])
         (apache-mx/apache-matrix [[7.0 9.0]]))))

(deftest qr-decomposition-test
  (is (spec-check apache-mx/qr-decomposition))
  (is= {::apache-mx/Q (apache-mx/apache-matrix [[]])
        ::apache-mx/R (apache-mx/apache-matrix [[]])}
       (apache-mx/qr-decomposition (apache-mx/apache-matrix [[]])))
  (is= {::apache-mx/Q (apache-mx/apache-matrix
                        [[-0.8574929257125441 0.5144957554275266]
                         [-0.5144957554275265 -0.8574929257125442]])
        ::apache-mx/R (apache-mx/apache-matrix
                        [[-1.16619037896906 -0.4973458969132756 -0.6345447650272829]
                         [0.0 -0.051449575542752646 -0.6688444820557842]])}
       (apache-mx/qr-decomposition
         (apache-mx/apache-matrix [[1.0 0.4 0.2] [0.6 0.3 0.9]])))
  (is= {::apache-mx/Q (apache-mx/apache-matrix [[-1.0]])
        ::apache-mx/R (apache-mx/apache-matrix [[-2.0]])}
       (apache-mx/qr-decomposition (apache-mx/apache-matrix [[2.0]])))
  (is= {::apache-mx/Q (apache-mx/apache-matrix [[-1.0]])
        ::apache-mx/R (apache-mx/apache-matrix [[-2.0 -3.0]])}
       (apache-mx/qr-decomposition (apache-mx/apache-matrix [[2.0 3.0]])))
  (is= {::apache-mx/Q (apache-mx/apache-matrix
                        [[-0.5547001962252294 -0.8320502943378437]
                         [-0.8320502943378437 0.5547001962252291]])
        ::apache-mx/R (apache-mx/apache-matrix [[-3.605551275463989] [0.0]])}
       (apache-mx/qr-decomposition (apache-mx/apache-matrix [[2.0] [3.0]]))))

(deftest rank-revealing-qr-decomposition-test
  (is (spec-check apache-mx/rank-revealing-qr-decomposition))
  (is= {::apache-mx/Q                (apache-mx/apache-matrix [[]])
        ::apache-mx/R                (apache-mx/apache-matrix [[]])
        ::apache-mx/RRQR-permutation (apache-mx/apache-matrix [[]])
        ::apache-mx/rank             0}
       (apache-mx/rank-revealing-qr-decomposition
         (apache-mx/apache-matrix [[]])
         1e-6))
  (is= {::apache-mx/Q                (apache-mx/apache-matrix
                                       [[-0.12403473458920855 0.9922778767136677]
                                        [-0.9922778767136677 -0.12403473458920833]])
        ::apache-mx/R                (apache-mx/apache-matrix
                                       [[-4.031128874149275 -2.108590488016544]
                                        [0.0 0.7442084075352513]])
        ::apache-mx/RRQR-permutation (apache-mx/apache-matrix
                                       [[0.0 1.0] [1.0 0.0]])
        ::apache-mx/rank             2}
       (apache-mx/rank-revealing-qr-decomposition
         (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
         1e-6))
  (is= {::apache-mx/Q                (apache-mx/apache-matrix [[-1.0]])
        ::apache-mx/R                (apache-mx/apache-matrix [[-1.0 -0.5]])
        ::apache-mx/RRQR-permutation (apache-mx/apache-matrix
                                       [[1.0 0.0] [0.0 1.0]])
        ::apache-mx/rank             1}
       (apache-mx/rank-revealing-qr-decomposition
         (apache-mx/apache-matrix [[1.0 0.5]])
         1e-6))
  (is= {::apache-mx/Q                (apache-mx/apache-matrix
                                       [[-0.894427190999916 -0.4472135954999579]
                                        [-0.4472135954999579 0.8944271909999159]])
        ::apache-mx/R                (apache-mx/apache-matrix
                                       [[-1.118033988749895] [0.0]])
        ::apache-mx/RRQR-permutation (apache-mx/apache-matrix [[1.0]])
        ::apache-mx/rank             1}
       (apache-mx/rank-revealing-qr-decomposition
         (apache-mx/apache-matrix [[1.0] [0.5]])
         1e-6)))

#_(ost/unstrument)