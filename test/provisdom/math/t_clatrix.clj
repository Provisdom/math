(ns provisdom.math.t-clatrix
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.clatrix :as clx]
    [provisdom.math.core :as m]
    [provisdom.math.matrix :as mx]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.math.random :as random]))

;;98 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

;;;TYPES
(deftest clatrix?-test
  (is (clx/clatrix? (clx/clatrix [[]])))
  (is (clx/clatrix? (clx/clatrix [[1]])))
  (is (clx/clatrix? (clx/clatrix [[1 2] [3 4]])))
  (is-not (clx/clatrix? "A"))
  (is-not (clx/clatrix? [[1 2 3] [3 4]])))

(deftest empty-clatrix?-test
  (is (clx/empty-clatrix? (clx/clatrix [[]])))
  (is-not (clx/empty-clatrix? (clx/clatrix [[1]]))))

(deftest clatrix-finite?-test
  (is (clx/clatrix-finite? (clx/clatrix [[]])))
  (is (clx/clatrix-finite? (clx/clatrix [[1]])))
  (is-not (clx/clatrix-finite? (clx/clatrix [[m/inf+]])))
  (is-not (clx/clatrix-finite? (clx/clatrix [[1 2] [3 m/inf+]])))
  (is-not (clx/clatrix-finite? "A"))
  (is-not (clx/clatrix-finite? [[1 2] [3 4]])))

(deftest row-clatrix?-test
  (is-not (clx/row-clatrix? (clx/clatrix [[]])))
  (is (clx/row-clatrix? (clx/clatrix [[1]])))
  (is (clx/row-clatrix? (clx/clatrix [[1 1]])))
  (is-not (clx/row-clatrix? (clx/clatrix [[1] [1]]))))

(deftest column-clatrix?-test
  (is-not (clx/column-clatrix? (clx/clatrix [[]])))
  (is (clx/column-clatrix? (clx/clatrix [[1]])))
  (is-not (clx/column-clatrix? (clx/clatrix [[1 1]])))
  (is (clx/column-clatrix? (clx/clatrix [[1] [1]]))))

(deftest square-clatrix?-test
  (is (clx/square-clatrix? (clx/clatrix [[]])))
  (is (clx/square-clatrix? (clx/clatrix [[1]])))
  (is-not (clx/square-clatrix? (clx/clatrix [[1 1]])))
  (is-not (clx/square-clatrix? (clx/clatrix [[1] [1]])))
  (is (clx/square-clatrix? (clx/clatrix [[1 1] [1 1]]))))

(deftest diagonal-clatrix?-test
  (is (clx/diagonal-clatrix? (clx/clatrix [[]])))
  (is (clx/diagonal-clatrix? (clx/clatrix [[1]])))
  (is-not (clx/diagonal-clatrix? (clx/clatrix [[1 1]])))
  (is-not (clx/diagonal-clatrix? (clx/clatrix [[1] [1]])))
  (is-not (clx/diagonal-clatrix? (clx/clatrix [[1 1] [1 1]])))
  (is (clx/diagonal-clatrix? (clx/clatrix [[1 0] [0 1]]))))

(deftest upper-triangular-clatrix?-test
  (is (clx/upper-triangular-clatrix? (clx/clatrix [[]])))
  (is (clx/upper-triangular-clatrix? (clx/clatrix [[1]])))
  (is-not (clx/upper-triangular-clatrix? (clx/clatrix [[1 1]])))
  (is-not (clx/upper-triangular-clatrix? (clx/clatrix [[1] [1]])))
  (is-not (clx/upper-triangular-clatrix? (clx/clatrix [[1 1] [1 1]])))
  (is (clx/upper-triangular-clatrix? (clx/clatrix [[1 0] [0 1]])))
  (is (clx/upper-triangular-clatrix? (clx/clatrix [[1 1] [0 1]]))))

(deftest lower-triangular-clatrix?-test
  (is (clx/lower-triangular-clatrix? (clx/clatrix [[]])))
  (is (clx/lower-triangular-clatrix? (clx/clatrix [[1]])))
  (is-not (clx/lower-triangular-clatrix? (clx/clatrix [[1 1]])))
  (is-not (clx/lower-triangular-clatrix? (clx/clatrix [[1] [1]])))
  (is-not (clx/lower-triangular-clatrix? (clx/clatrix [[1 1] [1 1]])))
  (is (clx/lower-triangular-clatrix? (clx/clatrix [[1 0] [0 1]])))
  (is (clx/lower-triangular-clatrix? (clx/clatrix [[1 0] [1 1]]))))

(deftest symmetric-clatrix?-test
  (is (clx/symmetric-clatrix? (clx/clatrix [[]])))
  (is (clx/symmetric-clatrix? (clx/clatrix [[1]])))
  (is-not (clx/symmetric-clatrix? (clx/clatrix [[1 1]])))
  (is-not (clx/symmetric-clatrix? (clx/clatrix [[1] [1]])))
  (is (clx/symmetric-clatrix? (clx/clatrix [[1 1] [1 1]])))
  (is (clx/symmetric-clatrix? (clx/clatrix [[1 0] [0 1]])))
  (is-not (clx/symmetric-clatrix? (clx/clatrix [[1 0] [1 1]]))))

(deftest positive-semidefinite-clatrix-finite?-test
  (is (clx/positive-semidefinite-clatrix-finite? (clx/clatrix [[]]) m/dbl-close))
  (is (clx/positive-semidefinite-clatrix-finite? (clx/clatrix [[0.0]]) m/dbl-close))
  (is (clx/positive-semidefinite-clatrix-finite? (clx/clatrix [[1.0]]) m/dbl-close))
  (is (clx/positive-semidefinite-clatrix-finite? (clx/clatrix [[0.0 0.0] [0.0 0.0]]) m/dbl-close))
  (is-not (clx/positive-semidefinite-clatrix-finite? (clx/clatrix [[1.0 0.5] [2.0 4.0]]) m/dbl-close))
  (is (clx/positive-semidefinite-clatrix-finite? (clx/clatrix [[1.0 0.5] [0.5 2.0]]) m/dbl-close))
  (is-not (clx/positive-semidefinite-clatrix-finite? (clx/clatrix [[1.0 -1.1] [-1.1 1.0]]) m/dbl-close))
  (is (clx/positive-semidefinite-clatrix-finite? (clx/clatrix [[1.0 -1.0] [-1.0 1.0]]) m/dbl-close))
  (is (clx/positive-semidefinite-clatrix-finite?
        (clx/clatrix [[1.0 (m/next-down -1.0)] [(m/next-down -1.0) 1.0]]) m/dbl-close))
  (is-not (clx/positive-semidefinite-clatrix-finite?
            (clx/clatrix [[1.0 (+ -1.0 -1.0E-14)] [(+ -1.0 -1.0E-14) 1.0]]) m/dbl-close))
  (is (clx/positive-semidefinite-clatrix-finite?
        (clx/clatrix [[1.0 (+ -1.0 -1.0E-14)] [(+ -1.0 -1.0E-14) 1.0]]) m/sgl-close)))

(deftest positive-definite-clatrix-finite?-test
  (is (clx/positive-definite-clatrix-finite? (clx/clatrix [[]]) m/sgl-close))
  (is-not (clx/positive-definite-clatrix-finite? (clx/clatrix [[0.0]]) m/sgl-close))
  (is (clx/positive-definite-clatrix-finite? (clx/clatrix [[1.0]]) m/sgl-close))
  (is-not (clx/positive-definite-clatrix-finite? (clx/clatrix [[0.0 0.0] [0.0 0.0]]) m/sgl-close))
  (is-not (clx/positive-definite-clatrix-finite? (clx/clatrix [[1.0 0.5] [2.0 4.0]]) m/sgl-close))
  (is (clx/positive-definite-clatrix-finite? (clx/clatrix [[1.0 0.5] [0.5 1.0]]) m/sgl-close))
  (is-not (clx/positive-definite-clatrix-finite? (clx/clatrix [[1.0 -1.1] [-1.1 1.0]]) m/sgl-close))
  (is-not (clx/positive-definite-clatrix-finite? (clx/clatrix [[1.0 -1.0] [-1.0 1.0]]) m/sgl-close))
  (is-not (clx/positive-definite-clatrix-finite? (clx/clatrix [[1.0 -1.0] [-1.0 1.0]]) 1e-40)) ;better than Apache here
  (is-not (clx/positive-definite-clatrix-finite?
            (clx/clatrix [[(m/next-up 1.0) -1.0] [-1.0 (m/next-up 1.0)]]) m/sgl-close))
  (is-not (clx/positive-definite-clatrix-finite?
            (clx/clatrix [[(inc 1.0E-14) -1.0] [-1.0 (inc 1.0E-14)]]) m/sgl-close))
  (is (clx/positive-definite-clatrix-finite?
        (clx/clatrix [[(inc 1.0E-14) -1.0] [-1.0 (inc 1.0E-14)]]) m/dbl-close)))

(deftest correlation-clatrix-finite?-test
  (is (clx/correlation-clatrix-finite? (clx/clatrix [[]]) m/sgl-close))
  (is (clx/correlation-clatrix-finite? (clx/clatrix [[1.0]]) m/sgl-close))
  (is (clx/correlation-clatrix-finite? (clx/clatrix [[1.0 0.2] [0.2 1.0]]) m/sgl-close))
  (is-not (clx/correlation-clatrix-finite? (clx/clatrix [[0.2]]) m/sgl-close))
  (is-not (clx/correlation-clatrix-finite? (clx/clatrix [[1.0 0.5] [2.0 4.0]]) m/sgl-close))
  (is-not (clx/correlation-clatrix-finite? (clx/clatrix [[1.0 0.5] [0.5 2.0]]) m/sgl-close))
  (is-not (clx/correlation-clatrix-finite? (clx/clatrix [[1.0 -1.1] [-1.1 1.0]]) m/sgl-close))
  (is-not (clx/correlation-clatrix-finite? (clx/clatrix [[1.0 -1.0] [-1.0 1.0]]) m/sgl-close))
  (is-not (clx/correlation-clatrix-finite?
            (clx/clatrix [[(m/next-up 1.0) -1.0] [-1.0 (m/next-up 1.0)]]) m/sgl-close))
  (is-not (clx/correlation-clatrix-finite? (clx/clatrix [[1.0 -1.0] [-1.0 1.0]]) m/sgl-close))
  (is-not (clx/correlation-clatrix-finite? (clx/clatrix [[1.0 -1.0] [-1.0 1.0]]) 1e-40))) ;better than Apache

;(defspec-test test-clatrix? `clx/clatrix?) ;slow-ish
;(defspec-test test-empty-clatrix? `clx/empty-clatrix?) ;slow-ish
;(defspec-test test-clatrix-finite? `clx/clatrix-finite?) ;slow-ish
;(defspec-test test-row-clatrix? `clx/row-clatrix?) ;slow-ish
;(defspec-test test-column-clatrix? `clx/column-clatrix?) ;slow-ish
;(defspec-test test-square-clatrix? `clx/square-clatrix?) ;slow-ish
;(defspec-test test-diagonal-clatrix? `clx/diagonal-clatrix?) ;slow-ish
;(defspec-test test-upper-triangular-clatrix? `clx/upper-triangular-clatrix?) ;slow-ish
;(defspec-test test-lower-triangular-clatrix? `clx/lower-triangular-clatrix?) ;slow-ish
;(defspec-test test-symmetric-clatrix? `clx/symmetric-clatrix?) ;slow-ish
;(defspec-test test-positive-semidefinite-clatrix-finite? `clx/positive-semidefinite-clatrix-finite?) ;slow-ish
;(defspec-test test-positive-definite-clatrix-finite? `clx/positive-definite-clatrix-finite?) ;slow-ish
;(defspec-test test-correlation-clatrix-finite? `clx/correlation-clatrix-finite?) ;slow-ish

;;;CONSTRUCTORS
(deftest clatrix-&-clatrix->matrix-test
  (is= [[]] (clx/clatrix->matrix (clx/clatrix [[]])))
  (is= [[1.0]] (clx/clatrix->matrix (clx/clatrix [[1.0]])))
  (is= [[1.0 2.0]] (clx/clatrix->matrix (clx/clatrix [[1.0 2.0]])))
  (is= [[1.0] [2.0]] (clx/clatrix->matrix (clx/clatrix [[1.0] [2.0]])))
  (is= [[1.0 0.5] [2.0 4.0]] (clx/clatrix->matrix (clx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest positive-semidefinite-clatrix-finite-by-squaring-test
  (is= (clx/clatrix [[]]) (clx/positive-semidefinite-clatrix-finite-by-squaring (clx/clatrix [[]])))
  (is= (clx/clatrix [[4]]) (clx/positive-semidefinite-clatrix-finite-by-squaring (clx/clatrix [[2]])))
  (is= (clx/clatrix [[5.0 11.0] [11.0 25.0]])
       (clx/positive-semidefinite-clatrix-finite-by-squaring (clx/clatrix [[1 2] [3 4]]))))

(deftest positive-definite-clatrix-finite-by-squaring-test
  (is= (clx/clatrix [[]]) (clx/positive-definite-clatrix-finite-by-squaring (clx/clatrix [[]])))
  (is= (clx/clatrix [[4]]) (clx/positive-definite-clatrix-finite-by-squaring (clx/clatrix [[2]])))
  (is= [[1.0E-4 3.0E-17] [3.0E-17 1.0000000000008E-4]]
       (clx/clatrix->matrix (clx/positive-definite-clatrix-finite-by-squaring (clx/clatrix [[0 0] [3 4]])))))

(deftest correlation-clatrix-finite-by-squaring-test
  (is= (clx/clatrix [[]]) (clx/correlation-clatrix-finite-by-squaring (clx/clatrix [[]])))
  (is= (clx/clatrix [[1.0]]) (clx/correlation-clatrix-finite-by-squaring (clx/clatrix [[2]])))
  (is= [[1.0 0.9838699100999075] [0.9838699100999075 1.0]]
       (clx/clatrix->matrix (clx/correlation-clatrix-finite-by-squaring (clx/clatrix [[1 2] [3 4]]))))
  (is= [[1.0 2.9999999999988E-13] [2.9999999999988E-13 1.0]]
       (clx/clatrix->matrix (clx/correlation-clatrix-finite-by-squaring (clx/clatrix [[0 0] [3 4]])))))

(deftest rnd-positive-definite-clatrix-finite!-test
  (random/bind-seed 0
    (is= (clx/clatrix [[]]) (clx/rnd-positive-definite-clatrix-finite! 0)))
  (random/bind-seed 0
    (is= (clx/clatrix [[0.8833108082136426]]) (clx/rnd-positive-definite-clatrix-finite! 1)))
  (random/bind-seed 0
    (is= (clx/clatrix [[0.6946098792362991 0.3550851337817903] [0.3550851337817903 0.21513470056994127]])
         (clx/rnd-positive-definite-clatrix-finite! 2))))

(deftest rnd-correlation-clatrix-finite!-test
  (random/bind-seed 0
    (is= (clx/clatrix [[]]) (clx/rnd-correlation-clatrix-finite! 0)))
  (random/bind-seed 0
    (is= (clx/clatrix [[1.0]]) (clx/rnd-correlation-clatrix-finite! 1)))
  (random/bind-seed 0
    (is= [[1.0 0.9185584128047] [0.9185584128047 1.0]] (clx/clatrix->matrix (clx/rnd-correlation-clatrix-finite! 2)))))

(defspec-test test-clatrix `clx/clatrix)
(defspec-test test-clatrix->matrix `clx/clatrix->matrix)
;(defspec-test test-positive-semidefinite-clatrix-finite-by-squaring `clx/positive-semidefinite-clatrix-finite-by-squaring) ;slow-ish
;(defspec-test test-positive-definite-clatrix-finite-by-squaring `clx/positive-definite-clatrix-finite-by-squaring) ;slow-ish
;(defspec-test test-correlation-clatrix-finite-by-squaring `clx/correlation-clatrix-finite-by-squaring) ;slow-ish
(defspec-test test-rnd-positive-definite-clatrix-finite! `clx/rnd-positive-definite-clatrix-finite!)
(defspec-test test-rnd-correlation-clatrix-finite! `clx/rnd-correlation-clatrix-finite!)

;;;INFO
(deftest rows-test
  (is= 0 (clx/rows (clx/clatrix [[]])))
  (is= 1 (clx/rows (clx/clatrix [[1.0]])))
  (is= 1 (clx/rows (clx/clatrix [[1.0 2.0]])))
  (is= 2 (clx/rows (clx/clatrix [[1.0] [2.0]])))
  (is= 2 (clx/rows (clx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest columns-test
  (is= 0 (clx/columns (clx/clatrix [[]])))
  (is= 1 (clx/columns (clx/clatrix [[1.0]])))
  (is= 2 (clx/columns (clx/clatrix [[1.0 2.0]])))
  (is= 1 (clx/columns (clx/clatrix [[1.0] [2.0]])))
  (is= 2 (clx/columns (clx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest get-entry-test
  (is= 1.0 (clx/get-entry (clx/clatrix [[1.0]]) 0 0))
  (is= 2.0 (clx/get-entry (clx/clatrix [[1.0 2.0]]) 0 1))
  (is= 4.0 (clx/get-entry (clx/clatrix [[1.0 0.5] [2.0 4.0]]) 1 1)))

(deftest get-row-test
  (is= [1.0] (clx/get-row (clx/clatrix [[1.0]]) 0))
  (is= [2.0] (clx/get-row (clx/clatrix [[1.0] [2.0]]) 1))
  (is= [1.0 0.5] (clx/get-row (clx/clatrix [[1.0 0.5] [2.0 4.0]]) 0)))

(deftest get-column-test
  (is= [1.0] (clx/get-column (clx/clatrix [[1.0]]) 0))
  (is= [2.0] (clx/get-column (clx/clatrix [[1.0 2.0]]) 1))
  (is= [1.0 2.0] (clx/get-column (clx/clatrix [[1.0 0.5] [2.0 4.0]]) 0)))

(deftest diagonal-test
  (is= [] (clx/diagonal (clx/clatrix [[]])))
  (is= [1.0] (clx/diagonal (clx/clatrix [[1.0]])))
  (is= [1.0] (clx/diagonal (clx/clatrix [[1.0 2.0]])))
  (is= [1.0] (clx/diagonal (clx/clatrix [[1.0] [2.0]])))
  (is= [1.0 4.0] (clx/diagonal (clx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest some-kv-test
  (is= nil
       (clx/some-kv (fn [row column number]
                      (> row (+ column number)))
                    (clx/clatrix [[1.0 4.0] [3.0 5.0]])))
  (is= 1.0
       (clx/some-kv (fn [row column number]
                      (< row (+ column number)))
                    (clx/clatrix [[1.0 4.0] [3.0 5.0]])))
  (is= 4.0
       (clx/some-kv (fn [row column number]
                      (> (dec row) (- column number)))
                    (clx/clatrix [[1.0 4.0] [3.0 5.0]])))
  (is= 3.0
       (clx/some-kv (fn [row column number]
                      (> (dec row) (- column number)))
                    (clx/clatrix [[1.0 4.0] [3.0 5.0]])
                    {::mx/by-row? false})))

(defspec-test test-rows `clx/rows)
(defspec-test test-columns `clx/columns)
(defspec-test test-get-entry `clx/get-entry)
(defspec-test test-get-row `clx/get-row)
(defspec-test test-get-column `clx/get-column)
(defspec-test test-diagonal `clx/diagonal)
(defspec-test test-some-kv `clx/some-kv)

;;;MANIPULATION
(deftest transpose-test
  (is= (clx/clatrix [[]]) (clx/transpose (clx/clatrix [[]])))
  (is= (clx/clatrix [[1.0]]) (clx/transpose (clx/clatrix [[1.0]])))
  (is= (clx/clatrix [[1.0] [2.0]]) (clx/transpose (clx/clatrix [[1.0 2.0]])))
  (is= (clx/clatrix [[1.0 2.0]]) (clx/transpose (clx/clatrix [[1.0] [2.0]])))
  (is= (clx/clatrix [[1.0 2.0] [0.5 4.0]]) (clx/transpose (clx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest assoc-diagonal-test
  (is= (clx/clatrix [[2.0]]) (clx/assoc-diagonal (clx/clatrix [[0.0]]) [2.0]))
  (is= (clx/clatrix [[5 2] [3 6]]) (clx/assoc-diagonal (clx/clatrix [[1 2] [3 4]]) [5 6])))

(deftest symmetric-clatrix-by-averaging-test
  (is= (clx/clatrix [[0.0]]) (clx/symmetric-clatrix-by-averaging (clx/clatrix [[0.0]])))
  (is= (clx/clatrix [[1.0 2.5] [2.5 4.0]]) (clx/symmetric-clatrix-by-averaging (clx/clatrix [[1 2] [3 4]]))))

(deftest correlation-clatrix->covariance-clatrix-test
  (is= [[]] (clx/clatrix->matrix (clx/correlation-clatrix->covariance-clatrix (clx/clatrix [[]]) [])))
  (is= [[2.9999999999999996]]
       (clx/clatrix->matrix (clx/correlation-clatrix->covariance-clatrix (clx/clatrix [[1.0]]) [3.0])))
  (is= [[2.9999999999999996 1.161895003862225] [1.161895003862225 5.000000000000001]]
       (clx/clatrix->matrix
         (clx/correlation-clatrix->covariance-clatrix (clx/clatrix [[1.0 0.3] [0.3 1.0]]) [3.0 5.0]))))

(deftest covariance-clatrix->correlation-clatrix-test
  (is= [[]] (clx/clatrix->matrix (clx/covariance-clatrix->correlation-clatrix (clx/clatrix [[]]))))
  (is= [[1.0]] (clx/clatrix->matrix (clx/covariance-clatrix->correlation-clatrix (clx/clatrix [[3.0]]))))
  (is= [[1.0 0.232379000772445] [0.232379000772445 1.0]]
       (clx/clatrix->matrix (clx/covariance-clatrix->correlation-clatrix (clx/clatrix [[3.0 0.9] [0.9 5.0]])))))

(defspec-test test-transpose `clx/transpose)
(defspec-test test-assoc-diagonal `clx/assoc-diagonal)
(defspec-test test-symmetric-clatrix-by-averaging `clx/symmetric-clatrix-by-averaging)
;(defspec-test test-correlation-clatrix->covariance-clatrix `clx/correlation-clatrix->covariance-clatrix) ;slow
;(defspec-test test-covariance-clatrix->correlation-clatrix `clx/covariance-clatrix->correlation-clatrix) ;slow

;;;MATH
(deftest ===-test
  (is (clx/=== (clx/clatrix [[1.0 0.5] [2.0 m/nan]]) (clx/clatrix [[1.0 0.5] [2.0 m/nan]])))
  (is (clx/=== (clx/clatrix [[1.0 0.5] [2.0 m/nan]]) (clx/clatrix [[1.0 0.5] [2.0 m/nan]])
               (clx/clatrix [[1.0 0.5] [2.0 m/nan]]))))

(deftest mx*-test
  (is= (clx/clatrix [[]]) (clx/mx* (clx/clatrix [[]])))
  (is= nil (clx/mx* (clx/clatrix [[1.0]]) (clx/clatrix [[]])))
  (is= (clx/clatrix [[6.0]]) (clx/mx* (clx/clatrix [[2.0]]) (clx/clatrix [[3.0]])))
  (is= (clx/clatrix [[26.0]]) (clx/mx* (clx/clatrix [[2.0 4.0]]) (clx/clatrix [[3.0] [5.0]])))
  (is= (clx/clatrix [[19.0 22.0] [43.0 50.0]])
       (clx/mx* (clx/clatrix [[1.0 2.0] [3.0 4.0]]) (clx/clatrix [[5.0 6.0] [7.0 8.0]])))
  (is= (clx/clatrix [[67.0 78.0] [201.0 234.0]])
       (clx/mx* (clx/clatrix [[1.0] [3.0]]) (clx/clatrix [[5.0 6.0]]) (clx/clatrix [[5.0 6.0] [7.0 8.0]]))))

(deftest add-test
  (is= (clx/clatrix [[]]) (clx/add (clx/clatrix [[]]) (clx/clatrix [[]])))
  (is= nil (clx/add (clx/clatrix [[1.0 0.5] [2.0 4.0]]) (clx/clatrix [[1.0 0.5]])))
  (is= (clx/clatrix [[2.0 1.0] [4.0 8.0]])
       (clx/add (clx/clatrix [[1.0 0.5] [2.0 4.0]]) (clx/clatrix [[1.0 0.5] [2.0 4.0]])))
  (is= (clx/clatrix [[2.0 1.0]]) (clx/add (clx/clatrix [[1.0 0.5]]) (clx/clatrix [[1.0 0.5]])))
  (is= (clx/clatrix [[1.0 2.0]]) (clx/add (clx/clatrix [[1.0 2.0]]))))

(deftest subtract-test
  (is= (clx/clatrix [[]]) (clx/subtract (clx/clatrix [[]]) (clx/clatrix [[]])))
  (is= nil (clx/subtract (clx/clatrix [[1.0 0.5] [2.0 4.0]]) (clx/clatrix [[1.0 0.5]])))
  (is= (clx/clatrix [[0.0 0.0] [0.0 0.0]])
       (clx/subtract (clx/clatrix [[1.0 0.5] [2.0 4.0]]) (clx/clatrix [[1.0 0.5] [2.0 4.0]])))
  (is= (clx/clatrix [[0.0 0.0]]) (clx/subtract (clx/clatrix [[1.0 0.5]]) (clx/clatrix [[1.0 0.5]])))
  (is= (clx/clatrix [[-1.0 -0.5]])
       (clx/subtract (clx/clatrix [[1.0 0.5]]) (clx/clatrix [[1.0 0.5]]) (clx/clatrix [[1.0 0.5]]))))

(defspec-test test-=== `clx/===)
(defspec-test test-mx* `clx/mx*)
(defspec-test test-add `clx/add)
(defspec-test test-subtract `clx/subtract)

;;;DECOMPOSITION
(deftest inverse-test
  (is= (clx/clatrix [[]]) (clx/inverse (clx/clatrix [[]])))
  (is= (clx/clatrix [[0.5]]) (clx/inverse (clx/clatrix [[2.0]])))
  (is= (clx/clatrix [[1.0 0.5] [2.0 4.0]])
       (clx/inverse
         (clx/clatrix [[1.3333333333333333 -0.16666666666666663] [-0.6666666666666666 0.3333333333333333]]))))

(deftest determinant-test
  (is (m/nan? (clx/determinant (clx/clatrix [[]]))))
  (is= 2.0 (clx/determinant (clx/clatrix [[2.0]])))
  (is= -2.0 (clx/determinant (clx/clatrix [[1.0 2.0] [3.0 4.0]])))
  (is= 6.661338147750939E-16 (clx/determinant (clx/clatrix [[1.0 2.0 3.0] [4.0 5.0 6.0] [7.0 8.0 9.0]]))))

(deftest lu-decomposition-test
  (is= {::clx/L              (clx/clatrix [[]])
        ::clx/U              (clx/clatrix [[]])
        ::clx/LU-permutation (clx/clatrix [[]])}
       (clx/lu-decomposition (clx/clatrix [[]])))
  (is= {::clx/L              (clx/clatrix [[1.0]])
        ::clx/U              (clx/clatrix [[2.0]])
        ::clx/LU-permutation (clx/clatrix [[1.0]])}
       (clx/lu-decomposition (clx/clatrix [[2.0]])))
  (is= {::clx/L              (clx/clatrix [[1.0 0.0] [0.0 1.0]])
        ::clx/U              (clx/clatrix [[1.0 0.0] [0.0 1.0]])
        ::clx/LU-permutation (clx/clatrix [[1.0 0.0] [0.0 1.0]])}
       (clx/lu-decomposition (clx/clatrix [[1 0] [0 1]])))
  (is= {::clx/L              (clx/clatrix [[1.0 0.0] [0.5 1.0]])
        ::clx/U              (clx/clatrix [[2.0 4.0] [0.0 -1.5]])
        ::clx/LU-permutation (clx/clatrix [[0.0 1.0] [1.0 0.0]])}
       (clx/lu-decomposition (clx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest eigen-decomposition-test
  (is= {::clx/eigenvalues  []
        ::clx/eigenvectors (clx/clatrix [[]])}
       (clx/eigen-decomposition (clx/clatrix [[]])))
  (is= {::clx/eigenvalues  [2.0]
        ::clx/eigenvectors (clx/clatrix [[1.0]])}
       (clx/eigen-decomposition (clx/clatrix [[2.0]])))
  (is= {::clx/eigenvalues  [1e100 1e100]
        ::clx/eigenvectors (clx/clatrix [[-1.0 0.0] [0.0 1.0]])}
       (clx/eigen-decomposition (clx/clatrix [[1e100 0.0] [0.0 1e100]])))
  (is= {::clx/eigenvalues  [0.6972243622680057 4.302775637731995]
        ::clx/eigenvectors (clx/clatrix [[-0.8553908861324307 -0.14968230549170644]
                                         [0.5179830421177655 -0.9887341439551319]])}
       (clx/eigen-decomposition (clx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest upper-cholesky-decomposition-test
  (is= (clx/clatrix [[]]) (clx/upper-cholesky-decomposition (clx/clatrix [[]])))
  (is= (clx/clatrix [[2.0]]) (clx/upper-cholesky-decomposition (clx/clatrix [[4.0]])))
  (is= (clx/clatrix [[1.0 0.5] [0.0 1.6583123951777]])
       (clx/upper-cholesky-decomposition (clx/clatrix [[1.0 0.5] [0.5 3.0]]))))

(deftest sv-decomposition-test
  (is= {::clx/svd-left        (clx/clatrix [[]])
        ::clx/singular-values (clx/clatrix [[]])
        ::clx/svd-right       (clx/clatrix [[]])
        ::clx/rank            0}
       (clx/sv-decomposition (clx/clatrix [[]])))
  (is= {::clx/svd-left        (clx/clatrix [[1.0]])
        ::clx/singular-values (clx/clatrix [[3.0]])
        ::clx/svd-right       (clx/clatrix [[1.0]])
        ::clx/rank            1}
       (clx/sv-decomposition (clx/clatrix [[3.0]])))
  (is= {::clx/svd-left        (clx/clatrix [[1.0]])
        ::clx/singular-values (clx/clatrix [[5.0]])
        ::clx/svd-right       (clx/clatrix [[0.6000000000000001] [0.8]])
        ::clx/rank            1}
       (clx/sv-decomposition (clx/clatrix [[3.0 4.0]])))
  (is= {::clx/svd-left        (clx/clatrix [[-0.6000000000000001] [-0.8]])
        ::clx/singular-values (clx/clatrix [[5.0]])
        ::clx/svd-right       (clx/clatrix [[-1.0]])
        ::clx/rank            1}
       (clx/sv-decomposition (clx/clatrix [[3.0] [4.0]])))
  (is= {::clx/svd-left        (clx/clatrix [[-0.4899250213574801 -0.8717645745543189]
                                            [-0.8717645745543189 0.48992502135748034]])
        ::clx/singular-values (clx/clatrix [[7.343420458864691 0.0] [0.0 0.27235264699921624]])
        ::clx/svd-right       (clx/clatrix [[-0.6082871552778867 0.7937170381968225]
                                            [-0.7937170381968225 -0.6082871552778867]])
        ::clx/rank            2}
       (clx/sv-decomposition (clx/clatrix [[2.0 3.0] [4.0 5.0]])))
  (is= {::clx/svd-left        (clx/clatrix [[-0.20027709794089957 -0.9797392939146469]
                                            [-0.9797392939146469 0.2002770979408998]])
        ::clx/singular-values (clx/clatrix [[4.562639046204301 0.0] [0.0 0.6575142082509741]])
        ::clx/svd-right       (clx/clatrix [[-0.4733566832482428 -0.8808708477547789]
                                            [-0.8808708477547789 0.4733566832482428]])
        ::clx/rank            2}
       (clx/sv-decomposition (clx/clatrix [[1.0 0.5] [2.0 4.0]])))
  (is= {::clx/svd-left        (clx/clatrix [[1.0]])
        ::clx/singular-values (clx/clatrix [[1.118033988749895]])
        ::clx/svd-right       (clx/clatrix [[0.8944271909999157] [0.4472135954999579]])
        ::clx/rank            1}
       (clx/sv-decomposition (clx/clatrix [[1.0 0.5]])))
  (is= {::clx/svd-left        (clx/clatrix [[-0.8944271909999157] [-0.4472135954999579]])
        ::clx/singular-values (clx/clatrix [[1.118033988749895]])
        ::clx/svd-right       (clx/clatrix [[-1.0]])
        ::clx/rank            1}
       (clx/sv-decomposition (clx/clatrix [[1.0] [0.5]]))))

(deftest condition-test
  (is (m/nan? (clx/condition (clx/clatrix [[]]))))
  (is= 1.0 (clx/condition (clx/clatrix [[2.0]])))
  (is= 2.0 (clx/condition (clx/clatrix [[1.0 0.0] [0.0 2.0]])))
  (is= 4.0 (clx/condition (clx/clatrix [[1.0 0.0] [0.0 4.0]]))))

(deftest qr-decomposition-test
  (is= {::clx/Q (clx/clatrix [[]])
        ::clx/R (clx/clatrix [[]])}
       (clx/qr-decomposition (clx/clatrix [[]])))
  (is= {::clx/Q (clx/clatrix [[1.0]])
        ::clx/R (clx/clatrix [[2.0]])}
       (clx/qr-decomposition (clx/clatrix [[2.0]])))
  (is= {::clx/Q (clx/clatrix [[1.0]])
        ::clx/R (clx/clatrix [[2.0 3.0]])}
       (clx/qr-decomposition (clx/clatrix [[2.0 3.0]])))
  (is= {::clx/Q (clx/clatrix [[-0.5547001962252294 -0.8320502943378437]
                              [-0.8320502943378437 0.5547001962252291]])
        ::clx/R (clx/clatrix [[-3.605551275463989] [0.0]])}
       (clx/qr-decomposition (clx/clatrix [[2.0] [3.0]])))
  (is= {::clx/Q (clx/clatrix [[-0.44721359549995787 -0.8944271909999157]
                              [-0.8944271909999157 0.4472135954999581]])
        ::clx/R (clx/clatrix [[-2.23606797749979 -3.801315561749642]
                              [0.0 1.341640786499874]])}
       (clx/qr-decomposition (clx/clatrix [[1.0 0.5] [2.0 4.0]])))
  (is= {::clx/Q (clx/clatrix [[1.0]])
        ::clx/R (clx/clatrix [[1.0 0.5]])}
       (clx/qr-decomposition (clx/clatrix [[1.0 0.5]])))
  (is= {::clx/Q (clx/clatrix [[-0.8944271909999157 -0.4472135954999579] [-0.4472135954999579 0.8944271909999159]])
        ::clx/R (clx/clatrix [[-1.118033988749895] [0.0]])}
       (clx/qr-decomposition (clx/clatrix [[1.0] [0.5]]))))

(defspec-test test-inverse `clx/inverse)
(defspec-test test-determinant `clx/determinant)
(defspec-test test-lu-decomposition `clx/lu-decomposition)
(defspec-test test-eigen-decomposition `clx/eigen-decomposition)
;(defspec-test test-upper-cholesky-decomposition `clx/upper-cholesky-decomposition) ;slow-ish
;(defspec-test test-sv-decomposition `clx/sv-decomposition) ;slow-ish
(defspec-test test-condition `clx/condition)
(defspec-test test-qr-decomposition `clx/qr-decomposition)

#_(ost/unstrument)