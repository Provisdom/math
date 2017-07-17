(ns provisdom.math.t-apache-matrix
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.apache-matrix :as apache-mx]
            [provisdom.math.core :as m]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]))

(ost/instrument)

(deftest clatrix?-test
  (is (apache-mx/clatrix? (apache-mx/clatrix [[]])))
  (is (apache-mx/clatrix? (apache-mx/clatrix [[1]])))
  (is (apache-mx/clatrix? (apache-mx/clatrix [[1 2] [3 4]])))
  (is-not (apache-mx/clatrix? "A"))
  (is-not (apache-mx/clatrix? [[1 2 3] [3 4]])))

(deftest empty-clatrix?-test
  (is (apache-mx/empty-clatrix? (apache-mx/clatrix [[]])))
  (is-not (apache-mx/empty-clatrix? (apache-mx/clatrix [[1]]))))

(deftest row-clatrix?-test
  (is-not (apache-mx/row-clatrix? (apache-mx/clatrix [[]])))
  (is (apache-mx/row-clatrix? (apache-mx/clatrix [[1]])))
  (is (apache-mx/row-clatrix? (apache-mx/clatrix [[1 1]])))
  (is-not (apache-mx/row-clatrix? (apache-mx/clatrix [[1] [1]]))))

(deftest column-clatrix?-test
  (is-not (apache-mx/column-clatrix? (apache-mx/clatrix [[]])))
  (is (apache-mx/column-clatrix? (apache-mx/clatrix [[1]])))
  (is-not (apache-mx/column-clatrix? (apache-mx/clatrix [[1 1]])))
  (is (apache-mx/column-clatrix? (apache-mx/clatrix [[1] [1]]))))

(deftest square-clatrix?-test
  (is (apache-mx/square-clatrix? (apache-mx/clatrix [[]])))
  (is (apache-mx/square-clatrix? (apache-mx/clatrix [[1]])))
  (is-not (apache-mx/square-clatrix? (apache-mx/clatrix [[1 1]])))
  (is-not (apache-mx/square-clatrix? (apache-mx/clatrix [[1] [1]])))
  (is (apache-mx/square-clatrix? (apache-mx/clatrix [[1 1] [1 1]]))))

(deftest diagonal-clatrix?-test
  (is (apache-mx/diagonal-clatrix? (apache-mx/clatrix [[]])))
  (is (apache-mx/diagonal-clatrix? (apache-mx/clatrix [[1]])))
  (is-not (apache-mx/diagonal-clatrix? (apache-mx/clatrix [[1 1]])))
  (is-not (apache-mx/diagonal-clatrix? (apache-mx/clatrix [[1] [1]])))
  (is-not (apache-mx/diagonal-clatrix? (apache-mx/clatrix [[1 1] [1 1]])))
  (is (apache-mx/diagonal-clatrix? (apache-mx/clatrix [[1 0] [0 1]]))))

(deftest upper-triangular-clatrix?-test
  (is (apache-mx/upper-triangular-clatrix? (apache-mx/clatrix [[]])))
  (is (apache-mx/upper-triangular-clatrix? (apache-mx/clatrix [[1]])))
  (is-not (apache-mx/upper-triangular-clatrix? (apache-mx/clatrix [[1 1]])))
  (is-not (apache-mx/upper-triangular-clatrix? (apache-mx/clatrix [[1] [1]])))
  (is-not (apache-mx/upper-triangular-clatrix? (apache-mx/clatrix [[1 1] [1 1]])))
  (is (apache-mx/upper-triangular-clatrix? (apache-mx/clatrix [[1 0] [0 1]])))
  (is (apache-mx/upper-triangular-clatrix? (apache-mx/clatrix [[1 1] [0 1]]))))

(deftest lower-triangular-clatrix?-test
  (is (apache-mx/lower-triangular-clatrix? (apache-mx/clatrix [[]])))
  (is (apache-mx/lower-triangular-clatrix? (apache-mx/clatrix [[1]])))
  (is-not (apache-mx/lower-triangular-clatrix? (apache-mx/clatrix [[1 1]])))
  (is-not (apache-mx/lower-triangular-clatrix? (apache-mx/clatrix [[1] [1]])))
  (is-not (apache-mx/lower-triangular-clatrix? (apache-mx/clatrix [[1 1] [1 1]])))
  (is (apache-mx/lower-triangular-clatrix? (apache-mx/clatrix [[1 0] [0 1]])))
  (is (apache-mx/lower-triangular-clatrix? (apache-mx/clatrix [[1 0] [1 1]]))))

(deftest symmetric-clatrix?-test
  (is (apache-mx/symmetric-clatrix? (apache-mx/clatrix [[]])))
  (is (apache-mx/symmetric-clatrix? (apache-mx/clatrix [[1]])))
  (is-not (apache-mx/symmetric-clatrix? (apache-mx/clatrix [[1 1]])))
  (is-not (apache-mx/symmetric-clatrix? (apache-mx/clatrix [[1] [1]])))
  (is (apache-mx/symmetric-clatrix? (apache-mx/clatrix [[1 1] [1 1]])))
  (is (apache-mx/symmetric-clatrix? (apache-mx/clatrix [[1 0] [0 1]])))
  (is-not (apache-mx/symmetric-clatrix? (apache-mx/clatrix [[1 0] [1 1]]))))

(deftest positive-clatrix?-test
  (is (apache-mx/positive-clatrix? (apache-mx/clatrix [[]])))
  (is-not (apache-mx/positive-clatrix? (apache-mx/clatrix [[1.0 0.5] [2.0 4.0]])))
  (is (apache-mx/positive-clatrix? (apache-mx/clatrix [[1.0 0.5] [0.5 1.0]])))
  (is-not (apache-mx/positive-clatrix? (apache-mx/clatrix [[1.0 -1.1] [-1.1 1.0]])))
  (is-not (apache-mx/positive-clatrix? (apache-mx/clatrix [[1.0 -1.0] [-1.0 1.0]])))
  (is-not (apache-mx/positive-clatrix? (apache-mx/clatrix [[(m/next-up 1.0) -1.0] [-1.0 (m/next-up 1.0)]])))
  (is-not (apache-mx/positive-clatrix?
            (apache-mx/clatrix [[(m/next-up 1.0) -1.0] [-1.0 (m/next-up 1.0)]]) {::apache-mx/accu m/*sgl-close*})))

(deftest clatrix-with-unit-diagonal?-test
  (is (apache-mx/clatrix-with-unit-diagonal? (apache-mx/clatrix [[]])))
  (is (apache-mx/clatrix-with-unit-diagonal? (apache-mx/clatrix [[1]])))
  (is (apache-mx/clatrix-with-unit-diagonal? (apache-mx/clatrix [[1 1]])))
  (is (apache-mx/clatrix-with-unit-diagonal? (apache-mx/clatrix [[1] [1]])))
  (is (apache-mx/clatrix-with-unit-diagonal? (apache-mx/clatrix [[1 1] [1 1]])))
  (is (apache-mx/clatrix-with-unit-diagonal? (apache-mx/clatrix [[1 0] [0 1]])))
  (is (apache-mx/clatrix-with-unit-diagonal? (apache-mx/clatrix [[1 0] [1 1]])))
  (is-not (apache-mx/clatrix-with-unit-diagonal? (apache-mx/clatrix [[1 0] [1 2]]))))

(deftest positive-clatrix-with-unit-diagonal?-test
  (is (apache-mx/positive-clatrix-with-unit-diagonal? (apache-mx/clatrix [[]])))
  (is-not (apache-mx/positive-clatrix-with-unit-diagonal? (apache-mx/clatrix [[1.0 0.5] [2.0 4.0]])))
  (is-not (apache-mx/positive-clatrix-with-unit-diagonal? (apache-mx/clatrix [[1.0 0.5] [0.5 2.0]])))
  (is-not (apache-mx/positive-clatrix-with-unit-diagonal? (apache-mx/clatrix [[1.0 -1.1] [-1.1 1.0]])))
  (is-not (apache-mx/positive-clatrix-with-unit-diagonal? (apache-mx/clatrix [[1.0 -1.0] [-1.0 1.0]])))
  (is-not (apache-mx/positive-clatrix-with-unit-diagonal? (apache-mx/clatrix [[(m/next-up 1.0) -1.0] [-1.0 (m/next-up 1.0)]])))
  (is-not (apache-mx/positive-clatrix-with-unit-diagonal?
            (apache-mx/clatrix [[(m/next-up 1.0) -1.0] [-1.0 (m/next-up 1.0)]]) {::apache-mx/accu m/*sgl-close*})))

(deftest non-negative-clatrix?-test
  (is (apache-mx/non-negative-clatrix? (apache-mx/clatrix [[]])))
  (is-not (apache-mx/non-negative-clatrix? (apache-mx/clatrix [[1.0 0.5] [2.0 4.0]])))
  (is (apache-mx/non-negative-clatrix? (apache-mx/clatrix [[1.0 0.5] [0.5 2.0]])))
  (is-not (apache-mx/non-negative-clatrix? (apache-mx/clatrix [[1.0 -1.1] [-1.1 1.0]])))
  (is (apache-mx/non-negative-clatrix? (apache-mx/clatrix [[1.0 -1.0] [-1.0 1.0]])))
  (is (apache-mx/non-negative-clatrix? (apache-mx/clatrix [[1.0 (m/next-down -1.0)] [(m/next-down -1.0) 1.0]])))
  (is-not (apache-mx/non-negative-clatrix?
            (apache-mx/clatrix [[1.0 (m/next-down -1.0)] [(m/next-down -1.0) 1.0]]) {::apache-mx/accu 1e-40})))

(deftest type-tests
  (clatrix?-test)
  (empty-clatrix?-test)
  (row-clatrix?-test)
  (column-clatrix?-test)
  (square-clatrix?-test)
  (diagonal-clatrix?-test)
  (upper-triangular-clatrix?-test)
  (lower-triangular-clatrix?-test)
  (symmetric-clatrix?-test)
  (positive-clatrix?-test)
  (clatrix-with-unit-diagonal?-test)
  (positive-clatrix-with-unit-diagonal?-test)
  (non-negative-clatrix?-test))

;(defspec-test test-clatrix? `clx/clatrix?) ;slow-ish
;(defspec-test test-empty-clatrix? `clx/empty-clatrix?) ;slow-ish
;(defspec-test test-row-clatrix? `clx/row-clatrix?) ;slow-ish
;(defspec-test test-column-clatrix? `clx/column-clatrix?) ;slow-ish
;(defspec-test test-square-clatrix? `clx/square-clatrix?) ;slow-ish
;(defspec-test test-diagonal-clatrix? `clx/diagonal-clatrix?) ;slow-ish
;(defspec-test test-upper-triangular-clatrix? `clx/upper-triangular-clatrix?) ;slow-ish
;(defspec-test test-lower-triangular-clatrix? `clx/lower-triangular-clatrix?) ;slow-ish
;(defspec-test test-symmetric-clatrix? `clx/symmetric-clatrix?) ;slow-ish
;(defspec-test test-positive-matrix? `clx/positive-clatrix?) ;slow-ish
;(defspec-test test-clatrix-with-unit-diagonal? `clx/clatrix-with-unit-diagonal?) ;slow-ish
;(defspec-test test-positive-matrix-with-unit-diagonal? `clx/positive-clatrix-with-unit-diagonal?) ;slow-ish
;(defspec-test test-non-negative-matrix? `clx/non-negative-clatrix?) ;slow-ish

(deftest clatrix-and-clatrix->matrix-test
  (is= [[]] (apache-mx/clatrix->matrix (apache-mx/clatrix [[]])))
  (is= [[1.0]] (apache-mx/clatrix->matrix (apache-mx/clatrix [[1.0]])))
  (is= [[1.0 2.0]] (apache-mx/clatrix->matrix (apache-mx/clatrix [[1.0 2.0]])))
  (is= [[1.0] [2.0]] (apache-mx/clatrix->matrix (apache-mx/clatrix [[1.0] [2.0]])))
  (is= [[1.0 0.5] [2.0 4.0]] (apache-mx/clatrix->matrix (apache-mx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest constructor-tests
  (clatrix-and-clatrix->matrix-test))

(defspec-test test-clatrix `apache-mx/clatrix)
(defspec-test test-clatrix->matrix `apache-mx/clatrix->matrix)

(deftest rows-test
  (is= 0 (apache-mx/rows (apache-mx/clatrix [[]])))
  (is= 1 (apache-mx/rows (apache-mx/clatrix [[1.0]])))
  (is= 1 (apache-mx/rows (apache-mx/clatrix [[1.0 2.0]])))
  (is= 2 (apache-mx/rows (apache-mx/clatrix [[1.0] [2.0]])))
  (is= 2 (apache-mx/rows (apache-mx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest columns-test
  (is= 0 (apache-mx/columns (apache-mx/clatrix [[]])))
  (is= 1 (apache-mx/columns (apache-mx/clatrix [[1.0]])))
  (is= 2 (apache-mx/columns (apache-mx/clatrix [[1.0 2.0]])))
  (is= 1 (apache-mx/columns (apache-mx/clatrix [[1.0] [2.0]])))
  (is= 2 (apache-mx/columns (apache-mx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest diagonal-test
  (is= [] (apache-mx/diagonal (apache-mx/clatrix [[]])))
  (is= [1.0] (apache-mx/diagonal (apache-mx/clatrix [[1.0]])))
  (is= [1.0] (apache-mx/diagonal (apache-mx/clatrix [[1.0 2.0]])))
  (is= [1.0] (apache-mx/diagonal (apache-mx/clatrix [[1.0] [2.0]])))
  (is= [1.0 4.0] (apache-mx/diagonal (apache-mx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest some-kv-test
  (is= nil (apache-mx/some-kv (fn [row column number] (> row (+ column number))) (apache-mx/clatrix [[1.0 4.0] [3.0 5.0]])))
  (is= 1.0 (apache-mx/some-kv (fn [row column number] (< row (+ column number))) (apache-mx/clatrix [[1.0 4.0] [3.0 5.0]])))
  (is= 4.0 (apache-mx/some-kv (fn [row column number] (> (dec row) (- column number))) (apache-mx/clatrix [[1.0 4.0] [3.0 5.0]])))
  (is= 3.0 (apache-mx/some-kv (fn [row column number] (> (dec row) (- column number)))
                              (apache-mx/clatrix [[1.0 4.0] [3.0 5.0]])
                              {::apache-mx/by-row? false})))

(deftest info-tests
  (rows-test)
  (columns-test)
  (diagonal-test)
  (some-kv-test))

(defspec-test test-rows `apache-mx/rows)
(defspec-test test-columns `apache-mx/columns)
(defspec-test test-diagonal `apache-mx/diagonal)
(defspec-test test-some-kv `apache-mx/some-kv)

(deftest transpose-test
  (is= (apache-mx/clatrix [[]]) (apache-mx/transpose (apache-mx/clatrix [[]])))
  (is= (apache-mx/clatrix [[1.0]]) (apache-mx/transpose (apache-mx/clatrix [[1.0]])))
  (is= (apache-mx/clatrix [[1.0] [2.0]]) (apache-mx/transpose (apache-mx/clatrix [[1.0 2.0]])))
  (is= (apache-mx/clatrix [[1.0 2.0]]) (apache-mx/transpose (apache-mx/clatrix [[1.0] [2.0]])))
  (is= (apache-mx/clatrix [[1.0 2.0] [0.5 4.0]]) (apache-mx/transpose (apache-mx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest manipulation-tests
  (transpose-test))

(defspec-test test-transpose `apache-mx/transpose)

(deftest mx*-test
  (is= (apache-mx/clatrix [[]]) (apache-mx/mx* (apache-mx/clatrix [[]])))
  (is= (apache-mx/clatrix [[]]) (apache-mx/mx* (apache-mx/clatrix [[1.0]]) (apache-mx/clatrix [[]])))
  (is= (apache-mx/clatrix [[6.0]]) (apache-mx/mx* (apache-mx/clatrix [[2.0]]) (apache-mx/clatrix [[3.0]])))
  (is= (apache-mx/clatrix [[26.0]]) (apache-mx/mx* (apache-mx/clatrix [[2.0 4.0]]) (apache-mx/clatrix [[3.0] [5.0]])))
  (is= (apache-mx/clatrix [[19.0 22.0] [43.0 50.0]])
       (apache-mx/mx* (apache-mx/clatrix [[1.0 2.0] [3.0 4.0]]) (apache-mx/clatrix [[5.0 6.0] [7.0 8.0]])))
  (is= (apache-mx/clatrix [[67.0 78.0] [201.0 234.0]])
       (apache-mx/mx* (apache-mx/clatrix [[1.0] [3.0]]) (apache-mx/clatrix [[5.0 6.0]]) (apache-mx/clatrix [[5.0 6.0] [7.0 8.0]]))))

(deftest math-tests
  (mx*-test))

(defspec-test test-mx* `apache-mx/mx*)

(deftest inverse-test
  (is= (apache-mx/clatrix [[]]) (apache-mx/inverse (apache-mx/clatrix [[]])))
  (is= (apache-mx/clatrix [[0.5]]) (apache-mx/inverse (apache-mx/clatrix [[2.0]])))
  (is= (apache-mx/clatrix [[1.0 0.5] [2.0 4.0]])
       (apache-mx/inverse (apache-mx/clatrix [[1.3333333333333333 -0.16666666666666663] [-0.6666666666666666 0.3333333333333333]]))))

(deftest eigen-decomposition-test
  (is= {::apache-mx/eigenvalues  []
        ::apache-mx/eigenvectors (apache-mx/clatrix [[]])}
       (apache-mx/eigen-decomposition (apache-mx/clatrix [[]])))
  (is= {::apache-mx/eigenvalues  [2.0]
        ::apache-mx/eigenvectors (apache-mx/clatrix [[1.0]])}
       (apache-mx/eigen-decomposition (apache-mx/clatrix [[2.0]])))
  (is= {::apache-mx/eigenvalues  [1e100 1e100]
        ::apache-mx/eigenvectors (apache-mx/clatrix [[-1.0 0.0] [0.0 1.0]])}
       (apache-mx/eigen-decomposition (apache-mx/clatrix [[1e100 0.0] [0.0 1e100]])))
  #_(is= {::apache-mx/eigenvalues  [m/nan m/nan]  ;probably should have returned [m/inf+ m/inf+] -- compare Apache
          ::apache-mx/eigenvectors (apache-mx/clatrix [[0.0 1.0] [1.0 0.0]])}
         (apache-mx/eigen-decomposition (apache-mx/clatrix [[m/inf+ 0.0] [0.0 m/inf+]])))
  #_(is= {::apache-mx/eigenvalues  [0.6972243622680055 4.302775637731996] ;off by a little, test at Apache?
          ::apache-mx/eigenvectors (apache-mx/clatrix [[-0.8553908861324309 -0.16211892282756657]
                                           [0.5179830421177656 -1.0708848574604801]])}
         (apache-mx/eigen-decomposition (apache-mx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest upper-cholesky-decomposition-test
  (is= (apache-mx/clatrix [[]]) (apache-mx/upper-cholesky-decomposition (apache-mx/clatrix [[]])))
  (is= (apache-mx/clatrix [[2.0]]) (apache-mx/upper-cholesky-decomposition (apache-mx/clatrix [[4.0]])))
  (is= (apache-mx/clatrix [[1.0 0.5] [0.0 1.6583123951777]])
       (apache-mx/upper-cholesky-decomposition (apache-mx/clatrix [[1.0 0.5] [0.5 3.0]]))))

(deftest sv-decomposition-test
  (is= {::apache-mx/S (apache-mx/clatrix [[]]), ::apache-mx/D (apache-mx/clatrix [[]]), ::apache-mx/VT (apache-mx/clatrix [[]]), ::apache-mx/rank 0}
       (apache-mx/sv-decomposition (apache-mx/clatrix [[]])))
  (is= {::apache-mx/S (apache-mx/clatrix [[3.0]]), ::apache-mx/D (apache-mx/clatrix [[1.0]]), ::apache-mx/VT (apache-mx/clatrix [[1.0]]), ::apache-mx/rank 1}
       (apache-mx/sv-decomposition (apache-mx/clatrix [[3.0]])))
  #_(is= {::apache-mx/S (apache-mx/clatrix [[5.0]]), ::apache-mx/D (apache-mx/clatrix [[1.0]]), ::apache-mx/VT (apache-mx/clatrix [[0.6 0.8]]), ::apache-mx/rank 1}
         (apache-mx/sv-decomposition (apache-mx/clatrix [[3.0 4.0]])))
  (is= {::apache-mx/S    (apache-mx/clatrix [[5.0]])
        ::apache-mx/D    (apache-mx/clatrix [[-0.6] [-0.8]])
        ::apache-mx/VT   (apache-mx/clatrix [[-1.0]])
        ::apache-mx/rank 1}
       (apache-mx/sv-decomposition (apache-mx/clatrix [[3.0] [4.0]])))
  #_(is= {::apache-mx/S    (apache-mx/clatrix [[7.34 0.0] [0.0 0.272]])
          ::apache-mx/D    (apache-mx/clatrix [[-0.49 -0.872] [-0.872 0.49]])
          ::apache-mx/VT   (apache-mx/clatrix [[-0.608 0.794] [-0.794 -0.608]])
          ::apache-mx/rank 2}
         (apache-mx/sv-decomposition (apache-mx/clatrix [[2.0 3.0] [4.0 5.0]])))
  (is= {::apache-mx/S    (apache-mx/clatrix [[4.562639046204301 0.0] [0.0 0.6575142082509741]])
        ::apache-mx/D    (apache-mx/clatrix [[-0.20027709794089957 -0.9797392939146469]
                                 [-0.9797392939146469 0.2002770979408998]])
        ::apache-mx/VT   (apache-mx/clatrix [[-0.4733566832482428 -0.8808708477547789]
                                 [-0.8808708477547789 0.4733566832482428]])
        ::apache-mx/rank 2}
       (apache-mx/sv-decomposition (apache-mx/clatrix [[1.0 0.5] [2.0 4.0]])))
  #_(is= {::apache-mx/S    (apache-mx/clatrix [[1.118033988749895]])
          ::apache-mx/D    (apache-mx/clatrix [[-1.0]])
          ::apache-mx/VT   (apache-mx/clatrix [[-0.8944271909999157 -0.4472135954999579]])
          ::apache-mx/rank 1}
         (apache-mx/sv-decomposition (apache-mx/clatrix [[1.0 0.5]])))
  (is= {::apache-mx/S    (apache-mx/clatrix [[1.118033988749895]])
        ::apache-mx/D    (apache-mx/clatrix [[-0.8944271909999157] [-0.4472135954999579]])
        ::apache-mx/VT   (apache-mx/clatrix [[-1.0]])
        ::apache-mx/rank 1}
       (apache-mx/sv-decomposition (apache-mx/clatrix [[1.0] [0.5]]))))

(deftest condition-test
  (is (m/nan? (apache-mx/condition (apache-mx/clatrix [[]]))))
  (is= 1.0 (apache-mx/condition (apache-mx/clatrix [[2.0]])))
  (is= 2.0 (apache-mx/condition (apache-mx/clatrix [[1.0 0.0] [0.0 2.0]])))
  (is= 4.0 (apache-mx/condition (apache-mx/clatrix [[1.0 0.0] [0.0 4.0]]))))

(deftest lu-decomposition-test
  (is= {::apache-mx/L (apache-mx/clatrix [[]])
        ::apache-mx/U (apache-mx/clatrix [[]])
        ::apache-mx/P (apache-mx/clatrix [[]])}
       (apache-mx/lu-decomposition (apache-mx/clatrix [[]])))
  (is= {::apache-mx/L (apache-mx/clatrix [[1.0]])
        ::apache-mx/U (apache-mx/clatrix [[2.0]])
        ::apache-mx/P (apache-mx/clatrix [[1.0]])}
       (apache-mx/lu-decomposition (apache-mx/clatrix [[2.0]])))
  (is= {::apache-mx/L (apache-mx/clatrix [[1.0 0.0] [0.0 1.0]])
        ::apache-mx/U (apache-mx/clatrix [[1.0 0.0] [0.0 1.0]])
        ::apache-mx/P (apache-mx/clatrix [[1.0 0.0] [0.0 1.0]])}
       (apache-mx/lu-decomposition (apache-mx/clatrix [[1 0] [0 1]])))
  (is= {::apache-mx/L (apache-mx/clatrix [[1.0 0.0] [0.5 1.0]])
        ::apache-mx/P (apache-mx/clatrix [[0.0 1.0] [1.0 0.0]])
        ::apache-mx/U (apache-mx/clatrix [[2.0 4.0] [0.0 -1.5]])}
       (apache-mx/lu-decomposition (apache-mx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest determinant-test
  (is (m/nan? (apache-mx/determinant (apache-mx/clatrix [[]]))))
  (is= 2.0 (apache-mx/determinant (apache-mx/clatrix [[2.0]])))
  (is= -2.0 (apache-mx/determinant (apache-mx/clatrix [[1.0 2.0] [3.0 4.0]])))
  (is= 6.661338147750939E-16 (apache-mx/determinant (apache-mx/clatrix [[1.0 2.0 3.0] [4.0 5.0 6.0] [7.0 8.0 9.0]]))))

(deftest qr-decomposition-test
  (is= {::apache-mx/Q (apache-mx/clatrix [[]])
        ::apache-mx/R (apache-mx/clatrix [[]])}
       (apache-mx/qr-decomposition (apache-mx/clatrix [[]])))
  (is= {::apache-mx/Q (apache-mx/clatrix [[1.0]])
        ::apache-mx/R (apache-mx/clatrix [[2.0]])}
       (apache-mx/qr-decomposition (apache-mx/clatrix [[2.0]])))
  (is= {::apache-mx/Q (apache-mx/clatrix [[1.0]])
        ::apache-mx/R (apache-mx/clatrix [[2.0 3.0]])}
       (apache-mx/qr-decomposition (apache-mx/clatrix [[2.0 3.0]])))
  #_(is= {::apache-mx/Q (apache-mx/clatrix [[-0.555 -0.832] [-0.832 0.555]])
          ::apache-mx/R (apache-mx/clatrix [[-3.61] [0.0]])}
         (apache-mx/qr-decomposition (apache-mx/clatrix [[2.0] [3.0]])))  ;test at Apache?  How do I get better Clatrix reporting?
  (is= {::apache-mx/Q (apache-mx/clatrix [[-0.44721359549995787 -0.8944271909999157]
                              [-0.8944271909999157 0.4472135954999581]])
        ::apache-mx/R (apache-mx/clatrix [[-2.23606797749979 -3.801315561749642]
                              [0.0 1.341640786499874]])}
       (apache-mx/qr-decomposition (apache-mx/clatrix [[1.0 0.5] [2.0 4.0]])))
  (is= {::apache-mx/Q (apache-mx/clatrix [[1.0]])
        ::apache-mx/R (apache-mx/clatrix [[1.0 0.5]])}
       (apache-mx/qr-decomposition (apache-mx/clatrix [[1.0 0.5]])))
  (is= {::apache-mx/Q (apache-mx/clatrix [[-0.8944271909999157 -0.4472135954999579] [-0.4472135954999579 0.8944271909999159]])
        ::apache-mx/R (apache-mx/clatrix [[-1.118033988749895] [0.0]])}
       (apache-mx/qr-decomposition (apache-mx/clatrix [[1.0] [0.5]]))))

(deftest decomposition-tests
  (inverse-test)
  (eigen-decomposition-test)
  (upper-cholesky-decomposition-test)
  (sv-decomposition-test)
  (condition-test)
  (lu-decomposition-test)
  (determinant-test)
  (qr-decomposition-test))

(defspec-test test-inverse `apache-mx/inverse)
(defspec-test test-eigen-decomposition `apache-mx/eigen-decomposition)
;(defspec-test test-upper-cholesky-decomposition `clx/upper-cholesky-decomposition) ;positive-matrix getting hung up
;(defspec-test test-sv-decomposition `clx/sv-decomposition) ;too slow, didn't finish
(defspec-test test-condition `apache-mx/condition)
(defspec-test test-lu-decomposition `apache-mx/lu-decomposition)
(defspec-test test-determinant `apache-mx/determinant)
(defspec-test test-qr-decomposition `apache-mx/qr-decomposition)

(comment

  (facts "decomposition"
         (fact "cholesky"
               (first (cholesky-decomposition
                        (apache-commons [[1.0 0.5] [0.5 3.0]])))
               => (apache-commons [[1.0 0.0] [0.5 1.6583123951777]])

               (first (cholesky-decomposition [[1.0 0.5] [0.5 3.0]]))
               => [[1.0 0.0] [0.5 1.6583123951777]])
         (fact "lower cholesky"
               (lower-cholesky (apache-commons [[1.0 0.5] [0.5 3.0]]))
               => (apache-commons [[1.0 0.0] [0.5 1.6583123951777]])
               (lower-cholesky [[1.0 0.5] [0.5 3.0]]) => [[1.0 0.0]
                                                          [0.5 1.6583123951777]])
         (fact "cholesky with positive-semi-definite"
               (second (cholesky-decomposition-semi-definite cl 1e-4))
               => (clatrix [[0.25 2.0] [0.9682458365518543 0.0]])
               (first (cholesky-decomposition
                        (apache-commons [[1.0 0.5] [0.5 3.0]])))
               => (apache-commons [[1.0 0.0] [0.5 1.6583123951777]])
               (first (cholesky-decomposition-semi-definite
                        (clatrix [[1.0 0.5 1e-9] [0.5 3.0 1e-9] [1e-9 1e-9 1e-6]])
                        1e-4))
               => (clatrix [[1.0 0.0 0.0] [0.5 1.6583123951777 0.0]
                            [1.0E-9 3.0151134457776366E-10
                             9.999999999994545E-4]])
               (second (cholesky-decomposition-semi-definite
                         (clatrix [[1.0 0.5 1e-9] [0.5 3.0 1e-9]
                                   [1e-9 1e-9 1e-6]]) 1e-4))
               => (clatrix [[1.0 0.5 1.0E-9]
                            [0.0 1.6583123951777 3.0151134457776366E-10]
                            [0.0 0.0 9.999999999994545E-4]])
               (first (cholesky-decomposition-semi-definite
                        [[1.0 0.5 1e-9] [0.5 3.0 1e-9] [1e-9 1e-9 1e-6]] 1e-4))
               => [[1.0 0.0 0.0] [0.5 1.6583123951777 0.0]
                   [1.0E-9 3.0151134457776366E-10 9.999999999994545E-4]]
               (second (cholesky-decomposition-semi-definite
                         [[0.08061440651728713 0.048368643910372044]
                          [0.048368643910372044 0.029021186346223526]] 1e-14))
               => [[0.28392676259431254 0.17035605755658673] [0.0 0.0]]
               (cholesky-decomposition-semi-definite cl-row) => (throws))
         (fact "cholesky rectangular"
               (:B (cholesky-rectangular ap 1e-4))
               => (apache-commons [[0.25 0.9682458365518543] [2.0 0.0]])
               (:B (cholesky-rectangular (apache-commons [[1.0 0.5] [0.5 3.0]])
                                         1e-4))
               => (apache-commons [[0.2886751345948129 0.9574271077563381]
                                   [1.7320508075688772 0.0]])
               (:B (cholesky-rectangular
                     (apache-commons [[1.0 0.5 1e-9] [0.5 3.0 1e-9]
                                      [1e-9 1e-9 1e-6]]) 1e-4))
               => (apache-commons [[0.2886751345948129 0.9574271077563381]
                                   [1.7320508075688772 0.0]
                                   [5.773502691896259E-10
                                    8.703882797784892E-10]])
               (:B (cholesky-rectangular (clatrix [[1.0 0.5 1e-9] [0.5 3.0 1e-9]
                                                   [1e-9 1e-9 1e-6]]) 1e-4))
               => (clatrix [[0.2886751345948129 0.9574271077563381]
                            [1.7320508075688772 0.0]
                            [5.773502691896259E-10 8.703882797784892E-10]])
               (cholesky-rectangular
                 [[1.0 0.5 1e-9] [0.5 3.0 1e-9] [1e-9 1e-9 1e-6]] 1e-4)
               => {:B [[0.2886751345948129 0.9574271077563381]
                       [1.7320508075688772 0.0]
                       [5.773502691896259E-10 8.703882797784892E-10]], :rank 2}
               (cholesky-rectangular cl-row) => (throws))
         (fact "sv-decomposition with rank"
               (sv-decomposition-with-rank ve)
               => {:S [[4.562639046204302 0.0] [0.0 0.6575142082509742]],
                   :U [[0.20027709794089957 0.9797392939146471]
                       [0.979739293914647 -0.2002770979408996]],
                   :UT [[0.20027709794089957 0.979739293914647]
                        [0.9797392939146471 -0.2002770979408996]],
                   :V [[0.47335668324824287 0.8808708477547789]
                       [0.8808708477547789 -0.47335668324824287]],
                   :VT [[0.47335668324824287 0.8808708477547789]
                        [0.8808708477547789 -0.47335668324824287]], :rank 2}
               (sv-decomposition-with-rank ve-row)
               => {:S  [[1.118033988749895]], :U [[-1.0]], :UT [[-1.0]],
                   :V  [[-0.8944271909999157] [-0.4472135954999579]],
                   :VT [[-0.8944271909999157 -0.4472135954999579]], :rank 1}
               (:S (sv-decomposition-with-rank cl-row))
               => (clatrix [[1.118033988749895]])
               (:U (sv-decomposition-with-rank cl-row)) => (clatrix [[1.0]])
               (:UT (sv-decomposition-with-rank cl-row)) => (clatrix [[1.0]])
               (:V (sv-decomposition-with-rank cl-row))
               => (clatrix [[0.8944271909999157] [0.4472135954999579]])
               (:VT (sv-decomposition-with-rank cl-row))
               => (clatrix [[0.8944271909999157 0.4472135954999579]])
               (sv-decomposition-with-rank ve-col)
               => {:S  [[1.118033988749895]],
                   :U  [[-0.8944271909999157] [-0.4472135954999579]],
                   :UT [[-0.8944271909999157 -0.4472135954999579]],
                   :V  [[-1.0]], :VT [[-1.0]], :rank 1}
               (sv-decomposition-with-rank ve1D) => (throws))
         (fact "sv-decomposition"
               (sv-decomposition ve)
               => [[[-0.20027709794089957 -0.9797392939146469]
                    [-0.9797392939146469 0.2002770979408998]],
                   [[4.562639046204301 0.0] [0.0 0.6575142082509741]],
                   [[-0.4733566832482428 -0.8808708477547789]
                    [-0.8808708477547789 0.4733566832482428]]]
               (sv-decomposition ap)
               => [(apache-commons [[0.20027709794089957 0.9797392939146471]
                                    [0.979739293914647 -0.2002770979408996]])
                   (apache-commons [[4.562639046204302 0.0]
                                    [0.0 0.6575142082509742]]),
                   (apache-commons [[0.47335668324824287 0.8808708477547789]
                                    [0.8808708477547789 -0.47335668324824287]])]
               (sv-decomposition cl)
               => [(clatrix [[-0.20027709794089957 -0.9797392939146469]
                             [-0.9797392939146469 0.2002770979408998]])
                   (clatrix [[4.562639046204301 0.0] [0.0 0.6575142082509741]]),
                   (clatrix [[-0.4733566832482428 -0.8808708477547789]
                             [-0.8808708477547789 0.4733566832482428]])])
         (fact "rank"
               (rank ve) => 2
               (rank ve 1e-6) => 2
               (rank (clatrix [[1.0 0.5 1e-9] [0.5 3.0 1e-9] [1e-9 1e-9 1e-6]])
                     1e-4) => 2
               (rank (clatrix [[1.0 0.5 1e-9] [0.5 3.0 1e-9] [1e-9 1e-9 1e-6]])
                     1e-10) => 3
               (rank ap) => 2
               (rank cl) => 2
               (rank ve-row) => 1
               (rank cl-col) => 1
               (rank ap1D) => (throws))
         (fact "condition"
               (def cm (diagonal (second (sv-decomposition ve))))
               (condition cm) => 6.939225021982696)
         (fact "lu-decomposition-with-permutation-matrix"
               (lu-decomposition-with-permutation-matrix ap)
               => {:L (apache-commons [[1.0 0.0] [0.5 1.0]]),
                   :P (apache-commons [[0.0 1.0] [1.0 0.0]]),
                   :U (apache-commons [[2.0 4.0] [0.0 -1.5]])}
               (lu-decomposition-with-permutation-matrix cl)
               => {:L (clatrix [[1.0 0.0] [0.5 1.0]]),
                   :P (clatrix [[0.0 1.0] [1.0 0.0]]),
                   :U (clatrix [[2.0 4.0] [0.0 -1.5]])}
               (lu-decomposition-with-permutation-matrix ve)
               => {:L [[1.0 0.0] [0.5 1.0]], :P [[0.0 1.0] [1.0 0.0]],
                   :U [[2.0 4.0] [0.0 -1.5]]}
               (lu-decomposition-with-permutation-matrix ve-col) => (throws)
               (lu-decomposition-with-permutation-matrix ap-row) => (throws))
         (fact "lu-decomposition"
               (lu-decomposition ap)
               => [(apache-commons [[1.0 0.0] [0.5 1.0]])
                   (apache-commons [[2.0 4.0] [0.0 -1.5]])]
               (lu-decomposition cl)
               => [(clatrix [[1.0 0.0] [0.5 1.0]])
                   (clatrix [[2.0 4.0] [0.0 -1.5]])]
               (lu-decomposition ve) => [[[1.0 0.0] [0.5 1.0]]
                                         [[2.0 4.0] [0.0 -1.5]]])
         (fact "qr-decomposition"
               (qr-decomposition ap)
               => [(apache-commons [[-0.44721359549995787 0.8944271909999159]
                                    [-0.8944271909999157 -0.447213595499958]])
                   (apache-commons [[-2.23606797749979 -3.801315561749642]
                                    [0.0 -1.341640786499874]])]
               (qr-decomposition cl)
               => [(clatrix [[-0.44721359549995787 -0.8944271909999157]
                             [-0.8944271909999157 0.4472135954999581]])
                   (clatrix [[-2.23606797749979 -3.801315561749642]
                             [0.0 1.341640786499874]])]
               (qr-decomposition ve)
               => [[[-0.44721359549995787 -0.8944271909999157]
                    [-0.8944271909999157 0.4472135954999581]]
                   [[-2.23606797749979 -3.801315561749642]
                    [0.0 1.341640786499874]]]
               (qr-decomposition ve-row) => [[[1.0]] [[1.0 0.5]]]
               (qr-decomposition ve-col)
               => [[[-0.8944271909999157 -0.4472135954999579]
                    [-0.4472135954999579 0.8944271909999159]]
                   [[-1.118033988749895] [0.0]]]
               (qr-decomposition ap-row)
               => [(apache-commons [[-1.0]])
                   (apache-commons [[-1.0 -0.5]])]
               (qr-decomposition ap-col)
               => [(apache-commons [[-0.894427190999916 -0.4472135954999579]
                                    [-0.4472135954999579 0.8944271909999159]])
                   (apache-commons [[-1.118033988749895] [0.0]])])
         (fact "rrqr-decomposition"
               (rrqr-decomposition ve 1e-6)
               => {:P [[0.0 1.0] [1.0 0.0]],
                   :Q [[-0.12403473458920855 0.9922778767136677]
                       [-0.9922778767136677 -0.12403473458920833]],
                   :QT [[-0.12403473458920855 -0.9922778767136677]
                        [0.9922778767136677 -0.12403473458920833]],
                   :R [[-4.031128874149275 -2.108590488016544]
                       [0.0 0.7442084075352513]], :rank 2}
               (rrqr-decomposition ve-row 1e-6)
               => {:P [[1.0 0.0] [0.0 1.0]], :Q [[-1.0]], :QT [[-1.0]],
                   :R [[-1.0 -0.5]], :rank 1}
               (rrqr-decomposition ve-col 1e-6)
               => {:P  [[1.0]],
                   :Q  [[-0.894427190999916 -0.4472135954999579]
                        [-0.4472135954999579 0.8944271909999159]],
                   :QT [[-0.894427190999916 -0.4472135954999579]
                        [-0.4472135954999579 0.8944271909999159]],
                   :R  [[-1.118033988749895] [0.0]], :rank 1})
         (fact "eigenvalues"
               (eigenvalues [[m/inf+ 0.0] [0.0 m/inf+]]) => [m/inf+ m/inf+]
               (eigenvalues ap) => [0.6972243622680055 4.302775637731996]
               (eigenvalues cl) => [0.6972243622680057 4.302775637731995]
               (eigenvalues ve-row) => (throws))
         (fact "eigen-decomposition"
               (eigen-decomposition cl)
               => [(clatrix [[-0.8553908861324309 -0.16211892282756657]
                             [0.5179830421177656 -1.0708848574604801]])
                   (clatrix [[0.6972243622680055 0.0] [0.0 4.302775637731996]])
                   (clatrix [[-0.8553908861324309 0.5179830421177656]
                             [-0.16211892282756657 -1.0708848574604801]])]
               (eigen-decomposition ve)
               => [[[-0.8553908861324309 -0.16211892282756657]
                    [0.5179830421177656 -1.0708848574604801]]
                   [[0.6972243622680055 0.0] [0.0 4.302775637731996]]
                   [[-0.8553908861324309 0.5179830421177656]
                    [-0.16211892282756657 -1.0708848574604801]]]
               (eigen-decomposition cl-row) => (throws)))

  (facts "solve"
         (fact "linear least squares"
               (linear-least-squares ap [7.0 9.0])
               => [7.833333333333333 -1.6666666666666665]
               (linear-least-squares [[1.0 0.4 0.2] [0.6 0.3 0.9]] [7.0 9.0])
               => [-24.999999999999996 80.0 0.0]
               (linear-least-squares [[1.0 0.4] [0.2 0.6] [0.3 0.9]]
                                     [7.0 9.0 12.0])
               => [1.6863905325443753 13.284023668639056]
               (linear-least-squares cl [7.0 9.0]) => [7.833333333333333
                                                       -1.6666666666666665]
               (linear-least-squares cl-col [7.0 9.0]) => [9.200000000000003]
               (linear-least-squares ap-row [7.0 9.0]) => (throws)
               (linear-least-squares ve1D [7.0 9.0]) => (throws))
         (fact "linear-least-squares-with-error-matrix"
               (linear-least-squares-with-error-matrix [[1.0 2.0]] [3.0])
               => (throws)
               (linear-least-squares-with-error-matrix ap [7.0 9.0])
               => {:E (apache-commons [[1.8055555555555545 -0.944444444444444]
                                       [-0.944444444444444 0.5555555555555554]]),
                   :S [7.833333333333333 -1.6666666666666665]}
               (linear-least-squares-with-error-matrix cl [7.0 9.0])
               => {:E (clatrix [[1.8055555555555545 -0.944444444444444]
                                [-0.944444444444444 0.5555555555555554]]),
                   :S [7.833333333333333 -1.6666666666666665]}
               (linear-least-squares-with-error-matrix cl-col [7.0 9.0])
               => {:E [0.7999999999999999], :S [9.200000000000003]}
               (linear-least-squares-with-error-matrix ap-row [7.0 9.0])
               => (throws)
               (linear-least-squares-with-error-matrix ve1D [7.0 9.0])
               => (throws))
         (fact "matrix-solve-iterative"
               (matrix-solve-iterative [[1.0 0.5] [0.5 3.0]] [7.0 9.0]
                                       :solver :symm)
               => [5.999999999999998 1.9999999999999984]
               (matrix-solve-iterative ap [7.0 9.0] :solver :symm) => (throws)))

  )

#_(ost/unstrument)