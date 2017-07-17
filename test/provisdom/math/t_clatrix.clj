(ns provisdom.math.t-clatrix
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.clatrix :as clx]
            [provisdom.math.core :as m]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]))

(ost/instrument)

(deftest clatrix?-test
  (is (clx/clatrix? (clx/clatrix [[]])))
  (is (clx/clatrix? (clx/clatrix [[1]])))
  (is (clx/clatrix? (clx/clatrix [[1 2] [3 4]])))
  (is-not (clx/clatrix? "A"))
  (is-not (clx/clatrix? [[1 2 3] [3 4]])))

(deftest empty-clatrix?-test
  (is (clx/empty-clatrix? (clx/clatrix [[]])))
  (is-not (clx/empty-clatrix? (clx/clatrix [[1]]))))

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

(deftest positive-clatrix?-test
  (is (clx/positive-clatrix? (clx/clatrix [[]])))
  (is-not (clx/positive-clatrix? (clx/clatrix [[1.0 0.5] [2.0 4.0]])))
  (is (clx/positive-clatrix? (clx/clatrix [[1.0 0.5] [0.5 1.0]])))
  (is-not (clx/positive-clatrix? (clx/clatrix [[1.0 -1.1] [-1.1 1.0]])))
  (is-not (clx/positive-clatrix? (clx/clatrix [[1.0 -1.0] [-1.0 1.0]])))
  (is-not (clx/positive-clatrix? (clx/clatrix [[(m/next-up 1.0) -1.0] [-1.0 (m/next-up 1.0)]])))
  (is-not (clx/positive-clatrix?
            (clx/clatrix [[(m/next-up 1.0) -1.0] [-1.0 (m/next-up 1.0)]]) {::clx/accu m/*sgl-close*})))

(deftest clatrix-with-unit-diagonal?-test
  (is (clx/clatrix-with-unit-diagonal? (clx/clatrix [[]])))
  (is (clx/clatrix-with-unit-diagonal? (clx/clatrix [[1]])))
  (is (clx/clatrix-with-unit-diagonal? (clx/clatrix [[1 1]])))
  (is (clx/clatrix-with-unit-diagonal? (clx/clatrix [[1] [1]])))
  (is (clx/clatrix-with-unit-diagonal? (clx/clatrix [[1 1] [1 1]])))
  (is (clx/clatrix-with-unit-diagonal? (clx/clatrix [[1 0] [0 1]])))
  (is (clx/clatrix-with-unit-diagonal? (clx/clatrix [[1 0] [1 1]])))
  (is-not (clx/clatrix-with-unit-diagonal? (clx/clatrix [[1 0] [1 2]]))))

(deftest positive-clatrix-with-unit-diagonal?-test
  (is (clx/positive-clatrix-with-unit-diagonal? (clx/clatrix [[]])))
  (is-not (clx/positive-clatrix-with-unit-diagonal? (clx/clatrix [[1.0 0.5] [2.0 4.0]])))
  (is-not (clx/positive-clatrix-with-unit-diagonal? (clx/clatrix [[1.0 0.5] [0.5 2.0]])))
  (is-not (clx/positive-clatrix-with-unit-diagonal? (clx/clatrix [[1.0 -1.1] [-1.1 1.0]])))
  (is-not (clx/positive-clatrix-with-unit-diagonal? (clx/clatrix [[1.0 -1.0] [-1.0 1.0]])))
  (is-not (clx/positive-clatrix-with-unit-diagonal? (clx/clatrix [[(m/next-up 1.0) -1.0] [-1.0 (m/next-up 1.0)]])))
  (is-not (clx/positive-clatrix-with-unit-diagonal?
            (clx/clatrix [[(m/next-up 1.0) -1.0] [-1.0 (m/next-up 1.0)]]) {::clx/accu m/*sgl-close*})))

(deftest non-negative-clatrix?-test
  (is (clx/non-negative-clatrix? (clx/clatrix [[]])))
  (is-not (clx/non-negative-clatrix? (clx/clatrix [[1.0 0.5] [2.0 4.0]])))
  (is (clx/non-negative-clatrix? (clx/clatrix [[1.0 0.5] [0.5 2.0]])))
  (is-not (clx/non-negative-clatrix? (clx/clatrix [[1.0 -1.1] [-1.1 1.0]])))
  (is (clx/non-negative-clatrix? (clx/clatrix [[1.0 -1.0] [-1.0 1.0]])))
  (is (clx/non-negative-clatrix? (clx/clatrix [[1.0 (m/next-down -1.0)] [(m/next-down -1.0) 1.0]])))
  (is-not (clx/non-negative-clatrix?
            (clx/clatrix [[1.0 (m/next-down -1.0)] [(m/next-down -1.0) 1.0]]) {::clx/accu 1e-40})))

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
  (is= [[]] (clx/clatrix->matrix (clx/clatrix [[]])))
  (is= [[1.0]] (clx/clatrix->matrix (clx/clatrix [[1.0]])))
  (is= [[1.0 2.0]] (clx/clatrix->matrix (clx/clatrix [[1.0 2.0]])))
  (is= [[1.0] [2.0]] (clx/clatrix->matrix (clx/clatrix [[1.0] [2.0]])))
  (is= [[1.0 0.5] [2.0 4.0]] (clx/clatrix->matrix (clx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest constructor-tests
  (clatrix-and-clatrix->matrix-test))

(defspec-test test-clatrix `clx/clatrix)
(defspec-test test-clatrix->matrix `clx/clatrix->matrix)

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

(deftest diagonal-test
  (is= [] (clx/diagonal (clx/clatrix [[]])))
  (is= [1.0] (clx/diagonal (clx/clatrix [[1.0]])))
  (is= [1.0] (clx/diagonal (clx/clatrix [[1.0 2.0]])))
  (is= [1.0] (clx/diagonal (clx/clatrix [[1.0] [2.0]])))
  (is= [1.0 4.0] (clx/diagonal (clx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest some-kv-test
  (is= nil (clx/some-kv (fn [row column number] (> row (+ column number))) (clx/clatrix [[1.0 4.0] [3.0 5.0]])))
  (is= 1.0 (clx/some-kv (fn [row column number] (< row (+ column number))) (clx/clatrix [[1.0 4.0] [3.0 5.0]])))
  (is= 4.0 (clx/some-kv (fn [row column number] (> (dec row) (- column number))) (clx/clatrix [[1.0 4.0] [3.0 5.0]])))
  (is= 3.0 (clx/some-kv (fn [row column number] (> (dec row) (- column number)))
                        (clx/clatrix [[1.0 4.0] [3.0 5.0]])
                        {::clx/by-row? false})))

(deftest info-tests
  (rows-test)
  (columns-test)
  (diagonal-test)
  (some-kv-test))

(defspec-test test-rows `clx/rows)
(defspec-test test-columns `clx/columns)
(defspec-test test-diagonal `clx/diagonal)
(defspec-test test-some-kv `clx/some-kv)

(deftest transpose-test
  (is= (clx/clatrix [[]]) (clx/transpose (clx/clatrix [[]])))
  (is= (clx/clatrix [[1.0]]) (clx/transpose (clx/clatrix [[1.0]])))
  (is= (clx/clatrix [[1.0] [2.0]]) (clx/transpose (clx/clatrix [[1.0 2.0]])))
  (is= (clx/clatrix [[1.0 2.0]]) (clx/transpose (clx/clatrix [[1.0] [2.0]])))
  (is= (clx/clatrix [[1.0 2.0] [0.5 4.0]]) (clx/transpose (clx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest manipulation-tests
  (transpose-test))

(defspec-test test-transpose `clx/transpose)

(deftest mx*-test
  (is= (clx/clatrix [[]]) (clx/mx* (clx/clatrix [[]])))
  (is= (clx/clatrix [[]]) (clx/mx* (clx/clatrix [[1.0]]) (clx/clatrix [[]])))
  (is= (clx/clatrix [[6.0]]) (clx/mx* (clx/clatrix [[2.0]]) (clx/clatrix [[3.0]])))
  (is= (clx/clatrix [[26.0]]) (clx/mx* (clx/clatrix [[2.0 4.0]]) (clx/clatrix [[3.0] [5.0]])))
  (is= (clx/clatrix [[19.0 22.0] [43.0 50.0]])
       (clx/mx* (clx/clatrix [[1.0 2.0] [3.0 4.0]]) (clx/clatrix [[5.0 6.0] [7.0 8.0]])))
  (is= (clx/clatrix [[67.0 78.0] [201.0 234.0]])
       (clx/mx* (clx/clatrix [[1.0] [3.0]]) (clx/clatrix [[5.0 6.0]]) (clx/clatrix [[5.0 6.0] [7.0 8.0]]))))

(deftest math-tests
  (mx*-test))

(defspec-test test-mx* `clx/mx*)

(deftest inverse-test
  (is= (clx/clatrix [[]]) (clx/inverse (clx/clatrix [[]])))
  (is= (clx/clatrix [[0.5]]) (clx/inverse (clx/clatrix [[2.0]])))
  (is= (clx/clatrix [[1.0 0.5] [2.0 4.0]])
       (clx/inverse (clx/clatrix [[1.3333333333333333 -0.16666666666666663] [-0.6666666666666666 0.3333333333333333]]))))

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
  #_(is= {::clx/eigenvalues  [m/nan m/nan]  ;probably should have returned [m/inf+ m/inf+] -- compare Apache
          ::clx/eigenvectors (clx/clatrix [[0.0 1.0] [1.0 0.0]])}
         (clx/eigen-decomposition (clx/clatrix [[m/inf+ 0.0] [0.0 m/inf+]])))
  #_(is= {::clx/eigenvalues  [0.6972243622680055 4.302775637731996] ;off by a little, test at Apache?
          ::clx/eigenvectors (clx/clatrix [[-0.8553908861324309 -0.16211892282756657]
                                           [0.5179830421177656 -1.0708848574604801]])}
         (clx/eigen-decomposition (clx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest upper-cholesky-decomposition-test
  (is= (clx/clatrix [[]]) (clx/upper-cholesky-decomposition (clx/clatrix [[]])))
  (is= (clx/clatrix [[2.0]]) (clx/upper-cholesky-decomposition (clx/clatrix [[4.0]])))
  (is= (clx/clatrix [[1.0 0.5] [0.0 1.6583123951777]])
       (clx/upper-cholesky-decomposition (clx/clatrix [[1.0 0.5] [0.5 3.0]]))))

(deftest sv-decomposition-test
  (is= {::clx/S (clx/clatrix [[]]), ::clx/D (clx/clatrix [[]]), ::clx/VT (clx/clatrix [[]]), ::clx/rank 0}
       (clx/sv-decomposition (clx/clatrix [[]])))
  (is= {::clx/S (clx/clatrix [[3.0]]), ::clx/D (clx/clatrix [[1.0]]), ::clx/VT (clx/clatrix [[1.0]]), ::clx/rank 1}
       (clx/sv-decomposition (clx/clatrix [[3.0]])))
  #_(is= {::clx/S (clx/clatrix [[5.0]]), ::clx/D (clx/clatrix [[1.0]]), ::clx/VT (clx/clatrix [[0.6 0.8]]), ::clx/rank 1}
       (clx/sv-decomposition (clx/clatrix [[3.0 4.0]])))
  (is= {::clx/S (clx/clatrix [[5.0]])
        ::clx/D (clx/clatrix [[-0.6] [-0.8]])
        ::clx/VT (clx/clatrix [[-1.0]])
        ::clx/rank 1}
       (clx/sv-decomposition (clx/clatrix [[3.0] [4.0]])))
  #_(is= {::clx/S (clx/clatrix [[7.34 0.0] [0.0 0.272]])
        ::clx/D (clx/clatrix [[-0.49 -0.872] [-0.872 0.49]])
        ::clx/VT (clx/clatrix [[-0.608 0.794] [-0.794 -0.608]])
        ::clx/rank 2}
       (clx/sv-decomposition (clx/clatrix [[2.0 3.0] [4.0 5.0]])))
  (is= {::clx/S    (clx/clatrix [[4.562639046204301 0.0] [0.0 0.6575142082509741]])
        ::clx/D    (clx/clatrix [[-0.20027709794089957 -0.9797392939146469]
                                 [-0.9797392939146469 0.2002770979408998]])
        ::clx/VT   (clx/clatrix [[-0.4733566832482428 -0.8808708477547789]
                                 [-0.8808708477547789 0.4733566832482428]])
        ::clx/rank 2}
       (clx/sv-decomposition (clx/clatrix [[1.0 0.5] [2.0 4.0]])))
  #_(is= {::clx/S    (clx/clatrix [[1.118033988749895]])
        ::clx/D    (clx/clatrix [[-1.0]])
        ::clx/VT   (clx/clatrix [[-0.8944271909999157 -0.4472135954999579]])
        ::clx/rank 1}
       (clx/sv-decomposition (clx/clatrix [[1.0 0.5]])))
  (is= {::clx/S    (clx/clatrix [[1.118033988749895]])
        ::clx/D    (clx/clatrix [[-0.8944271909999157] [-0.4472135954999579]])
        ::clx/VT   (clx/clatrix [[-1.0]])
        ::clx/rank 1}
       (clx/sv-decomposition (clx/clatrix [[1.0] [0.5]]))))

(deftest condition-test
  (is (m/nan? (clx/condition (clx/clatrix [[]]))))
  (is= 1.0 (clx/condition (clx/clatrix [[2.0]])))
  (is= 2.0 (clx/condition (clx/clatrix [[1.0 0.0] [0.0 2.0]])))
  (is= 4.0 (clx/condition (clx/clatrix [[1.0 0.0] [0.0 4.0]]))))

(deftest lu-decomposition-test
  (is= {::clx/L (clx/clatrix [[]])
        ::clx/U (clx/clatrix [[]])
        ::clx/P (clx/clatrix [[]])}
       (clx/lu-decomposition (clx/clatrix [[]])))
  (is= {::clx/L (clx/clatrix [[1.0]])
        ::clx/U (clx/clatrix [[2.0]])
        ::clx/P (clx/clatrix [[1.0]])}
       (clx/lu-decomposition (clx/clatrix [[2.0]])))
  (is= {::clx/L (clx/clatrix [[1.0 0.0] [0.0 1.0]])
        ::clx/U (clx/clatrix [[1.0 0.0] [0.0 1.0]])
        ::clx/P (clx/clatrix [[1.0 0.0] [0.0 1.0]])}
       (clx/lu-decomposition (clx/clatrix [[1 0] [0 1]])))
  (is= {::clx/L (clx/clatrix [[1.0 0.0] [0.5 1.0]])
        ::clx/P (clx/clatrix [[0.0 1.0] [1.0 0.0]])
        ::clx/U (clx/clatrix [[2.0 4.0] [0.0 -1.5]])}
       (clx/lu-decomposition (clx/clatrix [[1.0 0.5] [2.0 4.0]]))))

(deftest determinant-test
  (is (m/nan? (clx/determinant (clx/clatrix [[]]))))
  (is= 2.0 (clx/determinant (clx/clatrix [[2.0]])))
  (is= -2.0 (clx/determinant (clx/clatrix [[1.0 2.0] [3.0 4.0]])))
  (is= 6.661338147750939E-16 (clx/determinant (clx/clatrix [[1.0 2.0 3.0] [4.0 5.0 6.0] [7.0 8.0 9.0]]))))

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
  #_(is= {::clx/Q (clx/clatrix [[-0.555 -0.832] [-0.832 0.555]])
        ::clx/R (clx/clatrix [[-3.61] [0.0]])}
       (clx/qr-decomposition (clx/clatrix [[2.0] [3.0]])))  ;test at Apache?  How do I get better Clatrix reporting?
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

(deftest decomposition-tests
  (inverse-test)
  (eigen-decomposition-test)
  (upper-cholesky-decomposition-test)
  (sv-decomposition-test)
  (condition-test)
  (lu-decomposition-test)
  (determinant-test)
  (qr-decomposition-test))

(defspec-test test-inverse `clx/inverse)
(defspec-test test-eigen-decomposition `clx/eigen-decomposition)
;(defspec-test test-upper-cholesky-decomposition `clx/upper-cholesky-decomposition) ;positive-matrix getting hung up
;(defspec-test test-sv-decomposition `clx/sv-decomposition) ;too slow, didn't finish
(defspec-test test-condition `clx/condition)
(defspec-test test-lu-decomposition `clx/lu-decomposition)
(defspec-test test-determinant `clx/determinant)
(defspec-test test-qr-decomposition `clx/qr-decomposition)


#_(ost/unstrument)