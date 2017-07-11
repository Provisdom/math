(ns provisdom.test.math.matrix
  (:require [midje.sweet :refer :all]
            [criterium.core :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.matrix :refer :all]
            [provisdom.utility-belt.core :as co]
            [provisdom.math.core :as m]
            [provisdom.math.random :as ra]))

(def test-rnd-lazy "rnd-lazy for testing" (ra/random))
(def test-rnd "rnd for testing" (first test-rnd-lazy))

;;;REDUNDANCY
;;;matrix? mget mset row-matrix? column-matrix? square? zero-matrix? 
;;;identity-matrix row-count column-count get-row get-column new-vector 
;;;new-matrix compute-matrix zero-matrix div add sub abs vec? set-row negate 
;;;emin emax exp log log10 ecount eseq square mul to-vector dimensionality
;;;trace

(def se '((1.0 0.5) (2.0 4.0)))
(def ve (matrix se))
(def cl (clatrix se))
(def ap (apache-commons se))
(def se1D '(1.0 0.5))
(def ve1D (matrix se1D))
(def cl1D (clatrix se1D))
(def ap1D (apache-commons se1D))
(def se-row '((1.0 0.5)))
(def ve-row (matrix se-row))
(def cl-row (clatrix se-row))
(def ap-row (apache-commons se-row))
(def se-col '((1.0) (0.5)))
(def ve-col (matrix se-col))
(def cl-col (clatrix se-col))
(def ap-col (apache-commons se-col))
(def se-sym '((1.0 0.5) (0.5 2.0)))
(def ve-sym (matrix se-sym))
(def cl-sym (clatrix se-sym))
(def ap-sym (apache-commons se-sym))

(facts "get"
       (fact "row"
             (get-row-as-matrix se -1) => (throws)
             (get-row-as-matrix ve 0) => [[1.0 0.5]]
             (get-row-as-matrix ap 1) => (apache-commons [[2.0 4.0]])
             (get-row-as-matrix cl 1) => (clatrix [[2.0 4.0]])
             (get-row-as-matrix se 2) => (throws))
       (fact "column"
             (get-column-as-matrix se -1) => (throws)
             (get-column-as-matrix ve 0) => [[1.0] [2.0]]
             (get-column-as-matrix ap 1) => (apache-commons [[0.5] [4.0]])
             (get-column-as-matrix cl 1) => (clatrix [[0.5] [4.0]])
             (get-column-as-matrix se 2) => (throws))
       (fact "diagonal"
             (diagonal ap) => [1.0 4.0]
             (diagonal cl) => [1.0 4.0]
             (diagonal ve) => [1.0 4.0]
             (diagonal [[1]]) => [1]
             (diagonal [[]]) => []
             (diagonal ap1D) => (throws)
             (diagonal cl-row) => [1.0]
             (diagonal ve-col) => [1.0]
             (diagonal ve 1) => [0.5]
             (diagonal ve 2) => nil
             (diagonal ve -1) => [2.0]
             (diagonal ve -2) => nil)
       (fact "slices"
             (get-slices-as-matrix se :rows 0) => [[1.0 0.5]]
             (get-slices-as-matrix ve :rows [0 1]) => [[1.0 0.5] [2.0 4.0]]
             (get-slices-as-matrix cl :columns 0) => (clatrix [[1.0] [2.0]])
             (get-slices-as-matrix ap :columns [0 1])
             => (apache-commons [[1.0 0.5] [2.0 4.0]])
             (get-slices-as-matrix se :except-rows 0) => [[2.0 4.0]]
             (get-slices-as-matrix se :except-rows [0 1]) => [[]]
             (get-slices-as-matrix se :except-columns 0) => [[0.5] [4.0]]
             (get-slices-as-matrix se :except-columns [0 1]) => [[]]
             (get-slices-as-matrix se :rows 0 :except-rows 0) => [[]]
             (get-slices-as-matrix se :rows [0] :except-rows 0) => [[]]
             (get-slices-as-matrix se :rows 0 :except-rows [0]) => [[]]
             (get-slices-as-matrix se :except-rows '(1) :except-columns '(1))
             => [[1.0]]
             (get-slices-as-matrix se :rows [0] :except-rows [0]) => [[]]
             (get-slices-as-matrix se :rows [0] :columns [0]) => [[1.0]]
             (get-slices-as-matrix se :rows 0 :columns [0]) => [[1.0]]
             (get-slices-as-matrix se :rows [0] :columns 0) => [[1.0]]
             (get-slices-as-matrix se :rows 0 :columns 0) => [[1.0]])
       (fact "partition"
             (def s '((1.0 2.0 3.0 4.0) (5.0 6.0 7.0 8.0) (9.0 10.0 11.0 12.0)
                       (13.0 14.0 15.0 16.0)))
             (matrix-partition (matrix s) 2 2)
             => {:bottom-left  [[9.0 10.0] [13.0 14.0]],
                 :bottom-right [[11.0 12.0] [15.0 16.0]],
                 :top-left     [[1.0 2.0] [5.0 6.0]],
                 :top-right    [[3.0 4.0] [7.0 8.0]]}
             (matrix-partition (apache-commons s) 1 1)
             => {:bottom-left  (apache-commons [[5.0] [9.0] [13.0]]),
                 :bottom-right (apache-commons [[6.0 7.0 8.0] [10.0 11.0 12.0]
                                                [14.0 15.0 16.0]]),
                 :top-left     (apache-commons [[1.0]]),
                 :top-right    (apache-commons [[2.0 3.0 4.0]])}
             (matrix-partition (clatrix s) 0 3)
             => {:bottom-left  (clatrix [[1.0 2.0 3.0] [5.0 6.0 7.0]
                                         [9.0 10.0 11.0] [13.0 14.0 15.0]]),
                 :bottom-right (clatrix [[4.0] [8.0] [12.0] [16.0]]),
                 :top-left     (clatrix [[]]), :top-right (clatrix [[]])}
             (matrix-partition s 3 0)
             => {:bottom-left [[]], :bottom-right [[13.0 14.0 15.0 16.0]],
                 :top-left    [[]],
                 :top-right   [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                               [9.0 10.0 11.0 12.0]]}
             (matrix-partition s 0 4)
             => {:bottom-left  [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                                [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]],
                 :bottom-right [[]], :top-left [[]], :top-right [[]]}
             (matrix-partition s 4 0)
             => {:bottom-left [[]], :bottom-right [[]], :top-left [[]],
                 :top-right   [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                               [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]]}
             (matrix-partition s 0 0)
             => {:bottom-left  [[]],
                 :bottom-right [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                                [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]],
                 :top-left     [[]], :top-right [[]]}
             (matrix-partition s 4 4)
             => {:bottom-left [[]], :bottom-right [[]],
                 :top-left    [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0]
                               [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]],
                 :top-right   [[]]}
             (matrix-partition s 0 5) => (throws)
             (matrix-partition s 5 0) => (throws)
             (matrix-partition s -1 3) => (throws)
             (matrix-partition s 3 -1) => (throws)))

(facts "manipulation"
       (fact "transpose"
             (transpose ap) => (apache-commons [[1.0 2.0] [0.5 4.0]])
             (transpose cl) => (clatrix [[1.0 2.0] [0.5 4.0]])
             (transpose ve) => [[1.0 2.0] [0.5 4.0]]
             (transpose ap1D) => (apache-commons [1.0 0.5])
             (transpose cl1D) => (clatrix [1.0 0.5])
             (transpose ve1D) => [1.0 0.5]
             (transpose cl-row) => (clatrix [[1.0] [0.5]])
             (transpose ve-col) => [[1.0 0.5]])
       (fact "conj-rows"
             (conj-rows se ve) => [[1.0 0.5] [2.0 4.0] [1.0 0.5] [2.0 4.0]]
             (conj-rows ve se) => [[1.0 0.5] [2.0 4.0] [1.0 0.5] [2.0 4.0]]
             (conj-rows ap cl) => (apache-commons [[1.0 0.5] [2.0 4.0]
                                                   [1.0 0.5] [2.0 4.0]])
             (conj-rows cl ap) => [[1.0 0.5] [2.0 4.0] [1.0 0.5] [2.0 4.0]]
             (conj-rows cl ve) => [[1.0 0.5] [2.0 4.0] [1.0 0.5] [2.0 4.0]]
             (conj-rows ve cl) => [[1.0 0.5] [2.0 4.0] [1.0 0.5] [2.0 4.0]]
             (conj-rows (clatrix [1.0]) (apache-commons [2.0]))
             => (clatrix [[1.0] [2.0]])
             (conj-rows se1D se) => [[1.0 0.5] [1.0 0.5] [2.0 4.0]]
             (conj-rows ve se1D) => [[1.0 0.5] [2.0 4.0] [1.0 0.5]]
             (conj-rows ve1D ap) => [[1.0 0.5] [1.0 0.5] [2.0 4.0]]
             (conj-rows cl ve1D) => (clatrix [[1.0 0.5] [2.0 4.0] [1.0 0.5]])
             (conj-rows ap1D se1D) => (apache-commons [[1.0 0.5] [1.0 0.5]])
             (conj-rows ve1D ap1D) => [[1.0 0.5] [1.0 0.5]]
             (conj-rows cl1D ap1D) => (clatrix [[1.0 0.5] [1.0 0.5]])
             (conj-rows cl1D cl1D) => (clatrix [[1.0 0.5] [1.0 0.5]])
             (conj-rows cl1D cl1D ap1D) => (clatrix [[1.0 0.5] [1.0 0.5]
                                                     [1.0 0.5]])
             (conj-rows se [-5.0]) => (throws)
             (conj-rows [-5.0] se) => (throws))
       (fact "conj-columns"
             (conj-columns se ve) => [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0]]
             (conj-columns ve se) => [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0]]
             (conj-columns ap cl) => (apache-commons [[1.0 0.5 1.0 0.5]
                                                      [2.0 4.0 2.0 4.0]])
             (conj-columns cl ap) => [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0]]
             (conj-columns cl ve) => [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0]]
             (conj-columns ve cl) => [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0]]
             (conj-columns (clatrix [1.0]) (apache-commons [2.0]))
             => (clatrix [[1.0] [2.0]])
             (conj-columns se1D se) => [[1.0 1.0 0.5] [0.5 2.0 4.0]]
             (conj-columns ap1D se1D) => (apache-commons [[1.0 1.0] [0.5 0.5]])
             (conj-columns ve1D ap1D) => [[1.0 1.0] [0.5 0.5]]
             (conj-columns cl1D ap1D) => (clatrix [[1.0 1.0] [0.5 0.5]])
             (conj-columns cl1D cl1D) => (clatrix [[1.0 1.0] [0.5 0.5]])
             (conj-columns cl1D cl1D ap1D) => (clatrix [[1.0 1.0 1.0]
                                                        [0.5 0.5 0.5]])
             (conj-columns se [-5.0]) => (throws)
             (conj-columns [-5.0] se) => (throws))
       (fact "merge matrices"
             (merge-matrices se ve ap cl)
             => [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0] [1.0 0.5 1.0 0.5]
                 [2.0 4.0 2.0 4.0]]
             (merge-matrices cl se ve ap)
             => [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0] [1.0 0.5 1.0 0.5]
                 [2.0 4.0 2.0 4.0]]
             (merge-matrices ap cl se ve)
             => (apache-commons [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0]
                                 [1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0]])
             (merge-matrices ap (row-matrix cl1D) se (row-matrix ve1D))
             => (apache-commons [[1.0 0.5 1.0 0.5] [2.0 4.0 2.0 4.0]
                                 [1.0 0.5 1.0 0.5]])
             (merge-matrices (row-matrix ap1D) cl (row-matrix se1D) ve)
             => (apache-commons [[1.0 0.5 1.0 0.5] [1.0 0.5 1.0 0.5]
                                 [2.0 4.0 2.0 4.0]])
             (merge-matrices ap1D cl1D se ve) => (throws))
       (fact "conj-symmetrically"
             (conj-symmetrically se [[6.0 7.0 8.0 9.0] [10.0 11.0 12.0 13.0]])
             => [[1.0 0.5 6.0 10.0] [2.0 4.0 7.0 11.0] [6.0 7.0 8.0 9.0]
                 [10.0 11.0 12.0 13.0]]
             (conj-symmetrically se [[6.0 7.0 8.0]])
             => [[1.0 0.5 6.0] [2.0 4.0 7.0] [6.0 7.0 8.0]]
             (conj-symmetrically se [6.0 7.0 8.0])
             => [[1.0 0.5 6.0] [2.0 4.0 7.0] [6.0 7.0 8.0]]
             (conj-symmetrically ve '(6.0 7.0 8.0))
             => [[1.0 0.5 6.0] [2.0 4.0 7.0] [6.0 7.0 8.0]]
             (conj-symmetrically ap (clatrix '(6.0 7.0 8.0)))
             => (apache-commons [[1.0 0.5 6.0] [2.0 4.0 7.0] [6.0 7.0 8.0]]))
       (fact "conj-diagonally"
             (conj-diagonally se 3.0)
             => [[1.0 0.5 0.0] [2.0 4.0 0.0] [0.0 0.0 3.0]]
             (conj-diagonally ap 3.0)
             => (apache-commons [[1.0 0.5 0.0] [2.0 4.0 0.0] [0.0 0.0 3.0]])
             (conj-diagonally cl ap1D)
             => (clatrix [[1.0 0.5 0.0 0.0] [2.0 4.0 0.0 0.0]
                          [0.0 0.0 1.0 0.0] [0.0 0.0 0.0 0.5]])
             (conj-diagonally cl ap)
             => (clatrix [[1.0 0.5 0.0 0.0] [2.0 4.0 0.0 0.0]
                          [0.0 0.0 1.0 0.5] [0.0 0.0 2.0 4.0]]))
       (fact "replace submatrix"
             (replace-submatrix se se1D 1 0) => (throws)
             (replace-submatrix
               (apache-commons [[0.0 1.0 2.0] [3.0 4.0 5.0] [6.0 7.0 8.0]])
               cl 1 0) => (apache-commons [[0.0 1.0 2.0] [1.0 0.5 5.0]
                                           [2.0 4.0 8.0]])
             (replace-submatrix [[0.0 1.0 2.0] [3.0 4.0 5.0] [6.0 7.0 8.0]]
                                ap 0 1) => [[0.0 1.0 0.5] [3.0 2.0 4.0]
                                            [6.0 7.0 8.0]]
             (replace-submatrix [[0.0 1.0 2.0] [3.0 4.0 5.0] [6.0 7.0 8.0]]
                                se 2 0) => [[0.0 1.0 2.0] [3.0 4.0 5.0]
                                            [1.0 0.5 8.0] [2.0 4.0 0.0]]
             (replace-submatrix [[0.0 1.0 2.0] [3.0 4.0 5.0] [6.0 7.0 8.0]]
                                ve -1 3)
             => [[0.0 0.0 0.0 1.0 0.5] [0.0 1.0 2.0 2.0 4.0]
                 [3.0 4.0 5.0 0.0 0.0] [6.0 7.0 8.0 0.0 0.0]])
       (fact "permutation matrix"
             (permutation-matrix ap :rows [1 0])
             => (apache-commons [[2.0 4.0] [1.0 0.5]])
             (permutation-matrix cl :columns [1 0]) => [[0.5 1.0] [4.0 2.0]]
             (permutation-matrix se :rows [1 0] :columns [1 0]) => [[4.0 2.0]
                                                                    [0.5 1.0]]
             (permutation-matrix (identity-matrix 2) :rows [1 0]
                                 :columns [1 0]) => (identity-matrix 2)
             (permutation-matrix se1D :rows [1 0] :columns [1 0]) => (throws)))

(facts "matrix math"
       (fact "matrix multiply"
             (mx* '((1 1 1) (1 1 1)) '(1 1 1)) => [3 3]
             (mx* '((1 1 1) (1 1 1)) [1 1 1]) => [3 3])
       (fact "inner product"
             (vector/dot-product ve1D) => (inner-product ve1D)
             (inner-product ap1D) => (apache-commons [1 0.5])
             (inner-product cl1D) => (clatrix [1.0 0.5])
             (inner-product ap1D ve1D cl1D) => (apache-commons [1.25 0.625])
             (inner-product ap1D ve1D cl1D se1D) => 1.5625
             (inner-product ap1D cl) => (apache-commons [2.0 2.5])
             (inner-product ap cl) => (apache-commons [[2.0 2.5] [10.0 17.0]])
             (inner-product ve cl) => [[2.0 2.5] [10.0 17.0]]
             (inner-product ap) => (apache-commons '((1.0 0.5) (2.0 4.0)))
             (inner-product cl) => (clatrix '((1.0 0.5) (2.0 4.0))))
       (fact "kronecker product"
             (kronecker-product ap cl)
             => (apache-commons [[1.0 0.5 0.5 0.25] [2.0 4.0 1.0 2.0]
                                 [2.0 1.0 4.0 2.0] [4.0 8.0 8.0 16.0]])
             (kronecker-product ve cl ap)
             => [[1.0, 0.5, 0.5, 0.25, 0.5, 0.25, 0.25, 0.125]
                 [2.0, 4.0, 1.0, 2.0, 1.0, 2.0, 0.5, 1.0]
                 [2.0, 1.0, 4.0, 2.0, 1.0, 0.5, 2.0, 1.0]
                 [4.0, 8.0, 8.0, 16.0, 2.0, 4.0, 4.0, 8.0]
                 [2.0, 1.0, 1.0, 0.5, 4.0, 2.0, 2.0, 1.0]
                 [4.0, 8.0, 2.0, 4.0, 8.0, 16.0, 4.0, 8.0]
                 [4.0, 2.0, 8.0, 4.0, 8.0, 4.0, 16.0, 8.0]
                 [8.0, 16.0, 16.0, 32.0, 16.0, 32.0, 32.0, 64.0]]
             (kronecker-product ve1D cl1D) => (throws)
             (kronecker-product cl-row ap-row)
             => (clatrix [[1.0 0.5 0.5 0.25]])
             (kronecker-product cl-row ap-row ve-row)
             => (clatrix [[1.0 0.5 0.5 0.25 0.5 0.25 0.25 0.125]]))
       (fact "matrix pow"
             (matrix-pow ap 2)
             => [[0.7704918032786889 2.5245901639344264]
                 [2.5245901639344264 18.229508196721312]]
             (matrix-pow ap1D 2) => (throws)
             (matrix-pow ap-col 2) => (throws)
             (matrix-pow cl 4.3)
             => [[12.052805176613186 78.49641538319892]
                 [78.49641538319894 519.1897130056464]])
       (fact "determinant"
             (determinant ap) => 3.0
             (determinant cl) => 3.0
             (determinant ve) => 3.0
             (determinant se) => 3.0))

(facts "reduce"
       (fact "ereduce-kv"
             (ereduce-kv #(+ % %2 %3 %4 %5 %6) 3.4 ap cl ve true) => 29.9
             (ereduce-kv #(+ % %2 %3 %4) 3.4 cl-col true) => 5.9
             (ereduce-kv #(+ % %2 %3 %4) 3.4 cl-col false) => 5.9
             (ereduce-kv #(+ % %2 %3 %4) 3.4 cl-row true) => 5.9
             (ereduce-kv #(+ % %2 %3 %4) 3.4 cl-row false) => 5.9
             (ereduce-kv #(+ % %2 %3 %4 %5) 3.4 cl-row ap-row false) => 7.4
             (ereduce-kv #(+ % %2 %3 %4 %5) 3.4 se ve false) => 22.4)
       (fact "eevery?"
             (eevery? #(> (+ % %2) %3) se) => false)
       (fact "esome"
             (esome #(if (> (+ % %2) %3) %3) se) => 0.5
             (esome #(if (> (+ % %2) %3) %3) se {::mx/by-row false}) => 0.5))

(facts "filter"
       (fact "element filter"
             (efilter ap #(< % 2.1)) => '(1.0 0.5 2.0)
             (efilter ap #(< % 2.1) :byrow? false) => '(1.0 2.0 0.5)
             (efilter ap neg?) => '()
             (efilter ap1D neg?) => (throws)
             (efilter cl-col #(< % 0.7)) => '(0.5))
       (fact "element filter kv"
             (efilter-kv ap #(< %3 (+ % %2))) => [0.5]
             (efilter-kv ap #(< %3 (+ % %2)) :byrow? false) => [0.5]
             (efilter-kv ap1D #(< %3 (+ % %2))) => (throws)
             (efilter-kv cl-col #(< %3 (+ % %2))) => [0.5])
       (fact "sparse element filter"
             (sparse-efilter ap #(< % 2.1))
             => [[0 0 1.0] [0 1 0.5] [1 0 2.0]]
             (sparse-efilter ap #(< % 2.1) :byrow? false)
             => [[0 0 1.0] [1 0 2.0] [0 1 0.5]]
             (sparse-efilter ap neg?) => '()
             (sparse-efilter ap1D neg?) => (throws)
             (sparse-efilter cl-col #(< % 0.7)) => [[1 0 0.5]])
       (fact "sparse symmetric element filter"
             (sparse-symmetric-efilter ap #(< % 2.1))
             => [[0 0 1.0] [0 1 0.5]]
             (sparse-symmetric-efilter ap #(< % 2.1) :byrow? false)
             => [[0 0 1.0] [1 0 2.0]])
       (fact "filter by row"
             (filter-by-row cl #(< (esum-squares %) 2.0))
             => (clatrix [[1.0 0.5]])
             (filter-by-row cl1D #(< (esum-squares %) 2.0)) => (throws)
             (filter-by-row cl-row #(< (esum-squares %) 2.0))
             => (clatrix [[1.0 0.5]])
             (filter-by-row cl-col #(< (esum-squares %) 2.0))
             => (clatrix [[1.0] [0.5]])
             (filter-by-row ap #(< (esum-squares %) 2.0))
             => (apache-commons [[1.0 0.5]]))
       (fact "sparse filter by row"
             (sparse-filter-by-row cl #(< (esum-squares %) 2.0))
             => [[0 [1.0 0.5]]]
             (sparse-filter-by-row cl1D #(< (esum-squares %) 2.0)) => (throws)
             (sparse-filter-by-row cl-row #(< (esum-squares %) 2.0))
             => [[0 [1.0 0.5]]]
             (sparse-filter-by-row cl-col #(< (esum-squares %) 2.0))
             => [[0 [1.0]] [1 [0.5]]]
             (sparse-filter-by-row ap #(< (esum-squares %) 2.0))
             => [[0 [1.0 0.5]]])
       (fact "filter by column"
             (filter-by-column cl #(< (esum-squares %) 6.0))
             => (clatrix [[1.0] [2.0]])
             (filter-by-column cl1D #(< (esum-squares %) 6.0)) => (throws)
             (filter-by-column cl-row #(< (esum-squares %) 6.0))
             => (clatrix [[1.0] [0.5]])
             (filter-by-column cl-col #(< (esum-squares %) 6.0))
             => (clatrix [[1.0] [0.5]])
             (filter-by-column ap #(< (esum-squares %) 6.0))
             => (apache-commons [[1.0] [2.0]]))
       (fact "sparse filter by column"
             (sparse-filter-by-column cl #(< (esum-squares %) 6.0))
             => [[0 [1.0 2.0]]]
             (sparse-filter-by-column cl1D #(< (esum-squares %) 6.0))
             => (throws)
             (sparse-filter-by-column cl-row #(< (esum-squares %) 6.0))
             => [[0 [1.0]] [1 [0.5]]]
             (sparse-filter-by-column cl-col #(< (esum-squares %) 6.0))
             => [[0 [1.0 0.5]]]
             (sparse-filter-by-column ap #(< (esum-squares %) 6.0))
             => [[0 [1.0 2.0]]])
       (fact "filter symmetrically"
             (filter-symmetrically cl #(< (esum-squares %) 2.0))
             => (clatrix [[1.0]])
             (filter-symmetrically cl1D #(< (esum-squares %) 2.0)
                                   :byrow? false) => (throws)
             (filter-symmetrically cl-row #(< (esum-squares %) 2.0))
             => (clatrix [[1.0]])
             (filter-symmetrically cl-col #(< (esum-squares %) 2.0)
                                   :byrow? false) => (clatrix [[1.0]])
             (filter-symmetrically ap #(< (esum-squares %) 2.0))
             => (apache-commons [[1.0]])))

(facts "immutable changes"
       (fact "set column"
             (assoc-column ap 0 [8.0 9.0])
             => (apache-commons [[8.0 0.5] [9.0 4.0]])
             (assoc-column cl 0 [8.0 9.0]) => (clatrix [[8.0 0.5] [9.0 4.0]])
             (assoc-column cl1D 0 [8.0 9.0]) => (throws))
       (fact "insert row"
             (insert-row ap 1 [8.0 9.0])
             => (apache-commons [[1.0 0.5] [8.0 9.0] [2.0 4.0]])
             (insert-row cl 0 [8.0 9.0])
             => (clatrix [[8.0 9.0] [1.0 0.5] [2.0 4.0]])
             (insert-row cl1D 0 [8.0 9.0]) => (throws))
       (fact "insert column"
             (insert-column ap 0 [8.0 9.0])
             => (apache-commons [[8.0 1.0 0.5] [9.0 2.0 4.0]])
             (insert-column cl 1 [8.0 9.0])
             => (clatrix [[1.0 8.0 0.5] [2.0 9.0 4.0]])
             (insert-column cl1D 1 [8.0 9.0]) => (throws)))

(facts "numerical stability"
       (fact "round roughly zero rows"
             (round-roughly-zero-rows ap 1e-6)
             => (apache-commons [[1.0 0.5] [2.0 4.0]])
             (round-roughly-zero-rows ap1D 1e-6) => (throws)
             (round-roughly-zero-rows (apache-commons [[1e-13 1e-8]
                                                       [1.0 1e-17]]) 1e-6)
             => (apache-commons [[0.0 0.0] [1.0 1.0E-17]])
             (round-roughly-zero-rows (clatrix [[1e-13 1e-8] [1.0 1e-17]])
                                      1e-6) => (clatrix [[0.0 0.0]
                                                         [1.0 1.0E-17]]))
       (fact "round roughly zero columns"
             (round-roughly-zero-columns ap 1e-6)
             => (apache-commons [[1.0 0.5] [2.0 4.0]])
             (round-roughly-zero-columns ap1D 1e-6) => (throws)
             (round-roughly-zero-columns (apache-commons [[1e-13 1e-8]
                                                          [1.0 1e-17]]) 1e-6)
             => (apache-commons [[1.0E-13 0.0] [1.0 0.0]])
             (round-roughly-zero-columns (clatrix [[1e-13 1e-8] [1.0 1e-17]])
                                         1e-6) => (clatrix [[1.0E-13 0.0]
                                                            [1.0 0.0]]))
       (fact "round roughly zero row and columns"
             (round-roughly-zero-rows-and-columns
               (apache-commons [[1e-13 1e-8] [1.0 1e-17]]) 1e-6)
             => (apache-commons [[0.0 0.0] [1.0 0.0]])
             (round-roughly-zero-rows-and-columns
               (apache-commons [[9e-7 9e-7] [9e-7 9e-7]]) 1e-6)
             => (apache-commons [[0.0 0.0] [0.0 0.0]])))