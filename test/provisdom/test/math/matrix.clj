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

(def se '((1.0 0.5)(2.0 4.0)))
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
(def se-sym '((1.0 0.5)(0.5 2.0)))
(def ve-sym (matrix se-sym))
(def cl-sym (clatrix se-sym)) 
(def ap-sym (apache-commons se-sym))

(facts "special type helpers"
       (fact "diagonal?"
             (diagonal? se) => false
             (diagonal? [[1.0 0.0][0.0 2.0]]) => true)
       (fact "symmetric?"
             (symmetric? se) => false
             (symmetric? se-sym) => true)
       (fact "unit diagonal?"
             (unit-diagonal? se) => false 
             (unit-diagonal? se-sym) => false 
             (unit-diagonal? (identity-matrix 3)) => true)
       (fact "symmetric with unit diagonal?"
             (symmetric-with-unit-diagonal? se) => false 
             (symmetric-with-unit-diagonal? se-sym) => false 
             (symmetric-with-unit-diagonal? (identity-matrix 3)) => true)
       (fact "positive? -- can set accuracy"
             (positive? se) => false
             (positive? se-sym) => true
             (positive? '((1.0 -1.1)(-1.1 1.0))) => false
             (positive? '((1.0 -1.0)(-1.0 1.0))) => false
             ;;accuracy too strict
             (positive? '((1.0 -1.0)(-1.0 1.0)) 1e-32) => true)
       (fact "positive with unit diagonal? -- can set accuracy"
             (positive-with-unit-diagonal? se) => false
             (positive-with-unit-diagonal? se-sym) => false
             (positive-with-unit-diagonal? '((1.0 -1.1)(-1.1 1.0))) => false
             (positive-with-unit-diagonal? '((1.0 -1.0)(-1.0 1.0))) => false
             (positive-with-unit-diagonal? '((1.0 -1.0)(-1.0 1.0)) 
                                           1e-32) => true) ;accuracy too strict
       (fact "non-negative? -- can set accuracy"
             (non-negative? se) => false
             (non-negative? se-sym) => true
             (non-negative? '((1.0 -1.1)(-1.1 1.0))) => false
             (non-negative? '((1.0 -1.0)(-1.0 1.0))) => true
             (non-negative? '((1.0 -1.0001)(-1.0001 1.0))) => false
             ;;accuracy too lax
             (non-negative? '((1.0 -1.0001)(-1.0001 1.0)) 1e-3) => true) 
       (fact "row-or-column-matrix?"
             (row-or-column-matrix? #(* % %)) => false
             (row-or-column-matrix? se) => false
             (row-or-column-matrix? ve) => false
             (row-or-column-matrix? cl) => false
             (row-or-column-matrix? ap) => false
             (row-or-column-matrix? se-row) => true
             (row-or-column-matrix? ve-row) => true
             (row-or-column-matrix? cl-row) => true
             (row-or-column-matrix? ap-row) => true
             (row-or-column-matrix? se-col)  => true
             (row-or-column-matrix? ve-col) => true
             (row-or-column-matrix? cl-col) => true
             (row-or-column-matrix? ap-col) => true
             (row-or-column-matrix? se1D) => false
             (row-or-column-matrix? ve1D) => false
             (row-or-column-matrix? cl1D) => false
             (row-or-column-matrix? ap1D) => false)
       (fact "size-symmetric"
             (size-symmetric 1) => 1
             (size-symmetric 2) => (throws)
             (size-symmetric 3) => 2
             (size-symmetric 6) => 3)
       (fact "size-symmetric-with-unit-diagonal"
             (size-symmetric-with-unit-diagonal 1) => 2
             (size-symmetric-with-unit-diagonal 2) => (throws)
             (size-symmetric-with-unit-diagonal 3) => 3
             (size-symmetric-with-unit-diagonal 6) => 4)
       (fact "ecount-symmetric"
             (ecount-symmetric 1) => 1
             (ecount-symmetric 2) => 3
             (ecount-symmetric 3) => 6
             (ecount-symmetric 6) => 21)
       (fact "ecount-symmetric-with-unit-diagonal"
             (ecount-symmetric-with-unit-diagonal 1) => 0
             (ecount-symmetric-with-unit-diagonal 2) => 1
             (ecount-symmetric-with-unit-diagonal 3) => 3
             (ecount-symmetric-with-unit-diagonal 6) => 15)
       (fact "to-vector-from-symmetric" 
             (to-vector-from-symmetric #(+ 3 3) :byrow? false) => (throws)
             (to-vector-from-symmetric se :byrow? false) => [1.0 2.0 4.0]
             (to-vector-from-symmetric ve) => [1.0 0.5 4.0]
             (to-vector-from-symmetric cl :byrow? false) => [1.0 2.0 4.0]
             (to-vector-from-symmetric ap) => [1.0 0.5 4.0]
             (to-vector-from-symmetric se-sym :byrow? false) => [1.0 0.5 2.0]
             (to-vector-from-symmetric ve-sym) => [1.0 0.5 2.0]
             (to-vector-from-symmetric cl-sym :byrow? false) => [1.0 0.5 2.0]
             (to-vector-from-symmetric ap-sym) => [1.0 0.5 2.0]
             (to-vector-from-symmetric se1D) => throws
             (to-vector-from-symmetric se-row :byrow? false) => [1.0]
             (to-vector-from-symmetric se-row) => [1.0 0.5]
             (to-vector-from-symmetric se-col :byrow? false) => [1.0 0.5]
             (to-vector-from-symmetric se-col) => [1.0])
       (fact "to-vector-from-symmetric-with-unit-diagonal" 
             (to-vector-from-symmetric-with-unit-diagonal 
               #(+ 3 3) :byrow? false) => (throws)
             (to-vector-from-symmetric-with-unit-diagonal 
               se :byrow? false) => [2.0]
             (to-vector-from-symmetric-with-unit-diagonal ve) => [0.5]
             (to-vector-from-symmetric-with-unit-diagonal 
               cl :byrow? false) => [2.0]
             (to-vector-from-symmetric-with-unit-diagonal ap) => [0.5]
             (to-vector-from-symmetric-with-unit-diagonal 
               se-sym :byrow? false) => [0.5]
             (to-vector-from-symmetric-with-unit-diagonal ve-sym) => [0.5]
             (to-vector-from-symmetric-with-unit-diagonal 
               cl-sym :byrow? false) => [0.5]
             (to-vector-from-symmetric-with-unit-diagonal ap-sym) => [0.5]
             (to-vector-from-symmetric-with-unit-diagonal se1D) => throws
             (to-vector-from-symmetric-with-unit-diagonal 
               se-row :byrow? false) => []
             (to-vector-from-symmetric-with-unit-diagonal se-row) => [0.5]
             (to-vector-from-symmetric-with-unit-diagonal 
               se-col :byrow? false) => [0.5]
             (to-vector-from-symmetric-with-unit-diagonal se-col) => []))

(facts "matrix implementations"
       (fact "to nested vectors"
             (to-nested-vectors se) => ve
             (to-nested-vectors cl) => ve
             (to-nested-vectors ve) => ve
             (to-nested-vectors ap) => ve
             (to-nested-vectors se1D) => ve1D
             (to-nested-vectors cl1D) => ve1D
             (to-nested-vectors ve1D) => ve1D
             (to-nested-vectors ap1D) => ve1D
             (to-nested-vectors se-row) => ve-row
             (to-nested-vectors cl-row) => ve-row
             (to-nested-vectors ve-row) => ve-row
             (to-nested-vectors ap-row) => ve-row
             (to-nested-vectors se-col) => ve-col
             (to-nested-vectors cl-col) => ve-col
             (to-nested-vectors ve-col) => ve-col
             (to-nested-vectors ap-col) => ve-col))

(facts "vector constructors"
       (fact "maybe to vector"
             (maybe-to-vector nil) => nil
             (maybe-to-vector 1.0) => [1.0]
             (maybe-to-vector [1.0]) => [1.0]
             (maybe-to-vector '(1.0)) => [1.0])
       (fact "vector"
             (create-vector [2.0 3.0 4.0]) => [2.0 3.0 4.0]
             (create-vector nil 3 3.0) = [3.0 3.0 3.0]
             (create-vector :apache-commons [2.0 3.0 4.0]) 
             => (apache-commons [2.0 3.0 4.0])
             (create-vector :clatrix 3 #(+ 2.0 %)) => (clatrix [2.0 3.0 4.0]))
       (fact "sparse vector"
             (sparse-vector [[0 3.0]] [5.0 6.0]) => [3.0 6.0]
             (sparse-vector :clatrix [[0 3.0]] [5.0 6.0]) 
             => (clatrix [3.0 6.0])
             (sparse-vector [[0 3.0]] 2) => [3.0 0.0]
             (sparse-vector [[0 3.0] [1 2.0]] 2) => [3.0 2.0] 
             (sparse-vector [[2 3.0]] 2) => (throws)
             (sparse-vector :clatrix [[0 3.0] [1 2.0]] 2) 
             => (clatrix [3.0 2.0])
             (sparse-vector :apache-commons [[0 3.0] [1 2.0]] 2) 
             => (apache-commons [3.0 2.0])))

(facts "matrix constructors"
       (fact "matrix"
             (matrix :apache-commons #(+ % %2) [2 3]) => (apache-commons 
                                                           [[0 1 2][1 2 3]]))
       (fact "constant"
             (constant-matrix [2 2] 1.0) => [[1.0 1.0][1.0 1.0]]
             (constant-matrix :clatrix [2 2] 1.0) 
             => (clatrix [[1.0 1.0][1.0 1.0]])
             (constant-matrix :apache-commons [2 2] 1.0) 
             => (apache-commons '((1.0,1.0)(1.0,1.0))))
       (fact "sequence to matrix"
             (sequence-to-matrix se1D se1D 1 true) => [[1.0 0.5]]
             (sequence-to-matrix ve1D ve1D 1 true) => [[1.0 0.5]]
             (sequence-to-matrix ap1D ap1D 1 true) => (apache-commons 
                                                        [[1.0 0.5]])
             (sequence-to-matrix cl1D cl1D 1 true) => (clatrix [[1.0 0.5]])
             (sequence-to-matrix [1.0 2.0 3.0 4.0 5.0 6.0] 2 false) 
             => [[1.0 3.0 5.0] [2.0 4.0 6.0]]
             (sequence-to-matrix [1.0 2.0 3.0 4.0 5.0 6.0] 2 true) 
             => [[1.0 2.0 3.0][4.0 5.0 6.0]]
             (sequence-to-matrix [1.0 2.0 3.0 4.0 5.0 6.0 7.0] 2 true) 
             => (throws)
             (sequence-to-matrix :clatrix [1.0 2.0 3.0 4.0 5.0 6.0] 2 true) 
             => (clatrix [[1.0 2.0 3.0][4.0 5.0 6.0]])
             (sequence-to-matrix 
               :apache-commons [1.0 2.0 3.0 4.0 5.0 6.0] 2 true) 
             => (apache-commons '((1.0 2.0 3.0)(4.0 5.0 6.0))))
       (fact "square"
             (square-matrix [[1.0 2.0 3.0]]) => [[1.0]]
             (square-matrix 2 #(double (+ % %2))) => [[0.0 1.0] [1.0 2.0]]
             (square-matrix 2 3.0) => [[3.0 3.0][3.0 3.0]]
             (square-matrix :clatrix 2 #(+ % %2)) => (clatrix [[0.0 1.0] 
                                                               [1.0 2.0]])
             (square-matrix :apache-commons 2 #(+ % %2)) 
             => (apache-commons '((0.0 1.0)(1.0 2.0))))
       (fact "column"
             (column-matrix se1D se1D) => [[1.0] [0.5]]
             (column-matrix ve1D ve1D) => [[1.0] [0.5]]
             (column-matrix ap1D ap1D) => (apache-commons [[1.0] [0.5]])
             (column-matrix cl1D cl1D) => (clatrix [[1.0] [0.5]])
             (column-matrix nil 2 #(double %)) => [[0.0] [1.0]]
             (column-matrix nil 2 3.0) => [[3.0] [3.0]]
             (column-matrix :clatrix 2 #(+ % 0.0)) => (clatrix [[0.0] [1.0]])
             (column-matrix :apache-commons 2 #(+ % 0.0)) => (apache-commons 
                                                               '((0.0)(1.0))))
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
             (diagonal-matrix se1D se1D) => [[1.0 0.0][0.0 0.5]]
             (diagonal-matrix ve1D ve1D) => [[1.0 0.0][0.0 0.5]]
             (diagonal-matrix ap1D ap1D) => (apache-commons [[1.0 0.0]
                                                             [0.0 0.5]])
             (diagonal-matrix cl1D cl1D) => (clatrix [[1.0 0.0][0.0 0.5]])
             (diagonal-matrix nil 2 #(double %)) => [[0.0 0.0][0.0 1.0]]
             (diagonal-matrix nil [1.0 3.0]) => [[1.0 0.0] [0.0 3.0]]
             (diagonal-matrix nil 2 3.0) => [[3.0 0.0][0.0 3.0]]         
             (diagonal-matrix :clatrix 2 #(+ % 0.0)) => (clatrix [[0.0 0.0]
                                                                  [0.0 1.0]])
             (diagonal-matrix :apache-commons 2 #(+ % 0.0)) 
             => (apache-commons '((0.0 0.0)(0.0 1.0))))
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
             => [[0.0 1.0][1.0 3.0]]
             (symmetric-matrix :clatrix #(+ % (* 2 %2)) 2 true) 
             => (clatrix [[0.0 2.0][2.0 3.0]])
             (symmetric-matrix :apache-commons [1.0 2.0 3.0]) 
             => (apache-commons '((1.0 2.0)(2.0 3.0))))
       (fact "symmetric with unit diagonal"
             (symmetric-with-unit-diagonal-matrix nil [1.0 2.0]) => throws
             (symmetric-with-unit-diagonal-matrix [1.0 2.0 3.0]) 
             => [[1.0 1.0 2.0] [1.0 1.0 3.0] [2.0 3.0 1.0]]
             (symmetric-with-unit-diagonal-matrix 
               nil 3 #(double (+ % (* 2 %2))) true) 
             => [[1.0 2.0 4.0] [2.0 1.0 5.0] [4.0 5.0 1.0]] 
             (symmetric-with-unit-diagonal-matrix 
               nil 3 #(double (+ % (* 2 %2))) false) 
             => [[1.0 1.0 2.0] [1.0 1.0 4.0] [2.0 4.0 1.0]] 
             (symmetric-with-unit-diagonal-matrix 
               :clatrix 3 #(+ % (* 2 %2)) true) 
             => (clatrix [[1.0 2.0 4.0] [2.0 1.0 5.0] [4.0 5.0 1.0]])
             (symmetric-with-unit-diagonal-matrix 
               :apache-commons [1.0 2.0 3.0]) 
             => (apache-commons '((1.0 1.0 2.0)(1.0 1.0 3.0)(2.0 3.0 1.0))))
       (fact "symmetric matrix by averaging"
             (symmetric-by-averaging-matrix se1D) => (throws)
             (symmetric-by-averaging-matrix se) => [[1.0 1.25] [1.25 4.0]]
             (symmetric-by-averaging-matrix ve) => [[1.0 1.25] [1.25 4.0]]
             (symmetric-by-averaging-matrix cl) => [[1.0 1.25] [1.25 4.0]]
             (symmetric-by-averaging-matrix ap) => (apache-commons 
                                                     [[1.0 1.25][1.25 4.0]]))
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
       (fact "sparse"
             (sparse-matrix [[0 0 3.0]] [[4.0 5.0][6.0 7.0]]) 
             => [[3.0 5.0] [6.0 7.0]]
             (sparse-matrix :clatrix [[0 0 3.0]] [[4.0 5.0][6.0 7.0]]) 
             => (clatrix [[3.0 5.0] [6.0 7.0]])
             (sparse-matrix [[0 0 3.0]] [2 2]) => [[3.0 0.0] [0.0 0.0]]
             (sparse-matrix [[0 0 3.0]] [2 2]) => [[3.0 0.0] [0.0 0.0]] 
             (sparse-matrix [[0 0 3.0] [1 0 2.0] [0 1 5.0] [1 1 -1.0]] [2 2]) 
             => [[3.0 5.0] [2.0 -1.0]] 
             (sparse-matrix [[0 2 3.0]] [2 2]) => (throws)
             (sparse-matrix 
               :clatrix [[0 0 3.0] [1 0 2.0] [0 1 5.0] [1 1 -1.0]] [2 2]) 
             => (clatrix [[3.0 5.0] [2.0 -1.0]])
             (sparse-matrix 
               :apache-commons [[0 0 3.0] [1 0 2.0] [0 1 5.0] [1 1 -1.0]] 
               [2 2]) => (apache-commons [[3.0 5.0] [2.0 -1.0]])
             (sparse-symmetric-matrix 
               :clatrix [[0 0 3.0] [1 0 2.0] [0 1 5.0] [1 1 -1.0]] 2) 
             => (clatrix [[3.0 5.0] [5.0 -1.0]])
             (sparse-symmetric-matrix 
               :clatrix [[0 0 3.0] [1 0 2.0]] [[1.0 2.0][3.0 4.0]]) 
             => (clatrix [[3.0 2.0] [2.0 4.0]])))

(facts "get"
       (fact "rows"
             (rows se) => [[1.0 0.5] [2.0 4.0]]
             (rows ap-row) => [[1.0 0.5]]
             (rows cl-col) => [[1.0] [0.5]]
             (rows ve1D) => (throws))
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
             (diagonal ap1D) => (throws)
             (diagonal cl-row) => [1.0]
             (diagonal ve-col) => [1.0]
             (diagonal ve 1) => [1.0]
             (diagonal ve 2) => nil
             (diagonal ve -1) => [1.0]
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
             (def s '((1.0 2.0 3.0 4.0)(5.0 6.0 7.0 8.0)(9.0 10.0 11.0 12.0)
                                       (13.0 14.0 15.0 16.0)))
             (matrix-partition (matrix s) 2 2) 
             => {:bottom-left [[9.0 10.0] [13.0 14.0]], 
                 :bottom-right [[11.0 12.0] [15.0 16.0]], 
                 :top-left [[1.0 2.0] [5.0 6.0]], 
                 :top-right [[3.0 4.0] [7.0 8.0]]}
             (matrix-partition (apache-commons s) 1 1) 
             => {:bottom-left (apache-commons [[5.0] [9.0] [13.0]]), 
                 :bottom-right (apache-commons [[6.0 7.0 8.0] [10.0 11.0 12.0] 
                                                [14.0 15.0 16.0]]), 
                 :top-left (apache-commons [[1.0]]), 
                 :top-right (apache-commons [[2.0 3.0 4.0]])}
             (matrix-partition (clatrix s) 0 3) 
             => {:bottom-left (clatrix [[1.0 2.0 3.0] [5.0 6.0 7.0] 
                                        [9.0 10.0 11.0] [13.0 14.0 15.0]]), 
                 :bottom-right (clatrix [[4.0] [8.0] [12.0] [16.0]]), 
                 :top-left (clatrix [[]]), :top-right (clatrix [[]])}
             (matrix-partition s 3 0) 
             => {:bottom-left [[]], :bottom-right [[13.0 14.0 15.0 16.0]], 
                 :top-left [[]], 
                 :top-right [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0] 
                             [9.0 10.0 11.0 12.0]]}
             (matrix-partition s 0 4) 
             => {:bottom-left [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0] 
                               [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]], 
                 :bottom-right [[]], :top-left [[]], :top-right [[]]} 
             (matrix-partition s 4 0) 
             => {:bottom-left [[]], :bottom-right [[]], :top-left [[]], 
                 :top-right [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0] 
                             [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]]}
             (matrix-partition s 0 0) 
             => {:bottom-left [[]], 
                 :bottom-right [[1.0 2.0 3.0 4.0][5.0 6.0 7.0 8.0]
                                [9.0 10.0 11.0 12.0][13.0 14.0 15.0 16.0]], 
                 :top-left [[]], :top-right [[]]}
             (matrix-partition s 4 4) 
             => {:bottom-left [[]], :bottom-right [[]], 
                 :top-left [[1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0] 
                            [9.0 10.0 11.0 12.0] [13.0 14.0 15.0 16.0]], 
                 :top-right [[]]}
             (matrix-partition s 0 5) => (throws)
             (matrix-partition s 5 0) => (throws)
             (matrix-partition s -1 3) => (throws)
             (matrix-partition s 3 -1) => (throws)))

(facts "manipulation"
       (fact "emap"
             (emap #(% 2.0 3.0) +) => 5.0
             (emap #(% 2.0 3.0) [+ -]) => [5.0 -1.0]
             (emap m/sq 1.0) => 1.0
             (emap m/sq ap) => (apache-commons [[1.0 0.25] [4.0 16.0]])
             (emap m/sq cl) => (clatrix [[1.0 0.25] [4.0 16.0]])
             (emap m/sq cl1D) => (clatrix [1.0 0.25])
             (emap m/sq cl-row) => (clatrix [[1.0 0.25]])
             (emap m/sq cl-col) => (clatrix [[1.0] [0.25]])
             (emap #(+ % %2 %3) cl ap ve) => (clatrix [[3.0 1.5] [6.0 12.0]])
             (emap #(+ % %2 %3 %4) cl ap ve se) => (clatrix [[4.0 2.0] 
                                                             [8.0 16.0]])
             (emap #(+ % %2) ap cl) => (apache-commons [[2.0 1.0] [4.0 8.0]])
             (emap #(+ % %2) ap cl-row) => (throws))
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
             (conj-symmetrically se [[6.0 7.0 8.0 9.0][10.0 11.0 12.0 13.0]]) 
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
               (apache-commons [[0.0 1.0 2.0][3.0 4.0 5.0][6.0 7.0 8.0]]) 
               cl 1 0) => (apache-commons [[0.0 1.0 2.0] [1.0 0.5 5.0] 
                                           [2.0 4.0 8.0]])
             (replace-submatrix [[0.0 1.0 2.0][3.0 4.0 5.0][6.0 7.0 8.0]] 
                                ap 0 1) => [[0.0 1.0 0.5] [3.0 2.0 4.0] 
                                            [6.0 7.0 8.0]]
             (replace-submatrix [[0.0 1.0 2.0][3.0 4.0 5.0][6.0 7.0 8.0]] 
                                se 2 0) => [[0.0 1.0 2.0] [3.0 4.0 5.0] 
                                            [1.0 0.5 8.0] [2.0 4.0 0.0]]
             (replace-submatrix [[0.0 1.0 2.0][3.0 4.0 5.0][6.0 7.0 8.0]] 
                                ve -1 3) 
             => [[0.0 0.0 0.0 1.0 0.5] [0.0 1.0 2.0 2.0 4.0] 
                 [3.0 4.0 5.0 0.0 0.0] [6.0 7.0 8.0 0.0 0.0]])
       (fact "permutation matrix"
             (permutation-matrix ap :rows [1 0]) 
             => (apache-commons [[2.0 4.0][1.0 0.5]])
             (permutation-matrix cl :columns [1 0]) => [[0.5 1.0] [4.0 2.0]]
             (permutation-matrix se :rows [1 0] :columns [1 0]) => [[4.0 2.0] 
                                                                    [0.5 1.0]]
             (permutation-matrix (identity-matrix 2) :rows [1 0] 
                                 :columns [1 0]) => (identity-matrix 2)
             (permutation-matrix se1D :rows [1 0] :columns [1 0]) => (throws)))

(facts "matrix math"
       (fact "matrix multiply"
             (mmul '((1 1 1) (1 1 1)) '(1 1 1)) => [3 3]
             (mmul '((1 1 1) (1 1 1)) [1 1 1]) => [3 3])
       (fact "element sum"
             (esum 2.0) => 2.0
             (esum m/sq 2.0) => 4.0
             (esum ve-row) => 1.5
             (esum ap1D) => 1.5
             (esum cl) => 7.5
             (esum m/sq cl) => 21.25
             (esum m/sq ap1D) => 1.25)
       (fact "element average"
             (eaverage ve-row) => 0.75)
       (fact "element squared sum"
             (esum-squares 3.0) => 9.0
             (esum-squares ve-row) => 1.25
             (esum-squares ap1D) => 1.25
             (esum-squares cl) => 21.25)
       (fact "element product"
             (eproduct ve-row) => 0.5
             (eproduct ap1D) => 0.5
             (eproduct cl) => 4.0
             (eproduct m/sq ap1D) => 0.25)
       (fact "norm (this is the standard norm2)"
             (norm ap) => 4.6097722286464435
             (norm cl) => 4.6097722286464435
             (norm cl-row) => 1.118033988749895
             (norm ap1D) => 1.118033988749895
             (norm cl1D) => 1.118033988749895)
       (fact "norm1"
             (norm1 ap) => 7.5
             (norm1 cl) => 7.5
             (norm1 cl-row) => 1.5
             (norm1 ap1D) => 1.5
             (norm1 cl1D) => 1.5)
       (fact "normp"
             (normp ap 1.0) => 7.5
             (normp cl 3.4) => 4.118720689718815
             (normp cl-row 2.1) => 1.1049918154523823
             (normp ap1D 2.1) => 1.1049918154523823
             (normp cl1D 1.0) => 1.5
             (normp cl1D 0.5) => (throws))
       (fact "normalise"
             (normalise ap) 
             => (apache-commons [[0.21693045781865616 0.10846522890932808] 
                                 [0.4338609156373123 0.8677218312746247]])
             (normalise cl) 
             => (clatrix [[0.21693045781865616 0.10846522890932808] 
                          [0.4338609156373123 0.8677218312746247]])
             (normalise ap1D) 
             => (apache-commons [0.8944271909999159 0.4472135954999579])
             (normalise cl1D) 
             => (clatrix [0.8944271909999159 0.4472135954999579]))
       (fact "normalise1"
             (normalise1 ap) 
             => (apache-commons [[0.13333333333333333 0.06666666666666667] 
                                 [0.26666666666666666 0.5333333333333333]])
             (esum (normalise1 
                     [2.1242141025912059120591205912509510259021590125 
                      1.2398578935713571650983759872398572983 
                      2.1351365731650631856238056287035, 
                      3.235729375209357203975])) => 1.0
             (normalise1 cl) => 
             (clatrix [[0.13333333333333333 0.06666666666666667] 
                       [0.26666666666666666 0.5333333333333333]])
             (normalise1 ap1D) 
             => (apache-commons [0.6666666666666666 0.3333333333333333])
             (normalise1 cl1D) 
             => (clatrix [0.6666666666666666 0.3333333333333333]))
       (fact "normalisep"
             (normalisep ap 1.0) 
             => (apache-commons [[0.13333333333333333 0.06666666666666667] 
                                 [0.26666666666666666 0.5333333333333333]])
             (normalisep cl 3.4) 
             => (clatrix [[0.24279383705144375 0.12139691852572188] 
                          [0.4855876741028875 0.971175348205775]])
             (normalisep cl-row 2.1) 
             => (clatrix [[0.9049840786292169 0.45249203931460846]])
             (normalisep ap1D 2.1) 
             => (apache-commons [0.9049840786292169 0.45249203931460846])
             (normalisep cl1D 1.0) 
             => (clatrix [0.6666666666666666 0.3333333333333333])
             (normalisep cl1D 0.5) => (throws))
       (fact "inner product / dot product"
             (dot-product ap1D) => (inner-product ap1D)
             (inner-product ap1D) => (apache-commons [1 0.5])
             (inner-product cl1D) => (clatrix [1.0 0.5])
             (inner-product cl1D ap1D) => 1.25
             (inner-product ap1D cl1D) => 1.25
             (inner-product ap1D cl) => (apache-commons [2.0 2.5])
             (inner-product ap cl) => (apache-commons [[2.0 2.5] [10.0 17.0]])
             (inner-product ve cl) => [[2.0 2.5] [10.0 17.0]]
             (inner-product ap1D ve1D cl1D) => (apache-commons [1.25 0.625])
             (inner-product ap1D ve1D cl1D se1D) => 1.5625
             (inner-product ap) => (apache-commons '((1.0 0.5) (2.0 4.0)))
             (inner-product cl) => (clatrix '((1.0 0.5) (2.0 4.0))))
       (fact "kronecker product"
             (kronecker-product ap cl) 
             => (apache-commons [[1.0 0.5 0.5 0.25] [2.0 4.0 1.0 2.0] 
                                 [2.0 1.0 4.0 2.0] [4.0 8.0 8.0 16.0]])
             (kronecker-product ve cl ap) 
             => [[1.0,0.5,0.5,0.25,0.5,0.25,0.25,0.125] 
                 [2.0,4.0,1.0,2.0,1.0,2.0,0.5,1.0] 
                 [2.0,1.0,4.0,2.0,1.0,0.5,2.0,1.0] 
                 [4.0,8.0,8.0,16.0,2.0,4.0,4.0,8.0]
                 [2.0,1.0,1.0,0.5,4.0,2.0,2.0,1.0] 
                 [4.0,8.0,2.0,4.0,8.0,16.0,4.0,8.0] 
                 [4.0,2.0,8.0,4.0,8.0,4.0,16.0,8.0] 
                 [8.0,16.0,16.0,32.0,16.0,32.0,32.0,64.0]]
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
             (det ap) => 3.0
             (det cl) => 3.0
             (det ve) => 3.0
             (det se) => 3.0))

(facts "vector math"
       (fact "outer product"
             (outer-product 3) => 9
             (outer-product [1.0 2.0]) => [[1.0 2.0][2.0 4.0]]
             (outer-product :clatrix [1.0 2.0]) 
             => (clatrix [[1.0 2.0][2.0 4.0]])
             (outer-product :apache-commons [2.0 4.0 5.0]) 
             => (apache-commons [[4.0 8.0 10.0][8.0 16.0 20.0]
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
       (fact "cross product"
             (cross-product ap1D cl1D) => 0.0
             (cross-product cl1D ap1D) => 0.0
             (cross-product [5.0 6.0] cl1D) => -3.5
             (cross-product cl-row ap-col) => (throws))
       (fact "projection" 
             (projection ap-row cl-col) => (throws)
             (projection ap1D cl1D) => (apache-commons [1.0 0.5])
             (projection ve1D [5.0 6.0]) 
             => [0.6557377049180328 0.7868852459016393]
             (projection cl1D ap1D) => (clatrix [1.0 0.5]))
       (fact "cumulative sum"
             (cumulative-sum ap-row) => (throws)
             (cumulative-sum [3 4 6 14]) => [3 7 13 27]
             (cumulative-sum ap1D) => (apache-commons [1 1.5])
             (cumulative-sum cl1D) => (clatrix [1.0 1.5]))
       (fact "differences"
             (differences ap-row 1.0) => (throws)
             (differences [3 4 6 14] 1) => [2 1 2 8]
             (differences ap1D 1.0) => (apache-commons [0.0 -0.5])
             (differences cl1D 1.0 2.0) => (clatrix [0.0 -0.5 1.5])))

(facts "reduce"
       (fact "ereduce-kv"
             (ereduce-kv #(+ % %2 %3 %4 %5 %6) 3.4 ap cl ve true) => 29.9
             (ereduce-kv #(+ % %2 %3 %4) 3.4 cl-col true) => 5.9
             (ereduce-kv #(+ % %2 %3 %4) 3.4 cl-col false) => 5.9
             (ereduce-kv #(+ % %2 %3 %4) 3.4 cl-row true) => 5.9
             (ereduce-kv #(+ % %2 %3 %4) 3.4 cl-row false) => 5.9
             (ereduce-kv #(+ % %2 %3 %4 %5) 3.4 cl-row ap-row false) => 7.4
             (ereduce-kv #(+ % %2 %3 %4 %5) 3.4 se ve false) => 22.4)
       (fact "every-kv?"
             (every-kv? #(> % %2) se1D) => false)
       (fact "eevery?"
             (eevery? #(> (+ % %2) %3) se) => false)
       (fact "some-kv"
             (some-kv #(if (> % %2) %2) se1D) => 0.5)
       (fact "esome"
             (esome #(if (> (+ % %2) %3) %3) se true) => 0.5
             (esome #(if (> (+ % %2) %3) %3) se false) => 0.5))

(facts "filter"
       (fact "filter-kv"
             (filter-kv #(< % %2) se1D) => [1.0])
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
             (set-column ap 0 [8.0 9.0]) 
             => (apache-commons [[8.0 0.5] [9.0 4.0]])
             (set-column cl 0 [8.0 9.0]) => (clatrix [[8.0 0.5] [9.0 4.0]])
             (set-column cl1D 0 [8.0 9.0]) => (throws))
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
             (insert-column cl1D 1 [8.0 9.0]) => (throws))
       (fact "insert symmetrically"
             (insert-symmetrically ap 1 [8.0 9.0 10.0])
             => (apache-commons [[1.0 8.0 0.5] [8.0 9.0 10.0] [2.0 10.0 4.0]])
             (insert-symmetrically cl 1 [8.0 9.0 10.0]) 
             => (clatrix [[1.0 8.0 0.5] [8.0 9.0 10.0] [2.0 10.0 4.0]])
             (insert-symmetrically cl1D 1 [8.0 9.0 10.0]) => (throws)))

(facts "numerical stability"
       (fact "roughly?"
             (roughly? 1 1.01 0.05) => true
             (roughly? 1 1.01 0.005) => false
             (roughly? [1 2] [1.01 2] 0.05) => true
             (roughly? [1 2] [1.01 2] 0.005) => false
             (roughly? [[1 2][3 4]] [[1.01 2][3 4]] 0.05) => true
             (roughly? [[1 2][3 4]] [[1.01 2][3 4]] 0.005) => false)
       (fact "roughly-distinct"
             (roughly-distinct [1 1.01 1.001] 0.005) => [1 1.01]
             (roughly-distinct [[1 1][1.01 1.01][1.001 1.001]] 
                               0.005) => [[1 1][1.01 1.01]]
             (roughly-distinct [[1 1.01][1.01 1][1.01 1.01][1.001 1.001]] 
                               0.05) => [[1 1.01]])
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

(facts "decomposition"
       (fact "inverse"
             (inverse 2.0) => 0.5
             (inverse [2.0]) => [0.5]
             (inverse ap) 
             => (apache-commons [[1.3333333333333333 -0.16666666666666663] 
                                 [-0.6666666666666666 0.3333333333333333]])
             (inverse cl) 
             => (clatrix [[1.3333333333333333 -0.16666666666666663] 
                          [-0.6666666666666666 0.3333333333333333]])
             (inverse cl-row) => (throws)
             (inverse cl-col) => (throws)
             (inverse cl1D) => [1.0 2.0]
             (inverse ve) => [[1.3333333333333333 -0.16666666666666663] 
                              [-0.6666666666666666 0.3333333333333333]]
             (inverse se) => [[1.3333333333333333 -0.16666666666666663] 
                              [-0.6666666666666666 0.3333333333333333]])
       (fact "cholesky"
             (cholesky-decomposition ap) => (throws)
             (first (cholesky-decomposition 
                      (apache-commons [[1.0 0.5][0.5 3.0]]))) 
             => (apache-commons [[1.0 0.0] [0.5 1.6583123951777]])
             (second (cholesky-decomposition (clatrix [[1.0 0.5][0.5 3.0]]))) 
             => (clatrix [[1.0 0.5] [0.0 1.6583123951777]])
             (first (cholesky-decomposition [[1.0 0.5][0.5 3.0]])) 
             => [[1.0 0.0] [0.5 1.6583123951777]]
             (cholesky-decomposition cl-row) => (throws))
       (fact "lower cholesky"
             (lower-cholesky (apache-commons [[1.0 0.5][0.5 3.0]])) 
             => (apache-commons [[1.0 0.0] [0.5 1.6583123951777]])
             (lower-cholesky [[1.0 0.5][0.5 3.0]]) => [[1.0 0.0] 
                                                       [0.5 1.6583123951777]])
       (fact "upper cholesky"
             (upper-cholesky (apache-commons [[1.0 0.5][0.5 3.0]])) 
             => (apache-commons [[1.0 0.5] [0.0 1.6583123951777]])
             (upper-cholesky [[1.0 0.5][0.5 3.0]]) => [[1.0 0.5] 
                                                       [0.0 1.6583123951777]])
       (fact "cholesky with positive-semi-definite"
             (second (cholesky-decomposition-semi-definite cl 1e-4)) 
             => (clatrix [[0.25 2.0] [0.9682458365518543 0.0]])
             (first (cholesky-decomposition 
                      (apache-commons [[1.0 0.5][0.5 3.0]]))) 
             => (apache-commons [[1.0 0.0] [0.5 1.6583123951777]])
             (first (cholesky-decomposition-semi-definite 
                      (clatrix [[1.0 0.5 1e-9][0.5 3.0 1e-9][1e-9 1e-9 1e-6]]) 
                                                          1e-4)) 
             => (clatrix [[1.0 0.0 0.0][0.5 1.6583123951777 0.0]
                          [1.0E-9 3.0151134457776366E-10 
                           9.999999999994545E-4]])
             (second (cholesky-decomposition-semi-definite 
                       (clatrix [[1.0 0.5 1e-9][0.5 3.0 1e-9]
                                 [1e-9 1e-9 1e-6]]) 1e-4)) 
             => (clatrix [[1.0 0.5 1.0E-9] 
                          [0.0 1.6583123951777 3.0151134457776366E-10] 
                          [0.0 0.0 9.999999999994545E-4]])
             (first (cholesky-decomposition-semi-definite 
                      [[1.0 0.5 1e-9][0.5 3.0 1e-9][1e-9 1e-9 1e-6]] 1e-4)) 
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
             (:B (cholesky-rectangular (apache-commons [[1.0 0.5][0.5 3.0]]) 
                                       1e-4)) 
             => (apache-commons [[0.2886751345948129 0.9574271077563381] 
                                 [1.7320508075688772 0.0]])
             (:B (cholesky-rectangular 
                      (apache-commons [[1.0 0.5 1e-9][0.5 3.0 1e-9]
                                       [1e-9 1e-9 1e-6]]) 1e-4)) 
             => (apache-commons [[0.2886751345948129 0.9574271077563381] 
                                 [1.7320508075688772 0.0] 
                                 [5.773502691896259E-10 
                                  8.703882797784892E-10]])
             (:B (cholesky-rectangular (clatrix [[1.0 0.5 1e-9][0.5 3.0 1e-9]
                                                 [1e-9 1e-9 1e-6]]) 1e-4)) 
             => (clatrix [[0.2886751345948129 0.9574271077563381] 
                          [1.7320508075688772 0.0] 
                          [5.773502691896259E-10 8.703882797784892E-10]])
             (cholesky-rectangular 
                      [[1.0 0.5 1e-9][0.5 3.0 1e-9][1e-9 1e-9 1e-6]] 1e-4) 
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
             (sv-decomposition-with-rank cl) 
             => {:S (clatrix [[4.562639046204301 0.0]
                              [0.0 0.6575142082509741]]), 
                 :U (clatrix [[-0.20027709794089957 -0.9797392939146469]
                              [-0.9797392939146469 0.2002770979408998]]), 
                 :UT (clatrix [[-0.20027709794089957 -0.9797392939146469]
                               [-0.9797392939146469 0.2002770979408998]]), 
                 :V (clatrix [[-0.4733566832482428 -0.8808708477547789]
                              [-0.8808708477547789 0.4733566832482428]]), 
                 :VT (clatrix [[-0.4733566832482428 -0.8808708477547789]
                               [-0.8808708477547789 0.4733566832482428]]), 
                 :rank 2}
             (sv-decomposition-with-rank ve-row) 
             => {:S [[1.118033988749895]], :U [[-1.0]], :UT [[-1.0]], 
                 :V [[-0.8944271909999157] [-0.4472135954999579]], 
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
             => {:S [[1.118033988749895]], 
                 :U [[-0.8944271909999157] [-0.4472135954999579]], 
                 :UT [[-0.8944271909999157 -0.4472135954999579]], 
                 :V [[-1.0]], :VT [[-1.0]], :rank 1}
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
             (rank (clatrix [[1.0 0.5 1e-9][0.5 3.0 1e-9][1e-9 1e-9 1e-6]]) 
                   1e-4) => 2
             (rank (clatrix [[1.0 0.5 1e-9][0.5 3.0 1e-9][1e-9 1e-9 1e-6]]) 
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
             => {:P [[1.0]], 
                 :Q [[-0.894427190999916 -0.4472135954999579] 
                     [-0.4472135954999579 0.8944271909999159]], 
                 :QT [[-0.894427190999916 -0.4472135954999579] 
                      [-0.4472135954999579 0.8944271909999159]], 
                 :R [[-1.118033988749895] [0.0]], :rank 1})
       (fact "eigenvalues"
             (eigenvalues [[m/inf+ 0.0][0.0 m/inf+]]) => [m/inf+ m/inf+]
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
             (linear-least-squares [[1.0 0.4 0.2][0.6 0.3 0.9]] [7.0 9.0]) 
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
             (matrix-solve-iterative [[1.0 0.5][0.5 3.0]] [7.0 9.0] 
                                     :solver :symm) 
             => [5.999999999999998 1.9999999999999984]
             (matrix-solve-iterative ap [7.0 9.0] :solver :symm) => (throws)))

(facts "random"
       (fact "vector"
             (first (rnd-vec :clatrix 3 test-rnd-lazy)) 
             => (clatrix [0.8335762378570932 0.11249249636232017 
                          0.8502406979201282])
             (first (rnd-vec 3 test-rnd-lazy)) 
             => [0.8335762378570932 0.11249249636232017 0.8502406979201282])
       (fact "matrix"
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