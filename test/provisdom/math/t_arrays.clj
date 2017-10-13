(ns provisdom.math.t-arrays
  (:require
    [clojure.test :refer :all]
    [criterium.core :as criterium]
    [provisdom.test.core :refer :all]
    [provisdom.math.arrays :as array]
    [provisdom.math.core :as m]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost])
  (:import (java.util Arrays)))

(set! *warn-on-reflection* true)

(ost/instrument)

;;;ARRAY TYPE TESTS
(deftest array?-test
  (is (array/array? (long-array [1 2 3])))
  (is-not (array/array? [1]))
  (is-not (array/array? 1.0))
  (is-not (array/array? nil)))

(deftest array2D?-test
  (is (array/array2D? (array/array2D :long [[1 2 3]])))
  (is-not (array/array2D? (long-array [1])))
  (is-not (array/array2D? 1.0))
  (is-not (array/array2D? nil)))

(deftest array3D?-test
  (is (array/array2D? (array/array3D :long [[[1 2 3]]])))
  (is-not (array/array3D? [[1]]))
  (is-not (array/array3D? 1.0))
  (is-not (array/array3D? nil)))

(deftest double-array?-test
  (is-not (array/double-array? (long-array [1 2 3])))
  (is (array/double-array? (double-array [m/inf+ 2 3])))
  (is-not (array/double-array? [1]))
  (is-not (array/double-array? 1.0))
  (is-not (array/double-array? nil)))

(deftest double-array2D?-test
  (is-not (array/double-array2D? (array/array2D :long [[1 2 3]])))
  (is (array/double-array2D? (array/array2D :double [[1 2 3]])))
  (is-not (array/double-array2D? [1]))
  (is-not (array/double-array2D? 1.0))
  (is-not (array/double-array2D? nil)))

(deftest double-finite-array?-test
  (is-not (array/double-finite-array? (long-array [1 2 3])))
  (is-not (array/double-finite-array? (double-array [m/inf+ 2 3])))
  (is (array/double-finite-array? (double-array [1 2 3])))
  (is-not (array/double-finite-array? [1]))
  (is-not (array/double-finite-array? 1.0))
  (is-not (array/double-finite-array? nil)))

(defspec-test test-array? `array/array?)
(defspec-test test-array2D? `array/array2D?)
(defspec-test test-array3D? `array/array3D?)
(defspec-test test-double-array? `array/double-array?)
(defspec-test test-double-array2D? `array/double-array2D?)
(defspec-test test-double-finite-array? `array/double-finite-array?)

;;;ARRAY CONSTRUCTORS
(deftest arrayND->vector-test
  (is= [1.0 2.0 3.0] (array/arrayND->vector (double-array [1 2 3])))
  (is= [[1 2 3] [4 5 6]] (array/arrayND->vector (array/array2D :long [[1 2 3] [4 5 6]]))))

(deftest keyword->array-type-test
  (is= Long/TYPE (array/keyword->array-type :long))
  (is= Boolean/TYPE (array/keyword->array-type :boolean))
  (is= Character/TYPE (array/keyword->array-type :char))
  (is= Double/TYPE (array/keyword->array-type :double)))

(deftest array2D-test
  (let [arr (array/array2D :char [[\a \b \c]])]
    (is (array/array? arr))
    (is= 1 (count arr))
    (is (array/array? (aget ^"[[C" arr 0)))
    (is (instance? Character (aget ^"[[C" arr 0 0)))
    (is= \a (aget arr 0 0))))

(deftest array3D-test
  (let [arr (array/array3D :double [[[1.0 2.0 3.0] [4.0]]])]
    (is (array/array? arr))
    (is= 1 (count arr))
    (is (array/array? (aget ^"[[[D" arr 0)))
    (is (instance? Double (aget ^"[[[D" arr 0 0 0)))
    (is= 1.0 (aget arr 0 0 0))))

(defspec-test test-arrayND->vector `array/arrayND->vector)
(defspec-test test-keyword->array-type `array/keyword->array-type)
(defspec-test test-array2D `array/array2D)
(defspec-test test-array3D `array/array3D)

;;;DOUBLE ARRAY INFO
(deftest double-array-copy-test
  (let [arr (double-array [1 2 3])
        arr2 (array/double-array-copy arr)
        _ (array/double-array-set! arr 0 0.0)]
    (is (array/double-array= arr (double-array [0.0 2 3])))
    (is (array/double-array= arr2 (double-array [1 2 3])))))

(deftest double-array2D-copy-test
  (let [arr (array/array2D :double [[1 2 3] [4 5 6]])
        arr2 (array/double-array2D-copy arr)
        _ (aset arr 0 0 0.0)]
    (is (Arrays/equals ^"[[D" arr ^"[[D" (array/array2D :double [[0.0 2 3] [4 5 6]])))
    (is (Arrays/equals ^"[[D" arr2 ^"[[D" (array/array2D :double [[1 2 3] [4 5 6]])))))

(deftest double-array=-test
  (is-not (= (double-array [1 2 3]) (double-array [1 2 3])))
  (is (array/double-array= (double-array [1 2 3]) (double-array [1 2 3]))))

(deftest double-array-reduce-kv-test
  (is= 8.0
       (array/double-array-reduce-kv
         (fn [tot k v]
           (+ tot (* k v)))
         0.0
         (double-array [1 2 3]))))

(deftest double-array-find-all-test
  (is= [] (array/double-array-find-all (double-array []) 2.0))
  (is= [1 5] (array/double-array-find-all (double-array [1 2 3 4 3 2 1]) 2.0))
  (is= [] (array/double-array-find-all (double-array [1 2 3 4 3 2 1]) 5.0)))

(deftest double-array-sorted-find-test
  (is= -1 (array/double-array-sorted-find (double-array []) 2.0))
  (is= 2 (array/double-array-sorted-find (double-array [1 1 2 2 3 4]) 2.0))
  (is= -5 (array/double-array-sorted-find (double-array [1 1 2 2 3 4]) 2.5))
  (is= 0 (array/double-array-sorted-find (double-array [1 1 2 2 3 4]) 1.0)))

(defspec-test test-double-array-copy `array/double-array-copy)
(defspec-test test-double-array2D-copy `array/double-array2D-copy)
(defspec-test test-double-array= `array/double-array=)
;(defspec-test test-double-array-reduce-kv `array/double-array-reduce-kv) ;slow-ish
(defspec-test test-double-array-find-all `array/double-array-find-all)
(defspec-test test-double-array-sorted-find `array/double-array-sorted-find)

;;;DOUBLE ARRAY CHANGES
(deftest double-array-sort!-test
  (let [arr (double-array [2.0 1.0 3.0])
        _ (array/double-array-sort! arr)]
    (is= [1.0 2.0 3.0] (array/arrayND->vector arr))))

(deftest double-array-set!-test
  (let [arr (double-array [1.0 2.0 3.0])
        _ (array/double-array-set! arr 1 4.0)]
    (is= [1.0 4.0 3.0] (array/arrayND->vector arr))))

(defspec-test test-double-array-sort! `array/double-array-sort!)
(defspec-test test-double-array-set! `array/double-array-set!)

;;;DOUBLE ARRAY MANIPULATION
(deftest double-array-map-test
  (is= [4.0 5.0 6.0]
       (array/arrayND->vector
         (array/double-array-map
           (fn [e] (+ 3.0 e))
           (double-array [1 2 3])))))

(deftest double-array-map-indexed-test
  (is= [4.0 6.0 8.0]
       (array/arrayND->vector
         (array/double-array-map-indexed
           (fn [e k] (+ 3.0 e k))
           (double-array [1 2 3])))))

;(defspec-test test-double-array-map `array/double-array-map) ;slow-ish
;(defspec-test test-double-array-map-indexed `array/double-array-map-indexed) ;slow-ish

;;;DOUBLE ARRAY MATH
(deftest double-array-add-test
  (is= [3.0 5.0 7.0]
       (array/arrayND->vector
         (array/double-array-add
           (double-array [1 2 3])
           (double-array [2 3 4])))))

(deftest double-array-subtract-test
  (is= [-1.0 -1.0 -1.0]
       (array/arrayND->vector
         (array/double-array-subtract
           (double-array [1 2 3])
           (double-array [2 3 4])))))

(deftest double-array-sum-test
  (is= 0.0 (array/double-array-sum (double-array [])))
  (is= 6.0 (array/double-array-sum (double-array [1 2 3]))))

(deftest double-array-sum-of-squares-test
  (is= 0.0 (array/double-array-sum-of-squares (double-array [])))
  (is= 14.0 (array/double-array-sum-of-squares (double-array [1 2 3]))))

(deftest double-array-dot-product-test
  (is= 25.0 (array/double-array-dot-product (double-array [1 2 3]) (double-array [2 4 5]))))

(deftest double-array-projection-test
  (is= [1.7857142857142858 3.5714285714285716 5.357142857142858]
       (array/arrayND->vector
         (array/double-array-projection (double-array [1 2 3]) (double-array [2 4 5])))))

(deftest double-array-norm-test
  (is= 0.0 (array/double-array-norm (double-array [])))
  (is= 3.7416573867739413 (array/double-array-norm (double-array [-1 2 3]))))

(deftest double-array-norm1-test
  (is= 0.0 (array/double-array-norm1 (double-array [])))
  (is= 6.0 (array/double-array-norm1 (double-array [-1 2 3]))))

;(defspec-test test-double-array-add `array/double-array-add) ;slow-ish
;(defspec-test test-double-array-subtract `array/double-array-subtract) ;slow-ish
(defspec-test test-double-array-sum `array/double-array-sum)
(defspec-test test-double-array-sum-of-squares `array/double-array-sum-of-squares)
(defspec-test test-double-array-dot-product `array/double-array-dot-product)
(defspec-test test-double-array-projection `array/double-array-projection)
(defspec-test test-double-array-norm `array/double-array-norm)
(defspec-test test-double-array-norm1 `array/double-array-norm1)

;;;DOUBLE ARRAY STATS
(deftest double-array-mean-test
  (is (m/nan? (array/double-array-mean (double-array []))))
  (is= 2.0 (array/double-array-mean (double-array [1 2 3]))))

(deftest double-array-second-moment-test
  (is (m/nan? (array/double-array-second-moment (double-array []))))
  (is= 4.666666666666667 (array/double-array-second-moment (double-array [1 2 3]))))

(deftest double-array-variance-test
  (is (m/nan? (array/double-array-variance (double-array []))))
  (is= 0.0 (array/double-array-variance (double-array [1])))
  (is= 0.666666666666667 (array/double-array-variance (double-array [1 2 3]))))

(deftest double-array-std-dev-test
  (is (m/nan? (array/double-array-std-dev (double-array []))))
  (is= 0.0 (array/double-array-std-dev (double-array [1])))
  (is= 0.8164965809277263 (array/double-array-std-dev (double-array [1 2 3]))))

(deftest double-array-cross-moment-test
  (is (m/nan? (array/double-array-cross-moment (double-array []) (double-array []))))
  (is= 2.0 (array/double-array-cross-moment (double-array [1]) (double-array [2])))
  (is= 10.0 (array/double-array-cross-moment (double-array [1 2 3]) (double-array [3 6 5]))))

(deftest double-array-covariance-test
  (is (m/nan? (array/double-array-covariance (double-array []) (double-array []))))
  (is= 0.0 (array/double-array-covariance (double-array [1]) (double-array [2])))
  (is= 0.6666666666666661 (array/double-array-covariance (double-array [1 2 3]) (double-array [3 6 5]))))

(deftest double-array-correlation-test
  (is (m/nan? (array/double-array-correlation (double-array []) (double-array []))))
  (is (m/nan? (array/double-array-correlation (double-array [1]) (double-array [2]))))
  (is= 0.6546536707079776 (array/double-array-correlation (double-array [1 2 3]) (double-array [3 6 5]))))

(defspec-test test-double-array-mean `array/double-array-mean)
(defspec-test test-double-array-second-moment `array/double-array-second-moment)
(defspec-test test-double-array-variance `array/double-array-variance)
(defspec-test test-double-array-std-dev `array/double-array-std-dev)
(defspec-test test-double-array-cross-moment `array/double-array-cross-moment)
(defspec-test test-double-array-covariance `array/double-array-covariance)
(defspec-test test-double-array-correlation `array/double-array-correlation)

;;;DOUBLE-ARRAYS -- TIME TESTS -- useful for huge vectors or repetitive computation.
;;Conversion time is slow.
(comment
  ;; Perf comp between aget and aget-d
  (criterium/bench (aget arr (rand-int 3)))
  ;Evaluation count : 10252620 in 60 samples of 170877 calls.
  ;Execution time mean : 5.799010 µs
  ;Execution time std-deviation : 107.785879 ns
  ;Execution time lower quantile : 5.620088 µs ( 2.5%)
  ;Execution time upper quantile : 5.990912 µs (97.5%)
  ;Overhead used : 1.453119 ns
  ;
  ;Found 1 outliers in 60 samples (1.6667 %)
  ;low-severe	 1 (1.6667 %)
  ;Variance from outliers : 7.8099 % Variance is slightly inflated by outliers
  (criterium/bench (array/aget arr (rand-int 3)))
  ;Evaluation count : 1743407820 in 60 samples of 29056797 calls.
  ;Execution time mean : 32.368536 ns
  ;Execution time std-deviation : 0.239115 ns
  ;Execution time lower quantile : 32.111605 ns ( 2.5%)
  ;Execution time upper quantile : 32.846321 ns (97.5%)
  ;Overhead used : 1.453119 ns
  ;
  ;Found 5 outliers in 60 samples (8.3333 %)
  ;low-severe	 2 (3.3333 %)
  ;low-mild	 3 (5.0000 %)
  ;Variance from outliers : 1.6389 % Variance is slightly inflated by outliers
  (criterium/bench (aget ^doubles arr (rand-int 3)))
  ;Evaluation count : 1785795300 in 60 samples of 29763255 calls.
  ;Execution time mean : 32.860436 ns
  ;Execution time std-deviation : 0.813892 ns
  ;Execution time lower quantile : 32.099554 ns ( 2.5%)
  ;Execution time upper quantile : 35.539697 ns (97.5%)
  ;Overhead used : 1.453119 ns
  ;
  ;Found 5 outliers in 60 samples (8.3333 %)
  ;low-severe	 2 (3.3333 %)
  ;low-mild	 3 (5.0000 %)
  ;Variance from outliers : 12.5787 % Variance is moderately inflated by outliers
  )
