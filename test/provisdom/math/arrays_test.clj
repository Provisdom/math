(ns provisdom.math.arrays-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [criterium.core :as criterium]
    [provisdom.math.arrays :as array]
    [provisdom.math.core :as m]))

;1 seconds

(set! *warn-on-reflection* true)

;;;ARRAY TYPE TESTS
(deftest array?-test
  (with-instrument `array/array?
    (is (spec-check array/array?)))
  (with-instrument (st/instrumentable-syms)
    (is (array/array? (long-array [1 2 3])))
    (is-not (array/array? [1]))
    (is-not (array/array? 1.0))
    (is-not (array/array? nil))))

(deftest array2D?-test
  (with-instrument `array/array2D?
    (is (spec-check array/array2D?)))
  (with-instrument (st/instrumentable-syms)
    (is (array/array2D? (array/array2D :long [[1 2 3]])))
    (is-not (array/array2D? (long-array [1])))
    (is-not (array/array2D? 1.0))
    (is-not (array/array2D? nil))))

(deftest array3D?-test
  (with-instrument `array/array3D?
    (is (spec-check array/array3D?)))
  (with-instrument (st/instrumentable-syms)
    (is (array/array2D? (array/array3D :long [[[1 2 3]]])))
    (is-not (array/array3D? [[1]]))
    (is-not (array/array3D? 1.0))
    (is-not (array/array3D? nil))))

(deftest double-array?-test
  (with-instrument `array/double-array?
    (is (spec-check array/double-array?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (array/double-array? (long-array [1 2 3])))
    (is (array/double-array? (double-array [m/inf+ 2 3])))
    (is-not (array/double-array? [1]))
    (is-not (array/double-array? 1.0))
    (is-not (array/double-array? nil))))

(deftest double-array2D?-test
  (with-instrument `array/double-array2D?
    (is (spec-check array/double-array2D?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (array/double-array2D? (array/array2D :long [[1 2 3]])))
    (is (array/double-array2D? (array/array2D :double [[1 2 3]])))
    (is-not (array/double-array2D? [1]))
    (is-not (array/double-array2D? 1.0))
    (is-not (array/double-array2D? nil))))

(deftest double-finite-array?-test
  (with-instrument `array/double-finite-array?
    (is (spec-check array/double-finite-array?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (array/double-finite-array? (long-array [1 2 3])))
    (is-not (array/double-finite-array? (double-array [m/inf+ 2 3])))
    (is (array/double-finite-array? (double-array [1 2 3])))
    (is-not (array/double-finite-array? [1]))
    (is-not (array/double-finite-array? 1.0))
    (is-not (array/double-finite-array? nil))))

;;;ARRAY CONSTRUCTORS
(deftest arrayND->vector-test
  (with-instrument `array/arrayND->vector
    (is (spec-check array/arrayND->vector)))
  (with-instrument (st/instrumentable-syms)
    (is= [1.0 2.0 3.0] (array/arrayND->vector (double-array [1 2 3])))
    (is= [[1 2 3] [4 5 6]]
      (array/arrayND->vector (array/array2D :long [[1 2 3] [4 5 6]])))))

(deftest keyword->array-type-test
  (with-instrument `array/keyword->array-type
    (is (spec-check array/keyword->array-type)))
  (with-instrument (st/instrumentable-syms)
    (is= Long/TYPE (array/keyword->array-type :long))
    (is= Boolean/TYPE (array/keyword->array-type :boolean))
    (is= Character/TYPE (array/keyword->array-type :char))
    (is= Double/TYPE (array/keyword->array-type :double))))

(deftest array2D-test
  (with-instrument `array/array2D
    (is (spec-check array/array2D)))
  (with-instrument (st/instrumentable-syms)
    (let [arr (array/array2D :char [[\a \b \c]])]
      (is (array/array? arr))
      (is= 1 (count arr))
      (is (array/array? (aget ^"[[C" arr 0)))
      (is (instance? Character (aget ^"[[C" arr 0 0)))
      (is= \a (aget arr 0 0)))))

(deftest array3D-test
  (with-instrument `array/array3D
    (is (spec-check array/array3D)))
  (with-instrument (st/instrumentable-syms)
    (let [arr (array/array3D :double [[[1.0 2.0 3.0] [4.0]]])]
      (is (array/array? arr))
      (is= 1 (count arr))
      (is (array/array? (aget ^"[[[D" arr 0)))
      (is (instance? Double (aget ^"[[[D" arr 0 0 0)))
      (is= 1.0 (aget arr 0 0 0)))))

;;;DOUBLE ARRAY INFO
(deftest double-array-copy-test
  (with-instrument `array/double-array-copy
    (is (spec-check array/double-array-copy)))
  (with-instrument (st/instrumentable-syms)
    (let [arr (double-array [1 2 3])
          arr2 (array/double-array-copy arr)
          _ (array/double-array-set! arr 0 0.0)]
      (is (array/double-array= arr (double-array [0.0 2 3])))
      (is (array/double-array= arr2 (double-array [1 2 3]))))))

(deftest double-array2D-copy-test
  (with-instrument `array/double-array2D-copy
    (is (spec-check array/double-array2D-copy)))
  (with-instrument (st/instrumentable-syms)
    (let [arr (array/array2D :double [[1 2 3] [4 5 6]])
          arr2 (array/double-array2D-copy arr)
          _ (aset arr 0 0 0.0)]
      (is (array/double-array2D= arr
            (array/array2D :double [[0.0 2 3] [4 5 6]])))
      (is (array/double-array2D= arr2
            (array/array2D :double [[1 2 3] [4 5 6]]))))))

(deftest double-array=-test
  (with-instrument `array/double-array=
    (is (spec-check array/double-array=)))
  (with-instrument (st/instrumentable-syms)
    (is-not (= (double-array [1 2 3]) (double-array [1 2 3])))
    (is (array/double-array= (double-array [1 2 3]) (double-array [1 2 3])))))

(deftest double-array2D=-test
  (with-instrument `array/double-array2D=
    (is (spec-check array/double-array2D=)))
  (with-instrument (st/instrumentable-syms)
    (is-not (= (array/array2D :double [[1 2 3]])
              (array/array2D :double [[1 2 3]])))
    (is (array/double-array2D= (array/array2D :double [[1 2 3]])
          (array/array2D :double [[1 2 3]])))))

(deftest double-array-reduce-kv-test
  (with-instrument `array/double-array-reduce-kv
    (is (spec-check array/double-array-reduce-kv)))
  (with-instrument (st/instrumentable-syms)
    (is= 8.0
      (array/double-array-reduce-kv
        (fn [tot k v]
          (+ tot (* k v)))
        0.0
        (double-array [1 2 3])))))

(deftest double-array-find-all-test
  (with-instrument `array/double-array-find-all
    (is (spec-check array/double-array-find-all)))
  (with-instrument (st/instrumentable-syms)
    (is= [] (array/double-array-find-all (double-array []) 2.0))
    (is= [1 5] (array/double-array-find-all (double-array [1 2 3 4 3 2 1]) 2.0))
    (is= [] (array/double-array-find-all (double-array [1 2 3 4 3 2 1]) 5.0))))

(deftest double-array-sorted-find-test
  (with-instrument `array/double-array-sorted-find
    (is (spec-check array/double-array-sorted-find)))
  (with-instrument (st/instrumentable-syms)
    (is= -1 (array/double-array-sorted-find (double-array []) 2.0))
    (is= 2 (array/double-array-sorted-find (double-array [1 1 2 2 3 4]) 2.0))
    (is= -5 (array/double-array-sorted-find (double-array [1 1 2 2 3 4]) 2.5))
    (is= 0 (array/double-array-sorted-find (double-array [1 1 2 2 3 4]) 1.0))))

;;;DOUBLE ARRAY CHANGES
(deftest double-array-sort!-test
  (with-instrument `array/double-array-sort!
    (is (spec-check array/double-array-sort!)))
  (with-instrument (st/instrumentable-syms)
    (let [arr (double-array [2.0 1.0 3.0])
          _ (array/double-array-sort! arr)]
      (is= [1.0 2.0 3.0] (array/arrayND->vector arr)))))

(deftest double-array-set!-test
  (with-instrument `array/double-array-set!
    (is (spec-check array/double-array-set!)))
  (with-instrument (st/instrumentable-syms)
    (let [arr (double-array [1.0 2.0 3.0])
          _ (array/double-array-set! arr 1 4.0)]
      (is= [1.0 4.0 3.0] (array/arrayND->vector arr)))))

;;;DOUBLE ARRAY MANIPULATION
(deftest double-array-map-test
  (with-instrument `array/double-array-map
    (is (spec-check array/double-array-map)))
  (with-instrument (st/instrumentable-syms)
    (is= [4.0 5.0 6.0]
      (array/arrayND->vector
        (array/double-array-map
          (fn [e]
            (+ 3.0 e))
          (double-array [1 2 3]))))))

(deftest double-array-map-indexed-test
  (with-instrument `array/double-array-map-indexed
    (is (spec-check array/double-array-map-indexed)))
  (with-instrument (st/instrumentable-syms)
    (is= [4.0 6.0 8.0]
      (array/arrayND->vector
        (array/double-array-map-indexed
          (fn [e k]
            (+ 3.0 e k))
          (double-array [1 2 3]))))))

;;;DOUBLE ARRAY MATH
(deftest double-array-add-test
  (with-instrument `array/double-array-add
    (is (spec-check array/double-array-add)))
  (with-instrument (st/instrumentable-syms)
    (is= [3.0 5.0 7.0]
      (array/arrayND->vector
        (array/double-array-add
          (double-array [1 2 3])
          (double-array [2 3 4]))))))

(deftest double-array-subtract-test
  (with-instrument `array/double-array-subtract
    (is (spec-check array/double-array-subtract)))
  (with-instrument (st/instrumentable-syms)
    (is= [-1.0 -1.0 -1.0]
      (array/arrayND->vector
        (array/double-array-subtract
          (double-array [1 2 3])
          (double-array [2 3 4]))))))

(deftest double-array-sum-test
  (with-instrument `array/double-array-sum
    (is (spec-check array/double-array-sum)))
  (with-instrument (st/instrumentable-syms)
    (is= 0.0 (array/double-array-sum (double-array [])))
    (is= 6.0 (array/double-array-sum (double-array [1 2 3])))))

(deftest double-array-sum-of-squares-test
  (with-instrument `array/double-array-sum-of-squares
    (is (spec-check array/double-array-sum-of-squares)))
  (with-instrument (st/instrumentable-syms)
    (is= 0.0 (array/double-array-sum-of-squares (double-array [])))
    (is= 14.0 (array/double-array-sum-of-squares (double-array [1 2 3])))))

(deftest double-array-dot-product-test
  (with-instrument `array/double-array-dot-product
    (is (spec-check array/double-array-dot-product)))
  (with-instrument (st/instrumentable-syms)
    (is= 25.0
      (array/double-array-dot-product (double-array [1 2 3])
        (double-array [2 4 5])))))

(deftest double-array-projection-test
  (with-instrument `array/double-array-projection
    (is (spec-check array/double-array-projection)))
  (with-instrument (st/instrumentable-syms)
    (is= [1.7857142857142858 3.5714285714285716 5.357142857142858]
      (array/arrayND->vector
        (array/double-array-projection (double-array [1 2 3])
          (double-array [2 4 5]))))))

(deftest double-array-norm-test
  (with-instrument `array/double-array-norm
    (is (spec-check array/double-array-norm)))
  (with-instrument (st/instrumentable-syms)
    (is= 0.0 (array/double-array-norm (double-array [])))
    (is= 3.7416573867739413 (array/double-array-norm (double-array [-1 2 3])))))

(deftest double-array-norm1-test
  (with-instrument `array/double-array-norm1
    (is (spec-check array/double-array-norm1)))
  (with-instrument (st/instrumentable-syms)
    (is= 0.0 (array/double-array-norm1 (double-array [])))
    (is= 6.0 (array/double-array-norm1 (double-array [-1 2 3])))))

;;;DOUBLE ARRAY STATS
(deftest double-array-mean-test
  (with-instrument `array/double-array-mean
    (is (spec-check array/double-array-mean)))
  (with-instrument (st/instrumentable-syms)
    (is (m/nan? (array/double-array-mean (double-array []))))
    (is= 2.0 (array/double-array-mean (double-array [1 2 3])))))

(deftest double-array-second-moment-test
  (with-instrument `array/double-array-second-moment
    (is (spec-check array/double-array-second-moment)))
  (with-instrument (st/instrumentable-syms)
    (is (m/nan? (array/double-array-second-moment (double-array []))))
    (is= 4.666666666666667
      (array/double-array-second-moment (double-array [1 2 3])))))

(deftest double-array-variance-test
  (with-instrument `array/double-array-variance
    (is (spec-check array/double-array-variance)))
  (with-instrument (st/instrumentable-syms)
    (is (m/nan? (array/double-array-variance (double-array []))))
    (is= 0.0 (array/double-array-variance (double-array [1])))
    (is= 0.666666666666667
      (array/double-array-variance (double-array [1 2 3])))))

(deftest double-array-std-dev-test
  (with-instrument `array/double-array-std-dev
    (is (spec-check array/double-array-std-dev)))
  (with-instrument (st/instrumentable-syms)
    (is (m/nan? (array/double-array-std-dev (double-array []))))
    (is= 0.0 (array/double-array-std-dev (double-array [1])))
    (is= 0.8164965809277263
      (array/double-array-std-dev (double-array [1 2 3])))))

(deftest double-array-cross-moment-test
  (with-instrument `array/double-array-cross-moment
    (is (spec-check array/double-array-cross-moment)))
  (with-instrument (st/instrumentable-syms)
    (is (m/nan? (array/double-array-cross-moment (double-array [])
                  (double-array []))))
    (is= 2.0
      (array/double-array-cross-moment (double-array [1]) (double-array [2])))
    (is= 10.0
      (array/double-array-cross-moment (double-array [1 2 3])
        (double-array [3 6 5])))))

(deftest double-array-covariance-test
  (with-instrument `array/double-array-covariance
    (is (spec-check array/double-array-covariance)))
  (with-instrument (st/instrumentable-syms)
    (is (m/nan? (array/double-array-covariance (double-array [])
                  (double-array []))))
    (is= 0.0
      (array/double-array-covariance (double-array [1]) (double-array [2])))
    (is= 0.6666666666666661
      (array/double-array-covariance (double-array [1 2 3])
        (double-array [3 6 5])))))

(deftest double-array-correlation-test
  (with-instrument `array/double-array-correlation
    (is (spec-check array/double-array-correlation)))
  (with-instrument (st/instrumentable-syms)
    (is (m/nan? (array/double-array-correlation (double-array [])
                  (double-array []))))
    (is (m/nan? (array/double-array-correlation (double-array [1])
                  (double-array [2]))))
    (is= 0.6546536707079776
      (array/double-array-correlation (double-array [1 2 3])
        (double-array [3 6 5])))))

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
