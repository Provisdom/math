(ns provisdom.math.arrays-test
  (:require
    [clojure.test :refer :all]
    [criterium.core :as criterium]
    [provisdom.math.arrays :as array]
    [provisdom.math.core :as m]
    [provisdom.test.core :as t]))

;1 seconds

(set! *warn-on-reflection* true)

;;;ARRAY TYPE TESTS
(deftest array?-test
  (t/with-instrument `array/array?
    (t/is (t/spec-check array/array?)))
  (t/with-instrument :all
    (t/is (array/array? (long-array [1 2 3])))
    (t/is-not (array/array? [1]))
    (t/is-not (array/array? 1.0))
    (t/is-not (array/array? nil))))

(deftest array2D?-test
  (t/with-instrument `array/array2D?
    (t/is (t/spec-check array/array2D?)))
  (t/with-instrument :all
    (t/is (array/array2D? (array/array2D :long [[1 2 3]])))
    (t/is-not (array/array2D? (long-array [1])))
    (t/is-not (array/array2D? 1.0))
    (t/is-not (array/array2D? nil))))

(deftest array3D?-test
  (t/with-instrument `array/array3D?
    (t/is (t/spec-check array/array3D?)))
  (t/with-instrument :all
    (t/is (array/array2D? (array/array3D :long [[[1 2 3]]])))
    (t/is-not (array/array3D? [[1]]))
    (t/is-not (array/array3D? 1.0))
    (t/is-not (array/array3D? nil))))

(deftest double-array?-test
  (t/with-instrument `array/double-array?
    (t/is (t/spec-check array/double-array?)))
  (t/with-instrument :all
    (t/is-not (array/double-array? (long-array [1 2 3])))
    (t/is (array/double-array? (double-array [m/inf+ 2 3])))
    (t/is-not (array/double-array? [1]))
    (t/is-not (array/double-array? 1.0))
    (t/is-not (array/double-array? nil))))

(deftest double-array2D?-test
  (t/with-instrument `array/double-array2D?
    (t/is (t/spec-check array/double-array2D?)))
  (t/with-instrument :all
    (t/is-not (array/double-array2D? (array/array2D :long [[1 2 3]])))
    (t/is (array/double-array2D? (array/array2D :double [[1 2 3]])))
    (t/is-not (array/double-array2D? [1]))
    (t/is-not (array/double-array2D? 1.0))
    (t/is-not (array/double-array2D? nil))))

(deftest double-finite-array?-test
  (t/with-instrument `array/double-finite-array?
    (t/is (t/spec-check array/double-finite-array?)))
  (t/with-instrument :all
    (t/is-not (array/double-finite-array? (long-array [1 2 3])))
    (t/is-not (array/double-finite-array? (double-array [m/inf+ 2 3])))
    (t/is (array/double-finite-array? (double-array [1 2 3])))
    (t/is-not (array/double-finite-array? [1]))
    (t/is-not (array/double-finite-array? 1.0))
    (t/is-not (array/double-finite-array? nil))))

;;;ARRAY CONSTRUCTORS
(deftest arrayND->vector-test
  (t/with-instrument `array/arrayND->vector
    (t/is (t/spec-check array/arrayND->vector)))
  (t/with-instrument :all
    (t/is= [1.0 2.0 3.0] (array/arrayND->vector (double-array [1 2 3])))
    (t/is= [[1 2 3] [4 5 6]] (array/arrayND->vector (array/array2D :long [[1 2 3] [4 5 6]])))))

(deftest keyword->array-type-test
  (t/with-instrument `array/keyword->array-type
    (t/is (t/spec-check array/keyword->array-type)))
  (t/with-instrument :all
    (t/is= Long/TYPE (array/keyword->array-type :long))
    (t/is= Boolean/TYPE (array/keyword->array-type :boolean))
    (t/is= Character/TYPE (array/keyword->array-type :char))
    (t/is= Double/TYPE (array/keyword->array-type :double))))

(deftest array2D-test
  (t/with-instrument `array/array2D
    (t/is (t/spec-check array/array2D)))
  (t/with-instrument :all
    (let [arr (array/array2D :char [[\a \b \c]])]
      (t/is (array/array? arr))
      (t/is= 1 (count arr))
      (t/is (array/array? (aget ^"[[C" arr 0)))
      (t/is (instance? Character (aget ^"[[C" arr 0 0)))
      (t/is= \a (aget arr 0 0)))))

(deftest array3D-test
  (t/with-instrument `array/array3D
    (t/is (t/spec-check array/array3D)))
  (t/with-instrument :all
    (let [arr (array/array3D :double [[[1.0 2.0 3.0] [4.0]]])]
      (t/is (array/array? arr))
      (t/is= 1 (count arr))
      (t/is (array/array? (aget ^"[[[D" arr 0)))
      (t/is (instance? Double (aget ^"[[[D" arr 0 0 0)))
      (t/is= 1.0 (aget arr 0 0 0)))))

;;;DOUBLE ARRAY INFO
(deftest double-array-copy-test
  (t/with-instrument `array/double-array-copy
    (t/is (t/spec-check array/double-array-copy)))
  (t/with-instrument :all
    (let [arr (double-array [1 2 3])
          arr2 (array/double-array-copy arr)
          _ (array/double-array-set! arr 0 0.0)]
      (t/is (array/double-array= arr (double-array [0.0 2 3])))
      (t/is (array/double-array= arr2 (double-array [1 2 3]))))))

(deftest double-array2D-copy-test
  (t/with-instrument `array/double-array2D-copy
    (t/is (t/spec-check array/double-array2D-copy)))
  (t/with-instrument :all
    (let [arr (array/array2D :double [[1 2 3] [4 5 6]])
          arr2 (array/double-array2D-copy arr)
          _ (aset arr 0 0 0.0)]
      (t/is (array/double-array2D= arr (array/array2D :double [[0.0 2 3] [4 5 6]])))
      (t/is (array/double-array2D= arr2 (array/array2D :double [[1 2 3] [4 5 6]]))))))

(deftest double-array=-test
  (t/with-instrument `array/double-array=
    (t/is (t/spec-check array/double-array=)))
  (t/with-instrument :all
    (t/is-not (= (double-array [1 2 3]) (double-array [1 2 3])))
    (t/is (array/double-array= (double-array [1 2 3]) (double-array [1 2 3])))))

(deftest double-array2D=-test
  (t/with-instrument `array/double-array2D=
    (t/is (t/spec-check array/double-array2D=)))
  (t/with-instrument :all
    (t/is-not (= (array/array2D :double [[1 2 3]]) (array/array2D :double [[1 2 3]])))
    (t/is (array/double-array2D= (array/array2D :double [[1 2 3]])
            (array/array2D :double [[1 2 3]])))))

(deftest double-array-reduce-kv-test
  (t/with-instrument `array/double-array-reduce-kv
    (t/is (t/spec-check array/double-array-reduce-kv)))
  (t/with-instrument :all
    (t/is= 8.0
      (array/double-array-reduce-kv
        (fn [tot k v] (+ tot (* k v)))
        0.0
        (double-array [1 2 3])))))

(deftest double-array-find-all-test
  (t/with-instrument `array/double-array-find-all
    (t/is (t/spec-check array/double-array-find-all)))
  (t/with-instrument :all
    (t/is= [] (array/double-array-find-all (double-array []) 2.0))
    (t/is= [1 5] (array/double-array-find-all (double-array [1 2 3 4 3 2 1]) 2.0))
    (t/is= [] (array/double-array-find-all (double-array [1 2 3 4 3 2 1]) 5.0))))

(deftest double-array-sorted-find-test
  (t/with-instrument `array/double-array-sorted-find
    (t/is (t/spec-check array/double-array-sorted-find)))
  (t/with-instrument :all
    (t/is= -1 (array/double-array-sorted-find (double-array []) 2.0))
    (t/is= 2 (array/double-array-sorted-find (double-array [1 1 2 2 3 4]) 2.0))
    (t/is= -5 (array/double-array-sorted-find (double-array [1 1 2 2 3 4]) 2.5))
    (t/is= 0 (array/double-array-sorted-find (double-array [1 1 2 2 3 4]) 1.0))))

;;;DOUBLE ARRAY CHANGES
(deftest double-array-sort!-test
  (t/with-instrument `array/double-array-sort!
    (t/is (t/spec-check array/double-array-sort!)))
  (t/with-instrument :all
    (let [arr (double-array [2.0 1.0 3.0])
          _ (array/double-array-sort! arr)]
      (t/is= [1.0 2.0 3.0] (array/arrayND->vector arr)))))

(deftest double-array-set!-test
  (t/with-instrument `array/double-array-set!
    (t/is (t/spec-check array/double-array-set!)))
  (t/with-instrument :all
    (let [arr (double-array [1.0 2.0 3.0])
          _ (array/double-array-set! arr 1 4.0)]
      (t/is= [1.0 4.0 3.0] (array/arrayND->vector arr)))))

;;;DOUBLE ARRAY MANIPULATION
(deftest double-array-map-test
  (t/with-instrument `array/double-array-map
    (t/is (t/spec-check array/double-array-map)))
  (t/with-instrument :all
    (t/is= [4.0 5.0 6.0]
      (array/arrayND->vector
        (array/double-array-map
          (fn [e] (+ 3.0 e))
          (double-array [1 2 3]))))))

(deftest double-array-map-indexed-test
  (t/with-instrument `array/double-array-map-indexed
    (t/is (t/spec-check array/double-array-map-indexed)))
  (t/with-instrument :all
    (t/is= [4.0 6.0 8.0]
      (array/arrayND->vector
        (array/double-array-map-indexed
          (fn [e k] (+ 3.0 e k))
          (double-array [1 2 3]))))))

;;;DOUBLE ARRAY MATH
(deftest double-array-add-test
  (t/with-instrument `array/double-array-add
    (t/is (t/spec-check array/double-array-add)))
  (t/with-instrument :all
    (t/is= [3.0 5.0 7.0]
      (array/arrayND->vector
        (array/double-array-add (double-array [1 2 3]) (double-array [2 3 4]))))))

(deftest double-array-subtract-test
  (t/with-instrument `array/double-array-subtract
    (t/is (t/spec-check array/double-array-subtract)))
  (t/with-instrument :all
    (t/is= [-1.0 -1.0 -1.0]
      (array/arrayND->vector
        (array/double-array-subtract (double-array [1 2 3]) (double-array [2 3 4]))))))

(deftest double-array-sum-test
  (t/with-instrument `array/double-array-sum
    (t/is (t/spec-check array/double-array-sum)))
  (t/with-instrument :all
    (t/is= 0.0 (array/double-array-sum (double-array [])))
    (t/is= 6.0 (array/double-array-sum (double-array [1 2 3])))))

(deftest double-array-sum-of-squares-test
  (t/with-instrument `array/double-array-sum-of-squares
    (t/is (t/spec-check array/double-array-sum-of-squares)))
  (t/with-instrument :all
    (t/is= 0.0 (array/double-array-sum-of-squares (double-array [])))
    (t/is= 14.0 (array/double-array-sum-of-squares (double-array [1 2 3])))))

(deftest double-array-dot-product-test
  (t/with-instrument `array/double-array-dot-product
    (t/is (t/spec-check array/double-array-dot-product)))
  (t/with-instrument :all
    (t/is= 25.0
      (array/double-array-dot-product (double-array [1 2 3]) (double-array [2 4 5])))))

(deftest double-array-projection-test
  (t/with-instrument `array/double-array-projection
    (t/is (t/spec-check array/double-array-projection)))
  (t/with-instrument :all
    (t/is= [1.7857142857142858 3.5714285714285716 5.357142857142858]
      (array/arrayND->vector
        (array/double-array-projection (double-array [1 2 3]) (double-array [2 4 5]))))))

(deftest double-array-norm-test
  (t/with-instrument `array/double-array-norm
    (t/is (t/spec-check array/double-array-norm)))
  (t/with-instrument :all
    (t/is= 0.0 (array/double-array-norm (double-array [])))
    (t/is= 3.7416573867739413 (array/double-array-norm (double-array [-1 2 3])))))

(deftest double-array-norm1-test
  (t/with-instrument `array/double-array-norm1
    (t/is (t/spec-check array/double-array-norm1)))
  (t/with-instrument :all
    (t/is= 0.0 (array/double-array-norm1 (double-array [])))
    (t/is= 6.0 (array/double-array-norm1 (double-array [-1 2 3])))))

;;;DOUBLE ARRAY STATS
(deftest double-array-mean-test
  (t/with-instrument `array/double-array-mean
    (t/is (t/spec-check array/double-array-mean)))
  (t/with-instrument :all
    (t/is (m/nan? (array/double-array-mean (double-array []))))
    (t/is= 2.0 (array/double-array-mean (double-array [1 2 3])))))

(deftest double-array-second-moment-test
  (t/with-instrument `array/double-array-second-moment
    (t/is (t/spec-check array/double-array-second-moment)))
  (t/with-instrument :all
    (t/is (m/nan? (array/double-array-second-moment (double-array []))))
    (t/is= 4.666666666666667 (array/double-array-second-moment (double-array [1 2 3])))))

(deftest double-array-variance-test
  (t/with-instrument `array/double-array-variance
    (t/is (t/spec-check array/double-array-variance)))
  (t/with-instrument :all
    (t/is (m/nan? (array/double-array-variance (double-array []))))
    (t/is= 0.0 (array/double-array-variance (double-array [1])))
    (t/is= 0.666666666666667 (array/double-array-variance (double-array [1 2 3])))))

(deftest double-array-std-dev-test
  (t/with-instrument `array/double-array-std-dev
    (t/is (t/spec-check array/double-array-std-dev)))
  (t/with-instrument :all
    (t/is (m/nan? (array/double-array-std-dev (double-array []))))
    (t/is= 0.0 (array/double-array-std-dev (double-array [1])))
    (t/is= 0.8164965809277263 (array/double-array-std-dev (double-array [1 2 3])))))

(deftest double-array-cross-moment-test
  (t/with-instrument `array/double-array-cross-moment
    (t/is (t/spec-check array/double-array-cross-moment)))
  (t/with-instrument :all
    (t/is (m/nan? (array/double-array-cross-moment (double-array []) (double-array []))))
    (t/is= 2.0
      (array/double-array-cross-moment (double-array [1]) (double-array [2])))
    (t/is= 10.0
      (array/double-array-cross-moment (double-array [1 2 3]) (double-array [3 6 5])))))

(deftest double-array-covariance-test
  (t/with-instrument `array/double-array-covariance
    (t/is (t/spec-check array/double-array-covariance)))
  (t/with-instrument :all
    (t/is (m/nan? (array/double-array-covariance (double-array []) (double-array []))))
    (t/is= 0.0
      (array/double-array-covariance (double-array [1]) (double-array [2])))
    (t/is= 0.6666666666666661
      (array/double-array-covariance (double-array [1 2 3]) (double-array [3 6 5])))))

(deftest double-array-correlation-test
  (t/with-instrument `array/double-array-correlation
    (t/is (t/spec-check array/double-array-correlation)))
  (t/with-instrument :all
    (t/is (m/nan? (array/double-array-correlation (double-array []) (double-array []))))
    (t/is (m/nan? (array/double-array-correlation (double-array [1]) (double-array [2]))))
    (t/is= 0.6546536707079776
      (array/double-array-correlation (double-array [1 2 3]) (double-array [3 6 5])))))

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
  (criterium/bench (aget ^doubles arr (rand-int 3))))
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
  
