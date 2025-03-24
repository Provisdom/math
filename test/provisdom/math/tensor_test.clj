(ns provisdom.math.tensor-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.core :as m]
    [provisdom.math.random :as random]
    [provisdom.math.tensor :as tensor]))

;;1 seconds

(set! *warn-on-reflection* true)

;;TYPES
(deftest tensor?-test
  (with-instrument `tensor/tensor?
    (is (spec-check tensor/tensor?)))
  (with-instrument (st/instrumentable-syms)
    (is (tensor/tensor? []))
    (is-not (tensor/tensor? "A"))
    (is-not (tensor/tensor? ["A"]))
    (is (tensor/tensor? [[] []]))                           ;notice tensors can have multiple zero dims
    (is (tensor/tensor? [[]]))
    (is-not (tensor/tensor? [[2] 2]))
    (is-not (tensor/tensor? [2 [2]]))
    (is (tensor/tensor? [[[m/nan]]]))
    (is-not (tensor/tensor? '()))
    (is (tensor/tensor? 1))))

;;CONSTRUCTORS
(deftest to-tensor-test
  (with-instrument `tensor/to-tensor
    (is (spec-check tensor/to-tensor)))
  (with-instrument (st/instrumentable-syms)
    (is= [] (tensor/to-tensor '()))
    (is= nil (tensor/to-tensor "A"))
    (is= [] (tensor/to-tensor []))
    (is= nil (tensor/to-tensor [[2] 3]))
    (is= nil (tensor/to-tensor [2 [3]]))
    (is= nil (tensor/to-tensor [[2] [3 4]]))
    (is= [[2 3]] (tensor/to-tensor '((2 3))))))

(deftest compute-tensor-test
  (with-instrument `tensor/compute-tensor
    (is (spec-check tensor/compute-tensor)))
  (with-instrument (st/instrumentable-syms)
    (is= -2
      (tensor/compute-tensor [] (fn [[n]]
                                  (or n -2))))
    (is= []
      (tensor/compute-tensor [0] (fn [[n]]
                                   (or n -2))))
    (is= [[]]
      (tensor/compute-tensor [1 0] (fn [[_ n2]]
                                     (or n2 -2))))
    (is= [0 1]
      (tensor/compute-tensor [2] (fn [[n]]
                                   (or n -2))))
    (is= [[0 1 2] [0 1 2]]
      (tensor/compute-tensor [2 3] (fn [[n1 n2]]
                                     (if n1
                                       (or n2 -1)
                                       -2))))))

(deftest repeat-tensor-test
  (with-instrument `tensor/repeat-tensor
    (is (spec-check tensor/repeat-tensor)))
  (with-instrument (st/instrumentable-syms)
    (is= 0.0 (tensor/repeat-tensor []))
    (is= 1.0 (tensor/repeat-tensor [] 1.0))
    (is= [] (tensor/repeat-tensor [0] 1.0))
    (is= [[]] (tensor/repeat-tensor [1 0] 1.0))
    (is= [0.0] (tensor/repeat-tensor [1]))
    (is= [1.0] (tensor/repeat-tensor [1] 1.0))
    (is= [[2]] (tensor/repeat-tensor [1] [2]))
    (is= [[[0 0]] [[0 0]]] (tensor/repeat-tensor [2 1] [0 0]))))

(deftest fill-tensor-test
  (with-instrument `tensor/fill-tensor
    (is (spec-check tensor/fill-tensor)))
  (with-instrument (st/instrumentable-syms)
    (is= 1 (tensor/fill-tensor [] [1 2 3 4]))
    (is= [] (tensor/fill-tensor [0] [1 2 3 4]))
    (is= [[]] (tensor/fill-tensor [1 0] [1 2 3 4]))
    (is= [[1 2 3] [4 5 6]] (tensor/fill-tensor [2 3] [1 2 3 4 5 6 7 8]))
    (is= [[1 2 3] [4 0.0 0.0]] (tensor/fill-tensor [2 3] [1 2 3 4]))
    (is= [[[1 2] [3 4] [5 6]] [[7 8] [9 10] [11 12]]]
      (tensor/fill-tensor [2 3 2] [1 2 3 4 5 6 7 8 9 10 11 12 13 14]))))

(deftest rnd-tensor!-test
  (with-instrument `tensor/rnd-tensor!
    (is (spec-check tensor/rnd-tensor!)))
  (with-instrument (st/instrumentable-syms)
    (random/bind-seed 0
      (is= 0.8833108082136426 (tensor/rnd-tensor! [])))
    (random/bind-seed 0
      (is= [] (tensor/rnd-tensor! [0])))
    (random/bind-seed 0
      (is= [[]] (tensor/rnd-tensor! [1 0])))
    (random/bind-seed 0
      (is= [[0.8833108082136426 0.026433771592597743 0.10634669156721244]
            [0.17386786595968284 0.24568894884013137 0.39646797562881353]]
        (tensor/rnd-tensor! [2 3])))))

;;INFO
(deftest first-number-test
  (with-instrument `tensor/first-number
    (is (spec-check tensor/first-number)))
  (with-instrument (st/instrumentable-syms)
    (is= 2 (tensor/first-number 2))
    (is= nil (tensor/first-number []))
    (is= 2 (tensor/first-number [2]))
    (is= 2 (tensor/first-number [[2]]))))

(deftest ecount-test
  (with-instrument `tensor/ecount
    (is (spec-check tensor/ecount)))
  (with-instrument (st/instrumentable-syms)
    (is= 1 (tensor/ecount 0))
    (is= 0 (tensor/ecount [[]]))
    (is= 4 (tensor/ecount [[1.0 0.5] [2.0 4.0]]))))

(deftest rank-test
  (with-instrument `tensor/rank
    (is (spec-check tensor/rank)))
  (with-instrument (st/instrumentable-syms)
    (is= 2 (tensor/rank [[2] [1]]))
    (is= 1 (tensor/rank []))
    (is= 0 (tensor/rank 1))))

(deftest shape-test
  (with-instrument `tensor/shape
    (is (spec-check tensor/shape)))
  (with-instrument (st/instrumentable-syms)
    (is= [3] (tensor/shape [1 2 3]))
    (is= [2 0] (tensor/shape [[] []]))
    (is= [] (tensor/shape 1))
    (is= [2 2 1] (tensor/shape [[[1] [2]] [[3] [4]]]))))

(deftest every-kv?-test
  (with-instrument `tensor/every-kv?
    (is (spec-check tensor/every-kv?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (tensor/every-kv? (fn [indices v]
                                (and (not (empty? indices))
                                  (> (first indices) v)))
              [1.0 0.5]))
    (is (tensor/every-kv? (fn [indices v]
                            (and (not (empty? indices))
                              (> (first indices) -1)))
          [1.0 0.5]))
    (is (tensor/every-kv? (fn [indices v]
                            (and (= 2 (count indices))
                              (> (second indices) (- v 2.1))))
          [[1.0 0.5] [2.0 3.0]]))))

(deftest filter-kv-test
  (with-instrument `tensor/filter-kv
    (is (spec-check tensor/filter-kv)))
  (with-instrument (st/instrumentable-syms)
    (is= [[3 4]]
      (tensor/filter-kv (fn [index tensor]
                          (odd? index))
        [[1 2] [3 4]]))))

(deftest emap-test
  (with-instrument `tensor/emap
    (is (spec-check tensor/emap)))
  (with-instrument (st/instrumentable-syms)
    (is= [[6 6] [6 6]]
      (tensor/emap +
        1
        [2 2]
        [[3 3] [3 3]]))
    (is= 1.0 (tensor/emap m/sq 1.0))
    (is= [[1.0 0.25] [4.0 16.0]] (tensor/emap m/sq [[1.0 0.5] [2.0 4.0]]))
    (is= [1.0 0.25] (tensor/emap m/sq [1.0 0.5]))
    (is= [[1.0 0.25]] (tensor/emap m/sq [[1.0 0.5]]))
    (is= [[1.0] [0.25]] (tensor/emap m/sq [[1.0] [0.5]]))
    (is= [[3.0 1.5] [5.0 8.5]]
      (tensor/emap (fn [n1 n2 n3]
                     (+ n1 n2 n3))
        [[1.0 0.5] [2.0 4.0]]
        [[1.0 0.5] [2.0 4.0]]
        [1.0 0.5]))
    ;;notice need to check for spec with 4+ tensors
    (is= [[4.0 2.0] [7.0 12.5]]
      (tensor/emap (fn [& args]
                     (when (= 4 (count args))
                       (let [[n1 n2 n3 n4] args]
                         (+ n1 n2 n3 n4))))
        [[1.0 0.5] [2.0 4.0]]
        [[1.0 0.5] [2.0 4.0]]
        [1.0 0.5]
        [[1.0 0.5] [2.0 4.0]]))
    (is= 15.0
      (tensor/emap (fn [& args]
                     (when (= 5 (count args))
                       (let [[n1 n2 n3 n4 n5] args]
                         (+ n1 n2 n3 n4 n5))))
        1.0
        2.0
        3.0
        4.0
        5.0))
    (is= [[2.0 1.0] [4.0 8.0]]
      (tensor/emap (fn [n1 n2]
                     (+ n1 n2))
        [[1.0 0.5] [2.0 4.0]]
        [[1.0 0.5] [2.0 4.0]]))))

(deftest emap-kv-test
  (with-instrument `tensor/emap-kv
    (is (spec-check tensor/emap-kv)))
  (with-instrument (st/instrumentable-syms)
    ;;notice need to check indices for spec
    (is= [1.0 1.5]
      (tensor/emap-kv (fn [indices n1]
                        (when (m/one? (count indices))
                          (let [[i] indices]
                            (+ i n1))))
        [1.0 0.5]))
    ;;notice also need to check for spec with 4+ tensors
    (is= [[8 10] [12 17]]
      (tensor/emap-kv (fn [indices & args]
                        (when (and (= 4 (count args))
                                (= 2 (count indices)))
                          (let [[i1 i2] indices
                                [n1 n2 n3 n4] args]
                            (+ i1 i2 n1 n2 n3 n4))))
        1
        [2 3]
        [[4 5] [6 7]]
        [[1 0] [2 4]]))))

(deftest partition-recursively-test
  (with-instrument `tensor/partition-recursively
    (is (spec-check tensor/partition-recursively)))
  (with-instrument (st/instrumentable-syms)
    (is= nil (tensor/partition-recursively 3 []))
    (is= [1 2] (tensor/partition-recursively 2 [1 2]))
    (is= [1 2 3] (tensor/partition-recursively 3 [1 2 3]))
    (is= [[1 2 3]] (tensor/partition-recursively 3 [1 2 3 4]))
    (is= [[1 2 3] [4 5 6]] (tensor/partition-recursively 3 [1 2 3 4 5 6]))
    (is= [[[1 2 3] [4 5 6] [7 8 9]]]
      (tensor/partition-recursively 3 [1 2 3 4 5 6 7 8 9 10]))
    (is= [[[1 2 3] [4 5 6]] [[7 8 9] [10 11 12]]]
      (tensor/partition-recursively 2 [[1 2 3] [4 5 6] [7 8 9] [10 11 12]]))))

;;MATH
(deftest ===-test
  (with-instrument `tensor/===
    (is (spec-check tensor/===)))
  (with-instrument (st/instrumentable-syms)
    (is (tensor/=== [[1.0 0.5] [2.0 m/nan]] [[1.0 0.5] [2.0 m/nan]]))
    (is (tensor/=== [[1.0 0.5] [2.0 m/nan]]
          [[1.0 0.5] [2.0 m/nan]]
          [[1.0 0.5] [2.0 m/nan]]))))

(deftest add-test
  (with-instrument `tensor/add
    (is (spec-check tensor/add)))
  (with-instrument (st/instrumentable-syms)
    (is= [[2.0 1.0] [4.0 8.0]]
      (tensor/add [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
    (is= [2.0 1.0] (tensor/add [1.0 0.5] [1.0 0.5]))
    (is= [[2.0 1.0]] (tensor/add [[1.0 0.5]] [[1.0 0.5]]))
    (is= [1.0 2.0] (tensor/add [1.0 2.0]))
    (is= 0.0 (tensor/add))))

(deftest subtract-test
  (with-instrument `tensor/subtract
    (is (spec-check tensor/subtract)))
  (with-instrument (st/instrumentable-syms)
    (is= [[0.0 0.0] [0.0 0.0]]
      (tensor/subtract [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
    (is= [0.0 0.0] (tensor/subtract [1.0 0.5] [1.0 0.5]))
    (is= [[-1.0 -0.5]] (tensor/subtract [[1.0 0.5]] [[1.0 0.5]] [[1.0 0.5]]))))

(deftest multiply-test
  (with-instrument `tensor/multiply
    (is (spec-check tensor/multiply)))
  (with-instrument (st/instrumentable-syms)
    (is= [[1.0 0.25]] (tensor/multiply [[1.0 0.5]] [[1.0 0.5]]))
    (is= [1.0 0.125] (tensor/multiply [1.0 0.5] [1.0 0.5] [1.0 0.5]))
    (is= [[1.0 0.5]] (tensor/multiply [[1.0 0.5]]))
    (is= 1.0 (tensor/multiply))))

(deftest divide-test
  (with-instrument `tensor/divide
    (is (spec-check tensor/divide)))
  (with-instrument (st/instrumentable-syms)
    (is= [[1.0 1.0]] (tensor/divide [[1.0 0.5]] [[1.0 0.5]]))
    (is= [[3.0 6.0]] (tensor/divide 3.0 [[1.0 0.5]]))
    (is= [1.0 2.0] (tensor/divide [1.0 0.5] [1.0 0.5] [1.0 0.5]))
    (is= [[1.0 2.0]] (tensor/divide [[1.0 0.5]]))))

(deftest average-test
  (with-instrument `tensor/average
    (is (spec-check tensor/average)))
  (with-instrument (st/instrumentable-syms)
    (is= 1.875 (tensor/average [[1.0 0.5] [2.0 4.0]]))
    (is= 0.75 (tensor/average [[1.0 0.5]]))
    (is= 0.75 (tensor/average [1.0 0.5]))))

(deftest norm1-test
  (with-instrument `tensor/norm1
    (is (spec-check tensor/norm1)))
  (with-instrument (st/instrumentable-syms)
    (is= 7.5 (tensor/norm1 [[1.0 0.5] [2.0 4.0]]))
    (is= 1.5 (tensor/norm1 [[1.0 0.5]]))
    (is= 1.5 (tensor/norm1 [1.0 0.5]))))

(deftest norm-test
  (with-instrument `tensor/norm
    (is (spec-check tensor/norm)))
  (with-instrument (st/instrumentable-syms)
    (is= 4.6097722286464435 (tensor/norm [[1.0 0.5] [2.0 4.0]]))
    (is= 1.118033988749895 (tensor/norm [[1.0 0.5]]))
    (is= 1.118033988749895 (tensor/norm [1.0 0.5]))))

(deftest norm-p-test
  (with-instrument `tensor/norm-p
    (is (spec-check tensor/norm-p)))
  (with-instrument (st/instrumentable-syms)
    (is= 7.5 (tensor/norm-p [[1.0 0.5] [2.0 4.0]] 1.0))
    (is= 4.118720689718815 (tensor/norm-p [[1.0 0.5] [2.0 4.0]] 3.4))
    (is= 1.1049918154523823 (tensor/norm-p [[1.0 0.5]] 2.1))
    (is= 1.1049918154523823 (tensor/norm-p [1.0 0.5] 2.1))
    (is= 1.5 (tensor/norm-p [1.0 0.5] 1.0))))

(deftest normalize1-test
  (with-instrument `tensor/normalize1
    (is (spec-check tensor/normalize1)))
  (with-instrument (st/instrumentable-syms)
    (is= [[0.13333333333333333 0.06666666666666667]
          [0.26666666666666666 0.5333333333333333]]
      (tensor/normalize1 [[1.0 0.5] [2.0 4.0]]))
    (is= 0.9999999999999998
      (apply + (tensor/normalize1
                 [2.1242141025912059120591205912509510259021590125
                  1.2398578935713571650983759872398572983
                  2.1351365731650631856238056287035
                  3.235729375209357203975])))
    (is= [[0.13333333333333333 0.06666666666666667]
          [0.26666666666666666 0.5333333333333333]]
      (tensor/normalize1 [[1.0 0.5] [2.0 4.0]]))
    (is= [0.6666666666666666 0.3333333333333333] (tensor/normalize1 [1.0 0.5]))
    (is= [0.6666666666666666 0.3333333333333333] (tensor/normalize1 [1.0 0.5]))))

(deftest normalize-test
  (with-instrument `tensor/normalize
    (is (spec-check tensor/normalize)))
  (with-instrument (st/instrumentable-syms)
    (is= [[0.21693045781865616 0.10846522890932808]
          [0.4338609156373123 0.8677218312746247]]
      (tensor/normalize [[1.0 0.5] [2.0 4.0]]))
    (is= [[0.21693045781865616 0.10846522890932808]
          [0.4338609156373123 0.8677218312746247]]
      (tensor/normalize [[1.0 0.5] [2.0 4.0]]))
    (is= [0.8944271909999159 0.4472135954999579] (tensor/normalize [1.0 0.5]))
    (is= [0.8944271909999159 0.4472135954999579] (tensor/normalize [1.0 0.5]))))

(deftest normalize-p-test
  (with-instrument `tensor/normalize-p
    (is (spec-check tensor/normalize-p)))
  (with-instrument (st/instrumentable-syms)
    (is= [[0.13333333333333333 0.06666666666666667]
          [0.26666666666666666 0.5333333333333333]]
      (tensor/normalize-p [[1.0 0.5] [2.0 4.0]] 1.0))
    (is= [[0.24279383705144375 0.12139691852572188]
          [0.4855876741028875 0.971175348205775]]
      (tensor/normalize-p [[1.0 0.5] [2.0 4.0]] 3.4))
    (is= [[0.9049840786292169 0.45249203931460846]]
      (tensor/normalize-p [[1.0 0.5]] 2.1))
    (is= [0.9049840786292169 0.45249203931460846]
      (tensor/normalize-p [1.0 0.5] 2.1))
    (is= [0.6666666666666666 0.3333333333333333]
      (tensor/normalize-p [1.0 0.5] 1.0))))

(deftest inner-product-test
  (with-instrument `tensor/inner-product
    (is (spec-check tensor/inner-product)))
  (with-instrument (st/instrumentable-syms)
    (is= [48.0 54.0 60.0]
      (tensor/inner-product [1 2 3] [[4 5 6] [7 8 9] [10 11 12]]))))

;;ROUNDING
(deftest roughly?-test
  (with-instrument `tensor/roughly?
    (is (spec-check tensor/roughly?)))
  (with-instrument (st/instrumentable-syms)
    (is (tensor/roughly? 1 1.01 0.05))
    (is-not (tensor/roughly? 1 1.01 0.005))
    (is (tensor/roughly? [1 2] [1.01 2] 0.05))
    (is-not (tensor/roughly? [1 2] [1.01 2] 0.005))
    (is (tensor/roughly? [[1 2] [3 4]] [[1.01 2] [3 4]] 0.05))
    (is-not (tensor/roughly? [[1 2] [3 4]] [[1.01 2] [3 4]] 0.005))))

(deftest roughly-distinct-test
  (with-instrument `tensor/roughly-distinct
    (is (spec-check tensor/roughly-distinct)))
  (with-instrument (st/instrumentable-syms)
    (is= [1 1.01] (tensor/roughly-distinct [1 1.01 1.001] 0.005))
    (is= [[1 1] [1.01 1.01]]
      (tensor/roughly-distinct [[1 1] [1.01 1.01] [1.001 1.001]] 0.005))
    (is= [[1 1.01]]
      (tensor/roughly-distinct
        [[1 1.01] [1.01 1] [1.01 1.01] [1.001 1.001]]
        0.05))))
