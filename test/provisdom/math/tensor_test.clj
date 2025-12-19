(ns provisdom.math.tensor-test
  (:require

    [clojure.test :refer :all]
    [provisdom.test.core :as t]
    [provisdom.math.core :as m]
    [provisdom.math.random :as random]
    [provisdom.math.tensor :as tensor]))

;;1 seconds

(set! *warn-on-reflection* true)

;;TYPES
(deftest tensor?-test
  (t/with-instrument `tensor/tensor?
    (t/is-spec-check tensor/tensor?))
  (t/with-instrument :all
    (t/is (tensor/tensor? []))
    (t/is-not (tensor/tensor? "A"))
    (t/is-not (tensor/tensor? ["A"]))
    (t/is (tensor/tensor? [[] []]))                   ;notice tensors can have multiple zero dims
    (t/is (tensor/tensor? [[]]))
    (t/is-not (tensor/tensor? [[2] 2]))
    (t/is-not (tensor/tensor? [2 [2]]))
    (t/is (tensor/tensor? [[[m/nan]]]))
    (t/is-not (tensor/tensor? '()))
    (t/is (tensor/tensor? 1))))

;;CONSTRUCTORS
(deftest to-tensor-test
  (t/with-instrument `tensor/to-tensor
    (t/is-spec-check tensor/to-tensor))
  (t/with-instrument :all
    (t/is= [] (tensor/to-tensor '()))
    (t/is= nil (tensor/to-tensor "A"))
    (t/is= [] (tensor/to-tensor []))
    (t/is= nil (tensor/to-tensor [[2] 3]))
    (t/is= nil (tensor/to-tensor [2 [3]]))
    (t/is= nil (tensor/to-tensor [[2] [3 4]]))
    (t/is= [[2 3]] (tensor/to-tensor '((2 3))))))

(deftest compute-tensor-test
  (t/with-instrument `tensor/compute-tensor
    (t/is-spec-check tensor/compute-tensor))
  (t/with-instrument :all
    (t/is= -2
      (tensor/compute-tensor [] (fn [[n]]
                                  (or n -2))))
    (t/is= []
      (tensor/compute-tensor [0] (fn [[n]]
                                   (or n -2))))
    (t/is= [[]]
      (tensor/compute-tensor [1 0] (fn [[_ n2]]
                                     (or n2 -2))))
    (t/is= [0 1]
      (tensor/compute-tensor [2] (fn [[n]]
                                   (or n -2))))
    (t/is= [[0 1 2] [0 1 2]]
      (tensor/compute-tensor [2 3] (fn [[n1 n2]]
                                     (if n1
                                       (or n2 -1)
                                       -2))))))

(deftest repeat-tensor-test
  (t/with-instrument `tensor/repeat-tensor
    (t/is-spec-check tensor/repeat-tensor))
  (t/with-instrument :all
    (t/is= 0.0 (tensor/repeat-tensor []))
    (t/is= 1.0 (tensor/repeat-tensor [] 1.0))
    (t/is= [] (tensor/repeat-tensor [0] 1.0))
    (t/is= [[]] (tensor/repeat-tensor [1 0] 1.0))
    (t/is= [0.0] (tensor/repeat-tensor [1]))
    (t/is= [1.0] (tensor/repeat-tensor [1] 1.0))
    (t/is= [[2]] (tensor/repeat-tensor [1] [2]))
    (t/is= [[[0 0]] [[0 0]]] (tensor/repeat-tensor [2 1] [0 0]))))

(deftest fill-tensor-test
  (t/with-instrument `tensor/fill-tensor
    (t/is-spec-check tensor/fill-tensor))
  (t/with-instrument :all
    (t/is= 1 (tensor/fill-tensor [] [1 2 3 4]))
    (t/is= [] (tensor/fill-tensor [0] [1 2 3 4]))
    (t/is= [[]] (tensor/fill-tensor [1 0] [1 2 3 4]))
    (t/is= [[1 2 3] [4 5 6]] (tensor/fill-tensor [2 3] [1 2 3 4 5 6 7 8]))
    (t/is= [[1 2 3] [4 0.0 0.0]] (tensor/fill-tensor [2 3] [1 2 3 4]))
    (t/is= [[[1 2] [3 4] [5 6]] [[7 8] [9 10] [11 12]]]
      (tensor/fill-tensor [2 3 2] [1 2 3 4 5 6 7 8 9 10 11 12 13 14]))))

(deftest rnd-tensor!-test
  (t/with-instrument `tensor/rnd-tensor!
    (t/is-spec-check tensor/rnd-tensor!))
  (t/with-instrument :all
    (random/bind-seed 0
      (t/is= 0.2961287401299688 (tensor/rnd-tensor! [])))
    (random/bind-seed 0
      (t/is= [] (tensor/rnd-tensor! [0])))
    (random/bind-seed 0
      (t/is= [[]] (tensor/rnd-tensor! [1 0])))
    (random/bind-seed 0
      (t/is= [[0.2961287401299688 0.8622994122994543 0.07868284113948965]
              [0.548683671433349 0.11620266042486127 0.5772125043785624]]
        (tensor/rnd-tensor! [2 3])))))

;;INFO
(deftest first-number-test
  (t/with-instrument `tensor/first-number
    (t/is-spec-check tensor/first-number))
  (t/with-instrument :all
    (t/is= 2 (tensor/first-number 2))
    (t/is= nil (tensor/first-number []))
    (t/is= 2 (tensor/first-number [2]))
    (t/is= 2 (tensor/first-number [[2]]))))

(deftest ecount-test
  (t/with-instrument `tensor/ecount
    (t/is-spec-check tensor/ecount))
  (t/with-instrument :all
    (t/is= 1 (tensor/ecount 0))
    (t/is= 0 (tensor/ecount [[]]))
    (t/is= 4 (tensor/ecount [[1.0 0.5] [2.0 4.0]]))))

(deftest rank-test
  (t/with-instrument `tensor/rank
    (t/is-spec-check tensor/rank))
  (t/with-instrument :all
    (t/is= 2 (tensor/rank [[2] [1]]))
    (t/is= 1 (tensor/rank []))
    (t/is= 0 (tensor/rank 1))))

(deftest shape-test
  (t/with-instrument `tensor/shape
    (t/is-spec-check tensor/shape))
  (t/with-instrument :all
    (t/is= [3] (tensor/shape [1 2 3]))
    (t/is= [2 0] (tensor/shape [[] []]))
    (t/is= [] (tensor/shape 1))
    (t/is= [2 2 1] (tensor/shape [[[1] [2]] [[3] [4]]]))))

(deftest every-kv?-test
  (t/with-instrument `tensor/every-kv?
    (t/is-spec-check tensor/every-kv?))
  (t/with-instrument :all
    (t/is-not (tensor/every-kv? (fn [indices v]
                                  (boolean (and (seq indices)
                                             (> (first indices) v))))
                [1.0 0.5]))
    (t/is (tensor/every-kv? (fn [indices v]
                              (boolean (and (seq indices)
                                         (> (first indices) -1))))
            [1.0 0.5]))
    (t/is (tensor/every-kv? (fn [indices v]
                              (boolean (and (= 2 (count indices))
                                         (> (second indices) (- v 2.1)))))
            [[1.0 0.5] [2.0 3.0]]))))

(deftest filter-kv-test
  (t/with-instrument `tensor/filter-kv
    (t/is-spec-check tensor/filter-kv))
  (t/with-instrument :all
    (t/is= [[3 4]]
      (tensor/filter-kv (fn [index tensor]
                          (odd? index))
        [[1 2] [3 4]]))))

(deftest emap-test
  (t/with-instrument `tensor/emap
    (t/is-spec-check tensor/emap))
  (t/with-instrument :all
    (t/is= [[6 6] [6 6]]
      (tensor/emap +
        1
        [2 2]
        [[3 3] [3 3]]))
    (t/is= 1.0 (tensor/emap m/sq 1.0))
    (t/is= [[1.0 0.25] [4.0 16.0]] (tensor/emap m/sq [[1.0 0.5] [2.0 4.0]]))
    (t/is= [1.0 0.25] (tensor/emap m/sq [1.0 0.5]))
    (t/is= [[1.0 0.25]] (tensor/emap m/sq [[1.0 0.5]]))
    (t/is= [[1.0] [0.25]] (tensor/emap m/sq [[1.0] [0.5]]))
    (t/is= [[3.0 1.5] [5.0 8.5]]
      (tensor/emap (fn [n1 n2 n3]
                     (+ n1 n2 n3))
        [[1.0 0.5] [2.0 4.0]]
        [[1.0 0.5] [2.0 4.0]]
        [1.0 0.5]))
    ;;notice need to check for spec with 4+ tensors
    (t/is= [[4.0 2.0] [7.0 12.5]]
      (tensor/emap (fn [& args]
                     (when (= 4 (count args))
                       (let [[n1 n2 n3 n4] args]
                         (+ n1 n2 n3 n4))))
        [[1.0 0.5] [2.0 4.0]]
        [[1.0 0.5] [2.0 4.0]]
        [1.0 0.5]
        [[1.0 0.5] [2.0 4.0]]))
    (t/is= 15.0
      (tensor/emap (fn [& args]
                     (when (= 5 (count args))
                       (let [[n1 n2 n3 n4 n5] args]
                         (+ n1 n2 n3 n4 n5))))
        1.0
        2.0
        3.0
        4.0
        5.0))
    (t/is= [[2.0 1.0] [4.0 8.0]]
      (tensor/emap (fn [n1 n2]
                     (+ n1 n2))
        [[1.0 0.5] [2.0 4.0]]
        [[1.0 0.5] [2.0 4.0]]))))

(deftest emap-kv-test
  (t/with-instrument `tensor/emap-kv
    (t/is-spec-check tensor/emap-kv))
  (t/with-instrument :all
    ;;notice need to check indices for spec
    (t/is= [1.0 1.5]
      (tensor/emap-kv (fn [indices n1]
                        (when (m/one? (count indices))
                          (let [[i] indices]
                            (+ i n1))))
        [1.0 0.5]))
    ;;notice also need to check for spec with 4+ tensors
    (t/is= [[8 10] [12 17]]
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
  (t/with-instrument `tensor/partition-recursively
    (t/is-spec-check tensor/partition-recursively))
  (t/with-instrument :all
    (t/is= nil (tensor/partition-recursively 3 []))
    (t/is= [1 2] (tensor/partition-recursively 2 [1 2]))
    (t/is= [1 2 3] (tensor/partition-recursively 3 [1 2 3]))
    (t/is= [[1 2 3]] (tensor/partition-recursively 3 [1 2 3 4]))
    (t/is= [[1 2 3] [4 5 6]] (tensor/partition-recursively 3 [1 2 3 4 5 6]))
    (t/is= [[[1 2 3] [4 5 6] [7 8 9]]] (tensor/partition-recursively 3 [1 2 3 4 5 6 7 8 9 10]))
    (t/is= [[[1 2 3] [4 5 6]] [[7 8 9] [10 11 12]]]
      (tensor/partition-recursively 2 [[1 2 3] [4 5 6] [7 8 9] [10 11 12]]))))

;;MATH
(deftest ===-test
  (t/with-instrument `tensor/===
    (t/is-spec-check tensor/===))
  (t/with-instrument :all
    (t/is (tensor/=== [[1.0 0.5] [2.0 m/nan]] [[1.0 0.5] [2.0 m/nan]]))
    (t/is (tensor/=== [[1.0 0.5] [2.0 m/nan]]
            [[1.0 0.5] [2.0 m/nan]]
            [[1.0 0.5] [2.0 m/nan]]))))

(deftest add-test
  (t/with-instrument `tensor/add
    (t/is-spec-check tensor/add))
  (t/with-instrument :all
    (t/is= [[2.0 1.0] [4.0 8.0]] (tensor/add [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
    (t/is= [2.0 1.0] (tensor/add [1.0 0.5] [1.0 0.5]))
    (t/is= [[2.0 1.0]] (tensor/add [[1.0 0.5]] [[1.0 0.5]]))
    (t/is= [1.0 2.0] (tensor/add [1.0 2.0]))
    (t/is= 0.0 (tensor/add))))

(deftest subtract-test
  (t/with-instrument `tensor/subtract
    (t/is-spec-check tensor/subtract))
  (t/with-instrument :all
    (t/is= [[0.0 0.0] [0.0 0.0]] (tensor/subtract [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
    (t/is= [0.0 0.0] (tensor/subtract [1.0 0.5] [1.0 0.5]))
    (t/is= [[-1.0 -0.5]] (tensor/subtract [[1.0 0.5]] [[1.0 0.5]] [[1.0 0.5]]))))

(deftest multiply-test
  (t/with-instrument `tensor/multiply
    (t/is-spec-check tensor/multiply))
  (t/with-instrument :all
    (t/is= [[1.0 0.25]] (tensor/multiply [[1.0 0.5]] [[1.0 0.5]]))
    (t/is= [1.0 0.125] (tensor/multiply [1.0 0.5] [1.0 0.5] [1.0 0.5]))
    (t/is= [[1.0 0.5]] (tensor/multiply [[1.0 0.5]]))
    (t/is= 1.0 (tensor/multiply))))

(deftest divide-test
  (t/with-instrument `tensor/divide
    (t/is-spec-check tensor/divide))
  (t/with-instrument :all
    (t/is= [[1.0 1.0]] (tensor/divide [[1.0 0.5]] [[1.0 0.5]]))
    (t/is= [[3.0 6.0]] (tensor/divide 3.0 [[1.0 0.5]]))
    (t/is= [1.0 2.0] (tensor/divide [1.0 0.5] [1.0 0.5] [1.0 0.5]))
    (t/is= [[1.0 2.0]] (tensor/divide [[1.0 0.5]]))))

(deftest average-test
  (t/with-instrument `tensor/average
    (t/is-spec-check tensor/average))
  (t/with-instrument :all
    (t/is= 1.875 (tensor/average [[1.0 0.5] [2.0 4.0]]))
    (t/is= 0.75 (tensor/average [[1.0 0.5]]))
    (t/is= 0.75 (tensor/average [1.0 0.5]))))

(deftest norm1-test
  (t/with-instrument `tensor/norm1
    (t/is-spec-check tensor/norm1))
  (t/with-instrument :all
    (t/is= 7.5 (tensor/norm1 [[1.0 0.5] [2.0 4.0]]))
    (t/is= 1.5 (tensor/norm1 [[1.0 0.5]]))
    (t/is= 1.5 (tensor/norm1 [1.0 0.5]))))

(deftest norm-test
  (t/with-instrument `tensor/norm
    (t/is-spec-check tensor/norm))
  (t/with-instrument :all
    (t/is= 4.6097722286464435 (tensor/norm [[1.0 0.5] [2.0 4.0]]))
    (t/is= 1.118033988749895 (tensor/norm [[1.0 0.5]]))
    (t/is= 1.118033988749895 (tensor/norm [1.0 0.5]))))

(deftest norm-p-test
  (t/with-instrument `tensor/norm-p
    (t/is-spec-check tensor/norm-p))
  (t/with-instrument :all
    (t/is= 7.5 (tensor/norm-p [[1.0 0.5] [2.0 4.0]] 1.0))
    (t/is= 4.118720689718815 (tensor/norm-p [[1.0 0.5] [2.0 4.0]] 3.4))
    (t/is= 1.1049918154523823 (tensor/norm-p [[1.0 0.5]] 2.1))
    (t/is= 1.1049918154523823 (tensor/norm-p [1.0 0.5] 2.1))
    (t/is= 1.5 (tensor/norm-p [1.0 0.5] 1.0))))

(deftest norm-inf-test
  (t/with-instrument `tensor/norm-inf
    (t/is-spec-check tensor/norm-inf))
  (t/with-instrument :all
    (t/is= 4.0 (tensor/norm-inf [[1.0 0.5] [2.0 4.0]]))
    (t/is= 1.0 (tensor/norm-inf [[1.0 0.5]]))
    (t/is= 1.0 (tensor/norm-inf [1.0 0.5]))
    (t/is= 5.0 (tensor/norm-inf [-3 4 -5]))
    (t/is= 3.0 (tensor/norm-inf -3))))

(deftest normalize1-test
  (t/with-instrument `tensor/normalize1
    (t/is-spec-check tensor/normalize1))
  (t/with-instrument :all
    (t/is= [[0.13333333333333333 0.06666666666666667] [0.26666666666666666 0.5333333333333333]]
      (tensor/normalize1 [[1.0 0.5] [2.0 4.0]]))
    (t/is= 0.9999999999999998
      (apply + (tensor/normalize1
                 [2.1242141025912059120591205912509510259021590125
                  1.2398578935713571650983759872398572983
                  2.1351365731650631856238056287035
                  3.235729375209357203975])))
    (t/is= [[0.13333333333333333 0.06666666666666667]
            [0.26666666666666666 0.5333333333333333]]
      (tensor/normalize1 [[1.0 0.5] [2.0 4.0]]))
    (t/is= [0.6666666666666666 0.3333333333333333] (tensor/normalize1 [1.0 0.5]))
    (t/is= [0.6666666666666666 0.3333333333333333] (tensor/normalize1 [1.0 0.5]))))

(deftest normalize-test
  (t/with-instrument `tensor/normalize
    (t/is-spec-check tensor/normalize))
  (t/with-instrument :all
    (t/is= [[0.21693045781865616 0.10846522890932808] [0.4338609156373123 0.8677218312746247]]
      (tensor/normalize [[1.0 0.5] [2.0 4.0]]))
    (t/is= [[0.21693045781865616 0.10846522890932808] [0.4338609156373123 0.8677218312746247]]
      (tensor/normalize [[1.0 0.5] [2.0 4.0]]))
    (t/is= [0.8944271909999159 0.4472135954999579] (tensor/normalize [1.0 0.5]))
    (t/is= [0.8944271909999159 0.4472135954999579] (tensor/normalize [1.0 0.5]))))

(deftest normalize-p-test
  (t/with-instrument `tensor/normalize-p
    (t/is-spec-check tensor/normalize-p))
  (t/with-instrument :all
    (t/is= [[0.13333333333333333 0.06666666666666667]
            [0.26666666666666666 0.5333333333333333]]
      (tensor/normalize-p [[1.0 0.5] [2.0 4.0]] 1.0))
    (t/is= [[0.24279383705144375 0.12139691852572188]
            [0.4855876741028875 0.971175348205775]]
      (tensor/normalize-p [[1.0 0.5] [2.0 4.0]] 3.4))
    (t/is= [[0.9049840786292169 0.45249203931460846]]
      (tensor/normalize-p [[1.0 0.5]] 2.1))
    (t/is= [0.9049840786292169 0.45249203931460846]
      (tensor/normalize-p [1.0 0.5] 2.1))
    (t/is= [0.6666666666666666 0.3333333333333333]
      (tensor/normalize-p [1.0 0.5] 1.0))))

(deftest normalize-inf-test
  (t/with-instrument `tensor/normalize-inf
    (t/is-spec-check tensor/normalize-inf))
  (t/with-instrument :all
    (t/is= [[0.25 0.125] [0.5 1.0]]
      (tensor/normalize-inf [[1.0 0.5] [2.0 4.0]]))
    (t/is= [[1.0 0.5]]
      (tensor/normalize-inf [[1.0 0.5]]))
    (t/is= [1.0 0.5]
      (tensor/normalize-inf [1.0 0.5]))
    (t/is= [-0.6 0.8 -1.0]
      (tensor/normalize-inf [-3 4 -5]))
    (t/is= [0 0]
      (tensor/normalize-inf [0 0]))))

(deftest inner-product-test
  (t/with-instrument `tensor/inner-product
    (t/is-spec-check tensor/inner-product))
  (t/with-instrument :all
    (t/is= [48.0 54.0 60.0] (tensor/inner-product [1 2 3] [[4 5 6] [7 8 9] [10 11 12]]))))

;;ROUNDING
(deftest roughly?-test
  (t/with-instrument `tensor/roughly?
    (t/is-spec-check tensor/roughly?))
  (t/with-instrument :all
    (t/is (tensor/roughly? 1 1.01 0.05))
    (t/is-not (tensor/roughly? 1 1.01 0.005))
    (t/is (tensor/roughly? [1 2] [1.01 2] 0.05))
    (t/is-not (tensor/roughly? [1 2] [1.01 2] 0.005))
    (t/is (tensor/roughly? [[1 2] [3 4]] [[1.01 2] [3 4]] 0.05))
    (t/is-not (tensor/roughly? [[1 2] [3 4]] [[1.01 2] [3 4]] 0.005))))

(deftest roughly-distinct-test
  (t/with-instrument `tensor/roughly-distinct
    (t/is-spec-check tensor/roughly-distinct))
  (t/with-instrument :all
    (t/is= [1 1.01] (tensor/roughly-distinct [1 1.01 1.001] 0.005))
    (t/is= [[1 1] [1.01 1.01]] (tensor/roughly-distinct [[1 1] [1.01 1.01] [1.001 1.001]] 0.005))
    (t/is= [[1 1.01]]
      (tensor/roughly-distinct [[1 1.01] [1.01 1] [1.01 1.01] [1.001 1.001]] 0.05))))

;;REDUCTION
(deftest flatten-tensor-test
  (t/with-instrument `tensor/flatten-tensor
    (t/is-spec-check tensor/flatten-tensor))
  (t/with-instrument :all
    (t/is= [5] (tensor/flatten-tensor 5))
    (t/is= [] (tensor/flatten-tensor []))
    (t/is= [1 2 3] (tensor/flatten-tensor [1 2 3]))
    (t/is= [1 2 3 4] (tensor/flatten-tensor [[1 2] [3 4]]))
    (t/is= [1 2 3 4 5 6 7 8]
      (tensor/flatten-tensor [[[1 2] [3 4]] [[5 6] [7 8]]]))))

(deftest sum-test
  (t/with-instrument `tensor/sum
    (t/is-spec-check tensor/sum))
  (t/with-instrument :all
    (t/is= 5.0 (tensor/sum 5))
    (t/is= 0.0 (tensor/sum []))
    (t/is= 6.0 (tensor/sum [1 2 3]))
    (t/is= 10.0 (tensor/sum [[1 2] [3 4]]))
    (t/is= 36.0 (tensor/sum [[[1 2] [3 4]] [[5 6] [7 8]]]))))

(deftest product-test
  (t/with-instrument `tensor/product
    (t/is-spec-check tensor/product))
  (t/with-instrument :all
    (t/is= 5.0 (tensor/product 5))
    (t/is= 1.0 (tensor/product []))
    (t/is= 6.0 (tensor/product [1 2 3]))
    (t/is= 24.0 (tensor/product [[1 2] [3 4]]))
    (t/is= 40320.0 (tensor/product [[[1 2] [3 4]] [[5 6] [7 8]]]))))

;;SHAPE MANIPULATION
(deftest reshape-test
  (t/with-instrument `tensor/reshape
    (t/is-spec-check tensor/reshape))
  (t/with-instrument :all
    (t/is= [[1 2] [3 4]] (tensor/reshape [1 2 3 4] [2 2]))
    (t/is= [1 2 3 4] (tensor/reshape [[1 2] [3 4]] [4]))
    (t/is= [[1 2] [3 4] [5 6]] (tensor/reshape [[1 2 3] [4 5 6]] [3 2]))
    (t/is= [[[1 2] [3 4]] [[5 6] [7 8]]]
      (tensor/reshape [1 2 3 4 5 6 7 8] [2 2 2]))
    (t/is= nil (tensor/reshape [1 2 3] [2 2]))
    (t/is= 5 (tensor/reshape 5 []))
    (t/is= [5] (tensor/reshape 5 [1]))))

(deftest transpose-test
  (t/with-instrument `tensor/transpose
    (t/is-spec-check tensor/transpose {:num-tests 50}))
  ;; Use specific instrumentation since :all would instrument compute-tensor's
  ;; fspec which fails when spec-checking the inner lambda function
  (t/with-instrument `tensor/transpose
    ;; Scalars unchanged
    (t/is= 5 (tensor/transpose 5))
    ;; 1D unchanged
    (t/is= [1 2 3] (tensor/transpose [1 2 3]))
    ;; 2D standard transpose
    (t/is= [[]] (tensor/transpose [[]]))
    (t/is= [[1 4] [2 5] [3 6]] (tensor/transpose [[1 2 3] [4 5 6]]))
    (t/is= [[1 2] [3 4]] (tensor/transpose (tensor/transpose [[1 2] [3 4]])))
    ;; 3D default (reverse axes)
    (let [t3 [[[1 2] [3 4]] [[5 6] [7 8]]]]
      (t/is= [[[1 5] [3 7]] [[2 6] [4 8]]]
        (tensor/transpose t3)))
    ;; 3D with explicit axes
    (let [t3 [[[1 2] [3 4]] [[5 6] [7 8]]]]
      (t/is= [[[1 5] [3 7]] [[2 6] [4 8]]]
        (tensor/transpose t3 [2 1 0]))
      ;; Swap first two axes only
      (t/is= [[[1 2] [5 6]] [[3 4] [7 8]]]
        (tensor/transpose t3 [1 0 2])))
    ;; Invalid axes
    (t/is= nil (tensor/transpose [[1 2] [3 4]] [0]))
    (t/is= nil (tensor/transpose [[1 2] [3 4]] [0 0]))
    (t/is= nil (tensor/transpose [[1 2] [3 4]] [0 2]))))
