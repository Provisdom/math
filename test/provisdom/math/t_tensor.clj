(ns provisdom.math.t-tensor
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.core :as m]
    [provisdom.math.tensor :as tensor]
    [provisdom.math.random2 :as random]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

(ost/instrument)

(set! *warn-on-reflection* true)

;;TYPES
(deftest tensor?-test
  (is (tensor/tensor? []))
  (is-not (tensor/tensor? "A"))
  (is-not (tensor/tensor? ["A"]))
  (is (tensor/tensor? [[] []]))                             ;notice tensors can have multiple zero dims
  (is (tensor/tensor? [[]]))
  (is-not (tensor/tensor? [[2] 2]))
  (is-not (tensor/tensor? [2 [2]]))
  (is (tensor/tensor? [[[m/nan]]]))
  (is-not (tensor/tensor? '()))
  (is (tensor/tensor? 1)))

;(defspec-test test-tensor? `tensor/tensor?)                 ;slowish

;;CONSTRUCTORS
(deftest to-tensor-test
  (is= [] (tensor/to-tensor '()))
  (is= nil (tensor/to-tensor "A"))
  (is= [] (tensor/to-tensor []))
  (is= nil (tensor/to-tensor [[2] 3]))
  (is= nil (tensor/to-tensor [2 [3]]))
  (is= nil (tensor/to-tensor [[2] [3 4]]))
  (is= [[2 3]] (tensor/to-tensor '((2 3)))))

(deftest compute-tensor-test
  (is= -2 (tensor/compute-tensor [] (fn [[n]] (if n n -2))))
  (is= [] (tensor/compute-tensor [0] (fn [[n]] (if n n -2))))
  (is= [[]] (tensor/compute-tensor [1 0] (fn [[_ n2]] (if n2 n2 -2))))
  (is= [0 1] (tensor/compute-tensor [2] (fn [[n]] (if n n -2))))
  (is= [[0 1 2] [0 1 2]] (tensor/compute-tensor [2 3] (fn [[n1 n2]] (if n1 (if n2 n2 -1) -2)))))

(deftest repeat-tensor-test
  (is= 0.0 (tensor/repeat-tensor []))
  (is= 1.0 (tensor/repeat-tensor [] 1.0))
  (is= [] (tensor/repeat-tensor [0] 1.0))
  (is= [[]] (tensor/repeat-tensor [1 0] 1.0))
  (is= [0.0] (tensor/repeat-tensor [1]))
  (is= [1.0] (tensor/repeat-tensor [1] 1.0))
  (is= [[2]] (tensor/repeat-tensor [1] [2])))

(deftest fill-tensor-test
  (is= 1 (tensor/fill-tensor [] [1 2 3 4]))
  (is= [] (tensor/fill-tensor [0] [1 2 3 4]))
  (is= [[]] (tensor/fill-tensor [1 0] [1 2 3 4]))
  (is= [[1 2 3] [4 5 6]] (tensor/fill-tensor [2 3] [1 2 3 4 5 6 7 8]))
  (is= [[1 2 3] [4 0.0 0.0]] (tensor/fill-tensor [2 3] [1 2 3 4]))
  (is= [[[1 2] [3 4] [5 6]] [[7 8] [9 10] [11 12]]]
       (tensor/fill-tensor [2 3 2] [1 2 3 4 5 6 7 8 9 10 11 12 13 14])))

(deftest rnd-tensor!-test
  (random/bind-seed 0
    (is= 0.8833108082136426 (tensor/rnd-tensor! [])))
  (random/bind-seed 0
    (is= [] (tensor/rnd-tensor! [0])))
  (random/bind-seed 0
    (is= [[]] (tensor/rnd-tensor! [1 0])))
  (random/bind-seed 0
    (is= [[0.8833108082136426 0.026433771592597743 0.10634669156721244]
          [0.17386786595968284 0.24568894884013137 0.39646797562881353]]
         (tensor/rnd-tensor! [2 3]))))

;(defspec-test test-to-tensor `tensor/to-tensor) ;slowish
;(defspec-test test-compute-tensor `tensor/compute-tensor) ;slowish
;(defspec-test test-repeat-tensor `tensor/repeat-tensor) ;slow
(defspec-test test-fill-tensor `tensor/fill-tensor)
(defspec-test test-rnd-tensor! `tensor/rnd-tensor!)

;;INFO
(deftest ecount-test
  (is= 1 (tensor/ecount 0))
  (is= 0 (tensor/ecount [[]]))
  (is= 4 (tensor/ecount [[1.0 0.5] [2.0 4.0]])))

(deftest dimensionality-test
  (is= 2 (tensor/dimensionality [[2] [1]]))
  (is= 1 (tensor/dimensionality []))
  (is= 0 (tensor/dimensionality 1)))

(deftest shape-test
  (is= [3] (tensor/shape [1 2 3]))
  (is= [2 0] (tensor/shape [[] []]))
  (is= [] (tensor/shape 1))
  (is= [2 2 1] (tensor/shape [[[1] [2]] [[3] [4]]])))

(deftest every-kv?-test
  (is-not (tensor/every-kv? (fn [[n] v] (> n v)) [1.0 0.5]))
  (is (tensor/every-kv? (fn [[n] v] (> n -1)) [1.0 0.5]))
  (is (tensor/every-kv? (fn [[n1 n2] v] (> n2 (- v 2.1))) [[1.0 0.5] [2.0 3.0]])))

(deftest filter-kv-test
  (is= [[3 4]] (tensor/filter-kv (fn [index tensor] (odd? index)) [[1 2] [3 4]])))

(defspec-test test-ecount `tensor/ecount)
(defspec-test test-dimensionality `tensor/dimensionality)
(defspec-test test-shape `tensor/shape)
;(defspec-test test-every-kv? `tensor/every-kv?)             ;fspec issues
;(defspec-test test-filter-kv `tensor/filter-kv)             ;slow

(deftest emap-test
  (is= [[6 6] [6 6]] (tensor/emap + 1 [2 2] [[3 3] [3 3]]))
  (is= 1.0 (tensor/emap m/sq 1.0))
  (is= [[1.0 0.25] [4.0 16.0]] (tensor/emap m/sq [[1.0 0.5] [2.0 4.0]]))
  (is= [1.0 0.25] (tensor/emap m/sq [1.0 0.5]))
  (is= [[1.0 0.25]] (tensor/emap m/sq [[1.0 0.5]]))
  (is= [[1.0] [0.25]] (tensor/emap m/sq [[1.0] [0.5]]))
  (is= [[3.0 1.5] [5.0 8.5]] (tensor/emap #(+ % %2 %3) [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]] [1.0 0.5]))
  (is= [[4.0 2.0] [7.0 12.5]]
       (tensor/emap #(+ % %2 %3 %4) [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]] [1.0 0.5] [[1.0 0.5] [2.0 4.0]]))
  (is= [[2.0 1.0] [4.0 8.0]] (tensor/emap #(+ % %2) [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]])))

(deftest emap-kv-test
  (is= [[7 10] [10 13]] (tensor/emap-kv (fn [[n1 n2] v1 v2 v3] (+ n1 n2 v1 v2 v3)) 1 [2 3] [[4 5] [6 7]])))

(deftest manipulation-tests
  (emap-test)
  (emap-kv-test))

;(defspec-test test-emap `tensor/emap) ;fspec issues
;(defspec-test test-emap-kv `tensor/emap-kv) ;fspec issues

;;MATH
(deftest ===-test
  (is (tensor/=== [[1.0 0.5] [2.0 m/nan]] [[1.0 0.5] [2.0 m/nan]]))
  (is (tensor/=== [[1.0 0.5] [2.0 m/nan]] [[1.0 0.5] [2.0 m/nan]] [[1.0 0.5] [2.0 m/nan]])))

(deftest add-test
  (is= [[2.0 1.0] [4.0 8.0]] (tensor/add [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
  (is= [2.0 1.0] (tensor/add [1.0 0.5] [1.0 0.5]))
  (is= [[2.0 1.0]] (tensor/add [[1.0 0.5]] [[1.0 0.5]]))
  (is= [1.0 2.0] (tensor/add [1.0 2.0]))
  (is= 0.0 (tensor/add)))

(deftest subtract-test
  (is= [[0.0 0.0] [0.0 0.0]] (tensor/subtract [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]]))
  (is= [0.0 0.0] (tensor/subtract [1.0 0.5] [1.0 0.5]))
  (is= [[-1.0 -0.5]] (tensor/subtract [[1.0 0.5]] [[1.0 0.5]] [[1.0 0.5]])))

(deftest multiply-test
  (is= [[1.0 0.25]] (tensor/multiply [[1.0 0.5]] [[1.0 0.5]]))
  (is= [1.0 0.125] (tensor/multiply [1.0 0.5] [1.0 0.5] [1.0 0.5]))
  (is= [[1.0 0.5]] (tensor/multiply [[1.0 0.5]]))
  (is= 1.0 (tensor/multiply)))

(deftest divide-test
  (is= [[1.0 1.0]] (tensor/divide [[1.0 0.5]] [[1.0 0.5]]))
  (is= [[3.0 6.0]] (tensor/divide 3.0 [[1.0 0.5]]))
  (is= [1.0 2.0] (tensor/divide [1.0 0.5] [1.0 0.5] [1.0 0.5]))
  (is= [[1.0 2.0]] (tensor/divide [[1.0 0.5]])))

(deftest average-test
  (is= 1.875 (tensor/average [[1.0 0.5] [2.0 4.0]]))
  (is= 0.75 (tensor/average [[1.0 0.5]]))
  (is= 0.75 (tensor/average [1.0 0.5])))

(deftest norm1-test
  (is= 7.5 (tensor/norm1 [[1.0 0.5] [2.0 4.0]]))
  (is= 1.5 (tensor/norm1 [[1.0 0.5]]))
  (is= 1.5 (tensor/norm1 [1.0 0.5])))

(deftest norm-test
  (is= 4.6097722286464435 (tensor/norm [[1.0 0.5] [2.0 4.0]]))
  (is= 1.118033988749895 (tensor/norm [[1.0 0.5]]))
  (is= 1.118033988749895 (tensor/norm [1.0 0.5])))

(deftest norm-p-test
  (is= 7.5 (tensor/norm-p [[1.0 0.5] [2.0 4.0]] 1.0))
  (is= 4.118720689718815 (tensor/norm-p [[1.0 0.5] [2.0 4.0]] 3.4))
  (is= 1.1049918154523823 (tensor/norm-p [[1.0 0.5]] 2.1))
  (is= 1.1049918154523823 (tensor/norm-p [1.0 0.5] 2.1))
  (is= 1.5 (tensor/norm-p [1.0 0.5] 1.0)))

(deftest normalize1-test
  (is= [[0.13333333333333333 0.06666666666666667] [0.26666666666666666 0.5333333333333333]]
       (tensor/normalize1 [[1.0 0.5] [2.0 4.0]]))
  (is= 0.9999999999999998
       (apply + (tensor/normalize1 [2.1242141025912059120591205912509510259021590125 1.2398578935713571650983759872398572983
                                    2.1351365731650631856238056287035 3.235729375209357203975])))
  (is= [[0.13333333333333333 0.06666666666666667] [0.26666666666666666 0.5333333333333333]]
       (tensor/normalize1 [[1.0 0.5] [2.0 4.0]]))
  (is= [0.6666666666666666 0.3333333333333333] (tensor/normalize1 [1.0 0.5]))
  (is= [0.6666666666666666 0.3333333333333333] (tensor/normalize1 [1.0 0.5])))

(deftest normalize-test
  (is= [[0.21693045781865616 0.10846522890932808] [0.4338609156373123 0.8677218312746247]]
       (tensor/normalize [[1.0 0.5] [2.0 4.0]]))
  (is= [[0.21693045781865616 0.10846522890932808] [0.4338609156373123 0.8677218312746247]]
       (tensor/normalize [[1.0 0.5] [2.0 4.0]]))
  (is= [0.8944271909999159 0.4472135954999579] (tensor/normalize [1.0 0.5]))
  (is= [0.8944271909999159 0.4472135954999579] (tensor/normalize [1.0 0.5])))

(deftest normalize-p-test
  (is= [[0.13333333333333333 0.06666666666666667] [0.26666666666666666 0.5333333333333333]]
       (tensor/normalize-p [[1.0 0.5] [2.0 4.0]] 1.0))
  (is= [[0.24279383705144375 0.12139691852572188] [0.4855876741028875 0.971175348205775]]
       (tensor/normalize-p [[1.0 0.5] [2.0 4.0]] 3.4))
  (is= [[0.9049840786292169 0.45249203931460846]] (tensor/normalize-p [[1.0 0.5]] 2.1))
  (is= [0.9049840786292169 0.45249203931460846] (tensor/normalize-p [1.0 0.5] 2.1))
  (is= [0.6666666666666666 0.3333333333333333] (tensor/normalize-p [1.0 0.5] 1.0)))

(deftest inner-product-test
  (is= [48.0 54.0 60.0] (tensor/inner-product [1 2 3] [[4 5 6] [7 8 9] [10 11 12]])))

;(defspec-test test-=== `tensor/===) ;slow
;(defspec-test test-add `tensor/add) ;slow
;(defspec-test test-subtract `tensor/subtract) ;slow
;(defspec-test test-multiply `tensor/multiply) ;slow
;(defspec-test test-divide `tensor/divide) ;slow
(defspec-test test-average 'tensor/average)
(defspec-test test-norm1 `tensor/norm1)
(defspec-test test-norm `tensor/norm)
(defspec-test test-norm-p `tensor/norm-p)
;(defspec-test test-normalize1 `tensor/normalize1) ;slow
;(defspec-test test-normalize `tensor/normalize) ;slow
;(defspec-test test-normalize-p `tensor/normalize-p) ;slow
;(defspec-test test-inner-product `tensor/inner-product) ;slow

;;ROUNDING
(deftest roughly?-test
  (is (tensor/roughly? 1 1.01 0.05))
  (is-not (tensor/roughly? 1 1.01 0.005))
  (is (tensor/roughly? [1 2] [1.01 2] 0.05))
  (is-not (tensor/roughly? [1 2] [1.01 2] 0.005))
  (is (tensor/roughly? [[1 2] [3 4]] [[1.01 2] [3 4]] 0.05))
  (is-not (tensor/roughly? [[1 2] [3 4]] [[1.01 2] [3 4]] 0.005)))

(deftest roughly-distinct-test
  (is= [1 1.01] (tensor/roughly-distinct [1 1.01 1.001] 0.005))
  (is= [[1 1] [1.01 1.01]] (tensor/roughly-distinct [[1 1] [1.01 1.01] [1.001 1.001]] 0.005))
  (is= [[1 1.01]] (tensor/roughly-distinct [[1 1.01] [1.01 1] [1.01 1.01] [1.001 1.001]] 0.05)))

(defspec-test test-roughly? `tensor/roughly?)
(defspec-test test-roughly-distinct `tensor/roughly-distinct)

#_(ost/unstrument)