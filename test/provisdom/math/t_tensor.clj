(ns provisdom.math.t-tensor
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.core :as m]
            [provisdom.math.tensor :as tensor]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]))

(ost/instrument)

(deftest tensor?-test
  (is (tensor/tensor? []))
  (is-not (tensor/tensor? "A"))
  (is-not (tensor/tensor? ["A"]))
  (is (tensor/tensor? [[]]))
  (is-not (tensor/tensor? [[2] 2]))
  (is-not (tensor/tensor? [2 [2]]))
  (is (tensor/tensor? [[[m/nan]]]))
  (is-not (tensor/tensor? '()))
  (is (tensor/tensor? 1)))

(deftest to-tensor-test
  (is= [] (tensor/to-tensor '()))
  (is= nil (tensor/to-tensor "A"))
  (is= [] (tensor/to-tensor []))
  (is= nil (tensor/to-tensor [[2] 3]))
  (is= nil (tensor/to-tensor [2 [3]]))
  (is= nil (tensor/to-tensor [[2] [3 4]]))
  (is= [[2 3]] (tensor/to-tensor '((2 3)))))

(deftest compute-tensor-test
  (is= [1.0 2.0] (tensor/compute-tensor [2] inc)))

(deftest repeat-tensor-test
  (is= [0.0] (tensor/repeat-tensor [1]))
  (is= [1.0] (tensor/repeat-tensor [1] 1.0))
  (is= [[2]] (tensor/repeat-tensor [1] [2])))

(deftest dimensionality-test
  (is= 2 (tensor/dimensionality [[2] [1]]))
  (is= 1 (tensor/dimensionality []))
  (is= 0 (tensor/dimensionality 1)))

(deftest shape-test
  (is= [3] (tensor/shape [1 2 3]))
  (is= [2 0] (tensor/shape [[] []]))
  (is= [0] (tensor/shape 1))
  (is= [2 2 1] (tensor/shape [[[1] [2]] [[3] [4]]])))

(deftest every-kv?-test
  (is-not (tensor/every-kv? #(> % %2) [1.0 0.5])))

(deftest emap-test
  (is= 5.0 (tensor/emap + 1 [2 2] [[3 3] [3 3]]))
  (is= 5.0 (tensor/emap #(% 2.0 3.0) +))
  (is= [5.0 -1.0] (tensor/emap #(% 2.0 3.0) [+ -]))
  (is= 1.0 (tensor/emap m/sq 1.0))
  (is= [[1.0 0.25] [4.0 16.0]] (tensor/emap m/sq [[1.0 0.5] [2.0 4.0]]))
  (is= [1.0 0.25] (tensor/emap m/sq [1.0 0.5]))
  (is= [[1.0 0.25]] (tensor/emap m/sq [[1.0 0.5]]))
  (is= [[1.0] [0.25]] (tensor/emap m/sq [[1.0] [0.5]]))
  (is= [[3.0 1.5] [6.0 12.0]] (tensor/emap #(+ % %2 %3) [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]] [1.0 0.5]))
  (is= [[4.0 2.0] [8.0 16.0]]
       (tensor/emap #(+ % %2 %3 %4) [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]] [1.0 0.5] [[1.0 0.5] [2.0 4.0]]))
  (is= [[2.0 1.0] [4.0 8.0]] (tensor/emap #(+ % %2) [[1.0 0.5] [2.0 4.0]] [[1.0 0.5] [2.0 4.0]])))

(deftest emap-kv-test
  (is= 5.0 (tensor/emap-kv #(+ (second %1) %2 %3 %4) 1 [2 2] [[3 3] [3 3]])))

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

(deftest ecount-test
  (is= 4 (tensor/ecount [[1.0 0.5] [2.0 4.0]])))

(deftest norm1-test
  (is= 7.5 (tensor/norm1 [[1.0 0.5] [2.0 4.0]]))
  (is= 1.5 (tensor/norm1 [[1.0 0.5]]))
  (is= 1.5 (tensor/norm1 [1.0 0.5])))

(deftest norm-test
  (is= 4.6097722286464435 (tensor/norm [[1.0 0.5] [2.0 4.0]]))
  (is= 1.118033988749895 (tensor/norm [[1.0 0.5]]))
  (is= 1.118033988749895 (tensor/norm [1.0 0.5])))

(deftest norm-p-test
  (is= 7.5 (tensor/norm-p ap 1.0))
  (is= 4.118720689718815 (tensor/norm-p [[1.0 0.5] [2.0 4.0]] 3.4))
  (is= 1.1049918154523823 (tensor/norm-p [[1.0 0.5]] 2.1))
  (is= 1.1049918154523823 (tensor/norm-p [1.0 0.5] 2.1))
  (is= 1.5 (tensor/norm-p [1.0 0.5] 1.0)))

(deftest normalize1-test
  (is= [[0.13333333333333333 0.06666666666666667] [0.26666666666666666 0.5333333333333333]]
       (tensor/normalize1 [[1.0 0.5] [2.0 4.0]]))
  (is= 1.0
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

(deftest tensor-test
  (tensor?-test)
  (to-tensor-test)
  (compute-tensor-test)
  (repeat-tensor-test)
  (dimensionality-test)
  (shape-test)
  (every-kv?-test)
  (emap-test)
  (emap-kv-test)
  (add-test)
  (subtract-test)
  (multiply-test)
  (divide-test)
  (ecount-test)
  (norm1-test)
  (norm-test)
  (norm-p-test)
  (normalize1-test)
  (normalize-test)
  (normalize-p-test)
  (roughly?-test)
  (roughly-distinct-test))

;(defspec-test test-tensor? `tensor/tensor?)
;(defspec-test test-to-tensor `tensor/to-tensor)
;(defspec-test test-compute-tensor `tensor/compute-tensor)
;(defspec-test test-repeat-tensor `tensor/repeat-tensor)
;(defspec-test test-dimensionality `tensor/dimensionality)
;(defspec-test test-shape `tensor/shape)
;(defspec-test test-every-kv? `tensor/every-kv?)
;(defspec-test test-emap `tensor/emap)
;(defspec-test test-emap-kv `tensor/emap-kv)
;(defspec-test test-add `tensor/add)
;(defspec-test test-subtract `tensor/subtract)
;(defspec-test test-multiply `tensor/multiply)
;(defspec-test test-divide `tensor/divide)
;(defspec-test test-ecount `tensor/ecount)
;(defspec-test test-norm1 `tensor/norm1)
;(defspec-test test-norm `tensor/norm)
;(defspec-test test-norm-p `tensor/norm-p)
;(defspec-test test-normalize1 `tensor/normalize1)
;(defspec-test test-normalize `tensor/normalize)
;(defspec-test test-normalize-p `tensor/normalize-p)
;(defspec-test test-roughly? `tensor/roughly?)
;(defspec-test test-roughly-distinct `tensor/roughly-distinct)

#_(ost/unstrument)