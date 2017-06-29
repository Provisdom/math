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
  (is= [1.0 2.0] (tensor/compute-tensor [2] inc))           ;;;

  )

(deftest constant-tensor-test
  (is= [0.0] (tensor/constant-tensor [1]))
  (is= [1.0] (tensor/constant-tensor [1] 1.0))

  )

(deftest tensor-test
  (tensor?-test)
  (to-tensor-test)
  (compute-tensor-test)
  (constant-tensor-test))

;(defspec-test test-tensor? `tensor/tensor?)
;(defspec-test test-to-tensor `tensor/to-tensor)
;(defspec-test test-compute-tensor `tensor/compute-tensor)
;(defspec-test test-constant-tensor `tensor/constant-tensor)

#_(ost/unstrument)