(ns provisdom.math.t-vector
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.vector :as vector]
            [provisdom.math.random2 :as random]
            [provisdom.math.core :as m]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]))

(ost/instrument)

(deftest numbers?-test
  (is-not (vector/numbers? 1))
  (is (vector/numbers? '(1)))
  (is (vector/numbers? []))
  (is (vector/numbers? [2 3]))
  (is-not (vector/numbers? [[2]])))

(deftest vector?-test
  (is-not (vector/vector? 1))
  (is-not (vector/vector? '(1)))
  (is (vector/vector? []))
  (is (vector/vector? [2 3]))
  (is-not (vector/vector? [[2]])))

(deftest type-tests
  (numbers?-test)
  (vector?-test))

;(defspec-test test-numbers? `vector/numbers?) ;slow-ish
;(defspec-test test-vector? `vector/vector?) ;slow-ish

(deftest to-vector-test
  (is= [1] (vector/to-vector 1))
  (is= [1] (vector/to-vector '(1)))
  (is= [] (vector/to-vector []))
  (is= [2 3] (vector/to-vector [2 3]))
  (is= [2 1 4 5] (vector/to-vector [[2 1] [4 5]]))
  (is= nil (vector/to-vector nil)))

(deftest compute-vector-test
  (is= [0] (vector/compute-vector 1 identity))
  (is= [0 1] (vector/compute-vector 2 identity))
  (is= [0 3 6] (vector/compute-vector 3 (partial * 3)))
  (is= [2.0 3.0 4.0] (vector/compute-vector 3 #(+ 2.0 %))))

(deftest rnd-vector-test
  (random/bind-seed 0
    (is= [] (vector/rnd-vector 0)))
  (random/bind-seed 0
    (is= [0.8833108082136426] (vector/rnd-vector 1)))
  (random/bind-seed 0
    (is= [0.8833108082136426 0.026433771592597743] (vector/rnd-vector 2))))

(deftest sparse->vector-test
  (is= [3.0 0.0 4.0 0.0] (vector/sparse->vector '([2 4.0] [0 3.0]) (vec (repeat 4 0.0))))
  (is= [0.0 0.0] (vector/sparse->vector [] (vec (repeat 2 0.0))))
  (is= [1.0 1.0 2.0 1.0] (vector/sparse->vector [[2 2.0] [4 4.0]] (vec (repeat 4 1.0)))))

(deftest constructor-tests
  (to-vector-test)
  (compute-vector-test)
  (rnd-vector-test)
  (sparse->vector-test))

(defspec-test test-to-vector `vector/to-vector)
(defspec-test test-compute-vector `vector/compute-vector)
(defspec-test test-rnd-vector `vector/rnd-vector)
(defspec-test test-sparse->vector `vector/sparse->vector)

(deftest filter-kv-test
  (is= [] (vector/filter-kv (fn [k v] (> v k)) []))
  (is= [] (vector/filter-kv (fn [k v] (> v k)) [0]))
  (is= [2 4 6] (vector/filter-kv (fn [k v] (> v k)) [0 2 4 6])))

(deftest some-kv-test
  (is= nil (vector/some-kv (fn [k v] (> v k)) []))
  (is= nil (vector/some-kv (fn [k v] (> v k)) [0]))
  (is= 2 (vector/some-kv (fn [k v] (> v k)) [0 2 4 6])))

(deftest info-tests
  (filter-kv-test)
  (some-kv-test))

(defspec-test test-filter-kv `vector/filter-kv)
(defspec-test test-some-kv `vector/some-kv)

(deftest insertv-test
  (is= [9 0 2 4 6] (vector/insertv [0 2 4 6] 0 9))
  (is= [0 9 2 4 6] (vector/insertv [0 2 4 6] 1 9))
  (is= [0 2 4 9 6] (vector/insertv [0 2 4 6] 3 9))
  (is= [0 2 4 6 9] (vector/insertv [0 2 4 6] 4 9))
  (is= nil (vector/insertv [0 2 4 6] 5 9))
  (is= [9] (vector/insertv [] 0 9))
  (is= nil (vector/insertv [] 1 9)))

(deftest removev-test
  (is= [] (vector/removev [] 0))
  (is= [] (vector/removev [] 1))
  (is= [0 2 4] (vector/removev [0 2 4 6] 3))
  (is= [0 4 6] (vector/removev [0 2 4 6] 1)))

(deftest replace-nan-test
  (is= [] (vector/replace-nan 0 []))
  (is= [0 1 2 0] (vector/replace-nan 0 [m/nan 1 2 m/nan]))
  (is= '(0 1 2 0) (vector/replace-nan 0 (apply list [m/nan 1 2 m/nan]))))

(deftest manipulation-tests
  (insertv-test)
  (removev-test)
  (replace-nan-test))

(defspec-test test-insertv `vector/insertv)
(defspec-test test-removev `vector/removev)
(defspec-test test-replace-nan `vector/replace-nan)

(deftest kahan-sum-test
  (is= m/inf+ (vector/kahan-sum [m/inf+ m/inf+]))
  (is (m/nan? (vector/kahan-sum [m/inf+ m/inf-])))
  (is= 17.340604306430002 (vector/kahan-sum '(-3.0 6.34060430643 14.0)))
  (is= 4950.0 (vector/kahan-sum (map double (range 1 100))))
  (is= 15550.883635269476 (vector/kahan-sum (map (partial * m/PI) (range 1 100))))
  (is= 15550.883635269474 (apply + (map (partial * m/PI) (range 1 100)))))

(deftest dot-product-test
  (is= 0 (vector/dot-product [] []))
  (is= 9.0 (vector/dot-product [3] [3]))
  (is= 30.0 (vector/dot-product [0 1 2 3 4] [0 1 2 3 4])))

(deftest cross-product-test
  (is= 0.0 (vector/cross-product [3 4] [3 4]))
  (is= -2.0 (vector/cross-product [3 4] [5 6]))
  (is= 2.0 (vector/cross-product [5 6] [3 4]))
  (is= [-4.0 8.0 -4.0] (vector/cross-product [1 2 3] [5 6 7]))
  (is= [4.0 -8.0 4.0] (vector/cross-product [5 6 7] [1 2 3])))

(deftest projection-test
  (is= [] (vector/projection [] []))
  (is= [3.0] (vector/projection [3] [3]))
  (is= [3.0] (vector/projection [3] [4]))
  (is= [3.19672131147541 3.836065573770492] (vector/projection [3 4] [5 6]))
  (is= [0.0 1.0 2.0 3.0 4.0] (vector/projection [0 1 2 3 4] [0 1 2 3 4]))
  (is= [1.264367816091954 1.517241379310345 1.770114942528736 2.0229885057471266]
       (vector/projection [0 1 2 3] [5 6 7 8]))
  (is= [0.0 3.142857142857143 6.285714285714286 9.428571428571429]
       (vector/projection [5 6 7 8] [0 1 2 3] )))

(deftest math-tests
  (kahan-sum-test)
  (dot-product-test)
  (cross-product-test)
  (projection-test))

(defspec-test test-kahan-sum `vector/kahan-sum)
(defspec-test test-dot-product `vector/dot-product)
(defspec-test test-cross-product `vector/cross-product)
(defspec-test test-projection `vector/projection)

#_(ost/unstrument)