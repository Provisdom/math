(ns provisdom.math.vector-test
  (:require
    [clojure.test :refer [deftest is are testing run-tests]]
    [clojure.spec.test.alpha]
    [provisdom.test.core :refer [is-not is=]]
    [provisdom.math.vector :as vector]
    [provisdom.math.core :as m]
    [provisdom.math.random :as random]
    #?(:clj [orchestra.spec.test :as ost])))

;;37 SECONDS

#?(:clj (set! *warn-on-reflection* true))

#?(:clj (ost/instrument))

;;;TYPES
(deftest vector?-test
  (is (spec-check vector/vector?))
  (is-not (vector/vector? 1))
  (is-not (vector/vector? '(1)))
  (is (vector/vector? []))
  (is (vector/vector? [2 3]))
  (is-not (vector/vector? [[2]])))

(deftest vector-prob?-test
  (is (spec-check vector/vector-prob?))
  (is-not (vector/vector-prob? 1))
  (is (vector/vector-prob? []))
  (is (vector/vector-prob? [0.0 1.0]))
  (is (vector/vector-prob? [0.7 0.7])))

(deftest vector-open-prob?-test
  (is (spec-check vector/vector-open-prob?))
  (is-not (vector/vector-open-prob? 1))
  (is (vector/vector-open-prob? []))
  (is-not (vector/vector-open-prob? [0.0 1.0]))
  (is (vector/vector-open-prob? [0.7 0.7])))

(deftest vector-roughly-prob?-test
  (is (spec-check vector/vector-roughly-prob?))
  (is-not (vector/vector-roughly-prob? 1 0.01))
  (is (vector/vector-roughly-prob? [] 0.01))
  (is (vector/vector-roughly-prob? [0.0 1.0] 0.01))
  (is (vector/vector-roughly-prob? [0.0 1.01] 0.01))
  (is (vector/vector-roughly-prob? [0.7 0.7] 0.01)))

(deftest probs?-test
  (is (spec-check vector/probs?))
  (is-not (vector/probs? 1 0.01))
  (is-not (vector/probs? [] 0.01))
  (is (vector/probs? [0.0 1.0] 0.01))
  (is-not (vector/probs? [0.0 1.01] 0.01))
  (is-not (vector/probs? [0.7 0.7] 0.01)))

(deftest open-probs?-test
  (is (spec-check vector/open-probs?))
  (is-not (vector/open-probs? 1 0.01))
  (is-not (vector/open-probs? [] 0.01))
  (is-not (vector/open-probs? [0.0 1.0] 0.01))
  (is-not (vector/open-probs? [0.0 1.01] 0.01))
  (is-not (vector/open-probs? [0.7 0.7] 0.01))
  (is (vector/open-probs? [0.1 0.909] 0.01)))

(deftest roughly-probs?-test
  (is (spec-check vector/roughly-probs?))
  (is-not (vector/roughly-probs? 1 0.01 0.02))
  (is-not (vector/roughly-probs? [] 0.01 0.02))
  (is (vector/roughly-probs? [0.0 1.0] 0.01 0.02))
  (is (vector/roughly-probs? [0.0 1.009] 0.01 0.02))
  (is-not (vector/roughly-probs? [0.7 0.7] 0.01 0.02))
  (is (vector/roughly-probs? [0.01 1.009] 0.01 0.02)))

;;;CONSTRUCTORS
(deftest to-vector-test
  (is (spec-check vector/to-vector))
  (is= [1] (vector/to-vector 1))
  (is= [1] (vector/to-vector '(1)))
  (is= [] (vector/to-vector []))
  (is= [2 3] (vector/to-vector [2 3]))
  (is= [2 1 4 5] (vector/to-vector [[2 1] [4 5]]))
  (is= nil (vector/to-vector nil)))

(deftest compute-vector-test
  (is (spec-check vector/compute-vector))
  (is= [0] (vector/compute-vector 1 identity))
  (is= [0 1] (vector/compute-vector 2 identity))
  (is= [0 3 6] (vector/compute-vector 3 (partial * 3)))
  (is= [2.0 3.0 4.0] (vector/compute-vector 3 #(+ 2.0 %))))

(deftest compute-coll-test
  (is (spec-check vector/compute-coll))
  (is= '(0) (vector/compute-coll 1 identity))
  (is= '(0 1) (vector/compute-coll 2 identity))
  (is= '(0 3 6) (vector/compute-coll 3 (partial * 3)))
  (is= '(2.0 3.0 4.0) (vector/compute-coll 3 #(+ 2.0 %))))

(deftest rnd-vector!-test
  (is (spec-check vector/rnd-vector!))
  (random/bind-seed 0
    (is= [] (vector/rnd-vector! 0)))
  (random/bind-seed 0
    (is= [0.8833108082136426] (vector/rnd-vector! 1)))
  (random/bind-seed 0
    (is= [0.8833108082136426 0.026433771592597743]
         (vector/rnd-vector! 2))))

(deftest sparse->vector-test
  (is (spec-check vector/sparse->vector))
  (is= [3.0 0.0 4.0 0.0]
       (vector/sparse->vector '([2 4.0] [0 3.0]) (vec (repeat 4 0.0))))
  (is= [0.0 0.0] (vector/sparse->vector [] (vec (repeat 2 0.0))))
  (is= [1.0 1.0 2.0 1.0]
       (vector/sparse->vector [[2 2.0] [4 4.0]] (vec (repeat 4 1.0)))))

;;;INFO
(deftest indexes-of-test
  (is (spec-check vector/indexes-of))
  (is= [] (vector/indexes-of 3.0 [1.0]))
  (is= [2] (vector/indexes-of 3.0 [1.0 2.0 3.0]))
  (is= [1 4] (vector/indexes-of 3.0 [1.0 3.0 4.0 4.0 3.0])))

(deftest filter-kv-test
  (is (spec-check vector/filter-kv))
  (is= [] (vector/filter-kv (fn [k v] (> v k)) []))
  (is= [] (vector/filter-kv (fn [k v] (> v k)) [0]))
  (is= [2 4 6] (vector/filter-kv (fn [k v] (> v k)) [0 2 4 6])))

(deftest some-kv-test
  (is (spec-check vector/some-kv))
  (is= nil (vector/some-kv (fn [k v] (> v k)) []))
  (is= nil (vector/some-kv (fn [k v] (> v k)) [0]))
  (is= 2 (vector/some-kv (fn [k v] (> v k)) [0 2 4 6])))

;;;MANIPULATION
(deftest insertv-test
  (is (spec-check vector/insertv))
  (is= [9 0 2 4 6] (vector/insertv [0 2 4 6] 0 9))
  (is= [0 9 2 4 6] (vector/insertv [0 2 4 6] 1 9))
  (is= [0 2 4 9 6] (vector/insertv [0 2 4 6] 3 9))
  (is= [0 2 4 6 9] (vector/insertv [0 2 4 6] 4 9))
  (is= nil (vector/insertv [0 2 4 6] 5 9))
  (is= [9] (vector/insertv [] 0 9))
  (is= nil (vector/insertv [] 1 9)))

(deftest removev-test
  (is (spec-check vector/removev))
  (is= [] (vector/removev [] 0))
  (is= [] (vector/removev [] 1))
  (is= [0 2 4] (vector/removev [0 2 4 6] 3))
  (is= [0 4 6] (vector/removev [0 2 4 6] 1)))

(deftest concat-by-index-test
  (is (spec-check vector/concat-by-index))
  (is= '() (vector/concat-by-index [] [] 0))
  (is= '(4.0 5.0 6.0 nil 1.0 2.0 3.0)
       (vector/concat-by-index [1.0 2.0 3.0] [4.0 5.0 6.0] -4))
  (is= '(4.0 5.0 6.0 nil nil 1.0 2.0 3.0)
       (vector/concat-by-index [1.0 2.0 3.0] [4.0 5.0 6.0] -5))
  (is= '(1.0 2.0 3.0 nil nil 4.0 5.0 6.0)
       (vector/concat-by-index [1.0 2.0 3.0] [4.0 5.0 6.0] 5))
  (is= '(4.0 5.0 6.0) (vector/concat-by-index [1.0 2.0 3.0] [4.0 5.0 6.0] 0)))

(deftest replace-nan-test
  (is (spec-check vector/replace-nan))
  (is= [] (vector/replace-nan 0 []))
  (is= [0 1 2 0] (vector/replace-nan 0 [m/nan 1 2 m/nan]))
  (is= '(0 1 2 0) (vector/replace-nan 0 (apply list [m/nan 1 2 m/nan]))))

(deftest round-roughly-vector-prob-test
  (is (spec-check vector/round-roughly-vector-prob))
  (is= [] (vector/round-roughly-vector-prob [] 0.01))
  (is= [0.0 1.0] (vector/round-roughly-vector-prob [0.0 1.0] 0.01))
  (is= [0.0 1.0] (vector/round-roughly-vector-prob [0.0 1.01] 0.01))
  (is= [0.7 0.7] (vector/round-roughly-vector-prob [0.7 0.7] 0.01))
  (is= [0.01 1.0] (vector/round-roughly-vector-prob [0.01 1.01] 0.01)))

(deftest rnd-shuffle-vector!-test
  (is (spec-check vector/rnd-shuffle-vector!))
  (random/bind-seed 0
    (is= [] (vector/rnd-shuffle-vector! [])))
  (random/bind-seed 0
    (is= [0.0 1.0] (vector/rnd-shuffle-vector! [0.0 1.0])))
  (random/bind-seed 0
    (is= [6.0 2.0 3.0 4.0 5.0 1.0 8.0 0.0 7.0]
         (vector/rnd-shuffle-vector! [0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0]))))

;;;MATH
(deftest kahan-sum-test
  (is (spec-check vector/kahan-sum))
  (is= m/inf+ (vector/kahan-sum [m/inf+ m/inf+]))
  (is (m/nan? (vector/kahan-sum [m/inf+ m/inf-])))
  (is= 17.340604306430002 (vector/kahan-sum '(-3.0 6.34060430643 14.0)))
  (is= 4950.0 (vector/kahan-sum (map double (range 1 100))))
  (is= 15550.883635269476
       (vector/kahan-sum (map (partial * m/PI) (range 1 100))))
  (is= 15550.883635269474 (apply + (map (partial * m/PI) (range 1 100)))))

(deftest dot-product-test
  (is (spec-check vector/dot-product))
  (is= 0 (vector/dot-product [] []))
  (is= 9.0 (vector/dot-product [3] [3]))
  (is= 30.0 (vector/dot-product [0 1 2 3 4] [0 1 2 3 4])))

(deftest cross-product-test
  (is (spec-check vector/cross-product))
  (is= 0.0 (vector/cross-product [3 4] [3 4]))
  (is= -2.0 (vector/cross-product [3 4] [5 6]))
  (is= 2.0 (vector/cross-product [5 6] [3 4]))
  (is= [-4.0 8.0 -4.0] (vector/cross-product [1 2 3] [5 6 7]))
  (is= [4.0 -8.0 4.0] (vector/cross-product [5 6 7] [1 2 3])))

(deftest projection-test
  (is (spec-check vector/projection))
  (is= [] (vector/projection [] []))
  (is= [3.0] (vector/projection [3] [3]))
  (is= [3.0] (vector/projection [3] [4]))
  (is= [3.19672131147541 3.836065573770492] (vector/projection [3 4] [5 6]))
  (is= [0.0 1.0 2.0 3.0 4.0] (vector/projection [0 1 2 3 4] [0 1 2 3 4]))
  (is= [1.264367816091954 1.517241379310345
        1.770114942528736 2.0229885057471266]
       (vector/projection [0 1 2 3] [5 6 7 8]))
  (is= [0.0 3.142857142857143 6.285714285714286 9.428571428571429]
       (vector/projection [5 6 7 8] [0 1 2 3])))

#_(ost/unstrument)