(ns provisdom.math.vector-test
  (:require

    [clojure.test :refer :all]
    [provisdom.test.core :as t]
    [provisdom.math.core :as m]
    [provisdom.math.random :as random]
    [provisdom.math.vector :as vector]))

;;1 SECONDS

(set! *warn-on-reflection* true)

;;;TYPES
(deftest vector?-test
  (t/with-instrument `vector/vector?
    (t/is (t/spec-check vector/vector?)))
  (t/with-instrument :all
    (t/is-not (vector/vector? 1))
    (t/is-not (vector/vector? '(1)))
    (t/is (vector/vector? []))
    (t/is (vector/vector? [2 3]))
    (t/is-not (vector/vector? [[2]]))))

(deftest vector-prob?-test
  (t/with-instrument `vector/vector-prob?
    (t/is (t/spec-check vector/vector-prob?)))
  (t/with-instrument :all
    (t/is-not (vector/vector-prob? 1))
    (t/is (vector/vector-prob? []))
    (t/is (vector/vector-prob? [0.0 1.0]))
    (t/is (vector/vector-prob? [0.7 0.7]))))

(deftest vector-open-prob?-test
  (t/with-instrument `vector/vector-open-prob?
    (t/is (t/spec-check vector/vector-open-prob?)))
  (t/with-instrument :all
    (t/is-not (vector/vector-open-prob? 1))
    (t/is (vector/vector-open-prob? []))
    (t/is-not (vector/vector-open-prob? [0.0 1.0]))
    (t/is (vector/vector-open-prob? [0.7 0.7]))))

(deftest vector-roughly-prob?-test
  (t/with-instrument `vector/vector-roughly-prob?
    (t/is (t/spec-check vector/vector-roughly-prob?)))
  (t/with-instrument :all
    (t/is-not (vector/vector-roughly-prob? 1 0.01))
    (t/is (vector/vector-roughly-prob? [] 0.01))
    (t/is (vector/vector-roughly-prob? [0.0 1.0] 0.01))
    (t/is (vector/vector-roughly-prob? [0.0 1.01] 0.01))
    (t/is (vector/vector-roughly-prob? [0.7 0.7] 0.01))))

(deftest probs?-test
  (t/with-instrument `vector/probs?
    (t/is (t/spec-check vector/probs?)))
  (t/with-instrument :all
    (t/is-not (vector/probs? 1 0.01))
    (t/is-not (vector/probs? [] 0.01))
    (t/is (vector/probs? [0.0 1.0] 0.01))
    (t/is-not (vector/probs? [0.0 1.01] 0.01))
    (t/is-not (vector/probs? [0.7 0.7] 0.01))))

(deftest open-probs?-test
  (t/with-instrument `vector/open-probs?
    (t/is (t/spec-check vector/open-probs?)))
  (t/with-instrument :all
    (t/is-not (vector/open-probs? 1 0.01))
    (t/is-not (vector/open-probs? [] 0.01))
    (t/is-not (vector/open-probs? [0.0 1.0] 0.01))
    (t/is-not (vector/open-probs? [0.0 1.01] 0.01))
    (t/is-not (vector/open-probs? [0.7 0.7] 0.01))
    (t/is (vector/open-probs? [0.1 0.909] 0.01))))

(deftest roughly-probs?-test
  (t/with-instrument `vector/roughly-probs?
    (t/is (t/spec-check vector/roughly-probs?)))
  (t/with-instrument :all
    (t/is-not (vector/roughly-probs? 1 0.01 0.02))
    (t/is-not (vector/roughly-probs? [] 0.01 0.02))
    (t/is (vector/roughly-probs? [0.0 1.0] 0.01 0.02))
    (t/is (vector/roughly-probs? [0.0 1.009] 0.01 0.02))
    (t/is-not (vector/roughly-probs? [0.7 0.7] 0.01 0.02))
    (t/is (vector/roughly-probs? [0.01 1.009] 0.01 0.02))))

;;;CONSTRUCTORS
(deftest to-vector-test
  (t/with-instrument `vector/to-vector
    (t/is (t/spec-check vector/to-vector)))
  (t/with-instrument :all
    (t/is= [1] (vector/to-vector 1))
    (t/is= [1] (vector/to-vector '(1)))
    (t/is= [] (vector/to-vector []))
    (t/is= [2 3] (vector/to-vector [2 3]))
    (t/is= [2 1 4 5] (vector/to-vector [[2 1] [4 5]]))
    (t/is= nil (vector/to-vector nil))))

(deftest compute-vector-test
  (t/with-instrument `vector/compute-vector
    (t/is (t/spec-check vector/compute-vector)))
  (t/with-instrument :all
    (t/is= [0] (vector/compute-vector 1 identity))
    (t/is= [0 1] (vector/compute-vector 2 identity))
    (t/is= [0 3 6] (vector/compute-vector 3 (partial * 3)))
    (t/is= [2.0 3.0 4.0] (vector/compute-vector 3 #(+ 2.0 %)))))

(deftest compute-coll-test
  (t/with-instrument `vector/compute-coll
    (t/is (t/spec-check vector/compute-coll)))
  (t/with-instrument :all
    (t/is= '(0) (vector/compute-coll 1 identity))
    (t/is= '(0 1) (vector/compute-coll 2 identity))
    (t/is= '(0 3 6) (vector/compute-coll 3 (partial * 3)))
    (t/is= '(2.0 3.0 4.0) (vector/compute-coll 3 #(+ 2.0 %)))))

(deftest rnd-vector!-test
  (t/with-instrument `vector/rnd-vector!
    (t/is (t/spec-check vector/rnd-vector!)))
  (t/with-instrument :all
    (random/bind-seed 0
      (t/is= [] (vector/rnd-vector! 0)))
    (random/bind-seed 0
      (t/is= [0.8833108082136426] (vector/rnd-vector! 1)))
    (random/bind-seed 0
      (t/is= [0.8833108082136426 0.026433771592597743] (vector/rnd-vector! 2)))))

(deftest sparse->vector-test
  (t/with-instrument `vector/sparse->vector
    (t/is (t/spec-check vector/sparse->vector)))
  (t/with-instrument :all
    (t/is= [3.0 0.0 4.0 0.0] (vector/sparse->vector '([2 4.0] [0 3.0]) (vec (repeat 4 0.0))))
    (t/is= [0.0 0.0] (vector/sparse->vector [] (vec (repeat 2 0.0))))
    (t/is= [1.0 1.0 2.0 1.0] (vector/sparse->vector [[2 2.0] [4 4.0]] (vec (repeat 4 1.0))))))

;;;INFO
(deftest indexes-of-test
  (t/with-instrument `vector/indexes-of
    (t/is (t/spec-check vector/indexes-of)))
  (t/with-instrument :all
    (t/is= [] (vector/indexes-of 3.0 [1.0]))
    (t/is= [2] (vector/indexes-of 3.0 [1.0 2.0 3.0]))
    (t/is= [1 4] (vector/indexes-of 3.0 [1.0 3.0 4.0 4.0 3.0]))))

(deftest filter-kv-test
  (t/with-instrument `vector/filter-kv
    (t/is (t/spec-check vector/filter-kv)))
  (t/with-instrument :all
    (t/is= [] (vector/filter-kv (fn [k v] (> v k)) []))
    (t/is= [] (vector/filter-kv (fn [k v] (> v k)) [0]))
    (t/is= [2 4 6] (vector/filter-kv (fn [k v] (> v k)) [0 2 4 6]))))

(deftest some-kv-test
  (t/with-instrument `vector/some-kv
    (t/is (t/spec-check vector/some-kv)))
  (t/with-instrument :all
    (t/is= nil (vector/some-kv (fn [k v] (> v k)) []))
    (t/is= nil (vector/some-kv (fn [k v] (> v k)) [0]))
    (t/is= 2 (vector/some-kv (fn [k v] (> v k)) [0 2 4 6]))))

;;;MANIPULATION
(deftest insertv-test
  (t/with-instrument `vector/insertv
    (t/is (t/spec-check vector/insertv)))
  (t/with-instrument :all
    (t/is= [9 0 2 4 6] (vector/insertv [0 2 4 6] 0 9))
    (t/is= [0 9 2 4 6] (vector/insertv [0 2 4 6] 1 9))
    (t/is= [0 2 4 9 6] (vector/insertv [0 2 4 6] 3 9))
    (t/is= [0 2 4 6 9] (vector/insertv [0 2 4 6] 4 9))
    (t/is= nil (vector/insertv [0 2 4 6] 5 9))
    (t/is= [9] (vector/insertv [] 0 9))
    (t/is= nil (vector/insertv [] 1 9))))

(deftest removev-test
  (t/with-instrument `vector/removev
    (t/is (t/spec-check vector/removev)))
  (t/with-instrument :all
    (t/is= [] (vector/removev [] 0))
    (t/is= [] (vector/removev [] 1))
    (t/is= [0 2 4] (vector/removev [0 2 4 6] 3))
    (t/is= [0 4 6] (vector/removev [0 2 4 6] 1))))

(deftest concat-by-index-test
  (t/with-instrument `vector/concat-by-index
    (t/is (t/spec-check vector/concat-by-index)))
  (t/with-instrument :all
    (t/is= '() (vector/concat-by-index [] [] 0))
    (t/is= '(4.0 5.0 6.0 nil 1.0 2.0 3.0)
      (vector/concat-by-index [1.0 2.0 3.0] [4.0 5.0 6.0] -4))
    (t/is= '(4.0 5.0 6.0 nil nil 1.0 2.0 3.0)
      (vector/concat-by-index [1.0 2.0 3.0] [4.0 5.0 6.0] -5))
    (t/is= '(1.0 2.0 3.0 nil nil 4.0 5.0 6.0)
      (vector/concat-by-index [1.0 2.0 3.0] [4.0 5.0 6.0] 5))
    (t/is= '(4.0 5.0 6.0)
      (vector/concat-by-index [1.0 2.0 3.0] [4.0 5.0 6.0] 0))))

(deftest replace-nan-test
  (t/with-instrument `vector/replace-nan
    (t/is (t/spec-check vector/replace-nan)))
  (t/with-instrument :all
    (t/is= [] (vector/replace-nan 0 []))
    (t/is= [0 1 2 0] (vector/replace-nan 0 [m/nan 1 2 m/nan]))
    (t/is= '(0 1 2 0) (vector/replace-nan 0 (apply list [m/nan 1 2 m/nan])))))

(deftest round-roughly-vector-prob-test
  (t/with-instrument `vector/round-roughly-vector-prob
    (t/is (t/spec-check vector/round-roughly-vector-prob)))
  (t/with-instrument :all
    (t/is= [] (vector/round-roughly-vector-prob [] 0.01))
    (t/is= [0.0 1.0] (vector/round-roughly-vector-prob [0.0 1.0] 0.01))
    (t/is= [0.0 1.0] (vector/round-roughly-vector-prob [0.0 1.01] 0.01))
    (t/is= [0.7 0.7] (vector/round-roughly-vector-prob [0.7 0.7] 0.01))
    (t/is= [0.01 1.0] (vector/round-roughly-vector-prob [0.01 1.01] 0.01))))

(deftest rnd-shuffle-vector!-test
  (t/with-instrument `vector/rnd-shuffle-vector!
    (t/is (t/spec-check vector/rnd-shuffle-vector!)))
  (t/with-instrument :all
    (random/bind-seed 0
      (t/is= [] (vector/rnd-shuffle-vector! [])))
    (random/bind-seed 0
      (t/is= [0.0 1.0] (vector/rnd-shuffle-vector! [0.0 1.0])))
    (random/bind-seed 0
      (t/is= [6.0 2.0 3.0 4.0 5.0 1.0 8.0 0.0 7.0]
        (vector/rnd-shuffle-vector! [0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0])))))

;;;MATH
(deftest kahan-sum-test
  (t/with-instrument `vector/kahan-sum
    (t/is (t/spec-check vector/kahan-sum)))
  (t/with-instrument :all
    (t/is= m/inf+ (vector/kahan-sum [m/inf+ m/inf+]))
    (t/is (m/nan? (vector/kahan-sum [m/inf+ m/inf-])))
    (t/is= 17.340604306430002 (vector/kahan-sum '(-3.0 6.34060430643 14.0)))
    (t/is= 4950.0 (vector/kahan-sum (map double (range 1 100))))
    (t/is= 15550.883635269476 (vector/kahan-sum (map (partial * m/PI) (range 1 100))))
    (t/is= 15550.883635269474 (apply + (map (partial * m/PI) (range 1 100))))))

(deftest dot-product-test
  (t/with-instrument `vector/dot-product
    (t/is (t/spec-check vector/dot-product)))
  (t/with-instrument :all
    (t/is= 0 (vector/dot-product [] []))
    (t/is= 9.0 (vector/dot-product [3] [3]))
    (t/is= 30.0 (vector/dot-product [0 1 2 3 4] [0 1 2 3 4]))))

(deftest cross-product-test
  (t/with-instrument `vector/cross-product
    (t/is (t/spec-check vector/cross-product)))
  (t/with-instrument :all
    (t/is= 0.0 (vector/cross-product [3 4] [3 4]))
    (t/is= -2.0 (vector/cross-product [3 4] [5 6]))
    (t/is= 2.0 (vector/cross-product [5 6] [3 4]))
    (t/is= [-4.0 8.0 -4.0] (vector/cross-product [1 2 3] [5 6 7]))
    (t/is= [4.0 -8.0 4.0] (vector/cross-product [5 6 7] [1 2 3]))))

(deftest projection-test
  (t/with-instrument `vector/projection
    (t/is (t/spec-check vector/projection)))
  (t/with-instrument :all
    (t/is= [] (vector/projection [] []))
    (t/is= [3.0] (vector/projection [3] [3]))
    (t/is= [3.0] (vector/projection [3] [4]))
    (t/is= [3.19672131147541 3.836065573770492] (vector/projection [3 4] [5 6]))
    (t/is= [0.0 1.0 2.0 3.0 4.0] (vector/projection [0 1 2 3 4] [0 1 2 3 4]))
    (t/is= [1.264367816091954 1.517241379310345 1.770114942528736 2.0229885057471266]
      (vector/projection [0 1 2 3] [5 6 7 8]))
    (t/is= [0.0 3.142857142857143 6.285714285714286 9.428571428571429]
      (vector/projection [5 6 7 8] [0 1 2 3]))))

(deftest orthogonal?-test
  (t/with-instrument `vector/orthogonal?
    (t/is (t/spec-check vector/orthogonal?)))
  (t/with-instrument :all
    (t/is (vector/orthogonal? [1 0] [0 1] 1e-8))
    (t/is (vector/orthogonal? [1 1] [1 -1] 1e-8))
    (t/is-not (vector/orthogonal? [1 0] [1 1] 1e-8))
    (t/is-not (vector/orthogonal? [1 0] [1 0] 1e-8))
    (t/is (vector/orthogonal? [1 0 0] [0 1 0] 1e-8))
    (t/is (vector/orthogonal? [1 2 3] [-2 1 0] 1e-8))))

(deftest angle-between-test
  (t/with-instrument `vector/angle-between
    (t/is (t/spec-check vector/angle-between)))
  (t/with-instrument :all
    (t/is= 0.0 (vector/angle-between [1 0] [1 0]))
    (t/is= m/PI (vector/angle-between [1 0] [-1 0]))
    (t/is= 1.5707963267948966 (vector/angle-between [1 0] [0 1]))
    (t/is= 1.5707963267948966 (vector/angle-between [1 1] [1 -1]))
    (t/is= 0.05123716740341752 (vector/angle-between [3 4] [5 6]))))

(deftest distance-test
  (t/with-instrument `vector/distance
    (t/is (t/spec-check vector/distance)))
  (t/with-instrument :all
    (t/is= 0.0 (vector/distance [1 2 3] [1 2 3]))
    (t/is= 5.0 (vector/distance [0 0] [3 4]))
    (t/is= 5.0 (vector/distance [3 4] [0 0]))
    (t/is= 1.4142135623730951 (vector/distance [0 0] [1 1]))
    (t/is= 5.196152422706632 (vector/distance [1 2 3] [4 5 6]))))
