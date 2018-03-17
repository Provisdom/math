(ns provisdom.math.combinatorics-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.combinatorics :as combinatorics]
    [provisdom.math.core :as m]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;;38 seconds

(ost/instrument)

(set! *warn-on-reflection* true)

;;;FACTORIALS
(deftest factorial-test
  (is (spec-check combinatorics/factorial))
  (is= 1.0 (combinatorics/factorial 0))
  (is= 0.9513507698668734 (combinatorics/factorial 0.1))
  (is= 0.8862269254527579 (combinatorics/factorial 0.5))
  (is= 0.9617658319073873 (combinatorics/factorial 0.9))
  (is= 1.0 (combinatorics/factorial 1))
  (is= 1.329340388179137 (combinatorics/factorial 1.5))
  (is= 2.0 (combinatorics/factorial 2.0))
  (is= 5.109094217170944E19 (combinatorics/factorial 21))
  (is= 1.1240007277776077E21 (combinatorics/factorial 22))
  (is= 2.585201673888498E22 (combinatorics/factorial 23)))

(deftest factorial'-test
  (is (spec-check combinatorics/factorial'))
  (is= 1 (combinatorics/factorial' 0)))

(deftest log-factorial-test
  (is (spec-check combinatorics/log-factorial))
  (is= 0.0 (combinatorics/log-factorial 0))
  (is= -0.049872441259839764 (combinatorics/log-factorial 0.1))
  (is= -0.1207822376352452 (combinatorics/log-factorial 0.5))
  (is= -0.03898427592308336 (combinatorics/log-factorial 0.9))
  (is= 0.0 (combinatorics/log-factorial 1))
  (is= 0.2846828704729192 (combinatorics/log-factorial 1.5))
  (is= 0.6931471805599453 (combinatorics/log-factorial 2.0))
  (is= 51.60667556776437 (combinatorics/log-factorial 23)))

(deftest subfactorial-test
  (is (spec-check combinatorics/subfactorial))
  (is= 1 (combinatorics/subfactorial 0))
  (is= 0 (combinatorics/subfactorial 0.1))
  (is= 0 (combinatorics/subfactorial 0.5))
  (is= 0 (combinatorics/subfactorial 0.9))
  (is= 0 (combinatorics/subfactorial 1))
  (is= 0 (combinatorics/subfactorial 1.5))
  (is= 1 (combinatorics/subfactorial 2.0))
  (is= 895014631192902121 (combinatorics/subfactorial 20))
  (is= 18795307255050944540N (combinatorics/subfactorial 21))
  (is= 4.134967596111208E20 (combinatorics/subfactorial 22))
  (is= 9.510425471055779E21 (combinatorics/subfactorial 23)))

;;;CHOOSING
(deftest choose-k-from-n-test
  (is (spec-check combinatorics/choose-k-from-n))
  (is= 0.0 (combinatorics/choose-k-from-n -1 1))
  (is= 1.0 (combinatorics/choose-k-from-n 0 1))
  (is= 1.0 (combinatorics/choose-k-from-n 0 0))
  (is= 1.0 (combinatorics/choose-k-from-n 0 -1))
  (is= 0.9 (combinatorics/choose-k-from-n 1 0.9))
  (is= 0.0 (combinatorics/choose-k-from-n 1 0))
  (is= 1.0 (combinatorics/choose-k-from-n 1 1))
  (is= 1.4 (combinatorics/choose-k-from-n 1 1.4))
  (is= 0.2799999999999999 (combinatorics/choose-k-from-n 2 1.4))
  (is= 4.0 (combinatorics/choose-k-from-n 1.0 4))
  (is= 10.0 (combinatorics/choose-k-from-n 2 5))
  (is= 1.2689769520640436E24 (combinatorics/choose-k-from-n 12 545.0)))

(deftest choose-k-from-n'-test
  (is (spec-check combinatorics/choose-k-from-n'))
  (is= 4 (combinatorics/choose-k-from-n' 1.0 4)))

(deftest log-choose-k-from-n-test
  (is (spec-check combinatorics/log-choose-k-from-n))
  (is= 0.0 (combinatorics/log-choose-k-from-n 0 1))
  (is= 0.0 (combinatorics/log-choose-k-from-n 0 0))
  (is= 0.0 (combinatorics/log-choose-k-from-n 1 1))
  (is= 0.33647223662121284 (combinatorics/log-choose-k-from-n 1 1.4))
  (is= 1.3862943611198908 (combinatorics/log-choose-k-from-n 1.0 4))
  (is= 2.3025850929940455 (combinatorics/log-choose-k-from-n 2 5))
  (is= 55.50025325814249 (combinatorics/log-choose-k-from-n 12 545.0)))

(deftest stirling-number-of-the-second-kind-test
  (is (spec-check combinatorics/stirling-number-of-the-second-kind))
  (is= 0.0 (combinatorics/stirling-number-of-the-second-kind 0 1))
  (is= 1.0 (combinatorics/stirling-number-of-the-second-kind 0 0))
  (is= 1.0 (combinatorics/stirling-number-of-the-second-kind 1 1))
  (is= 1.0 (combinatorics/stirling-number-of-the-second-kind 1 4.0))
  (is= 15.0 (combinatorics/stirling-number-of-the-second-kind 2.0 5))
  (is= 1.4318980615233435E207
       (combinatorics/stirling-number-of-the-second-kind 12 200.0)))

(deftest stirling-number-of-the-second-kind'-test
  (is (spec-check combinatorics/stirling-number-of-the-second-kind'))
  (is= 0 (combinatorics/stirling-number-of-the-second-kind' 0 1)))

(deftest bell-number-test
  (is (spec-check combinatorics/bell-number))
  (is= 1 (combinatorics/bell-number 0))
  (is= 1 (combinatorics/bell-number 1))
  (is= 2 (combinatorics/bell-number 2.0))
  (is= 52 (combinatorics/bell-number 5))
  (is= 49631246523618756274N (combinatorics/bell-number 26))
  (is= 5.4571704793605997E20 (combinatorics/bell-number 27))
  (is= 6.160539404599935E21 (combinatorics/bell-number 28.0)))

(deftest binomial-probability-test
  (is (spec-check combinatorics/binomial-probability))
  (is= 1.0 (combinatorics/binomial-probability 0 0 0.4))
  (is= 0.4 (combinatorics/binomial-probability 1 1 0.4))
  (is= 0.45650814137931667 (combinatorics/binomial-probability 1 1.4 0.4))
  (is= 0.34559999999999996 (combinatorics/binomial-probability 1.0 4 0.4))
  (is= 0.3456 (combinatorics/binomial-probability 2 5 0.4))
  (is= 1.210013134840654E-99 (combinatorics/binomial-probability 12 545.0 0.4))
  (is= 0.0 (combinatorics/binomial-probability 12 24 0.0))
  (is= 0.0 (combinatorics/binomial-probability 12 24 1.0)))

(deftest log-binomial-probability-test
  (is (spec-check combinatorics/log-binomial-probability))
  (is= 0.0 (combinatorics/log-binomial-probability 0 0 0.4))
  (is= -0.916290731874155 (combinatorics/log-binomial-probability 1 1 0.4))
  (is= -0.7841487447593384 (combinatorics/log-binomial-probability 1 1.4 0.4))
  (is= -1.0624732420522363 (combinatorics/log-binomial-probability 1.0 4 0.4))
  (is= -1.0624732420522367 (combinatorics/log-binomial-probability 2 5 0.4))
  (is= -227.7652929916204 (combinatorics/log-binomial-probability 12 545.0 0.4))
  (is= m/inf- (combinatorics/log-binomial-probability 12 24 0.0))
  (is= m/inf- (combinatorics/log-binomial-probability 12 24 1.0)))

;;;UNORDERED COMBINATIONS
(deftest combinations-test
  (is (spec-check combinatorics/combinations))
  (is= '(()) (combinatorics/combinations [1 2 3] 0))
  (is= '((1 2) (1 3) (2 3)) (combinatorics/combinations [1 2 3] 2))
  (is= '((1 2) (1 3) (2 3)) (combinatorics/combinations '(1 2 3) 2))
  (is= '((1 3) (1 2) (3 2)) (combinatorics/combinations #{1 2 3} 2))
  (is= '((1 1) (1 2) (1 2)) (combinatorics/combinations [1 1 2] 2))
  (is= '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))
       (combinatorics/combinations [1 2 3]))
  (is= '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))
       (combinatorics/combinations '(1 2 3)))
  (is= '(() (1) (3) (2) (1 3) (1 2) (3 2) (1 3 2))
       (combinatorics/combinations #{1 2 3}))
  (is= '(() (1) (1) (2) (1 1) (1 2) (1 2) (1 1 2))
       (combinatorics/combinations [1 1 2])))

(deftest combinations-with-complements-test
  (is (spec-check combinatorics/combinations-with-complements))
  (is= '((() (1 2 3))
          ((1) (2 3))
          ((2) (1 3))
          ((3) (1 2))
          ((1 2) (3))
          ((1 3) (2))
          ((2 3) (1))
          ((1 2 3) ()))
       (combinatorics/combinations-with-complements [1 2 3]))
  (is= '((() (1 2 3))
          ((1) (2 3))
          ((2) (1 3))
          ((3) (1 2))
          ((1 2) (3))
          ((1 3) (2))
          ((2 3) (1))
          ((1 2 3) ()))
       (combinatorics/combinations-with-complements '(1 2 3)))
  (is= '((() (1 3 2))
          ((1) (3 2))
          ((3) (1 2))
          ((2) (1 3))
          ((1 3) (2))
          ((1 2) (3))
          ((3 2) (1))
          ((1 3 2) ()))
       (combinatorics/combinations-with-complements #{1 2 3}))
  (is= '((() (1 1 2))
          ((1) (1 2))
          ((1) (1 2))
          ((2) (1 1))
          ((1 1) (2))
          ((1 2) (1))
          ((1 2) (1))
          ((1 1 2) ()))
       (combinatorics/combinations-with-complements [1 1 2]))
  (is= '(((1 2) (3)) ((1 3) (2)) ((2 3) (1)))
       (combinatorics/combinations-with-complements [1 2 3] 2))
  (is= '(((1 2) (3)) ((1 3) (2)) ((2 3) (1)))
       (combinatorics/combinations-with-complements '(1 2 3) 2))
  (is= '(((1 3) (2)) ((1 2) (3)) ((3 2) (1)))
       (combinatorics/combinations-with-complements #{1 2 3} 2))
  (is= '(((1 1) (2)) ((1 2) (1)) ((1 2) (1)))
       (combinatorics/combinations-with-complements [1 1 2] 2))
  (is= '((() (1 2 3)))
       (combinatorics/combinations-with-complements [1 2 3] 0)))


(deftest combinations-using-all-test
  (is (spec-check combinatorics/combinations-using-all))
  (is= '(((1 2) (3 4))
          ((1 3) (2 4))
          ((1 4) (2 3))
          ((2 3) (1 4))
          ((2 4) (1 3))
          ((3 4) (1 2)))
       (combinatorics/combinations-using-all [1 2 3 4] [2 2]))
  (is= '(((1 2) (3 4))
          ((1 3) (2 4))
          ((1 4) (2 3))
          ((2 3) (1 4))
          ((2 4) (1 3))
          ((3 4) (1 2)))
       (combinatorics/combinations-using-all '(1 2 3 4) [2 2]))
  (is= '(((1 4) (3 2))
          ((1 3) (4 2))
          ((1 2) (4 3))
          ((4 3) (1 2))
          ((4 2) (1 3))
          ((3 2) (1 4)))
       (combinatorics/combinations-using-all #{1 2 3 4} [2 2]))
  (is= '(((1 1) (2 3))
          ((1 2) (1 3))
          ((1 3) (1 2))
          ((1 2) (1 3))
          ((1 3) (1 2))
          ((2 3) (1 1)))
       (combinatorics/combinations-using-all [1 1 2 3] [2 2]))
  (is= '(((1 2 3) (4))
          ((1 2 4) (3))
          ((1 3 4) (2))
          ((2 3 4) (1)))
       (combinatorics/combinations-using-all [1 2 3 4] [3 1])))

(deftest distinct-combinations-with-replacement-test
  (is (spec-check combinatorics/distinct-combinations-with-replacement))
  (is= '(())
       (combinatorics/distinct-combinations-with-replacement [1 2 3 4] 0))
  (is= '(() (1) (2) (3) (4))
       (combinatorics/distinct-combinations-with-replacement [1 2 3 4] 1))
  (is= '(() (1) (2) (3) (4) (1 1) (1 2) (1 3) (1 4) (2 2) (2 3) (2 4) (3 3) (3 4) (4 4))
       (combinatorics/distinct-combinations-with-replacement [1 2 3 4] 2))
  (is= '(() (1) (2) (3) (4) (1 1) (1 2) (1 3) (1 4) (2 2) (2 3) (2 4) (3 3) (3 4) (4 4))
       (combinatorics/distinct-combinations-with-replacement '(1 2 3 4) 2))
  (is= '(() (1) (4) (3) (2) (1 1) (1 4) (1 3) (1 2) (4 4) (4 3) (4 2) (3 3) (3 2) (2 2))
       (combinatorics/distinct-combinations-with-replacement #{1 2 3 4} 2))
  (is= '(() (1) (2) (3) (1 1) (1 2) (1 3) (2 2) (2 3) (3 3))
       (combinatorics/distinct-combinations-with-replacement [1 1 2 3] 2))
  (is= '(() (1) (2) (1 1) (1 2) (2 2) (1 1 1) (1 1 2) (1 2 2) (2 2 2))
       (combinatorics/distinct-combinations-with-replacement [1 2] 3)))

;;;ORDERED COMBINATIONS
(deftest permutations-test
  (is (spec-check combinatorics/permutations))
  (is= '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
       (combinatorics/permutations '(1 2 3)))
  (is= '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
       (combinatorics/permutations [1 2 3]))
  (is= '((1 3 2) (1 2 3) (3 1 2) (3 2 1) (2 1 3) (2 3 1))
       (combinatorics/permutations #{1 2 3}))
  (is= '((1 1 2) (1 2 1) (1 1 2) (1 2 1) (2 1 1) (2 1 1))
       (combinatorics/permutations [1 1 2])))

(deftest cartesian-product-test
  (is (spec-check combinatorics/cartesian-product
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 150}}))
  (is= '() (combinatorics/cartesian-product '((1 2)) '()))
  (is= '((1 8.0) (1 9.0) (2 8.0) (2 9.0) (3 8.0) (3 9.0))
       (combinatorics/cartesian-product [1 2 3] [8.0 9.0]))
  (is= '((1 8.0) (1 9.0) (2 8.0) (2 9.0) (3 8.0) (3 9.0))
       (combinatorics/cartesian-product '(1 2 3) [8.0 9.0]))
  (is= '((1 8.0) (1 9.0) (1 8.0) (1 9.0) (2 8.0) (2 9.0))
       (combinatorics/cartesian-product [1 1 2] [8.0 9.0])))

(deftest selections-test
  (is (spec-check combinatorics/selections))
  (is= '(()) (combinatorics/selections [1 2 3] 0))
  (is= '((1) (2) (3)) (combinatorics/selections [1 2 3] 1))
  (is= '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))
       (combinatorics/selections [1 2 3] 2))
  (is= '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))
       (combinatorics/selections '(1 2 3) 2))
  (is= '((1 1) (1 3) (1 2) (3 1) (3 3) (3 2) (2 1) (2 3) (2 2))
       (combinatorics/selections #{1 2 3} 2))
  (is= '((1 1) (1 1) (1 2) (1 1) (1 1) (1 2) (2 1) (2 1) (2 2))
       (combinatorics/selections [1 1 2] 2)))

#_(ost/unstrument)