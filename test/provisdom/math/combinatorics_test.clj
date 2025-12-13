(ns provisdom.math.combinatorics-test
  (:require [clojure.test :refer :all]
            [provisdom.math.combinatorics :as combo]
            [provisdom.test.core :as t])
  (:import
    [org.apache.commons.math3.util
     CombinatoricsUtils]))

;;1 seconds

(set! *warn-on-reflection* true)

;;;FACTORIALS
(deftest factorial-test
  (t/with-instrument `combo/factorial
    (t/is (t/spec-check combo/factorial)))
  (t/with-instrument :all
    (t/is= 1.0 (combo/factorial 0))
    (t/is= 0.9513507698668731 (combo/factorial 0.1))
    (t/is= 0.886226925452758 (combo/factorial 0.5))
    (t/is= 0.9617658319073874 (combo/factorial 0.9))
    (t/is= 1.0 (combo/factorial 1))
    (t/is= 1.329340388179137 (combo/factorial 1.5))
    (t/is= 2.0 (combo/factorial 2.0))
    (t/is= 5.109094217170945E19 (combo/factorial 21))
    (t/is= 1.1240007277776072E21 (combo/factorial 22))
    (t/is= 2.585201673888498E22 (combo/factorial 23))
    (t/is= 4.714723635992578E284 (combo/factorial 160))
    (t/is= 4.7147236359931136E284 (CombinatoricsUtils/factorialDouble 160))))

(deftest factorial'-test
  (t/with-instrument `combo/factorial'
    (t/is (t/spec-check combo/factorial')))
  (t/with-instrument :all
    (t/is= 1 (combo/factorial' 0))))

(deftest log-factorial-test
  (t/with-instrument `combo/log-factorial
    (t/is (t/spec-check combo/log-factorial)))
  (t/with-instrument :all
    (t/is= 0.0 (combo/log-factorial 0))
    (t/is= -0.04987244125983972 (combo/log-factorial 0.1))
    (t/is= -0.1207822376352452 (combo/log-factorial 0.5))
    (t/is= -0.03898427592308332 (combo/log-factorial 0.9))
    (t/is= 0.0 (combo/log-factorial 1))
    (t/is= 0.2846828704729192 (combo/log-factorial 1.5))
    (t/is= 0.6931471805599453 (combo/log-factorial 2.0))
    (t/is= 51.60667556776438 (combo/log-factorial 23))
    (t/is= 655.4848567108892 (combo/log-factorial 160))))

(deftest subfactorial-test
  (t/with-instrument `combo/subfactorial
    (t/is (t/spec-check combo/subfactorial)))
  (t/with-instrument :all
    (t/is= 1 (combo/subfactorial 0))
    (t/is= 0 (combo/subfactorial 0.1))
    (t/is= 0 (combo/subfactorial 0.5))
    (t/is= 0 (combo/subfactorial 0.9))
    (t/is= 0 (combo/subfactorial 1))
    (t/is= 0 (combo/subfactorial 1.5))
    (t/is= 1 (combo/subfactorial 2.0))
    (t/is= 895014631192902121 (combo/subfactorial 20))
    (t/is= 18795307255050944540N (combo/subfactorial 21))
    (t/is= 4.134967596111206E20 (combo/subfactorial 22))
    (t/is= 9.510425471055779E21 (combo/subfactorial 23))))

;;;CHOOSING
(deftest choose-k-from-n-test
  (t/with-instrument `combo/choose-k-from-n
    (t/is (t/spec-check combo/choose-k-from-n)))
  (t/with-instrument :all
    (t/is= 1.0 (combo/choose-k-from-n 0 1))
    (t/is= 1.0 (combo/choose-k-from-n 0 0))
    (t/is= 1.0 (combo/choose-k-from-n 1 1))
    (t/is= 2.0 (combo/choose-k-from-n 1 2))
    (t/is= 4.0 (combo/choose-k-from-n 1 4))
    (t/is= 10.0 (combo/choose-k-from-n 2 5))
    (t/is= 1.268976952064044E24 (combo/choose-k-from-n 12 545))))

(deftest choose-k-from-n'-test
  (t/with-instrument `combo/choose-k-from-n'
    (t/is (t/spec-check combo/choose-k-from-n'
            {:num-tests 50})))
  (t/with-instrument :all
    (t/is= 4 (combo/choose-k-from-n' 1 4))))

(deftest log-choose-k-from-n-test
  (t/with-instrument `combo/log-choose-k-from-n
    (t/is (t/spec-check combo/log-choose-k-from-n)))
  (t/with-instrument :all
    (t/is= 0.0 (combo/log-choose-k-from-n 0 1))
    (t/is= 0.0 (combo/log-choose-k-from-n 0 0))
    (t/is= 0.0 (combo/log-choose-k-from-n 1 1))
    (t/is= 0.33647223662121284 (combo/log-choose-k-from-n 1 1.4))
    (t/is= 1.3862943611198908 (combo/log-choose-k-from-n 1 4))
    (t/is= 2.3025850929940455 (combo/log-choose-k-from-n 2 5))
    (t/is= 55.50025325814249 (combo/log-choose-k-from-n 12 545))))

(deftest stirling-number-of-the-second-kind-test
  (t/with-instrument `combo/stirling-number-of-the-second-kind
    (t/is (t/spec-check combo/stirling-number-of-the-second-kind)))
  (t/with-instrument :all
    (t/is= 0.0 (combo/stirling-number-of-the-second-kind 0 1))
    (t/is= 1.0 (combo/stirling-number-of-the-second-kind 0 0))
    (t/is= 1.0 (combo/stirling-number-of-the-second-kind 1 1))
    (t/is= 1.0 (combo/stirling-number-of-the-second-kind 1 4))
    (t/is= 15.0 (combo/stirling-number-of-the-second-kind 2 5))
    (t/is= 1.4318980615233435E207 (combo/stirling-number-of-the-second-kind 12 200))))

(deftest stirling-number-of-the-second-kind'-test
  (t/with-instrument `combo/stirling-number-of-the-second-kind'
    (t/is (t/spec-check combo/stirling-number-of-the-second-kind')))
  (t/with-instrument :all
    (t/is= 0 (combo/stirling-number-of-the-second-kind' 0 1))))

(deftest bell-number-test
  (t/with-instrument `combo/bell-number
    (t/is (t/spec-check combo/bell-number)))
  (t/with-instrument :all
    (t/is= 1 (combo/bell-number 0))
    (t/is= 1 (combo/bell-number 1))
    (t/is= 2 (combo/bell-number 2))
    (t/is= 52 (combo/bell-number 5))
    (t/is= 49631246523618756274N (combo/bell-number 26))
    (t/is= 5.4571704793605997E20 (combo/bell-number 27))
    (t/is= 6.160539404599935E21 (combo/bell-number 28))))

(deftest binomial-probability-test
  (t/with-instrument `combo/binomial-probability
    (t/is (t/spec-check combo/binomial-probability)))
  (t/with-instrument :all
    (t/is= 1.0 (combo/binomial-probability 0 0 0.4))
    (t/is= 0.4 (combo/binomial-probability 1 1 0.4))
    (t/is= 0.48 (combo/binomial-probability 1 2 0.4))
    (t/is= 0.34559999999999996 (combo/binomial-probability 1 4 0.4))
    (t/is= 0.3456 (combo/binomial-probability 2 5 0.4))
    (t/is= 1.2100131348406543E-99 (combo/binomial-probability 12 545 0.4))))

(deftest log-binomial-probability-test
  (t/with-instrument `combo/log-binomial-probability
    (t/is (t/spec-check combo/log-binomial-probability)))
  (t/with-instrument :all
    (t/is= 0.0 (combo/log-binomial-probability 0 0 0.4))
    (t/is= -0.916290731874155 (combo/log-binomial-probability 1 1 0.4))
    (t/is= -0.7841487447593384 (combo/log-binomial-probability 1 1.4 0.4))
    (t/is= -1.0624732420522363 (combo/log-binomial-probability 1 4 0.4))
    (t/is= -1.0624732420522367 (combo/log-binomial-probability 2 5 0.4))
    (t/is= -227.7652929916204 (combo/log-binomial-probability 12 545.0 0.4))))

;;;UNORDERED COMBINATIONS
(deftest combinations-test
  (t/with-instrument `combo/combinations
    (t/is (t/spec-check combo/combinations)))
  (t/with-instrument :all
    (t/is= '(()) (combo/combinations [1 2 3] 0))
    (t/is= '((1 2) (1 3) (2 3)) (combo/combinations [1 2 3] 2))
    (t/is= '((1 2) (1 3) (2 3)) (combo/combinations '(1 2 3) 2))
    (t/is= '((1 3) (1 2) (3 2)) (combo/combinations #{1 2 3} 2))
    (t/is= '((1 1) (1 2) (1 2)) (combo/combinations [1 1 2] 2))
    (t/is= '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)) (combo/combinations [1 2 3]))
    (t/is= '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)) (combo/combinations '(1 2 3)))
    (t/is= '(() (1) (3) (2) (1 3) (1 2) (3 2) (1 3 2)) (combo/combinations #{1 2 3}))
    (t/is= '(() (1) (1) (2) (1 1) (1 2) (1 2) (1 1 2)) (combo/combinations [1 1 2]))))

(deftest combinations-with-complements-test
  (t/with-instrument `combo/combinations-with-complements
    (t/is (t/spec-check combo/combinations-with-complements)))
  (t/with-instrument :all
    (t/is= '((() (1 2 3))
             ((1) (2 3))
             ((2) (1 3))
             ((3) (1 2))
             ((1 2) (3))
             ((1 3) (2))
             ((2 3) (1))
             ((1 2 3) ()))
      (combo/combinations-with-complements [1 2 3]))
    (t/is= '((() (1 2 3))
             ((1) (2 3))
             ((2) (1 3))
             ((3) (1 2))
             ((1 2) (3))
             ((1 3) (2))
             ((2 3) (1))
             ((1 2 3) ()))
      (combo/combinations-with-complements '(1 2 3)))
    (t/is= '((() (1 3 2))
             ((1) (3 2))
             ((3) (1 2))
             ((2) (1 3))
             ((1 3) (2))
             ((1 2) (3))
             ((3 2) (1))
             ((1 3 2) ()))
      (combo/combinations-with-complements #{1 2 3}))
    (t/is= '((() (1 1 2))
             ((1) (1 2))
             ((1) (1 2))
             ((2) (1 1))
             ((1 1) (2))
             ((1 2) (1))
             ((1 2) (1))
             ((1 1 2) ()))
      (combo/combinations-with-complements [1 1 2]))
    (t/is= '(((1 2) (3)) ((1 3) (2)) ((2 3) (1))) (combo/combinations-with-complements [1 2 3] 2))
    (t/is= '(((1 2) (3)) ((1 3) (2)) ((2 3) (1))) (combo/combinations-with-complements '(1 2 3) 2))
    (t/is= '(((1 3) (2)) ((1 2) (3)) ((3 2) (1))) (combo/combinations-with-complements #{1 2 3} 2))
    (t/is= '(((1 1) (2)) ((1 2) (1)) ((1 2) (1))) (combo/combinations-with-complements [1 1 2] 2))
    (t/is= '((() (1 2 3))) (combo/combinations-with-complements [1 2 3] 0))))

(deftest combinations-using-all-test
  (t/with-instrument `combo/combinations-using-all
    (t/is (t/spec-check combo/combinations-using-all)))
  (t/with-instrument :all
    (t/is= '(((1 2) (3 4))
             ((1 3) (2 4))
             ((1 4) (2 3))
             ((2 3) (1 4))
             ((2 4) (1 3))
             ((3 4) (1 2)))
      (combo/combinations-using-all [1 2 3 4] [2 2]))
    (t/is= '(((1 2) (3 4))
             ((1 3) (2 4))
             ((1 4) (2 3))
             ((2 3) (1 4))
             ((2 4) (1 3))
             ((3 4) (1 2)))
      (combo/combinations-using-all '(1 2 3 4) [2 2]))
    (t/is= '(((1 4) (3 2))
             ((1 3) (4 2))
             ((1 2) (4 3))
             ((4 3) (1 2))
             ((4 2) (1 3))
             ((3 2) (1 4)))
      (combo/combinations-using-all #{1 2 3 4} [2 2]))
    (t/is= '(((1 1) (2 3))
             ((1 2) (1 3))
             ((1 3) (1 2))
             ((1 2) (1 3))
             ((1 3) (1 2))
             ((2 3) (1 1)))
      (combo/combinations-using-all [1 1 2 3] [2 2]))
    (t/is= '(((1 2 3) (4))
             ((1 2 4) (3))
             ((1 3 4) (2))
             ((2 3 4) (1)))
      (combo/combinations-using-all [1 2 3 4] [3 1]))))

(deftest distinct-combinations-with-replacement-test
  (t/with-instrument `combo/distinct-combinations-with-replacement
    (t/is (t/spec-check combo/distinct-combinations-with-replacement)))
  (t/with-instrument :all
    (t/is= '(())
      (combo/distinct-combinations-with-replacement [1 2 3 4] 0))
    (t/is= '(() (1) (2) (3) (4))
      (combo/distinct-combinations-with-replacement [1 2 3 4] 1))
    (t/is= '(() (1) (2) (3) (4) (1 1) (1 2) (1 3) (1 4) (2 2) (2 3) (2 4) (3 3) (3 4) (4 4))
      (combo/distinct-combinations-with-replacement [1 2 3 4] 2))
    (t/is= '(() (1) (2) (3) (4) (1 1) (1 2) (1 3) (1 4) (2 2) (2 3) (2 4) (3 3) (3 4) (4 4))
      (combo/distinct-combinations-with-replacement '(1 2 3 4) 2))
    (t/is= '(() (1) (4) (3) (2) (1 1) (1 4) (1 3) (1 2) (4 4) (4 3) (4 2) (3 3) (3 2) (2 2))
      (combo/distinct-combinations-with-replacement #{1 2 3 4} 2))
    (t/is= '(() (1) (2) (3) (1 1) (1 2) (1 3) (2 2) (2 3) (3 3))
      (combo/distinct-combinations-with-replacement [1 1 2 3] 2))
    (t/is= '(() (1) (2) (1 1) (1 2) (2 2) (1 1 1) (1 1 2) (1 2 2) (2 2 2))
      (combo/distinct-combinations-with-replacement [1 2] 3))))

;;;ORDERED COMBINATIONS
(deftest permutations-test
  (t/with-instrument `combo/permutations
    (t/is (t/spec-check combo/permutations)))
  (t/with-instrument :all
    (t/is= '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)) (combo/permutations '(1 2 3)))
    (t/is= '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)) (combo/permutations [1 2 3]))
    (t/is= '((1 3 2) (1 2 3) (3 1 2) (3 2 1) (2 1 3) (2 3 1)) (combo/permutations #{1 2 3}))
    (t/is= '((1 1 2) (1 2 1) (1 1 2) (1 2 1) (2 1 1) (2 1 1)) (combo/permutations [1 1 2]))))

(deftest cartesian-product-test
  (t/with-instrument `combo/cartesian-product
    (t/is (t/spec-check combo/cartesian-product)))
  (t/with-instrument :all
    (t/is= '() (combo/cartesian-product '((1 2)) '()))
    (t/is= '((1 8.0) (1 9.0) (2 8.0) (2 9.0) (3 8.0) (3 9.0))
      (combo/cartesian-product [1 2 3] [8.0 9.0]))
    (t/is= '((1 8.0) (1 9.0) (2 8.0) (2 9.0) (3 8.0) (3 9.0))
      (combo/cartesian-product '(1 2 3) [8.0 9.0]))
    (t/is= '((1 8.0) (1 9.0) (1 8.0) (1 9.0) (2 8.0) (2 9.0))
      (combo/cartesian-product [1 1 2] [8.0 9.0]))))

(deftest selections-test
  (t/with-instrument `combo/selections
    (t/is (t/spec-check combo/selections)))
  (t/with-instrument :all
    (t/is= '(()) (combo/selections [1 2 3] 0))
    (t/is= '((1) (2) (3)) (combo/selections [1 2 3] 1))
    (t/is= '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3)) (combo/selections [1 2 3] 2))
    (t/is= '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3)) (combo/selections '(1 2 3) 2))
    (t/is= '((1 1) (1 3) (1 2) (3 1) (3 3) (3 2) (2 1) (2 3) (2 2)) (combo/selections #{1 2 3} 2))
    (t/is= '((1 1) (1 1) (1 2) (1 1) (1 1) (1 2) (2 1) (2 1) (2 2)) (combo/selections [1 1 2] 2))))
