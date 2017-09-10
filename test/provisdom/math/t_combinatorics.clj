(ns provisdom.math.t-combinatorics
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.combinatorics :as combo]
    [provisdom.math.core :as m]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

(ost/instrument)

(set! *warn-on-reflection* true)

;;;FACTORIALS
(deftest factorial-test
  (is= 1.0 (combo/factorial 0))
  (is= 0.9513507698668734 (combo/factorial 0.1))
  (is= 0.8862269254527579 (combo/factorial 0.5))
  (is= 0.9617658319073873 (combo/factorial 0.9))
  (is= 1.0 (combo/factorial 1))
  (is= 1.329340388179137 (combo/factorial 1.5))
  (is= 2.0 (combo/factorial 2.0))
  (is= 5.109094217170944E19 (combo/factorial 21))
  (is= 1.1240007277776077E21 (combo/factorial 22))
  (is= 2.585201673888498E22 (combo/factorial 23)))

(deftest factorial'-test
  (is= 1 (combo/factorial' 0)))

(deftest log-factorial-test
  (is= 0.0 (combo/log-factorial 0))
  (is= -0.049872441259839764 (combo/log-factorial 0.1))
  (is= -0.1207822376352452 (combo/log-factorial 0.5))
  (is= -0.03898427592308336 (combo/log-factorial 0.9))
  (is= 0.0 (combo/log-factorial 1))
  (is= 0.2846828704729192 (combo/log-factorial 1.5))
  (is= 0.6931471805599453 (combo/log-factorial 2.0))
  (is= 51.60667556776437 (combo/log-factorial 23)))

(deftest subfactorial-test
  (is= 1 (combo/subfactorial 0))
  (is= 0 (combo/subfactorial 0.1))
  (is= 0 (combo/subfactorial 0.5))
  (is= 0 (combo/subfactorial 0.9))
  (is= 0 (combo/subfactorial 1))
  (is= 0 (combo/subfactorial 1.5))
  (is= 1 (combo/subfactorial 2.0))
  (is= 895014631192902121 (combo/subfactorial 20))
  (is= 18795307255050944540N (combo/subfactorial 21))
  (is= 4.134967596111208E20 (combo/subfactorial 22))
  (is= 9.510425471055779E21 (combo/subfactorial 23)))

(defspec-test test-factorial `combo/factorial)
(defspec-test test-log-factorial `combo/log-factorial)
(defspec-test test-subfactorial `combo/subfactorial)

;;;CHOOSING
(deftest choose-k-from-n-test
  (is= 0.0 (combo/choose-k-from-n -1 1))
  (is= 1.0 (combo/choose-k-from-n 0 1))
  (is= 1.0 (combo/choose-k-from-n 0 0))
  (is= 1.0 (combo/choose-k-from-n 0 -1))
  (is= 0.9 (combo/choose-k-from-n 1 0.9))
  (is= 0.0 (combo/choose-k-from-n 1 0))
  (is= 1.0 (combo/choose-k-from-n 1 1))
  (is= 1.4 (combo/choose-k-from-n 1 1.4))
  (is= 0.2799999999999999 (combo/choose-k-from-n 2 1.4))
  (is= 4.0 (combo/choose-k-from-n 1.0 4))
  (is= 10.0 (combo/choose-k-from-n 2 5))
  (is= 1.2689769520640436E24 (combo/choose-k-from-n 12 545.0)))

(deftest choose-k-from-n'-test
  (is= 4 (combo/choose-k-from-n' 1.0 4)))

(deftest log-choose-k-from-n-test
  (is= 0.0 (combo/log-choose-k-from-n 0 1))
  (is= 0.0 (combo/log-choose-k-from-n 0 0))
  (is= 0.0 (combo/log-choose-k-from-n 1 1))
  (is= 0.33647223662121284 (combo/log-choose-k-from-n 1 1.4))
  (is= 1.3862943611198908 (combo/log-choose-k-from-n 1.0 4))
  (is= 2.3025850929940455 (combo/log-choose-k-from-n 2 5))
  (is= 55.50025325814249 (combo/log-choose-k-from-n 12 545.0)))

(deftest stirling-number-of-the-second-kind-test
  (is= 0.0 (combo/stirling-number-of-the-second-kind 0 1))
  (is= 1.0 (combo/stirling-number-of-the-second-kind 0 0))
  (is= 1.0 (combo/stirling-number-of-the-second-kind 1 1))
  (is= 1.0 (combo/stirling-number-of-the-second-kind 1 4.0))
  (is= 15.0 (combo/stirling-number-of-the-second-kind 2.0 5))
  (is= 1.4318980615233435E207 (combo/stirling-number-of-the-second-kind 12 200.0)))

(deftest stirling-number-of-the-second-kind'-test
  (is= 0 (combo/stirling-number-of-the-second-kind' 0 1)))

(deftest bell-number-test
  (is= 1 (combo/bell-number 0))
  (is= 1 (combo/bell-number 1))
  (is= 2 (combo/bell-number 2.0))
  (is= 52 (combo/bell-number 5))
  (is= 49631246523618756274N (combo/bell-number 26))
  (is= 5.4571704793605997E20 (combo/bell-number 27))
  (is= 6.160539404599935E21 (combo/bell-number 28.0)))

(deftest binomial-probability-test
  (is= 1.0 (combo/binomial-probability 0 0 0.4))
  (is= 0.4 (combo/binomial-probability 1 1 0.4))
  (is= 0.45650814137931667 (combo/binomial-probability 1 1.4 0.4))
  (is= 0.34559999999999996 (combo/binomial-probability 1.0 4 0.4))
  (is= 0.3456 (combo/binomial-probability 2 5 0.4))
  (is= 1.210013134840654E-99 (combo/binomial-probability 12 545.0 0.4))
  (is= 0.0 (combo/binomial-probability 12 24 0.0))
  (is= 0.0 (combo/binomial-probability 12 24 1.0)))

(deftest log-binomial-probability-test
  (is= 0.0 (combo/log-binomial-probability 0 0 0.4))
  (is= -0.916290731874155 (combo/log-binomial-probability 1 1 0.4))
  (is= -0.7841487447593384 (combo/log-binomial-probability 1 1.4 0.4))
  (is= -1.0624732420522363 (combo/log-binomial-probability 1.0 4 0.4))
  (is= -1.0624732420522367 (combo/log-binomial-probability 2 5 0.4))
  (is= -227.7652929916204 (combo/log-binomial-probability 12 545.0 0.4))
  (is= m/inf- (combo/log-binomial-probability 12 24 0.0))
  (is= m/inf- (combo/log-binomial-probability 12 24 1.0)))

;(defspec-test test-choose-k-from-n `combo/choose-k-from-n) ;slow-ish
;(defspec-test test-choose-k-from-n' `combo/choose-k-from-n') ;slow-ish
(defspec-test test-log-choose-k-from-n `combo/log-choose-k-from-n)
(defspec-test test-stirling-number-of-the-second-kind `combo/stirling-number-of-the-second-kind)
(defspec-test test-stirling-number-of-the-second-kind' `combo/stirling-number-of-the-second-kind')
(defspec-test test-bell-number `combo/bell-number)
;(defspec-test test-binomial-probability `combo/binomial-probability) ;slow-ish
(defspec-test test-log-binomial-probability `combo/log-binomial-probability)

;;;UNORDERED COMBINATIONS
(deftest combinations-test
  (is= '(()) (combo/combinations [1 2 3] 0))
  (is= '((1 2) (1 3) (2 3)) (combo/combinations [1 2 3] 2))
  (is= '((1 2) (1 3) (2 3)) (combo/combinations '(1 2 3) 2))
  (is= '((1 3) (1 2) (3 2)) (combo/combinations #{1 2 3} 2))
  (is= '((1 1) (1 2) (1 2)) (combo/combinations [1 1 2] 2))
  (is= '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)) (combo/combinations [1 2 3]))
  (is= '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)) (combo/combinations '(1 2 3)))
  (is= '(() (1) (3) (2) (1 3) (1 2) (3 2) (1 3 2)) (combo/combinations #{1 2 3}))
  (is= '(() (1) (1) (2) (1 1) (1 2) (1 2) (1 1 2)) (combo/combinations [1 1 2])))

(deftest combinations-with-complements-test
  (is= '((() (1 2 3))
          ((1) (2 3))
          ((2) (1 3))
          ((3) (1 2))
          ((1 2) (3))
          ((1 3) (2))
          ((2 3) (1))
          ((1 2 3) ()))
       (combo/combinations-with-complements [1 2 3]))
  (is= '((() (1 2 3))
          ((1) (2 3))
          ((2) (1 3))
          ((3) (1 2))
          ((1 2) (3))
          ((1 3) (2))
          ((2 3) (1))
          ((1 2 3) ()))
       (combo/combinations-with-complements '(1 2 3)))
  (is= '((() (1 3 2))
          ((1) (3 2))
          ((3) (1 2))
          ((2) (1 3))
          ((1 3) (2))
          ((1 2) (3))
          ((3 2) (1))
          ((1 3 2) ()))
       (combo/combinations-with-complements #{1 2 3}))
  (is= '((() (1 1 2))
          ((1) (1 2))
          ((1) (1 2))
          ((2) (1 1))
          ((1 1) (2))
          ((1 2) (1))
          ((1 2) (1))
          ((1 1 2) ()))
       (combo/combinations-with-complements [1 1 2]))
  (is= '(((1 2) (3)) ((1 3) (2)) ((2 3) (1)))
       (combo/combinations-with-complements [1 2 3] 2))
  (is= '(((1 2) (3)) ((1 3) (2)) ((2 3) (1)))
       (combo/combinations-with-complements '(1 2 3) 2))
  (is= '(((1 3) (2)) ((1 2) (3)) ((3 2) (1)))
       (combo/combinations-with-complements #{1 2 3} 2))
  (is= '(((1 1) (2)) ((1 2) (1)) ((1 2) (1)))
       (combo/combinations-with-complements [1 1 2] 2))
  (is= '((() (1 2 3))) (combo/combinations-with-complements [1 2 3] 0)))


(deftest combinations-using-all-test
  (is= '(((1 2) (3 4))
          ((1 3) (2 4))
          ((1 4) (2 3))
          ((2 3) (1 4))
          ((2 4) (1 3))
          ((3 4) (1 2)))
       (combo/combinations-using-all [1 2 3 4] [2 2]))
  (is= '(((1 2) (3 4))
          ((1 3) (2 4))
          ((1 4) (2 3))
          ((2 3) (1 4))
          ((2 4) (1 3))
          ((3 4) (1 2)))
       (combo/combinations-using-all '(1 2 3 4) [2 2]))
  (is= '(((1 4) (3 2))
          ((1 3) (4 2))
          ((1 2) (4 3))
          ((4 3) (1 2))
          ((4 2) (1 3))
          ((3 2) (1 4)))
       (combo/combinations-using-all #{1 2 3 4} [2 2]))
  (is= '(((1 1) (2 3))
          ((1 2) (1 3))
          ((1 3) (1 2))
          ((1 2) (1 3))
          ((1 3) (1 2))
          ((2 3) (1 1)))
       (combo/combinations-using-all [1 1 2 3] [2 2]))
  (is= '(((1 2 3) (4))
          ((1 2 4) (3))
          ((1 3 4) (2))
          ((2 3 4) (1)))
       (combo/combinations-using-all [1 2 3 4] [3 1])))

(deftest distinct-combinations-with-replacement-test
  (is= '(())
       (combo/distinct-combinations-with-replacement [1 2 3 4] 0))
  (is= '(() (1) (2) (3) (4))
       (combo/distinct-combinations-with-replacement [1 2 3 4] 1))
  (is= '(() (1) (2) (3) (4) (1 1) (1 2) (1 3) (1 4) (2 2) (2 3) (2 4) (3 3) (3 4) (4 4))
       (combo/distinct-combinations-with-replacement [1 2 3 4] 2))
  (is= '(() (1) (2) (3) (4) (1 1) (1 2) (1 3) (1 4) (2 2) (2 3) (2 4) (3 3) (3 4) (4 4))
       (combo/distinct-combinations-with-replacement '(1 2 3 4) 2))
  (is= '(() (1) (4) (3) (2) (1 1) (1 4) (1 3) (1 2) (4 4) (4 3) (4 2) (3 3) (3 2) (2 2))
       (combo/distinct-combinations-with-replacement #{1 2 3 4} 2))
  (is= '(() (1) (2) (3) (1 1) (1 2) (1 3) (2 2) (2 3) (3 3))
       (combo/distinct-combinations-with-replacement [1 1 2 3] 2))
  (is= '(() (1) (2) (1 1) (1 2) (2 2) (1 1 1) (1 1 2) (1 2 2) (2 2 2))
       (combo/distinct-combinations-with-replacement [1 2] 3)))

;(defspec-test test-combinations `combo/combinations)        ;slow
;(defspec-test test-combinations-with-complements `combo/combinations-with-complements) ;slow
(defspec-test test-combinations-using-all `combo/combinations-using-all)
; (defspec-test test-distinct-combinations-with-replacement `combo/distinct-combinations-with-replacement) ;slow

;;;ORDERED COMBINATIONS
(deftest permutations-test
  (is= '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
       (combo/permutations '(1 2 3)))
  (is= '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
       (combo/permutations [1 2 3]))
  (is= '((1 3 2) (1 2 3) (3 1 2) (3 2 1) (2 1 3) (2 3 1))
       (combo/permutations #{1 2 3}))
  (is= '((1 1 2) (1 2 1) (1 1 2) (1 2 1) (2 1 1) (2 1 1))
       (combo/permutations [1 1 2])))

(deftest cartesian-product-test
  (is= '() (combo/cartesian-product '((1 2)) '()))
  (is= '((1 8.0) (1 9.0) (2 8.0) (2 9.0) (3 8.0) (3 9.0))
       (combo/cartesian-product [1 2 3] [8.0 9.0]))
  (is= '((1 8.0) (1 9.0) (2 8.0) (2 9.0) (3 8.0) (3 9.0))
       (combo/cartesian-product '(1 2 3) [8.0 9.0]))
  (is= '((1 8.0) (1 9.0) (1 8.0) (1 9.0) (2 8.0) (2 9.0))
       (combo/cartesian-product [1 1 2] [8.0 9.0])))

(deftest selections-test
  (is= '(()) (combo/selections [1 2 3] 0))
  (is= '((1) (2) (3)) (combo/selections [1 2 3] 1))
  (is= '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))
       (combo/selections [1 2 3] 2))
  (is= '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))
       (combo/selections '(1 2 3) 2))
  (is= '((1 1) (1 3) (1 2) (3 1) (3 3) (3 2) (2 1) (2 3) (2 2))
       (combo/selections #{1 2 3} 2))
  (is= '((1 1) (1 1) (1 2) (1 1) (1 1) (1 2) (2 1) (2 1) (2 2))
       (combo/selections [1 1 2] 2)))

;(defspec-test test-permutations `combo/permutations) ;slow
;(defspec-test test-cartesian-product `combo/cartesian-product) ;slow
;(defspec-test test-selections `combo/selections) ;slow

#_(ost/unstrument)