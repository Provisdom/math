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
    (t/is-spec-check combo/factorial))
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
    (t/is-spec-check combo/factorial'))
  (t/with-instrument :all
    (t/is= 1 (combo/factorial' 0))))

(deftest log-factorial-test
  (t/with-instrument `combo/log-factorial
    (t/is-spec-check combo/log-factorial))
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
    (t/is-spec-check combo/subfactorial))
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
    (t/is-spec-check combo/choose-k-from-n))
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
    (t/is-spec-check combo/choose-k-from-n'
            {:num-tests 50}))
  (t/with-instrument :all
    (t/is= 4 (combo/choose-k-from-n' 1 4))))

(deftest log-choose-k-from-n-test
  (t/with-instrument `combo/log-choose-k-from-n
    (t/is-spec-check combo/log-choose-k-from-n))
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
    (t/is-spec-check combo/stirling-number-of-the-second-kind))
  (t/with-instrument :all
    (t/is= 0.0 (combo/stirling-number-of-the-second-kind 0 1))
    (t/is= 1.0 (combo/stirling-number-of-the-second-kind 0 0))
    (t/is= 1.0 (combo/stirling-number-of-the-second-kind 1 1))
    (t/is= 1.0 (combo/stirling-number-of-the-second-kind 1 4))
    (t/is= 15.0 (combo/stirling-number-of-the-second-kind 2 5))
    (t/is= 1.4318980615233435E207 (combo/stirling-number-of-the-second-kind 12 200))))

(deftest stirling-number-of-the-second-kind'-test
  (t/with-instrument `combo/stirling-number-of-the-second-kind'
    (t/is-spec-check combo/stirling-number-of-the-second-kind'))
  (t/with-instrument :all
    (t/is= 0 (combo/stirling-number-of-the-second-kind' 0 1))))

(deftest bell-number-test
  (t/with-instrument `combo/bell-number
    (t/is-spec-check combo/bell-number))
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
    (t/is-spec-check combo/binomial-probability))
  (t/with-instrument :all
    (t/is= 1.0 (combo/binomial-probability 0 0 0.4))
    (t/is= 0.4 (combo/binomial-probability 1 1 0.4))
    (t/is= 0.48 (combo/binomial-probability 1 2 0.4))
    (t/is= 0.34559999999999996 (combo/binomial-probability 1 4 0.4))
    (t/is= 0.3456 (combo/binomial-probability 2 5 0.4))
    (t/is= 1.2100131348406543E-99 (combo/binomial-probability 12 545 0.4))))

(deftest log-binomial-probability-test
  (t/with-instrument `combo/log-binomial-probability
    (t/is-spec-check combo/log-binomial-probability))
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
    (t/is-spec-check combo/combinations))
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
    (t/is-spec-check combo/combinations-with-complements))
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
    (t/is-spec-check combo/combinations-using-all))
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
    (t/is-spec-check combo/distinct-combinations-with-replacement))
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
    (t/is-spec-check combo/permutations))
  (t/with-instrument :all
    (t/is= '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)) (combo/permutations '(1 2 3)))
    (t/is= '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)) (combo/permutations [1 2 3]))
    (t/is= '((1 3 2) (1 2 3) (3 1 2) (3 2 1) (2 1 3) (2 3 1)) (combo/permutations #{1 2 3}))
    (t/is= '((1 1 2) (1 2 1) (1 1 2) (1 2 1) (2 1 1) (2 1 1)) (combo/permutations [1 1 2]))))

(deftest cartesian-product-test
  (t/with-instrument `combo/cartesian-product
    (t/is-spec-check combo/cartesian-product))
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
    (t/is-spec-check combo/selections))
  (t/with-instrument :all
    (t/is= '(()) (combo/selections [1 2 3] 0))
    (t/is= '((1) (2) (3)) (combo/selections [1 2 3] 1))
    (t/is= '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3)) (combo/selections [1 2 3] 2))
    (t/is= '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3)) (combo/selections '(1 2 3) 2))
    (t/is= '((1 1) (1 3) (1 2) (3 1) (3 3) (3 2) (2 1) (2 3) (2 2)) (combo/selections #{1 2 3} 2))
    (t/is= '((1 1) (1 1) (1 2) (1 1) (1 1) (1 2) (2 1) (2 1) (2 2)) (combo/selections [1 1 2] 2))))

;;;DOUBLE FACTORIALS
(deftest double-factorial-test
  (t/with-instrument `combo/double-factorial
    (t/is-spec-check combo/double-factorial))
  (t/with-instrument :all
    (t/is= 1.0 (combo/double-factorial 0))
    (t/is= 1.0 (combo/double-factorial 1))
    (t/is= 2.0 (combo/double-factorial 2))
    (t/is= 3.0 (combo/double-factorial 3))
    (t/is= 8.0 (combo/double-factorial 4))
    (t/is= 15.0 (combo/double-factorial 5))
    (t/is= 48.0 (combo/double-factorial 6))
    (t/is= 105.0 (combo/double-factorial 7))
    (t/is= 1.0 (combo/double-factorial -1))))

(deftest double-factorial'-test
  (t/with-instrument `combo/double-factorial'
    (t/is-spec-check combo/double-factorial'))
  (t/with-instrument :all
    (t/is= 1 (combo/double-factorial' 0))
    (t/is= 105 (combo/double-factorial' 7))))

;;;POCHHAMMER SYMBOLS
(deftest rising-factorial-test
  (t/with-instrument `combo/rising-factorial
    (t/is-spec-check combo/rising-factorial))
  (t/with-instrument :all
    (t/is= 1.0 (combo/rising-factorial 5 0))
    (t/is= 5.0 (combo/rising-factorial 5 1))
    (t/is= 30.0 (combo/rising-factorial 5 2))     ;; 5 × 6
    (t/is= 360.0 (combo/rising-factorial 3 4))    ;; 3 × 4 × 5 × 6
    (t/is= 120.0 (combo/rising-factorial 1 5))    ;; same as 5!
    (t/is= 1.875 (combo/rising-factorial 0.5 3)))) ;; 0.5 × 1.5 × 2.5

(deftest falling-factorial-test
  (t/with-instrument `combo/falling-factorial
    (t/is-spec-check combo/falling-factorial))
  (t/with-instrument :all
    (t/is= 1.0 (combo/falling-factorial 5 0))
    (t/is= 5.0 (combo/falling-factorial 5 1))
    (t/is= 20.0 (combo/falling-factorial 5 2))    ;; 5 × 4
    (t/is= 60.0 (combo/falling-factorial 5 3))    ;; 5 × 4 × 3
    (t/is= 120.0 (combo/falling-factorial 5 5))   ;; same as 5!
    (t/is= 8.75 (combo/falling-factorial 3.5 2)))) ;; 3.5 × 2.5

;;;MULTINOMIAL COEFFICIENTS
(deftest multinomial-coefficient-test
  (t/with-instrument `combo/multinomial-coefficient
    (t/is-spec-check combo/multinomial-coefficient))
  (t/with-instrument :all
    (t/is= 1.0 (combo/multinomial-coefficient [3]))
    (t/is= 6.0 (combo/multinomial-coefficient [2 2]))     ;; same as C(4,2)
    (t/is= 60.0 (combo/multinomial-coefficient [2 3 1]))  ;; 6!/(2!3!1!)
    (t/is= 6.0 (combo/multinomial-coefficient [1 1 1])))) ;; 3!

(deftest log-multinomial-coefficient-test
  (t/with-instrument `combo/log-multinomial-coefficient
    (t/is-spec-check combo/log-multinomial-coefficient))
  (t/with-instrument :all
    (t/is= 0.0 (combo/log-multinomial-coefficient [3]))
    (t/is-approx= 4.0943445622221 (combo/log-multinomial-coefficient [2 3 1]) :tolerance 1e-10)))

;;;STIRLING NUMBERS
(deftest log-stirling-number-of-the-second-kind-test
  (t/with-instrument `combo/log-stirling-number-of-the-second-kind
    (t/is-spec-check combo/log-stirling-number-of-the-second-kind))
  (t/with-instrument :all
    (t/is= 0.0 (combo/log-stirling-number-of-the-second-kind 0 0))
    (t/is= 0.0 (combo/log-stirling-number-of-the-second-kind 5 5))
    (t/is-approx= 2.708050201102210 (combo/log-stirling-number-of-the-second-kind 2 5) :tolerance 1e-10))) ;; ln(15)

(deftest stirling-number-of-the-first-kind-test
  (t/with-instrument `combo/stirling-number-of-the-first-kind
    (t/is-spec-check combo/stirling-number-of-the-first-kind))
  (t/with-instrument :all
    (t/is= 1.0 (combo/stirling-number-of-the-first-kind 0 0))
    (t/is= 0.0 (combo/stirling-number-of-the-first-kind 0 3))
    (t/is= 1.0 (combo/stirling-number-of-the-first-kind 4 4))
    (t/is= 6.0 (combo/stirling-number-of-the-first-kind 1 4))    ;; (4-1)! = 6
    (t/is= 11.0 (combo/stirling-number-of-the-first-kind 2 4))
    (t/is= 6.0 (combo/stirling-number-of-the-first-kind 3 4))))

(deftest stirling-number-of-the-first-kind'-test
  (t/with-instrument `combo/stirling-number-of-the-first-kind'
    (t/is-spec-check combo/stirling-number-of-the-first-kind'))
  (t/with-instrument :all
    (t/is= 1 (combo/stirling-number-of-the-first-kind' 0 0))
    (t/is= 6 (combo/stirling-number-of-the-first-kind' 1 4))))

;;;CATALAN NUMBERS
(deftest catalan-number-test
  (t/with-instrument `combo/catalan-number
    (t/is-spec-check combo/catalan-number))
  (t/with-instrument :all
    (t/is= 1.0 (combo/catalan-number 0))
    (t/is= 1.0 (combo/catalan-number 1))
    (t/is= 2.0 (combo/catalan-number 2))
    (t/is= 5.0 (combo/catalan-number 3))
    (t/is-approx= 14.0 (combo/catalan-number 4))
    (t/is-approx= 42.0 (combo/catalan-number 5))
    (t/is-approx= 16796.0 (combo/catalan-number 10))))

(deftest catalan-number'-test
  (t/with-instrument `combo/catalan-number'
    (t/is-spec-check combo/catalan-number'))
  (t/with-instrument :all
    (t/is= 1 (combo/catalan-number' 0))
    (t/is= 42 (combo/catalan-number' 5))))

;;;COUNTING FUNCTIONS
(deftest count-combinations-test
  (t/with-instrument `combo/count-combinations
    (t/is-spec-check combo/count-combinations))
  (t/with-instrument :all
    (t/is= 1.0 (combo/count-combinations 0 5))
    (t/is= 10.0 (combo/count-combinations 2 5))
    (t/is= 1.0 (combo/count-combinations 5 5))))

(deftest count-permutations-test
  (t/with-instrument `combo/count-permutations
    (t/is-spec-check combo/count-permutations))
  (t/with-instrument :all
    (t/is= 120.0 (combo/count-permutations 5))
    (t/is= 20.0 (combo/count-permutations 2 5))    ;; P(5,2) = 5 × 4
    (t/is= 120.0 (combo/count-permutations 5 5)))) ;; P(5,5) = 5!

;;;INTEGER PARTITIONS
(deftest integer-partitions-test
  (t/with-instrument `combo/integer-partitions
    (t/is-spec-check combo/integer-partitions {:num-tests 30}))
  (t/with-instrument :all
    (t/is= '(()) (combo/integer-partitions 0))
    (t/is= '((1)) (combo/integer-partitions 1))
    (t/is= '((3) (2 1) (1 1 1)) (combo/integer-partitions 3))
    (t/is= '((4) (3 1) (2 2) (2 1 1) (1 1 1 1)) (combo/integer-partitions 4))
    ;; With max-part constraint
    (t/is= '((2 2) (2 1 1) (1 1 1 1)) (combo/integer-partitions 4 2))
    (t/is= '((3 2) (3 1 1) (2 2 1) (2 1 1 1) (1 1 1 1 1)) (combo/integer-partitions 5 3))))

(deftest count-integer-partitions-test
  (t/with-instrument `combo/count-integer-partitions
    (t/is-spec-check combo/count-integer-partitions {:num-tests 30}))
  (t/with-instrument :all
    (t/is= 1 (combo/count-integer-partitions 0))
    (t/is= 1 (combo/count-integer-partitions 1))
    (t/is= 3 (combo/count-integer-partitions 3))
    (t/is= 5 (combo/count-integer-partitions 4))
    (t/is= 7 (combo/count-integer-partitions 5))
    (t/is= 42 (combo/count-integer-partitions 10))
    (t/is= 190569292 (combo/count-integer-partitions 100))))

;;;K-PERMUTATIONS
(deftest k-permutations-test
  (t/with-instrument `combo/k-permutations
    (t/is-spec-check combo/k-permutations {:num-tests 30}))
  (t/with-instrument :all
    (t/is= '([]) (combo/k-permutations [1 2 3] 0))
    (t/is= '([1] [2] [3]) (combo/k-permutations [1 2 3] 1))
    (t/is= '([1 2] [2 1] [1 3] [3 1] [2 3] [3 2]) (combo/k-permutations [1 2 3] 2))
    (t/is= nil (combo/k-permutations [1 2] 3))))

;;;DIRECT ACCESS
(deftest nth-combination-test
  (t/with-instrument `combo/nth-combination
    (t/is-spec-check combo/nth-combination {:num-tests 30}))
  (t/with-instrument :all
    (t/is= '(1 2) (combo/nth-combination [1 2 3 4 5] 2 0))
    (t/is= '(4 5) (combo/nth-combination [1 2 3 4 5] 2 9))
    (t/is= nil (combo/nth-combination [1 2 3] 2 10))))

(deftest nth-permutation-test
  (t/with-instrument `combo/nth-permutation
    (t/is-spec-check combo/nth-permutation {:num-tests 30}))
  (t/with-instrument :all
    (t/is= '(1 2 3) (combo/nth-permutation [1 2 3] 0))
    (t/is= '(3 2 1) (combo/nth-permutation [1 2 3] 5))
    (t/is= nil (combo/nth-permutation [1 2 3] 6))))

(deftest nth-k-permutation-test
  (t/with-instrument `combo/nth-k-permutation
    (t/is-spec-check combo/nth-k-permutation {:num-tests 30}))
  (t/with-instrument :all
    (t/is= '(1 2) (combo/nth-k-permutation [1 2 3 4] 2 0))
    (t/is= '(4 3) (combo/nth-k-permutation [1 2 3 4] 2 11))
    (t/is= nil (combo/nth-k-permutation [1 2 3] 2 10))))
