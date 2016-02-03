(ns provisdom.test.math.combinatorics
  (:require [midje.sweet :refer :all]
            [criterium.core :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.combinatorics :refer :all]
            [provisdom.math [core :as m]]
            [provisdom.utility-belt.core :as co]))

(facts "factorials"
       (fact "factorial"
             (factorial -0.1) => (throws)
             (factorial 0) => 1
             (factorial 0.1) => 0.9513507698668734
             (factorial 0.5) => 0.8862269254527579
             (factorial 0.9) => 0.9617658319073873
             (factorial 1) => 1
             (factorial 1.5) => 1.329340388179137
             (factorial 2.0) => 2
             (factorial 21) => 5.109094217170944E19
             (factorial 22) => 1.1240007277776077E21
             (factorial 23) => 2.585201673888498E22)
       (fact "log factorial"
             (log-factorial -0.1) => (throws)
             (log-factorial 0) => 0.0
             (log-factorial 0.1) => -0.049872441259839764
             (log-factorial 0.5) => -0.1207822376352452
             (log-factorial 0.9) => -0.03898427592308336
             (log-factorial 1) => 0.0
             (log-factorial 1.5) => 0.2846828704729192
             (log-factorial 2.0) => 0.6931471805599453
             (log-factorial 23) => 51.60667556776437)
       (fact "subfactorial"
             (subfactorial -0.1) => (throws)
             (subfactorial 0) => 1
             (subfactorial 0.1) => 0
             (subfactorial 0.5) => 0
             (subfactorial 0.9) => 0
             (subfactorial 1) => 0
             (subfactorial 1.5) => 0
             (subfactorial 2.0) => 1
             (subfactorial 20) => 895014631192902121
             (subfactorial 21) => 18795307255050944540N
             (subfactorial 22) => 4.134967596111208E20
             (subfactorial 23) => 9.510425471055779E21))

(facts "choose"
       (fact "choose k from n"
             (choose-k-from-n -1 1) => 0
             (choose-k-from-n 0 1) => 1
             (choose-k-from-n 0 0) => 1
             (choose-k-from-n 0 -1) => 1
             (choose-k-from-n 1 0.9) => 0.9
             (choose-k-from-n 1 0) => 0
             (choose-k-from-n 1 1) => 1
             (choose-k-from-n 1.4 1) => (throws)
             (choose-k-from-n 1 1.4) => 1.4
             (choose-k-from-n 2 1.4) => 0.2799999999999999
             (choose-k-from-n 1.0 4) => 4
             (choose-k-from-n 2 5) => 10
             (choose-k-from-n 12 545.0) => 1.2689769520640436E24)
       (fact "log choose k from n"
             (log-choose-k-from-n -1 1) => (throws)
             (log-choose-k-from-n 0 1) => 0.0
             (log-choose-k-from-n 0 0) => 0.0
             (log-choose-k-from-n 0 -1) => (throws)
             (log-choose-k-from-n 1 0.9) => (throws)
             (log-choose-k-from-n 1 0) => (throws)
             (log-choose-k-from-n 1 1) => 0.0
             (log-choose-k-from-n 1.4 1) => (throws)
             (log-choose-k-from-n 1 1.4) => 0.33647223662121284
             (log-choose-k-from-n 2 1.4) => (throws)
             (log-choose-k-from-n 1.0 4) => 1.3862943611198908
             (log-choose-k-from-n 2 5) => 2.3025850929940455
             (log-choose-k-from-n 12 545.0) => 55.50025325814249)
       (fact "stirling number of the second kind"
             (stirling-number-of-the-second-kind 1 -1) => (throws)
             (stirling-number-of-the-second-kind 1 0) => 0
             (stirling-number-of-the-second-kind 0 0) => 1
             (stirling-number-of-the-second-kind -1 0) => (throws)
             (stirling-number-of-the-second-kind 0.9 1) => (throws)
             (stirling-number-of-the-second-kind 0 1) => 0
             (stirling-number-of-the-second-kind 1 1) => 1
             (stirling-number-of-the-second-kind 1 1.4) => (throws)
             (stirling-number-of-the-second-kind 4.0 1) => 1
             (stirling-number-of-the-second-kind 5 2.0) => 15
             (stirling-number-of-the-second-kind 200.0 12)
             => 1.4318980615233435E207)
       (fact "bell number"
             (bell-number -1) => (throws)
             (bell-number 0) => 1
             (bell-number 0.5) => (throws)
             (bell-number 1) => 1
             (bell-number 2.0) => 2
             (bell-number 5) => 52
             (bell-number 26) => 49631246523618756274N
             (bell-number 27) => 5.4571704793605997E20
             (bell-number 28.0) => 6.160539404599935E21)
       (fact "binomial probability"
             (binomial-probability -1 1 0.5) => 0.0
             (binomial-probability 0 0 0.4) => 1.0
             (binomial-probability 1 1 0.4) => 0.4
             (binomial-probability 1 1.4 0.4)
             => (test-roughly 0.45650814137931667 1e-14)
             (binomial-probability 1.4 2 0.4) => (throws)
             (binomial-probability 2 1.4 0.4) => (throws)
             (binomial-probability 1.0 4 0.4) => 0.34559999999999996
             (binomial-probability 2 5 0.4) => 0.3456
             (binomial-probability 12 545.0 0.4) => 1.210013134840654E-99
             (binomial-probability 12 24 -0.1) => (throws)
             (binomial-probability 12 24 1.1) => (throws)
             (binomial-probability 12 24 0.0) => 0.0
             (binomial-probability 12 24 1.0) => 0.0)
       (fact "log binomial probability"
             (log-binomial-probability -1 1 0.5) => (throws)
             (log-binomial-probability 0 0 0.4) => 0.0
             (log-binomial-probability 1 1 0.4) => -0.916290731874155
             (log-binomial-probability 1 1.4 0.4) => -0.7841487447593384
             (log-binomial-probability 1.4 2 0.4) => (throws)
             (log-binomial-probability 2 1.4 0.4) => (throws)
             (log-binomial-probability 1.0 4 0.4) => -1.0624732420522363
             (log-binomial-probability 2 5 0.4) => -1.0624732420522367
             (log-binomial-probability 12 545.0 0.4) => -227.7652929916204
             (log-binomial-probability 12 24 -0.1) => (throws)
             (log-binomial-probability 12 24 1.1) => (throws)
             (log-binomial-probability 12 24 0.0) => m/inf-
             (log-binomial-probability 12 24 1.0) => m/inf-))

(facts "combinations"
       (def items [1 2 3])
       (def items2 [1 1 2])
       (fact "basics"
             (selections items -1) => (throws)
             (selections items 0) => '(())
             (selections items 2.0)
             => '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))
             (combinations items -1) => (throws)
             (combinations items 0) => '(())
             (combinations items 2.0) => '((1 2) (1 3) (2 3))
             (subseqs items) => '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))
             (permutations items)
             => '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
             (lex-permutations items)
             => '([1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1])
             (cartesian-product items [8.0 9.0])
             => '((1 8.0) (1 9.0) (2 8.0) (2 9.0) (3 8.0) (3 9.0)))
       (fact "with complements"
             (subseqs-with-complements items)
             => '((() (1 2 3)) ((1) (2 3)) ((2) (1 3)) ((3) (1 2)) ((1 2) (3))
                   ((1 3) (2)) ((2 3) (1)) ((1 2 3) ()))
             (subseqs-with-complements items2)
             => '((() (1 1 2)) ((1) (1 2)) ((1) (1 2)) ((2) (1 1)) ((1 1) (2))
                   ((1 2) (1)) ((1 2) (1)) ((1 1 2) ()))
             (combinations-with-complements items 2.0)
             => '(((1 2) (3)) ((1 3) (2)) ((2 3) (1)))
             (combinations-with-complements items 0) => '((() (1 2 3)))
             (combinations-with-complements items -1) => (throws))
       (fact "combinations using all"
             (combinations-using-all [1 2 3 4] [2 2])
             => '(((1 2) (3 4)) ((1 3) (2 4)) ((1 4) (2 3))
                   ((2 3) (1 4)) ((2 4) (1 3)) ((3 4) (1 2)))
             (combinations-using-all [1 2 3 4] [3 1])
             => '(((1 2 3) (4)) ((1 2 4) (3)) ((1 3 4) (2)) ((2 3 4) (1)))
             (combinations-using-all [1 2 3 4] [2 1]) => (throws))
       (fact "unique unordered"
             (unique-unordered-subseqs-with-replacement [1 2 3 4] -1)
             => (throws)
             (unique-unordered-subseqs-with-replacement [1 2 3 4] 0) => '(())
             (unique-unordered-subseqs-with-replacement [1 2 3 4] 1)
             => '(() (1) (2) (3) (4)) 0.0
             (unique-unordered-subseqs-with-replacement [1 2 3 4] 2)
             => '(() (1) (2) (3) (4) (1 2) (1 3) (1 4) (1 1) (2 3) (2 4) (2 2)
                   (3 4) (3 3) (4 4))
             (unique-unordered-subseqs-with-replacement [1 2] 3)
             => '(() (1) (2) (1 2) (1 1) (2 2) (1 1 2) (1 2 2) (1 1 1) (2 2 2))
             (unique-unordered-combinations-using-all [1 2 3 4] 0) => (throws)
             (unique-unordered-combinations-using-all [1 2 3 4] 3)
             => (throws)                                    ;must be able to partition items into size n
             (unique-unordered-combinations-using-all [1 2 3 4] 1)
             => '(((1) (2) (3) (4)))
             (unique-unordered-combinations-using-all [1 2 3 4] 2)
             => '(((1 2) (3 4)) ((1 3) (2 4)) ((1 4) (2 3)))))

