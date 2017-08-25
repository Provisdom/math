(ns provisdom.test.math.combinatorics
  (:require [midje.sweet :refer :all]
            [criterium.core :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.combinatorics :refer :all]
            [provisdom.math [core :as m]]
            [provisdom.utility-belt.core :as co]))

(facts "choose"

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
             (stirling-number-of-the-second-kind 200.0 12) => 1.4318980615233435E207)
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
             (binomial-probability 1 1.4 0.4) => (test-roughly 0.45650814137931667 1e-14)
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
       (def itemsb '(1 2 3))
       (def itemsc #{1 2 3})
       (def items2 [1 1 2])
       (def items3 [1 2 3 4])
       (def items3b '(1 2 3 4))
       (def items3c #{1 2 3 4})
       (def items4 [1 1 2 3])
       (fact "combinations"
             (combinations items -1) => (throws)
             (combinations items 0) => '(())
             (combinations items 2.0) => '((1 2) (1 3) (2 3))
             (combinations itemsb 2.0) => '((1 2) (1 3) (2 3))
             (combinations itemsc 2.0) => '((1 3) (1 2) (3 2))
             (combinations items2 2.0) => '((1 1) (1 2) (1 2))
             (combinations items) => '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))
             (combinations itemsb) => '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))
             (combinations itemsc) => '(() (1) (3) (2) (1 3) (1 2) (3 2) (1 3 2))
             (combinations items2) => '(() (1) (1) (2) (1 1) (1 2) (1 2) (1 1 2)))
       (fact "with complements"
             (combinations-with-complements items)
             => '((() (1 2 3)) ((1) (2 3)) ((2) (1 3)) ((3) (1 2)) ((1 2) (3)) ((1 3) (2)) ((2 3) (1)) ((1 2 3) ()))
             (combinations-with-complements itemsb)
             => '((() (1 2 3)) ((1) (2 3)) ((2) (1 3)) ((3) (1 2)) ((1 2) (3)) ((1 3) (2)) ((2 3) (1)) ((1 2 3) ()))
             (combinations-with-complements itemsc)
             => '((() (1 3 2)) ((1) (3 2)) ((3) (1 2)) ((2) (1 3)) ((1 3) (2)) ((1 2) (3)) ((3 2) (1)) ((1 3 2) ()))
             (combinations-with-complements items2)
             => '((() (1 1 2)) ((1) (1 2)) ((1) (1 2)) ((2) (1 1)) ((1 1) (2)) ((1 2) (1)) ((1 2) (1)) ((1 1 2) ()))
             (combinations-with-complements items 2.0) => '(((1 2) (3)) ((1 3) (2)) ((2 3) (1)))
             (combinations-with-complements itemsb 2.0) => '(((1 2) (3)) ((1 3) (2)) ((2 3) (1)))
             (combinations-with-complements itemsc 2.0) => '(((1 3) (2)) ((1 2) (3)) ((3 2) (1)))
             (combinations-with-complements items2 2.0) => '(((1 1) (2)) ((1 2) (1)) ((1 2) (1)))
             (combinations-with-complements items 0) => '((() (1 2 3)))
             (combinations-with-complements items -1) => (throws))
       (fact "using all"
             (combinations-using-all items3 [2 2])
             => '(((1 2) (3 4)) ((1 3) (2 4)) ((1 4) (2 3)) ((2 3) (1 4)) ((2 4) (1 3)) ((3 4) (1 2)))
             (combinations-using-all items3b [2 2])
             => '(((1 2) (3 4)) ((1 3) (2 4)) ((1 4) (2 3)) ((2 3) (1 4)) ((2 4) (1 3)) ((3 4) (1 2)))
             (combinations-using-all items3c [2 2])
             => '(((1 4) (3 2)) ((1 3) (4 2)) ((1 2) (4 3)) ((4 3) (1 2)) ((4 2) (1 3)) ((3 2) (1 4)))
             (combinations-using-all items4 [2 2])
             => '(((1 1) (2 3)) ((1 2) (1 3)) ((1 3) (1 2)) ((1 2) (1 3)) ((1 3) (1 2)) ((2 3) (1 1)))
             (combinations-using-all items3 [3 1])
             => '(((1 2 3) (4)) ((1 2 4) (3)) ((1 3 4) (2)) ((2 3 4) (1)))
             (combinations-using-all items3 [2 1]) => (throws))
       (fact "distinct with replacement"
             (distinct-combinations-with-replacement items3 -1) => (throws)
             (distinct-combinations-with-replacement items3 0) => '(())
             (distinct-combinations-with-replacement items3 1) => '(() (1) (2) (3) (4)) 0.0
             (distinct-combinations-with-replacement items3 2)
             => '(() (1) (2) (3) (4) (1 2) (1 3) (1 4) (1 1) (2 3) (2 4) (2 2) (3 4) (3 3) (4 4))
             (distinct-combinations-with-replacement items3b 2)
             => '(() (1) (2) (3) (4) (1 2) (1 3) (1 4) (1 1) (2 3) (2 4) (2 2) (3 4) (3 3) (4 4))
             (distinct-combinations-with-replacement items3c 2)
             => '(() (1) (4) (3) (2) (1 4) (1 3) (1 2) (1 1) (3 4) (2 4) (4 4) (2 3) (3 3) (2 2))
             (distinct-combinations-with-replacement items4 2)
             => '(() (1) (2) (3) (1 1) (1 2) (1 3) (2 3) (2 2) (3 3))
             (distinct-combinations-with-replacement [1 2] 3)
             => '(() (1) (2) (1 2) (1 1) (2 2) (1 1 2) (1 2 2) (1 1 1) (2 2 2))))

(facts "with ordering"
       (fact "permutations"
             (permutations items) => '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
             (permutations itemsb) => '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
             (permutations itemsc) => '((1 3 2) (1 2 3) (3 1 2) (3 2 1) (2 1 3) (2 3 1))
             (permutations items2) => '((1 1 2) (1 2 1) (1 1 2) (1 2 1) (2 1 1) (2 1 1)))
       (fact "cartesian product"
             (cartesian-product '(1 2) '()) => '()
             (cartesian-product items [8.0 9.0]) => '((1 8.0) (1 9.0) (2 8.0) (2 9.0) (3 8.0) (3 9.0))
             (cartesian-product itemsb [8.0 9.0]) => '((1 8.0) (1 9.0) (2 8.0) (2 9.0) (3 8.0) (3 9.0))
             (cartesian-product itemsc [8.0 9.0]) => '((1 8.0) (1 9.0) (3 8.0) (3 9.0) (2 8.0) (2 9.0))
             (cartesian-product items2 [8.0 9.0]) => '((1 8.0) (1 9.0) (1 8.0) (1 9.0) (2 8.0) (2 9.0)))
       (fact "selections"
             (selections items -1) => (throws)
             (selections items 0) => '(())
             (selections items 1) => '((1) (2) (3))
             (selections items 2.0) => '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))
             (selections itemsb 2.0) => '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))
             (selections itemsc 2.0) => '((1 1) (1 3) (1 2) (3 1) (3 3) (3 2) (2 1) (2 3) (2 2))
             (selections items2 2.0) => '((1 1) (1 1) (1 2) (1 1) (1 1) (1 2) (2 1) (2 1) (2 2))))

