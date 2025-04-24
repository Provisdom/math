(ns provisdom.math.core-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.core :as m]))

;;1 seconds

(set! *warn-on-reflection* true)

;;TYPE TESTS
(deftest numbers?-test
  (with-instrument `m/numbers?
    (is (spec-check m/numbers?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/numbers? 1))
    (is (m/numbers? '(1)))
    (is (m/numbers? []))
    (is (m/numbers? [2 3]))
    (is-not (m/numbers? [[2]]))))

(deftest num?-test
  (with-instrument `m/num?
    (is (spec-check m/num?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/num? "A"))
    (is (m/num? 3.3E30))
    (is (m/num? -3.3E30))
    (is (m/num? m/inf+))
    (is (m/num? m/inf-))
    (is-not (m/num? m/nan))))

(deftest nan?-test
  (with-instrument `m/nan?
    (is (spec-check m/nan?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/nan? 3.3))
    (is-not (m/nan? "A"))
    (is-not (m/nan? m/inf+))
    (is-not (m/nan? m/inf-))
    (is (m/nan? m/nan))))

(deftest pos?-test
  (with-instrument `m/pos?
    (is (spec-check m/pos?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/pos? "A"))
    (is (m/pos? 3.3E30))
    (is-not (m/pos? -3.3E30))
    (is (m/pos? m/inf+))
    (is-not (m/pos? m/inf-))
    (is-not (m/pos? m/nan))))

(deftest neg?-test
  (with-instrument `m/neg?
    (is (spec-check m/neg?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/neg? "A"))
    (is-not (m/neg? 3.3E30))
    (is (m/neg? -3.3E30))
    (is-not (m/neg? m/inf+))
    (is (m/neg? m/inf-))
    (is-not (m/neg? m/nan))))

(deftest non-?-test
  (with-instrument `m/non-?
    (is (spec-check m/non-?)))
  (with-instrument (st/instrumentable-syms)
    (is (m/non-? 0))
    (is (m/non-? 1))
    (is-not (m/non-? -1))
    (is (m/non-? m/inf+))
    (is-not (m/non-? m/inf-))
    (is-not (m/non-? m/nan))
    (is-not (m/non-? "A"))))

(deftest non+?-test
  (with-instrument `m/non+?
    (is (spec-check m/non+?)))
  (with-instrument (st/instrumentable-syms)
    (is (m/non+? 0))
    (is-not (m/non+? 1))
    (is (m/non+? -1))
    (is-not (m/non+? m/inf+))
    (is (m/non+? m/inf-))
    (is-not (m/non+? m/nan))
    (is-not (m/non+? "A"))))

(deftest finite?-test
  (with-instrument `m/finite?
    (is (spec-check m/finite?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/finite? "A"))
    (is (m/finite? 3.3E30))
    (is (m/finite? -3.3E30))
    (is-not (m/finite? m/inf+))
    (is-not (m/finite? m/inf-))
    (is-not (m/finite? m/nan))))

(deftest finite+?-test
  (with-instrument `m/finite+?
    (is (spec-check m/finite+?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/finite+? "A"))
    (is (m/finite+? 3.3E30))
    (is-not (m/finite+? -3.3E30))
    (is-not (m/finite+? m/inf+))
    (is-not (m/finite+? m/inf-))
    (is-not (m/finite+? m/nan))))

(deftest finite-?-test
  (with-instrument `m/finite-?
    (is (spec-check m/finite-?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/finite-? "A"))
    (is-not (m/finite-? 3.3E30))
    (is (m/finite-? -3.3E30))
    (is-not (m/finite-? m/inf+))
    (is-not (m/finite-? m/inf-))
    (is-not (m/finite-? m/nan))))

(deftest finite-non-?-test
  (with-instrument `m/finite-non-?
    (is (spec-check m/finite-non-?)))
  (with-instrument (st/instrumentable-syms)
    (is (m/finite-non-? 0))
    (is (m/finite-non-? 1))
    (is-not (m/finite-non-? -1))
    (is-not (m/finite-non-? m/inf+))
    (is-not (m/finite-non-? m/inf-))
    (is-not (m/finite-non-? m/nan))
    (is-not (m/finite-non-? "A"))))

(deftest finite-non+?-test
  (with-instrument `m/finite-non+?
    (is (spec-check m/finite-non+?)))
  (with-instrument (st/instrumentable-syms)
    (is (m/finite-non+? 0))
    (is-not (m/finite-non+? 1))
    (is (m/finite-non+? -1))
    (is-not (m/finite-non+? m/inf+))
    (is-not (m/finite-non+? m/inf-))
    (is-not (m/finite-non+? m/nan))
    (is-not (m/finite-non+? "A"))))

(deftest double-finite?-test
  (with-instrument `m/double-finite?
    (is (spec-check m/double-finite?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/double-finite? "A"))
    (is-not (m/double-finite? 0))
    (is (m/double-finite? 0.0))
    (is (m/double-finite? 3.3E300))
    (is (m/double-finite? -3.3E300))
    (is-not (m/double-finite? m/inf+))
    (is-not (m/double-finite? m/inf-))
    (is-not (m/double-finite? m/nan))))

(deftest double-finite+?-test
  (with-instrument `m/double-finite+?
    (is (spec-check m/double-finite+?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/double-finite? "A"))
    (is-not (m/double-finite? 0))
    (is (m/double-finite? 0.0))
    (is (m/double-finite? 3.3E300))
    (is (m/double-finite? -3.3E300))
    (is-not (m/double-finite? m/inf+))
    (is-not (m/double-finite? m/inf-))
    (is-not (m/double-finite? m/nan))))

(deftest single?-test
  (with-instrument `m/single?
    (is (spec-check m/single?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/single? "A"))
    (is-not (m/single? 0))
    (is (m/single? 0.0))
    (is (m/single? 3.3E30))
    (is (m/single? -3.3E30))
    (is-not (m/single? 3.3E300))
    (is-not (m/single? -3.3E300))
    (is (m/single? m/inf+))
    (is (m/single? m/inf-))
    (is (m/single? m/nan))))

(deftest single-finite?-test
  (with-instrument `m/single-finite?
    (is (spec-check m/single-finite?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/single-finite? "A"))
    (is-not (m/single-finite? 0))
    (is (m/single-finite? 0.0))
    (is (m/single-finite? 3.3E30))
    (is (m/single-finite? -3.3E30))
    (is-not (m/single-finite? 3.3E300))
    (is-not (m/single-finite? -3.3E300))
    (is-not (m/single-finite? m/inf+))
    (is-not (m/single-finite? m/inf-))
    (is-not (m/single-finite? m/nan))))

(deftest long?-test
  (with-instrument `m/long?
    (is (spec-check m/long?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/long? 3.3))
    (is (m/long? 3))
    (is-not (m/long? 3.0))
    (is-not (m/long? "A"))
    (is-not (m/long? 3.4E15))
    (is-not (m/long? 3.3E30))
    (is-not (m/long? -3.3E30))
    (is-not (m/long? m/nan))))

(deftest long+?-test
  (with-instrument `m/long+?
    (is (spec-check m/long+?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/long+? 3.3))
    (is (m/long+? 3))
    (is-not (m/long+? -3))
    (is-not (m/long+? 3.0))
    (is-not (m/long+? "A"))
    (is-not (m/long+? 3.4E15))
    (is-not (m/long+? 3.3E30))
    (is-not (m/long+? -3.3E30))
    (is-not (m/long+? m/nan))))

(deftest long-?-test
  (with-instrument `m/long-?
    (is (spec-check m/long-?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/long-? 3.3))
    (is-not (m/long-? 3))
    (is (m/long-? -3))
    (is-not (m/long-? 3.0))
    (is-not (m/long-? "A"))
    (is-not (m/long-? -3.4E15))
    (is-not (m/long-? 3.3E30))
    (is-not (m/long-? -3.3E30))
    (is-not (m/long-? m/nan))))

(deftest long-non-?-test
  (with-instrument `m/long-non-?
    (is (spec-check m/long-non-?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/long-non-? 3.3))
    (is (m/long-non-? 3))
    (is-not (m/long-non-? -3))
    (is (m/long-non-? 0))
    (is-not (m/long-non-? 3.0))
    (is-not (m/long-non-? "A"))
    (is-not (m/long-non-? 3.4E15))
    (is-not (m/long-non-? 3.3E30))
    (is-not (m/long-non-? -3.3E30))
    (is-not (m/long-non-? m/nan))))

(deftest long-non+?-test
  (with-instrument `m/long-non+?
    (is (spec-check m/long-non+?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/long-non+? 3.3))
    (is-not (m/long-non+? 3))
    (is (m/long-non+? -3))
    (is (m/long-non+? 0))
    (is-not (m/long-non+? 3.0))
    (is-not (m/long-non+? "A"))
    (is-not (m/long-non+? 3.4E15))
    (is-not (m/long-non+? 3.3E30))
    (is-not (m/long-non+? -3.3E30))
    (is-not (m/long-non+? m/nan))))

(deftest int?-test
  (with-instrument `m/int?
    (is (spec-check m/int?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/int? 3.3))
    (is (m/int? 3))
    (is-not (m/int? 3.0))
    (is-not (m/int? "A"))
    (is-not (m/int? 3.4E15))
    (is-not (m/int? 3.3E30))
    (is-not (m/int? -3.3E30))
    (is-not (m/int? m/nan))))

(deftest int+?-test
  (with-instrument `m/int+?
    (is (spec-check m/int+?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/int+? 3.3))
    (is (m/int+? 3))
    (is-not (m/int+? -3))
    (is-not (m/int+? 3.0))
    (is-not (m/int+? "A"))
    (is-not (m/int+? 3.4E15))
    (is-not (m/int+? 3.3E30))
    (is-not (m/int+? -3.3E30))
    (is-not (m/int+? m/nan))))

(deftest int-?-test
  (with-instrument `m/int-?
    (is (spec-check m/int-?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/int-? 3.3))
    (is-not (m/int-? 3))
    (is (m/int-? -3))
    (is-not (m/int-? 3.0))
    (is-not (m/int-? "A"))
    (is-not (m/int-? -3.4E15))
    (is-not (m/int-? 3.3E30))
    (is-not (m/int-? -3.3E30))
    (is-not (m/int-? m/nan))))

(deftest int-non-?-test
  (with-instrument `m/int-non-?
    (is (spec-check m/int-non-?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/int-non-? 3.3))
    (is (m/int-non-? 3))
    (is-not (m/int-non-? -3))
    (is (m/int-non-? 0))
    (is-not (m/int-non-? 3.0))
    (is-not (m/int-non-? "A"))
    (is-not (m/int-non-? 3.4E15))
    (is-not (m/int-non-? 3.3E30))
    (is-not (m/int-non-? -3.3E30))
    (is-not (m/int-non-? m/nan))))

(deftest int-non+?-test
  (with-instrument `m/int-non+?
    (is (spec-check m/int-non+?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/int-non+? 3.3))
    (is-not (m/int-non+? 3))
    (is (m/int-non+? -3))
    (is (m/int-non+? 0))
    (is-not (m/int-non+? 3.0))
    (is-not (m/int-non+? "A"))
    (is-not (m/int-non+? 3.4E15))
    (is-not (m/int-non+? 3.3E30))
    (is-not (m/int-non+? -3.3E30))
    (is-not (m/int-non+? m/nan))))

(deftest long-able?-test
  (with-instrument `m/long-able?
    (is (spec-check m/long-able?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/long-able? 3.3))
    (is (m/long-able? 3))
    (is (m/long-able? 3.0))
    (is-not (m/long-able? "A"))
    (is (m/long-able? 3.4E15))
    (is-not (m/long-able? 3.3E30))
    (is-not (m/long-able? -3.3E30))
    (is-not (m/long-able? m/nan))))

(deftest long-able+?-test
  (with-instrument `m/long-able+?
    (is (spec-check m/long-able+?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/long-able+? 3.3))
    (is-not (m/long-able+? 0.0))
    (is (m/long-able+? 1.0))
    (is-not (m/long-able+? -1.0))
    (is-not (m/long-able+? -3.3))))

(deftest long-able-?-test
  (with-instrument `m/long-able-?
    (is (spec-check m/long-able-?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/long-able-? 3.3))
    (is-not (m/long-able-? 0.0))
    (is-not (m/long-able-? 1.0))
    (is (m/long-able-? -1.0))
    (is-not (m/long-able-? -3.3))))

(deftest long-able-non+?-test
  (with-instrument `m/long-able-non+?
    (is (spec-check m/long-able-non+?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/long-able-non+? 3.3))
    (is (m/long-able-non+? 0.0))
    (is-not (m/long-able-non+? 1.0))
    (is (m/long-able-non+? -1.0))
    (is-not (m/long-able-non+? -3.3))))

(deftest long-able-non-?-test
  (with-instrument `m/long-able-non-?
    (is (spec-check m/long-able-non-?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/long-able-non-? 3.3))
    (is (m/long-able-non-? 0.0))
    (is (m/long-able-non-? 1.0))
    (is-not (m/long-able-non-? -1.0))
    (is-not (m/long-able-non-? -3.3))))

(deftest inf+?-test
  (with-instrument `m/inf+?
    (is (spec-check m/inf+?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/inf+? 3.3))
    (is-not (m/inf+? "A"))
    (is (m/inf+? m/inf+))
    (is-not (m/inf+? m/inf-))
    (is-not (m/inf+? m/nan))))

(deftest inf-?-test
  (with-instrument `m/inf-?
    (is (spec-check m/inf-?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/inf-? 3.3))
    (is-not (m/inf-? "A"))
    (is-not (m/inf-? m/inf+))
    (is (m/inf-? m/inf-))
    (is-not (m/inf-? m/nan))))

(deftest inf?-test
  (with-instrument `m/inf?
    (is (spec-check m/inf?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/inf? 3.3))
    (is-not (m/inf? "A"))
    (is (m/inf? m/inf+))
    (is (m/inf? m/inf-))
    (is-not (m/inf? m/nan))))

(deftest one?-test
  (with-instrument `m/one?
    (is (spec-check m/one?)))
  (with-instrument (st/instrumentable-syms)
    (is (m/one? 1))
    (is (m/one? 1.0))
    (is-not (m/one? "A"))
    (is-not (m/one? -1))
    (is-not (m/one? m/nan))))

(deftest prob?-test
  (with-instrument `m/prob?
    (is (spec-check m/prob?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/prob? -0.5))
    (is (m/prob? 0))
    (is (m/prob? 0.5))
    (is (m/prob? 1))
    (is-not (m/prob? 1.5))
    (is-not (m/prob? m/inf+))
    (is-not (m/prob? m/inf-))
    (is-not (m/prob? "A"))
    (is-not (m/prob? m/nan))))

(deftest open-prob?-test
  (with-instrument `m/open-prob?
    (is (spec-check m/open-prob?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/open-prob? -0.5))
    (is-not (m/open-prob? 0))
    (is (m/open-prob? 0.5))
    (is-not (m/open-prob? 1))
    (is-not (m/open-prob? 1.5))
    (is-not (m/open-prob? m/inf+))
    (is-not (m/open-prob? m/inf-))
    (is-not (m/open-prob? m/nan))
    (is-not (m/open-prob? "A"))))

(deftest corr?-test
  (with-instrument `m/corr?
    (is (spec-check m/corr?)))
  (with-instrument (st/instrumentable-syms)
    (is (m/corr? -0.5))
    (is (m/corr? 0))
    (is (m/corr? 0.5))
    (is (m/corr? 1))
    (is-not (m/corr? 1.5))
    (is (m/corr? -1))
    (is-not (m/corr? -1.5))
    (is-not (m/corr? m/inf+))
    (is-not (m/corr? m/inf-))
    (is-not (m/corr? m/nan))
    (is-not (m/corr? "A"))))

(deftest open-corr?-test
  (with-instrument `m/open-corr?
    (is (spec-check m/open-corr?)))
  (with-instrument (st/instrumentable-syms)
    (is (m/open-corr? -0.5))
    (is (m/open-corr? 0))
    (is (m/open-corr? 0.5))
    (is-not (m/open-corr? 1))
    (is-not (m/open-corr? 1.5))
    (is-not (m/open-corr? -1))
    (is-not (m/open-corr? -1.5))
    (is-not (m/open-corr? m/inf+))
    (is-not (m/open-corr? m/inf-))
    (is-not (m/open-corr? m/nan))
    (is-not (m/open-corr? "A"))))

(deftest maybe-long-able-test
  (with-instrument `m/maybe-long-able
    (is (spec-check m/maybe-long-able)))
  (with-instrument (st/instrumentable-syms)
    (is (zero? (m/maybe-long-able 0.0)))
    (is (zero? (m/maybe-long-able 0)))
    (is= 23423423 (m/maybe-long-able 2.3423423E7))
    (is= 234234324234234234234N (m/maybe-long-able 234234324234234234234N))
    (is= 2.3423432423423423E20 (m/maybe-long-able 2.3423432423423423E20))
    (is= m/inf+ (m/maybe-long-able m/inf+))
    (is= m/inf- (m/maybe-long-able m/inf-))
    (is (m/nan? (m/maybe-long-able m/nan)))
    (is (nil? (m/maybe-long-able nil)))))

;;BASIC MATH TESTS
(deftest ===-test
  (with-instrument `m/===
    (is (spec-check m/===)))
  (with-instrument (st/instrumentable-syms)
    (is (m/=== m/nan))
    (is (m/=== m/nan m/nan))
    (is (m/=== m/nan m/nan m/nan))
    (is (m/=== 3 3 3 3))
    (is-not (m/=== 3 m/nan))))

(deftest next-up-test
  (with-instrument `m/next-up
    (is (spec-check m/next-up)))
  (with-instrument (st/instrumentable-syms)
    (is= 3.0000000000000004 (m/next-up 3))
    (is= -2.9999999999999996 (m/next-up -3))
    (is (m/nan? (m/next-up m/nan)))
    (is= m/inf+ (m/next-up m/inf+))
    (is= m/min-dbl (m/next-up m/inf-))
    (is= -2.9999999999999996 (m/next-up -3.0))))

(deftest next-down-test
  (with-instrument `m/next-down
    (is (spec-check m/next-down)))
  (with-instrument (st/instrumentable-syms)
    (is= 2.9999999999999996 (m/next-down 3))
    (is= -3.0000000000000004 (m/next-down -3))
    (is (m/nan? (m/next-down m/nan)))
    (is= m/max-dbl (m/next-down m/inf+))
    (is= m/inf- (m/next-down m/inf-))
    (is= 2.9999999999999996 (m/next-down 3.0))))

(deftest div-test
  (with-instrument `m/div
    (is (spec-check m/div)))
  (with-instrument (st/instrumentable-syms)
    (is (ratio? (m/div 4)))
    (is= 0.25 (m/div 4.0))
    (is= 1 (m/div 3 3))
    (is= -1 (m/div -3 3))
    (is (m/nan? (m/div m/nan 0)))
    (is (m/nan? (m/div 0 m/nan)))
    (is= m/inf+ (m/div m/inf+ 0))
    (is= m/inf- (m/div m/inf- 0))
    (is= 0.0 (m/div 0 m/inf+))
    (is= 0.0 (m/div 0 m/inf-))
    (is (m/nan? (m/div 0 0)))
    (is (m/nan? (m/div 0 0 m/nan)))))

(deftest one--test
  (with-instrument `m/one-
    (is (spec-check m/one-)))
  (with-instrument (st/instrumentable-syms)
    (is= -2 (m/one- 3))
    (is= 0.0 (m/one- 3 -2))
    (is= -16.0 (m/one- 3 4 2 8))
    (is= 4 (m/one- -3))
    (is (m/nan? (m/one- m/nan)))
    (is= m/inf- (m/one- m/inf+))
    (is= m/inf+ (m/one- m/inf-))
    (is= 4.0 (m/one- -3.0))))

(deftest sq'-test
  (with-instrument `m/sq'
    (is (spec-check m/sq')))
  (with-instrument (st/instrumentable-syms)
    (is= 9 (m/sq' 3))
    (is= 9 (m/sq' -3))
    (is (m/nan? (m/sq' m/nan)))
    (is= m/inf+ (m/sq' m/inf+))
    (is= m/inf+ (m/sq' m/inf-))
    (is= 9 (m/sq' -3.0))))

(deftest cube'-test
  (with-instrument `m/cube'
    (is (spec-check m/cube')))
  (with-instrument (st/instrumentable-syms)
    (is= 27 (m/cube' 3))
    (is (m/nan? (m/cube' m/nan)))
    (is= m/inf+ (m/cube' m/inf+))
    (is= m/inf- (m/cube' m/inf-))
    (is= -27 (m/cube' -3))
    (is= -27 (m/cube' -3.0))))

(deftest sgn-test
  (with-instrument `m/sgn
    (is (spec-check m/sgn)))
  (with-instrument (st/instrumentable-syms)
    (is= 1 (m/sgn 3))
    (is= -1 (m/sgn -3))
    (is (m/nan? (m/sgn m/nan)))
    (is= 1 (m/sgn m/inf+))
    (is= -1 (m/sgn m/inf-))
    (is (zero? (m/sgn 0)))
    (is (zero? (m/sgn 0.0)))
    (is= -1 (m/sgn -3.0))))

(deftest log2-test
  (with-instrument `m/log2
    (is (spec-check m/log2)))
  (with-instrument (st/instrumentable-syms)
    (is= 1.5849625007211563 (m/log2 3))
    (is= m/inf- (m/log2 0))
    (is= m/inf+ (m/log2 m/inf+))
    (is (m/nan? (m/log2 m/nan)))
    (is (m/nan? (m/log2 -3.0)))
    (is= 0.0 (m/log2 1.0))
    (is= -0.15200309344504997 (m/log2 0.9))))

(deftest logn-test
  (with-instrument `m/logn
    (is (spec-check m/logn)))
  (with-instrument (st/instrumentable-syms)
    (is= 1.0 (m/logn 3 3))
    (is= m/inf- (m/logn 0 3))
    (is= m/inf+ (m/logn m/inf+ 3))
    (is (m/nan? (m/logn -3.0 3)))
    (is (m/nan? (m/logn m/nan 3)))
    (is= 0.0 (m/logn 1.0 3))
    (is= -0.09590327428938458 (m/logn 0.9 3))
    (is= m/inf- (m/logn 0.9 1))
    (is= 0.15200309344504997 (m/logn 0.9 0.5))
    (is= 0.0 (m/logn 0.9 0))
    (is= -0.0 (m/logn 0.9 m/inf+))))

(deftest abs'-test
  (with-instrument `m/abs'
    (is (spec-check m/abs')))
  (with-instrument (st/instrumentable-syms)
    (is= 3.3 (m/abs' -3.3))
    (is= 3 (m/abs' -3))
    (is= 300000000 (m/abs' 3.0E8))
    (is (zero? (m/abs' 0)))
    (is (zero? (m/abs' 0.0)))
    (is= m/inf+ (m/abs' m/inf+))
    (is= m/inf+ (m/abs' m/inf-))
    (is (m/nan? (m/abs' m/nan)))))

(deftest cbrt-test
  (with-instrument `m/cbrt
    (is (spec-check m/cbrt)))
  (with-instrument (st/instrumentable-syms)
    (is= 0.0 (m/cbrt 0.0))
    (is= 1.0 (m/cbrt 1.0))
    (is= -1.0 (m/cbrt -1.0))
    (is= -2.0 (m/cbrt -8))
    (is= m/inf+ (m/cbrt m/inf+))
    (is= m/inf- (m/cbrt m/inf-))
    (is (m/nan? (m/cbrt m/nan)))))

;;TRIGONOMETRY
(deftest sin-test
  (with-instrument `m/sin
    (is (spec-check m/sin))))

(deftest sinh-test
  (with-instrument `m/sinh
    (is (spec-check m/sinh))))

(deftest asin-test
  (with-instrument `m/asin
    (is (spec-check m/asin))))

(deftest asinh-test
  (with-instrument `m/asinh
    (is (spec-check m/asinh)))
  (with-instrument (st/instrumentable-syms)
    (is= 0.0 (m/asinh 0.0))
    (is= 0.48121182505960347 (m/asinh 0.5))
    (is= -0.8813735870195428 (m/asinh -1.0))
    (is= 0.8813735870195429 (m/asinh 1.0))
    (is= -1.4436354751788099 (m/asinh -2.0))
    (is= 1.4436354751788103 (m/asinh 2.0))
    (is= m/inf+ (m/asinh m/inf+))
    (is (m/nan? (m/asinh m/inf-)))
    (is (m/nan? (m/asinh m/nan)))))

(deftest cos-test
  (with-instrument `m/cos
    (is (spec-check m/cos))))

(deftest cosh-test
  (with-instrument `m/cosh
    (is (spec-check m/cosh))))

(deftest acos-test
  (with-instrument `m/acos
    (is (spec-check m/acos))))

(deftest acosh-test
  (with-instrument `m/acosh
    (is (spec-check m/acosh)))
  (with-instrument (st/instrumentable-syms)
    (is (m/nan? (m/acosh 0.0)))
    (is= 0.0 (m/acosh 1.0))
    (is= 1.3169578969248166 (m/acosh 2.0))
    (is= m/inf+ (m/acosh m/inf+))
    (is (m/nan? (m/acosh m/nan)))))

(deftest tan-test
  (with-instrument `m/tan
    (is (spec-check m/tan))))

(deftest tanh-test
  (with-instrument `m/tanh
    (is (spec-check m/tanh))))

(deftest atan-test
  (with-instrument `m/atan
    (is (spec-check m/atan))))

(deftest atan2-test
  (with-instrument `m/atan2
    (is (spec-check m/atan2))))

(deftest atanh-test
  (with-instrument `m/atanh
    (is (spec-check m/atanh)))
  (with-instrument (st/instrumentable-syms)
    (is= 0.0 (m/atanh 0.0))
    (is= -0.2027325540540822 (m/atanh 0.5))
    (is= m/inf- (m/atanh -1.0))
    (is= m/inf+ (m/atanh 1.0))
    (is (m/nan? (m/atanh -2.0)))
    (is (m/nan? (m/atanh m/nan)))))

(deftest hypot-test
  (with-instrument `m/hypot
    (is (spec-check m/hypot))))

;;ROUNDING
(deftest round-test
  (with-instrument `m/round
    (is (spec-check m/round)))
  (with-instrument (st/instrumentable-syms)
    (is= 1 (m/round 0.5 :up))
    (is= 2.342342342342342E22 (m/round 2.342342342342342E22 :up))
    (is (zero? (m/round -0.5 :up)))
    (is= -1 (m/round -0.5 :down))
    (is= -1 (m/round -0.5 :away-from-zero))
    (is (zero? (m/round -0.5 :toward-zero)))
    (is (zero? (m/round 0.5 :down)))
    (is= 1 (m/round 0.5 :away-from-zero))
    (is (zero? (m/round 0.5 :toward-zero)))
    (is= m/inf+ (m/round m/inf+ :up))
    (is= m/inf- (m/round m/inf- :up))
    (is (m/nan? (m/round m/nan :up)))))

(deftest round-significant-test
  (with-instrument `m/round-significant
    (is (spec-check m/round-significant)))
  (with-instrument (st/instrumentable-syms)
    (is= 120.0 (m/round-significant 123.45 2 :down))
    (is= 123.5 (m/round-significant 123.45 4 :up))
    (is= 123.4 (m/round-significant 123.45 4 :down))
    (is= 123.4 (m/round-significant 123.45 4 :toward-zero))
    (is= 123.5 (m/round-significant 123.45 4 :away-from-zero))
    (is= -120.0 (m/round-significant -123.45 2 :down))
    (is= -123.4 (m/round-significant -123.45 4 :up))
    (is= -123.5 (m/round-significant -123.45 4 :down))
    (is= -123.4 (m/round-significant -123.45 4 :toward-zero))
    (is= -123.5 (m/round-significant -123.45 4 :away-from-zero))
    (is= -123.45 (m/round-significant -123.45 10 :toward-zero))
    (is= m/inf+ (m/round-significant m/max-dbl 10 :down))
    (is= 1.79769313E308 (m/round-significant m/max-dbl 9 :down))
    (is= -1.79769313E308 (m/round-significant m/min-dbl 9 :up))
    (is= m/inf- (m/round-significant m/min-dbl 10 :up))
    (is= m/inf+ (m/round-significant m/inf+ 10 :up))
    (is= m/inf- (m/round-significant m/inf- 1 :up))
    (is (m/nan? (m/round-significant m/nan 5 :up)))
    ;;notice these stay tiny-dbl -- due to double limitations
    ;; (i.e., 5.0E-324 = 4.9E-324)
    (is= m/tiny-dbl (m/round-significant m/tiny-dbl 1 :up))
    (is= (- m/tiny-dbl) (m/round-significant (- m/tiny-dbl) 1 :down))))

(deftest floor-test
  (with-instrument `m/floor
    (is (spec-check m/floor)))
  (with-instrument (st/instrumentable-syms)
    (is (zero? (m/floor 0.4)))
    (is= 2.3423423423423425E26 (m/floor 234234234234234234234343242N))
    (is= -1.0 (m/floor -0.4))
    (is= m/inf+ (m/floor m/inf+))
    (is= m/inf- (m/floor m/inf-))
    (is (m/nan? (m/floor m/nan)))))

(deftest floor'-test
  (with-instrument `m/floor'
    (is (spec-check m/floor')))
  (with-instrument (st/instrumentable-syms)
    (is= -1 (m/floor' -0.4))))

(deftest ceil-test
  (with-instrument `m/ceil
    (is (spec-check m/ceil)))
  (with-instrument (st/instrumentable-syms)
    (is= 1.0 (m/ceil 0.4))
    (is= 2.3423423423423425E26 (m/ceil 234234234234234234234343242N))
    (is (zero? (m/ceil -0.4)))
    (is= m/inf+ (m/ceil m/inf+))
    (is= m/inf- (m/ceil m/inf-))
    (is (m/nan? (m/ceil m/nan)))))

(deftest ceil'-test
  (with-instrument `m/ceil'
    (is (spec-check m/ceil')))
  (with-instrument (st/instrumentable-syms)
    (is= 1 (m/ceil' 0.4))))

(deftest roughly-floor-test
  (with-instrument `m/roughly-floor
    (is (spec-check m/roughly-floor)))
  (with-instrument (st/instrumentable-syms)
    (is= 1.0 (m/roughly-floor 0.99 0.02))
    (is (zero? (m/roughly-floor 0.99 0.005)))
    (is= 2.3423423423423425E26
         (m/roughly-floor 234234234234234234234343242N 0.02))
    (is= 2.3423423423423425E26 (m/roughly-floor 2.3423423423423425E26 0.02))
    (is (zero? (m/roughly-floor -0.01 0.02)))
    (is= m/inf+ (m/roughly-floor m/inf+ 0.02))
    (is= m/inf- (m/roughly-floor m/inf- 0.02))
    (is (m/nan? (m/roughly-floor m/nan 0.02)))))

(deftest roughly-floor'-test
  (with-instrument `m/roughly-floor'
    (is (spec-check m/roughly-floor')))
  (with-instrument (st/instrumentable-syms)
    (is= 1 (m/roughly-floor' 0.99 0.02))))

(deftest roughly-ceil-test
  (with-instrument `m/roughly-ceil
    (is (spec-check m/roughly-ceil)))
  (with-instrument (st/instrumentable-syms)
    (is (zero? (m/roughly-ceil 0.01 0.02)))
    (is= 1.0 (m/roughly-ceil 0.01 0.005))
    (is= 2.3423423423423425E26
         (m/roughly-ceil 234234234234234234234343242N 0.02))
    (is= 2.3423423423423425E26 (m/roughly-ceil 2.3423423423423425E26 0.02))
    (is= -1.0 (m/roughly-ceil -0.99 0.02))
    (is= m/inf+ (m/roughly-ceil m/inf+ 0.02))
    (is= m/inf- (m/roughly-ceil m/inf- 0.02))
    (is (m/nan? (m/roughly-ceil m/nan 0.02)))))

(deftest roughly-ceil'-test
  (with-instrument `m/roughly-ceil'
    (is (spec-check m/roughly-ceil')))
  (with-instrument (st/instrumentable-syms)
    (is= 1 (m/roughly-ceil' 0.01 0.005))))

(deftest roughly?-test
  (with-instrument `m/roughly?
    (is (spec-check m/roughly?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/roughly? 0.01 0.02 0.005))
    (is (m/roughly? 0.01 0.02 0.01))
    (is (m/roughly? 0.01 0.02 0.02))
    (is (m/roughly? 2.3423423423423425E26 2.3423423423423425E26 0.03))
    (is (m/roughly? 2.3423423423423425E26 2.3423423423423425E26 0.005))
    (is-not (m/roughly? m/inf+ m/inf+ 0.01))
    (is (m/roughly? m/inf- 0.02 m/inf+))
    (is-not (m/roughly? m/nan 0.02 0.01))))

(deftest roughly-round?-test
  (with-instrument `m/roughly-round?
    (is (spec-check m/roughly-round?)))
  (with-instrument (st/instrumentable-syms)
    (is (m/roughly-round? 0.01 0.02))
    (is-not (m/roughly-round? 0.01 0.005))
    (is (m/roughly-round? 2.3423423423423425E26 0.03))
    (is (m/roughly-round? 2.3423423423423425E26 0.005))
    (is (m/roughly-round? m/inf+ m/inf+))
    (is-not (m/roughly-round? m/inf- 0.4))
    (is-not (m/roughly-round? m/nan 0.01))))

(deftest roughly-round-non-?-test
  (with-instrument `m/roughly-round-non-?
    (is (spec-check m/roughly-round-non-?)))
  (with-instrument (st/instrumentable-syms)
    (is (m/roughly-round-non-? 0 0.02))
    (is-not (m/roughly-round-non-? -0.01 0.02))
    (is (m/roughly-round-non-? 0.01 0.02))
    (is-not (m/roughly-round-non-? 0.01 0.005))
    (is (m/roughly-round-non-? 2.3423423423423425E26 0.03))
    (is (m/roughly-round-non-? 2.3423423423423425E26 0.005))
    (is (m/roughly-round-non-? m/inf+ m/inf+))
    (is-not (m/roughly-round-non-? m/inf- m/inf+))
    (is-not (m/roughly-round-non-? m/inf+ 0.4))
    (is-not (m/roughly-round-non-? m/nan 0.01))))

(deftest roughly-round-non+?-test
  (with-instrument `m/roughly-round-non+?
    (is (spec-check m/roughly-round-non+?)))
  (with-instrument (st/instrumentable-syms)
    (is (m/roughly-round-non+? 0 0.02))
    (is-not (m/roughly-round-non+? 0.01 0.02))
    (is (m/roughly-round-non+? -0.01 0.02))
    (is-not (m/roughly-round-non+? -0.01 0.005))
    (is (m/roughly-round-non+? -2.3423423423423425E26 0.03))
    (is (m/roughly-round-non+? -2.3423423423423425E26 0.005))
    (is-not (m/roughly-round-non+? m/inf+ m/inf+))
    (is (m/roughly-round-non+? m/inf- m/inf+))
    (is-not (m/roughly-round-non+? m/inf- 0.4))
    (is-not (m/roughly-round-non+? m/nan 0.01))))

(deftest roughly-round+?-test
  (with-instrument `m/roughly-round+?
    (is (spec-check m/roughly-round+?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/roughly-round+? 0 0.02))
    (is-not (m/roughly-round+? -0.01 0.02))
    (is (m/roughly-round+? 0.01 0.02))
    (is-not (m/roughly-round+? 0.01 0.005))
    (is (m/roughly-round+? 2.3423423423423425E26 0.03))
    (is (m/roughly-round+? 2.3423423423423425E26 0.005))
    (is (m/roughly-round+? m/inf+ m/inf+))
    (is-not (m/roughly-round+? m/inf- m/inf+))
    (is-not (m/roughly-round+? m/inf+ 0.4))
    (is-not (m/roughly-round+? m/nan 0.01))))

(deftest roughly-round-?-test
  (with-instrument `m/roughly-round-?
    (is (spec-check m/roughly-round-?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/roughly-round-? 0 0.02))
    (is-not (m/roughly-round-? 0.01 0.02))
    (is (m/roughly-round-? -0.01 0.02))
    (is-not (m/roughly-round-? -0.01 0.005))
    (is (m/roughly-round-? -2.3423423423423425E26 0.03))
    (is (m/roughly-round-? -2.3423423423423425E26 0.005))
    (is-not (m/roughly-round-? m/inf+ m/inf+))
    (is (m/roughly-round-? m/inf- m/inf+))
    (is-not (m/roughly-round-? m/inf- 0.4))
    (is-not (m/roughly-round-? m/nan 0.01))))

(deftest roughly-non-?-test
  (with-instrument `m/roughly-non-?
    (is (spec-check m/roughly-non-?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/roughly-non-? -0.01 0.005))
    (is (m/roughly-non-? -0.02 0.02))
    (is (m/roughly-non-? 0.01 0.001))
    (is (m/roughly-non-? m/inf+ m/inf+))
    (is (m/roughly-non-? m/inf- m/inf+))
    (is-not (m/roughly-non-? m/inf- 0.4))
    (is (m/roughly-non-? m/inf+ 0.4))
    (is-not (m/roughly-non-? m/nan 0.01))))

(deftest roughly-non+?-test
  (with-instrument `m/roughly-non+?
    (is (spec-check m/roughly-non+?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/roughly-non+? 0.01 0.005))
    (is (m/roughly-non+? 0.02 0.02))
    (is (m/roughly-non+? -0.01 0.001))
    (is (m/roughly-non+? m/inf+ m/inf+))
    (is (m/roughly-non+? m/inf- m/inf+))
    (is (m/roughly-non+? m/inf- 0.4))
    (is-not (m/roughly-non+? m/inf+ 0.4))
    (is-not (m/roughly-non+? m/nan 0.01))))

(deftest roughly-prob?-test
  (with-instrument `m/roughly-prob?
    (is (spec-check m/roughly-prob?)))
  (with-instrument (st/instrumentable-syms)
    (is (m/roughly-prob? 0.01 0.005))
    (is (m/roughly-prob? 0.02 0.02))
    (is-not (m/roughly-prob? -0.01 0.001))
    (is (m/roughly-prob? 1.01 0.01))
    (is (m/roughly-prob? 1.01 0.01))
    (is (m/roughly-prob? m/inf+ m/inf+))
    (is (m/roughly-prob? m/inf- m/inf+))
    (is-not (m/roughly-prob? m/inf- 0.4))
    (is-not (m/roughly-prob? m/inf+ 0.4))
    (is-not (m/roughly-prob? m/nan 0.01))))

(deftest roughly-corr?-test
  (with-instrument `m/roughly-corr?
    (is (spec-check m/roughly-corr?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (m/roughly-corr? -1.01 0.005))
    (is (m/roughly-corr? -1.02 0.02))
    (is-not (m/roughly-corr? -1.01 0.001))
    (is (m/roughly-corr? 1.01 0.01))
    (is (m/roughly-corr? 1.01 0.01))
    (is (m/roughly-corr? m/inf+ m/inf+))
    (is (m/roughly-corr? m/inf- m/inf+))
    (is-not (m/roughly-corr? m/inf- 0.4))
    (is-not (m/roughly-corr? m/inf+ 0.4))
    (is-not (m/roughly-corr? m/nan 0.01))))

;;;QUOTIENTS
(deftest quot'-test
  (with-instrument `m/quot'
    (is (spec-check m/quot')))
  (with-instrument (st/instrumentable-syms)
    (is= 1 (m/quot' 3 2))
    (is= -1 (m/quot' -3 2))
    (is= -1 (m/quot' 3 -2))
    (is= 1 (m/quot' -3 -2))
    (is= 1 (m/quot' 3.0 2))
    (is= 1 (m/quot' 3 2.0))
    (is= 1 (m/quot' 3 2.12))
    (is= 1.4150943396226415E40 (m/quot' 3.0E40 2.12))
    (is (m/nan? (m/quot' m/inf+ 3)))
    (is (m/nan? (m/quot' m/inf- 4)))
    (is (m/nan? (m/quot' m/inf+ -3)))
    (is (m/nan? (m/quot' m/inf- -4)))
    (is (m/nan? (m/quot' m/nan 2)))
    (is (m/nan? (m/quot' 3 m/inf+)))
    (is (m/nan? (m/quot' 4 m/inf-)))
    (is (m/nan? (m/quot' 2 m/nan)))))

(deftest mod'-test
  (with-instrument `m/mod'
    (is (spec-check m/mod')))
  (with-instrument (st/instrumentable-syms)
    (is= 1 (m/mod' 3 2))
    (is= 1 (m/mod' -3 2))
    (is= -1 (m/mod' 3 -2))
    (is= -1 (m/mod' -3 -2))
    (is= 1 (m/mod' 3.0 2))
    (is= 1 (m/mod' 3 2.0))
    (is= 0.8799999999999999 (m/mod' 3 2.12))
    (is (zero? (m/mod' 3.0E40 2.12)))
    (is (m/nan? (m/mod' m/inf+ 3)))
    (is (m/nan? (m/mod' m/inf- 4)))
    (is (m/nan? (m/mod' m/nan 2)))
    (is (m/nan? (m/mod' 3 m/inf+)))
    (is (m/nan? (m/mod' 4 m/inf-)))
    (is (m/nan? (m/mod' -3 m/inf+)))
    (is (m/nan? (m/mod' -4 m/inf-)))
    (is (m/nan? (m/mod' 2 m/nan)))))

(deftest rem'-test
  (with-instrument `m/rem'
    (is (spec-check m/rem')))
  (with-instrument (st/instrumentable-syms)
    (is= 1 (m/rem' 3 2))
    (is= -1 (m/rem' -3 2))
    (is= 1 (m/rem' 3 -2))
    (is= -1 (m/rem' -3 -2))
    (is= 1 (m/rem' 3.0 2))
    (is= 1 (m/rem' 3 2.0))
    (is= 0.8799999999999999 (m/rem' 3 2.12))
    (is (zero? (m/rem' 3.0E40 2.12)))
    (is (m/nan? (m/rem' m/inf+ 3)))
    (is (m/nan? (m/rem' m/inf- 4)))
    (is (m/nan? (m/rem' m/nan 2)))
    (is (m/nan? (m/rem' 3 m/inf+)))
    (is (m/nan? (m/rem' 4 m/inf-)))
    (is (m/nan? (m/rem' -3 m/inf+)))
    (is (m/nan? (m/rem' -4 m/inf-)))
    (is (m/nan? (m/rem' 2 m/nan)))))

(deftest quot-and-rem'-test
  (with-instrument `m/quot-and-rem'
    (is (spec-check m/quot-and-rem')))
  (with-instrument (st/instrumentable-syms)
    (is= [4 0] (m/quot-and-rem' 16 4))
    (is= [1 1] (m/quot-and-rem' 3 2))
    (is= [-1 -1] (m/quot-and-rem' -3 2))
    (is= [-1 1] (m/quot-and-rem' 3 -2))
    (is= [1 -1] (m/quot-and-rem' -3 -2))
    (is= [0 3] (m/quot-and-rem' 3 4))
    (is= [0 -3] (m/quot-and-rem' -3 4))
    (is= [0 3] (m/quot-and-rem' 3 -4))
    (is= [0 -3] (m/quot-and-rem' -3 -4))
    (is= [1 1] (m/quot-and-rem' 3.0 2))
    (is= [1 1] (m/quot-and-rem' 3 2.0))
    (is= [1 0.8799999999999999] (m/quot-and-rem' 3 2.12))
    (is= [1.4150943396226415E40 0] (m/quot-and-rem' 3.0E40 2.12))
    (is (every? m/nan? (m/quot-and-rem' m/inf+ 3)))
    (is (every? m/nan? (m/quot-and-rem' m/inf- 4)))
    (is (every? m/nan? (m/quot-and-rem' m/nan 2)))
    (is (every? m/nan? (m/quot-and-rem' 3 m/inf+)))
    (is (every? m/nan? (m/quot-and-rem' 2 m/nan)))))

(deftest quot-and-mod'-test
  (with-instrument `m/quot-and-mod'
    (is (spec-check m/quot-and-mod')))
  (with-instrument (st/instrumentable-syms)
    (is= [4 0] (m/quot-and-mod' 16 4))
    (is= [0 0] (m/quot-and-mod' 0 4))
    (is= [0 0] (m/quot-and-mod' 0 -4))
    (is (every? m/nan? (m/quot-and-mod' 4 0)))
    (is (every? m/nan? (m/quot-and-mod' -4 0)))
    (is (every? m/nan? (m/quot-and-mod' 0 0)))
    (is= [1 1] (m/quot-and-mod' 3 2))
    (is= [-2 1] (m/quot-and-mod' -3 2))
    (is= [-2 -1] (m/quot-and-mod' 3 -2))
    (is= [1 -1] (m/quot-and-mod' -3 -2))
    (is= [0 3] (m/quot-and-mod' 3 4))
    (is= [-1 1] (m/quot-and-mod' -3 4))
    (is= [-1 -1] (m/quot-and-mod' 3 -4))
    (is= [0 -3] (m/quot-and-mod' -3 -4))
    (is= [1 1] (m/quot-and-mod' 3.0 2))
    (is= [1 1] (m/quot-and-mod' 3 2.0))
    (is= [1 0.8799999999999999] (m/quot-and-mod' 3 2.12))
    (is= [1.4150943396226415E40 0] (m/quot-and-mod' 3.0E40 2.12))
    (is (every? m/nan? (m/quot-and-mod' m/inf+ 3)))
    (is (every? m/nan? (m/quot-and-mod' m/inf- 4)))
    (is (every? m/nan? (m/quot-and-mod' m/nan 2)))
    (is (every? m/nan? (m/quot-and-mod' 3 m/inf+)))
    (is (every? m/nan? (m/quot-and-mod' 4 m/inf-)))
    (is (every? m/nan? (m/quot-and-mod' -3 m/inf+)))
    (is (every? m/nan? (m/quot-and-mod' -4 m/inf-)))
    (is (every? m/nan? (m/quot-and-mod' 2 m/nan)))))

(deftest gcd-test
  (with-instrument `m/gcd
    (is (spec-check m/gcd)))
  (with-instrument (st/instrumentable-syms)
    (is= 7 (m/gcd 271284701247 12467364728))))

;;;ANGLES
(deftest reduce-angle'-test
  (with-instrument `m/reduce-angle'
    (is (spec-check m/reduce-angle')))
  (with-instrument (st/instrumentable-syms)
    (is= 30.4 (m/reduce-angle' 30.4))
    (is= 350.2 (m/reduce-angle' -9.8))
    (is= 118 (m/reduce-angle' 478.0))
    (is= 26 (m/reduce-angle' -8399494))
    (is (m/nan? (m/reduce-angle' m/nan)))
    (is (m/nan? (m/reduce-angle' m/inf+)))
    (is (m/nan? (m/reduce-angle' m/inf-)))))

(deftest reduce-radians'-test
  (with-instrument `m/reduce-radians'
    (is (spec-check m/reduce-radians')))
  (with-instrument (st/instrumentable-syms)
    (is= 5.267258771281654 (m/reduce-radians' 30.4))
    (is (zero? (m/reduce-radians' m/two-pi)))
    (is= m/PI (m/reduce-radians' m/PI))
    (is= 0.06552912132908517 (m/reduce-radians' -8399494))
    (is (m/nan? (m/reduce-radians' m/nan)))
    (is (m/nan? (m/reduce-radians' m/inf+)))
    (is (m/nan? (m/reduce-radians' m/inf-)))))

(deftest radians->angle'-test
  (with-instrument `m/radians->angle'
    (is (spec-check m/radians->angle')))
  (with-instrument (st/instrumentable-syms)
    (is (zero? (m/radians->angle' 0)))
    (is= 194.8056503444799 (m/radians->angle' 3.4))
    (is (zero? (m/radians->angle' m/two-pi)))
    (is= 165.1943496555201 (m/radians->angle' -3.4))
    (is= 58.31007808870436 (m/radians->angle' 45))
    (is (m/nan? (m/radians->angle' m/nan)))
    (is= m/inf+ (m/radians->angle' m/inf+))
    (is= m/inf- (m/radians->angle' m/inf-))))

(deftest angle->radians'-test
  (with-instrument `m/angle->radians'
    (is (spec-check m/angle->radians')))
  (with-instrument (st/instrumentable-syms)
    (is (zero? (m/angle->radians' 0)))
    (is= 0.059341194567807204 (m/angle->radians' 3.4))
    (is= 0.002777777777777778 (m/angle->radians' m/inv-two-pi))
    (is= 6.223844112611779 (m/angle->radians' -3.4))
    (is= 0.7853981633974483 (m/angle->radians' 45))
    (is (m/nan? (m/angle->radians' m/nan)))
    (is= m/inf+ (m/angle->radians' m/inf+))
    (is= m/inf- (m/angle->radians' m/inf-))))
