(ns provisdom.math.core-test
  (:require
    [clojure.test :refer :all]
    [provisdom.math.core :as m]
    [provisdom.test.core :as t]))

;;1 seconds

(set! *warn-on-reflection* true)

;;TYPE TESTS
(deftest numbers?-test
  (t/with-instrument `m/numbers?
    (t/is (t/spec-check m/numbers?)))
  (t/with-instrument :all
    (t/is-not (m/numbers? 1))
    (t/is (m/numbers? '(1)))
    (t/is (m/numbers? []))
    (t/is (m/numbers? [2 3]))
    (t/is-not (m/numbers? [[2]]))))

(deftest num?-test
  (t/with-instrument `m/num?
    (t/is (t/spec-check m/num?)))
  (t/with-instrument :all
    (t/is-not (m/num? "A"))
    (t/is (m/num? 3.3E30))
    (t/is (m/num? -3.3E30))
    (t/is (m/num? m/inf+))
    (t/is (m/num? m/inf-))
    (t/is-not (m/num? m/nan))))

(deftest nan?-test
  (t/with-instrument `m/nan?
    (t/is (t/spec-check m/nan?)))
  (t/with-instrument :all
    (t/is-not (m/nan? 3.3))
    (t/is-not (m/nan? "A"))
    (t/is-not (m/nan? m/inf+))
    (t/is-not (m/nan? m/inf-))
    (t/is (m/nan? m/nan))))

(deftest pos?-test
  (t/with-instrument `m/pos?
    (t/is (t/spec-check m/pos?)))
  (t/with-instrument :all
    (t/is-not (m/pos? "A"))
    (t/is (m/pos? 3.3E30))
    (t/is-not (m/pos? -3.3E30))
    (t/is (m/pos? m/inf+))
    (t/is-not (m/pos? m/inf-))
    (t/is-not (m/pos? m/nan))))

(deftest neg?-test
  (t/with-instrument `m/neg?
    (t/is (t/spec-check m/neg?)))
  (t/with-instrument :all
    (t/is-not (m/neg? "A"))
    (t/is-not (m/neg? 3.3E30))
    (t/is (m/neg? -3.3E30))
    (t/is-not (m/neg? m/inf+))
    (t/is (m/neg? m/inf-))
    (t/is-not (m/neg? m/nan))))

(deftest non-?-test
  (t/with-instrument `m/non-?
    (t/is (t/spec-check m/non-?)))
  (t/with-instrument :all
    (t/is (m/non-? 0))
    (t/is (m/non-? 1))
    (t/is-not (m/non-? -1))
    (t/is (m/non-? m/inf+))
    (t/is-not (m/non-? m/inf-))
    (t/is-not (m/non-? m/nan))
    (t/is-not (m/non-? "A"))))

(deftest non+?-test
  (t/with-instrument `m/non+?
    (t/is (t/spec-check m/non+?)))
  (t/with-instrument :all
    (t/is (m/non+? 0))
    (t/is-not (m/non+? 1))
    (t/is (m/non+? -1))
    (t/is-not (m/non+? m/inf+))
    (t/is (m/non+? m/inf-))
    (t/is-not (m/non+? m/nan))
    (t/is-not (m/non+? "A"))))

(deftest finite?-test
  (t/with-instrument `m/finite?
    (t/is (t/spec-check m/finite?)))
  (t/with-instrument :all
    (t/is-not (m/finite? "A"))
    (t/is (m/finite? 3.3E30))
    (t/is (m/finite? -3.3E30))
    (t/is-not (m/finite? m/inf+))
    (t/is-not (m/finite? m/inf-))
    (t/is-not (m/finite? m/nan))))

(deftest finite+?-test
  (t/with-instrument `m/finite+?
    (t/is (t/spec-check m/finite+?)))
  (t/with-instrument :all
    (t/is-not (m/finite+? "A"))
    (t/is (m/finite+? 3.3E30))
    (t/is-not (m/finite+? -3.3E30))
    (t/is-not (m/finite+? m/inf+))
    (t/is-not (m/finite+? m/inf-))
    (t/is-not (m/finite+? m/nan))))

(deftest finite-?-test
  (t/with-instrument `m/finite-?
    (t/is (t/spec-check m/finite-?)))
  (t/with-instrument :all
    (t/is-not (m/finite-? "A"))
    (t/is-not (m/finite-? 3.3E30))
    (t/is (m/finite-? -3.3E30))
    (t/is-not (m/finite-? m/inf+))
    (t/is-not (m/finite-? m/inf-))
    (t/is-not (m/finite-? m/nan))))

(deftest finite-non-?-test
  (t/with-instrument `m/finite-non-?
    (t/is (t/spec-check m/finite-non-?)))
  (t/with-instrument :all
    (t/is (m/finite-non-? 0))
    (t/is (m/finite-non-? 1))
    (t/is-not (m/finite-non-? -1))
    (t/is-not (m/finite-non-? m/inf+))
    (t/is-not (m/finite-non-? m/inf-))
    (t/is-not (m/finite-non-? m/nan))
    (t/is-not (m/finite-non-? "A"))))

(deftest finite-non+?-test
  (t/with-instrument `m/finite-non+?
    (t/is (t/spec-check m/finite-non+?)))
  (t/with-instrument :all
    (t/is (m/finite-non+? 0))
    (t/is-not (m/finite-non+? 1))
    (t/is (m/finite-non+? -1))
    (t/is-not (m/finite-non+? m/inf+))
    (t/is-not (m/finite-non+? m/inf-))
    (t/is-not (m/finite-non+? m/nan))
    (t/is-not (m/finite-non+? "A"))))

(deftest double-finite?-test
  (t/with-instrument `m/double-finite?
    (t/is (t/spec-check m/double-finite?)))
  (t/with-instrument :all
    (t/is-not (m/double-finite? "A"))
    (t/is-not (m/double-finite? 0))
    (t/is (m/double-finite? 0.0))
    (t/is (m/double-finite? 3.3E300))
    (t/is (m/double-finite? -3.3E300))
    (t/is-not (m/double-finite? m/inf+))
    (t/is-not (m/double-finite? m/inf-))
    (t/is-not (m/double-finite? m/nan))))

(deftest double-finite+?-test
  (t/with-instrument `m/double-finite+?
    (t/is (t/spec-check m/double-finite+?)))
  (t/with-instrument :all
    (t/is-not (m/double-finite? "A"))
    (t/is-not (m/double-finite? 0))
    (t/is (m/double-finite? 0.0))
    (t/is (m/double-finite? 3.3E300))
    (t/is (m/double-finite? -3.3E300))
    (t/is-not (m/double-finite? m/inf+))
    (t/is-not (m/double-finite? m/inf-))
    (t/is-not (m/double-finite? m/nan))))

(deftest single?-test
  (t/with-instrument `m/single?
    (t/is (t/spec-check m/single?)))
  (t/with-instrument :all
    (t/is-not (m/single? "A"))
    (t/is-not (m/single? 0))
    (t/is (m/single? 0.0))
    (t/is (m/single? 3.3E30))
    (t/is (m/single? -3.3E30))
    (t/is-not (m/single? 3.3E300))
    (t/is-not (m/single? -3.3E300))
    (t/is (m/single? m/inf+))
    (t/is (m/single? m/inf-))
    (t/is (m/single? m/nan))))

(deftest single-finite?-test
  (t/with-instrument `m/single-finite?
    (t/is (t/spec-check m/single-finite?)))
  (t/with-instrument :all
    (t/is-not (m/single-finite? "A"))
    (t/is-not (m/single-finite? 0))
    (t/is (m/single-finite? 0.0))
    (t/is (m/single-finite? 3.3E30))
    (t/is (m/single-finite? -3.3E30))
    (t/is-not (m/single-finite? 3.3E300))
    (t/is-not (m/single-finite? -3.3E300))
    (t/is-not (m/single-finite? m/inf+))
    (t/is-not (m/single-finite? m/inf-))
    (t/is-not (m/single-finite? m/nan))))

(deftest long?-test
  (t/with-instrument `m/long?
    (t/is (t/spec-check m/long?)))
  (t/with-instrument :all
    (t/is-not (m/long? 3.3))
    (t/is (m/long? 3))
    (t/is-not (m/long? 3.0))
    (t/is-not (m/long? "A"))
    (t/is-not (m/long? 3.4E15))
    (t/is-not (m/long? 3.3E30))
    (t/is-not (m/long? -3.3E30))
    (t/is-not (m/long? m/nan))))

(deftest long+?-test
  (t/with-instrument `m/long+?
    (t/is (t/spec-check m/long+?)))
  (t/with-instrument :all
    (t/is-not (m/long+? 3.3))
    (t/is (m/long+? 3))
    (t/is-not (m/long+? -3))
    (t/is-not (m/long+? 3.0))
    (t/is-not (m/long+? "A"))
    (t/is-not (m/long+? 3.4E15))
    (t/is-not (m/long+? 3.3E30))
    (t/is-not (m/long+? -3.3E30))
    (t/is-not (m/long+? m/nan))))

(deftest long-?-test
  (t/with-instrument `m/long-?
    (t/is (t/spec-check m/long-?)))
  (t/with-instrument :all
    (t/is-not (m/long-? 3.3))
    (t/is-not (m/long-? 3))
    (t/is (m/long-? -3))
    (t/is-not (m/long-? 3.0))
    (t/is-not (m/long-? "A"))
    (t/is-not (m/long-? -3.4E15))
    (t/is-not (m/long-? 3.3E30))
    (t/is-not (m/long-? -3.3E30))
    (t/is-not (m/long-? m/nan))))

(deftest long-non-?-test
  (t/with-instrument `m/long-non-?
    (t/is (t/spec-check m/long-non-?)))
  (t/with-instrument :all
    (t/is-not (m/long-non-? 3.3))
    (t/is (m/long-non-? 3))
    (t/is-not (m/long-non-? -3))
    (t/is (m/long-non-? 0))
    (t/is-not (m/long-non-? 3.0))
    (t/is-not (m/long-non-? "A"))
    (t/is-not (m/long-non-? 3.4E15))
    (t/is-not (m/long-non-? 3.3E30))
    (t/is-not (m/long-non-? -3.3E30))
    (t/is-not (m/long-non-? m/nan))))

(deftest long-non+?-test
  (t/with-instrument `m/long-non+?
    (t/is (t/spec-check m/long-non+?)))
  (t/with-instrument :all
    (t/is-not (m/long-non+? 3.3))
    (t/is-not (m/long-non+? 3))
    (t/is (m/long-non+? -3))
    (t/is (m/long-non+? 0))
    (t/is-not (m/long-non+? 3.0))
    (t/is-not (m/long-non+? "A"))
    (t/is-not (m/long-non+? 3.4E15))
    (t/is-not (m/long-non+? 3.3E30))
    (t/is-not (m/long-non+? -3.3E30))
    (t/is-not (m/long-non+? m/nan))))

(deftest int?-test
  (t/with-instrument `m/int?
    (t/is (t/spec-check m/int?)))
  (t/with-instrument :all
    (t/is-not (m/int? 3.3))
    (t/is (m/int? 3))
    (t/is-not (m/int? 3.0))
    (t/is-not (m/int? "A"))
    (t/is-not (m/int? 3.4E15))
    (t/is-not (m/int? 3.3E30))
    (t/is-not (m/int? -3.3E30))
    (t/is-not (m/int? m/nan))))

(deftest int+?-test
  (t/with-instrument `m/int+?
    (t/is (t/spec-check m/int+?)))
  (t/with-instrument :all
    (t/is-not (m/int+? 3.3))
    (t/is (m/int+? 3))
    (t/is-not (m/int+? -3))
    (t/is-not (m/int+? 3.0))
    (t/is-not (m/int+? "A"))
    (t/is-not (m/int+? 3.4E15))
    (t/is-not (m/int+? 3.3E30))
    (t/is-not (m/int+? -3.3E30))
    (t/is-not (m/int+? m/nan))))

(deftest int-?-test
  (t/with-instrument `m/int-?
    (t/is (t/spec-check m/int-?)))
  (t/with-instrument :all
    (t/is-not (m/int-? 3.3))
    (t/is-not (m/int-? 3))
    (t/is (m/int-? -3))
    (t/is-not (m/int-? 3.0))
    (t/is-not (m/int-? "A"))
    (t/is-not (m/int-? -3.4E15))
    (t/is-not (m/int-? 3.3E30))
    (t/is-not (m/int-? -3.3E30))
    (t/is-not (m/int-? m/nan))))

(deftest int-non-?-test
  (t/with-instrument `m/int-non-?
    (t/is (t/spec-check m/int-non-?)))
  (t/with-instrument :all
    (t/is-not (m/int-non-? 3.3))
    (t/is (m/int-non-? 3))
    (t/is-not (m/int-non-? -3))
    (t/is (m/int-non-? 0))
    (t/is-not (m/int-non-? 3.0))
    (t/is-not (m/int-non-? "A"))
    (t/is-not (m/int-non-? 3.4E15))
    (t/is-not (m/int-non-? 3.3E30))
    (t/is-not (m/int-non-? -3.3E30))
    (t/is-not (m/int-non-? m/nan))))

(deftest int-non+?-test
  (t/with-instrument `m/int-non+?
    (t/is (t/spec-check m/int-non+?)))
  (t/with-instrument :all
    (t/is-not (m/int-non+? 3.3))
    (t/is-not (m/int-non+? 3))
    (t/is (m/int-non+? -3))
    (t/is (m/int-non+? 0))
    (t/is-not (m/int-non+? 3.0))
    (t/is-not (m/int-non+? "A"))
    (t/is-not (m/int-non+? 3.4E15))
    (t/is-not (m/int-non+? 3.3E30))
    (t/is-not (m/int-non+? -3.3E30))
    (t/is-not (m/int-non+? m/nan))))

(deftest long-able?-test
  (t/with-instrument `m/long-able?
    (t/is (t/spec-check m/long-able?)))
  (t/with-instrument :all
    (t/is-not (m/long-able? 3.3))
    (t/is (m/long-able? 3))
    (t/is (m/long-able? 3.0))
    (t/is-not (m/long-able? "A"))
    (t/is (m/long-able? 3.4E15))
    (t/is-not (m/long-able? 3.3E30))
    (t/is-not (m/long-able? -3.3E30))
    (t/is-not (m/long-able? m/nan))))

(deftest long-able+?-test
  (t/with-instrument `m/long-able+?
    (t/is (t/spec-check m/long-able+?)))
  (t/with-instrument :all
    (t/is-not (m/long-able+? 3.3))
    (t/is-not (m/long-able+? 0.0))
    (t/is (m/long-able+? 1.0))
    (t/is-not (m/long-able+? -1.0))
    (t/is-not (m/long-able+? -3.3))))

(deftest long-able-?-test
  (t/with-instrument `m/long-able-?
    (t/is (t/spec-check m/long-able-?)))
  (t/with-instrument :all
    (t/is-not (m/long-able-? 3.3))
    (t/is-not (m/long-able-? 0.0))
    (t/is-not (m/long-able-? 1.0))
    (t/is (m/long-able-? -1.0))
    (t/is-not (m/long-able-? -3.3))))

(deftest long-able-non+?-test
  (t/with-instrument `m/long-able-non+?
    (t/is (t/spec-check m/long-able-non+?)))
  (t/with-instrument :all
    (t/is-not (m/long-able-non+? 3.3))
    (t/is (m/long-able-non+? 0.0))
    (t/is-not (m/long-able-non+? 1.0))
    (t/is (m/long-able-non+? -1.0))
    (t/is-not (m/long-able-non+? -3.3))))

(deftest long-able-non-?-test
  (t/with-instrument `m/long-able-non-?
    (t/is (t/spec-check m/long-able-non-?)))
  (t/with-instrument :all
    (t/is-not (m/long-able-non-? 3.3))
    (t/is (m/long-able-non-? 0.0))
    (t/is (m/long-able-non-? 1.0))
    (t/is-not (m/long-able-non-? -1.0))
    (t/is-not (m/long-able-non-? -3.3))))

(deftest inf+?-test
  (t/with-instrument `m/inf+?
    (t/is (t/spec-check m/inf+?)))
  (t/with-instrument :all
    (t/is-not (m/inf+? 3.3))
    (t/is-not (m/inf+? "A"))
    (t/is (m/inf+? m/inf+))
    (t/is-not (m/inf+? m/inf-))
    (t/is-not (m/inf+? m/nan))))

(deftest inf-?-test
  (t/with-instrument `m/inf-?
    (t/is (t/spec-check m/inf-?)))
  (t/with-instrument :all
    (t/is-not (m/inf-? 3.3))
    (t/is-not (m/inf-? "A"))
    (t/is-not (m/inf-? m/inf+))
    (t/is (m/inf-? m/inf-))
    (t/is-not (m/inf-? m/nan))))

(deftest inf?-test
  (t/with-instrument `m/inf?
    (t/is (t/spec-check m/inf?)))
  (t/with-instrument :all
    (t/is-not (m/inf? 3.3))
    (t/is-not (m/inf? "A"))
    (t/is (m/inf? m/inf+))
    (t/is (m/inf? m/inf-))
    (t/is-not (m/inf? m/nan))))

(deftest one?-test
  (t/with-instrument `m/one?
    (t/is (t/spec-check m/one?)))
  (t/with-instrument :all
    (t/is (m/one? 1))
    (t/is (m/one? 1.0))
    (t/is-not (m/one? "A"))
    (t/is-not (m/one? -1))
    (t/is-not (m/one? m/nan))))

(deftest prob?-test
  (t/with-instrument `m/prob?
    (t/is (t/spec-check m/prob?)))
  (t/with-instrument :all
    (t/is-not (m/prob? -0.5))
    (t/is (m/prob? 0))
    (t/is (m/prob? 0.5))
    (t/is (m/prob? 1))
    (t/is-not (m/prob? 1.5))
    (t/is-not (m/prob? m/inf+))
    (t/is-not (m/prob? m/inf-))
    (t/is-not (m/prob? "A"))
    (t/is-not (m/prob? m/nan))))

(deftest open-prob?-test
  (t/with-instrument `m/open-prob?
    (t/is (t/spec-check m/open-prob?)))
  (t/with-instrument :all
    (t/is-not (m/open-prob? -0.5))
    (t/is-not (m/open-prob? 0))
    (t/is (m/open-prob? 0.5))
    (t/is-not (m/open-prob? 1))
    (t/is-not (m/open-prob? 1.5))
    (t/is-not (m/open-prob? m/inf+))
    (t/is-not (m/open-prob? m/inf-))
    (t/is-not (m/open-prob? m/nan))
    (t/is-not (m/open-prob? "A"))))

(deftest corr?-test
  (t/with-instrument `m/corr?
    (t/is (t/spec-check m/corr?)))
  (t/with-instrument :all
    (t/is (m/corr? -0.5))
    (t/is (m/corr? 0))
    (t/is (m/corr? 0.5))
    (t/is (m/corr? 1))
    (t/is-not (m/corr? 1.5))
    (t/is (m/corr? -1))
    (t/is-not (m/corr? -1.5))
    (t/is-not (m/corr? m/inf+))
    (t/is-not (m/corr? m/inf-))
    (t/is-not (m/corr? m/nan))
    (t/is-not (m/corr? "A"))))

(deftest open-corr?-test
  (t/with-instrument `m/open-corr?
    (t/is (t/spec-check m/open-corr?)))
  (t/with-instrument :all
    (t/is (m/open-corr? -0.5))
    (t/is (m/open-corr? 0))
    (t/is (m/open-corr? 0.5))
    (t/is-not (m/open-corr? 1))
    (t/is-not (m/open-corr? 1.5))
    (t/is-not (m/open-corr? -1))
    (t/is-not (m/open-corr? -1.5))
    (t/is-not (m/open-corr? m/inf+))
    (t/is-not (m/open-corr? m/inf-))
    (t/is-not (m/open-corr? m/nan))
    (t/is-not (m/open-corr? "A"))))

(deftest maybe-long-able-test
  (t/with-instrument `m/maybe-long-able
    (t/is (t/spec-check m/maybe-long-able)))
  (t/with-instrument :all
    (t/is (zero? (m/maybe-long-able 0.0)))
    (t/is (zero? (m/maybe-long-able 0)))
    (t/is= 23423423 (m/maybe-long-able 2.3423423E7))
    (t/is= 234234324234234234234N (m/maybe-long-able 234234324234234234234N))
    (t/is= 2.3423432423423423E20 (m/maybe-long-able 2.3423432423423423E20))
    (t/is= m/inf+ (m/maybe-long-able m/inf+))
    (t/is= m/inf- (m/maybe-long-able m/inf-))
    (t/is (m/nan? (m/maybe-long-able m/nan)))
    (t/is (nil? (m/maybe-long-able nil)))))

;;BASIC MATH TESTS
(deftest ===-test
  (t/with-instrument `m/===
    (t/is (t/spec-check m/===)))
  (t/with-instrument :all
    (t/is (m/=== m/nan))
    (t/is (m/=== m/nan m/nan))
    (t/is (m/=== m/nan m/nan m/nan))
    (t/is (m/=== 3 3 3 3))
    (t/is-not (m/=== 3 m/nan))))

(deftest next-up-test
  (t/with-instrument `m/next-up
    (t/is (t/spec-check m/next-up)))
  (t/with-instrument :all
    (t/is= 3.0000000000000004 (m/next-up 3))
    (t/is= -2.9999999999999996 (m/next-up -3))
    (t/is (m/nan? (m/next-up m/nan)))
    (t/is= m/inf+ (m/next-up m/inf+))
    (t/is= m/min-dbl (m/next-up m/inf-))
    (t/is= -2.9999999999999996 (m/next-up -3.0))))

(deftest next-down-test
  (t/with-instrument `m/next-down
    (t/is (t/spec-check m/next-down)))
  (t/with-instrument :all
    (t/is= 2.9999999999999996 (m/next-down 3))
    (t/is= -3.0000000000000004 (m/next-down -3))
    (t/is (m/nan? (m/next-down m/nan)))
    (t/is= m/max-dbl (m/next-down m/inf+))
    (t/is= m/inf- (m/next-down m/inf-))
    (t/is= 2.9999999999999996 (m/next-down 3.0))))

(deftest div-test
  (t/with-instrument `m/div
    (t/is (t/spec-check m/div)))
  (t/with-instrument :all
    (t/is (ratio? (m/div 4)))
    (t/is= 0.25 (m/div 4.0))
    (t/is= 1 (m/div 3 3))
    (t/is= -1 (m/div -3 3))
    (t/is (m/nan? (m/div m/nan 0)))
    (t/is (m/nan? (m/div 0 m/nan)))
    (t/is= m/inf+ (m/div m/inf+ 0))
    (t/is= m/inf- (m/div m/inf- 0))
    (t/is= 0.0 (m/div 0 m/inf+))
    (t/is= 0.0 (m/div 0 m/inf-))
    (t/is (m/nan? (m/div 0 0)))
    (t/is (m/nan? (m/div 0 0 m/nan)))))

(deftest one--test
  (t/with-instrument `m/one-
    (t/is (t/spec-check m/one-)))
  (t/with-instrument :all
    (t/is= -2 (m/one- 3))
    (t/is= 0.0 (m/one- 3 -2))
    (t/is= -16.0 (m/one- 3 4 2 8))
    (t/is= 4 (m/one- -3))
    (t/is (m/nan? (m/one- m/nan)))
    (t/is= m/inf- (m/one- m/inf+))
    (t/is= m/inf+ (m/one- m/inf-))
    (t/is= 4.0 (m/one- -3.0))))

(deftest sq'-test
  (t/with-instrument `m/sq'
    (t/is (t/spec-check m/sq')))
  (t/with-instrument :all
    (t/is= 9 (m/sq' 3))
    (t/is= 9 (m/sq' -3))
    (t/is (m/nan? (m/sq' m/nan)))
    (t/is= m/inf+ (m/sq' m/inf+))
    (t/is= m/inf+ (m/sq' m/inf-))
    (t/is= 9 (m/sq' -3.0))))

(deftest cube'-test
  (t/with-instrument `m/cube'
    (t/is (t/spec-check m/cube')))
  (t/with-instrument :all
    (t/is= 27 (m/cube' 3))
    (t/is (m/nan? (m/cube' m/nan)))
    (t/is= m/inf+ (m/cube' m/inf+))
    (t/is= m/inf- (m/cube' m/inf-))
    (t/is= -27 (m/cube' -3))
    (t/is= -27 (m/cube' -3.0))))

(deftest sgn-test
  (t/with-instrument `m/sgn
    (t/is (t/spec-check m/sgn)))
  (t/with-instrument :all
    (t/is= 1 (m/sgn 3))
    (t/is= -1 (m/sgn -3))
    (t/is (m/nan? (m/sgn m/nan)))
    (t/is= 1 (m/sgn m/inf+))
    (t/is= -1 (m/sgn m/inf-))
    (t/is (zero? (m/sgn 0)))
    (t/is (zero? (m/sgn 0.0)))
    (t/is= -1 (m/sgn -3.0))))

(deftest log2-test
  (t/with-instrument `m/log2
    (t/is (t/spec-check m/log2)))
  (t/with-instrument :all
    (t/is= 1.5849625007211563 (m/log2 3))
    (t/is= m/inf- (m/log2 0))
    (t/is= m/inf+ (m/log2 m/inf+))
    (t/is (m/nan? (m/log2 m/nan)))
    (t/is (m/nan? (m/log2 -3.0)))
    (t/is= 0.0 (m/log2 1.0))
    (t/is= -0.15200309344504997 (m/log2 0.9))))

(deftest logn-test
  (t/with-instrument `m/logn
    (t/is (t/spec-check m/logn)))
  (t/with-instrument :all
    (t/is= 1.0 (m/logn 3 3))
    (t/is= m/inf- (m/logn 0 3))
    (t/is= m/inf+ (m/logn m/inf+ 3))
    (t/is (m/nan? (m/logn -3.0 3)))
    (t/is (m/nan? (m/logn m/nan 3)))
    (t/is= 0.0 (m/logn 1.0 3))
    (t/is= -0.09590327428938458 (m/logn 0.9 3))
    (t/is= m/inf- (m/logn 0.9 1))
    (t/is= 0.15200309344504997 (m/logn 0.9 0.5))
    (t/is= 0.0 (m/logn 0.9 0))
    (t/is= -0.0 (m/logn 0.9 m/inf+))))

(deftest abs'-test
  (t/with-instrument `m/abs'
    (t/is (t/spec-check m/abs')))
  (t/with-instrument :all
    (t/is= 3.3 (m/abs' -3.3))
    (t/is= 3 (m/abs' -3))
    (t/is= 300000000 (m/abs' 3.0E8))
    (t/is (zero? (m/abs' 0)))
    (t/is (zero? (m/abs' 0.0)))
    (t/is= m/inf+ (m/abs' m/inf+))
    (t/is= m/inf+ (m/abs' m/inf-))
    (t/is (m/nan? (m/abs' m/nan)))))

(deftest cbrt-test
  (t/with-instrument `m/cbrt
    (t/is (t/spec-check m/cbrt)))
  (t/with-instrument :all
    (t/is= 0.0 (m/cbrt 0.0))
    (t/is= 1.0 (m/cbrt 1.0))
    (t/is= -1.0 (m/cbrt -1.0))
    (t/is= -2.0 (m/cbrt -8))
    (t/is= m/inf+ (m/cbrt m/inf+))
    (t/is= m/inf- (m/cbrt m/inf-))
    (t/is (m/nan? (m/cbrt m/nan)))))

;;TRIGONOMETRY
(deftest sin-test
  (t/with-instrument `m/sin
    (t/is (t/spec-check m/sin))))

(deftest sinh-test
  (t/with-instrument `m/sinh
    (t/is (t/spec-check m/sinh))))

(deftest asin-test
  (t/with-instrument `m/asin
    (t/is (t/spec-check m/asin))))

(deftest asinh-test
  (t/with-instrument `m/asinh
    (t/is (t/spec-check m/asinh)))
  (t/with-instrument :all
    (t/is= 0.0 (m/asinh 0.0))
    (t/is= 0.48121182505960347 (m/asinh 0.5))
    (t/is= -0.8813735870195428 (m/asinh -1.0))
    (t/is= 0.8813735870195429 (m/asinh 1.0))
    (t/is= -1.4436354751788099 (m/asinh -2.0))
    (t/is= 1.4436354751788103 (m/asinh 2.0))
    (t/is= m/inf+ (m/asinh m/inf+))
    (t/is (m/nan? (m/asinh m/inf-)))
    (t/is (m/nan? (m/asinh m/nan)))))

(deftest cos-test
  (t/with-instrument `m/cos
    (t/is (t/spec-check m/cos))))

(deftest cosh-test
  (t/with-instrument `m/cosh
    (t/is (t/spec-check m/cosh))))

(deftest acos-test
  (t/with-instrument `m/acos
    (t/is (t/spec-check m/acos))))

(deftest acosh-test
  (t/with-instrument `m/acosh
    (t/is (t/spec-check m/acosh)))
  (t/with-instrument :all
    (t/is (m/nan? (m/acosh 0.0)))
    (t/is= 0.0 (m/acosh 1.0))
    (t/is= 1.3169578969248166 (m/acosh 2.0))
    (t/is= m/inf+ (m/acosh m/inf+))
    (t/is (m/nan? (m/acosh m/nan)))))

(deftest tan-test
  (t/with-instrument `m/tan
    (t/is (t/spec-check m/tan))))

(deftest tanh-test
  (t/with-instrument `m/tanh
    (t/is (t/spec-check m/tanh))))

(deftest atan-test
  (t/with-instrument `m/atan
    (t/is (t/spec-check m/atan))))

(deftest atan2-test
  (t/with-instrument `m/atan2
    (t/is (t/spec-check m/atan2))))

(deftest atanh-test
  (t/with-instrument `m/atanh
    (t/is (t/spec-check m/atanh)))
  (t/with-instrument :all
    (t/is= 0.0 (m/atanh 0.0))
    (t/is-approx= 0.5493061443340549 (m/atanh 0.5) :tolerance 1e-14)
    (t/is-approx= -0.5493061443340549 (m/atanh -0.5) :tolerance 1e-14)
    (t/is= m/inf- (m/atanh -1.0))
    (t/is= m/inf+ (m/atanh 1.0))
    (t/is (m/nan? (m/atanh -2.0)))
    (t/is (m/nan? (m/atanh m/nan)))))

(deftest hypot-test
  (t/with-instrument `m/hypot
    (t/is (t/spec-check m/hypot))))

;;ROUNDING
(deftest round-test
  (t/with-instrument `m/round
    (t/is (t/spec-check m/round)))
  (t/with-instrument :all
    (t/is= 1 (m/round 0.5 :up))
    (t/is= 2.342342342342342E22 (m/round 2.342342342342342E22 :up))
    (t/is (zero? (m/round -0.5 :up)))
    (t/is= -1 (m/round -0.5 :down))
    (t/is= -1 (m/round -0.5 :away-from-zero))
    (t/is (zero? (m/round -0.5 :toward-zero)))
    (t/is (zero? (m/round 0.5 :down)))
    (t/is= 1 (m/round 0.5 :away-from-zero))
    (t/is (zero? (m/round 0.5 :toward-zero)))
    (t/is= m/inf+ (m/round m/inf+ :up))
    (t/is= m/inf- (m/round m/inf- :up))
    (t/is (m/nan? (m/round m/nan :up)))))

(deftest round-significant-test
  (t/with-instrument `m/round-significant
    (t/is (t/spec-check m/round-significant)))
  (t/with-instrument :all
    (t/is= 120.0 (m/round-significant 123.45 2 :down))
    (t/is= 123.5 (m/round-significant 123.45 4 :up))
    (t/is= 123.4 (m/round-significant 123.45 4 :down))
    (t/is= 123.4 (m/round-significant 123.45 4 :toward-zero))
    (t/is= 123.5 (m/round-significant 123.45 4 :away-from-zero))
    (t/is= -120.0 (m/round-significant -123.45 2 :down))
    (t/is= -123.4 (m/round-significant -123.45 4 :up))
    (t/is= -123.5 (m/round-significant -123.45 4 :down))
    (t/is= -123.4 (m/round-significant -123.45 4 :toward-zero))
    (t/is= -123.5 (m/round-significant -123.45 4 :away-from-zero))
    (t/is= -123.45 (m/round-significant -123.45 10 :toward-zero))
    (t/is= m/inf+ (m/round-significant m/max-dbl 10 :down))
    (t/is= 1.79769313E308 (m/round-significant m/max-dbl 9 :down))
    (t/is= -1.79769313E308 (m/round-significant m/min-dbl 9 :up))
    (t/is= m/inf- (m/round-significant m/min-dbl 10 :up))
    (t/is= m/inf+ (m/round-significant m/inf+ 10 :up))
    (t/is= m/inf- (m/round-significant m/inf- 1 :up))
    (t/is (m/nan? (m/round-significant m/nan 5 :up)))
    ;;notice these stay tiny-dbl -- due to double limitations
    ;; (i.e., 5.0E-324 = 4.9E-324)
    (t/is= m/tiny-dbl (m/round-significant m/tiny-dbl 1 :up))
    (t/is= (- m/tiny-dbl) (m/round-significant (- m/tiny-dbl) 1 :down))))

(deftest floor-test
  (t/with-instrument `m/floor
    (t/is (t/spec-check m/floor)))
  (t/with-instrument :all
    (t/is (zero? (m/floor 0.4)))
    (t/is= 2.3423423423423425E26 (m/floor 234234234234234234234343242N))
    (t/is= -1.0 (m/floor -0.4))
    (t/is= m/inf+ (m/floor m/inf+))
    (t/is= m/inf- (m/floor m/inf-))
    (t/is (m/nan? (m/floor m/nan)))))

(deftest floor'-test
  (t/with-instrument `m/floor'
    (t/is (t/spec-check m/floor')))
  (t/with-instrument :all
    (t/is= -1 (m/floor' -0.4))))

(deftest ceil-test
  (t/with-instrument `m/ceil
    (t/is (t/spec-check m/ceil)))
  (t/with-instrument :all
    (t/is= 1.0 (m/ceil 0.4))
    (t/is= 2.3423423423423425E26 (m/ceil 234234234234234234234343242N))
    (t/is (zero? (m/ceil -0.4)))
    (t/is= m/inf+ (m/ceil m/inf+))
    (t/is= m/inf- (m/ceil m/inf-))
    (t/is (m/nan? (m/ceil m/nan)))))

(deftest ceil'-test
  (t/with-instrument `m/ceil'
    (t/is (t/spec-check m/ceil')))
  (t/with-instrument :all
    (t/is= 1 (m/ceil' 0.4))))

(deftest roughly-floor-test
  (t/with-instrument `m/roughly-floor
    (t/is (t/spec-check m/roughly-floor)))
  (t/with-instrument :all
    (t/is= 1.0 (m/roughly-floor 0.99 0.02))
    (t/is (zero? (m/roughly-floor 0.99 0.005)))
    (t/is= 2.3423423423423425E26 (m/roughly-floor 234234234234234234234343242N 0.02))
    (t/is= 2.3423423423423425E26 (m/roughly-floor 2.3423423423423425E26 0.02))
    (t/is (zero? (m/roughly-floor -0.01 0.02)))
    (t/is= m/inf+ (m/roughly-floor m/inf+ 0.02))
    (t/is= m/inf- (m/roughly-floor m/inf- 0.02))
    (t/is (m/nan? (m/roughly-floor m/nan 0.02)))))

(deftest roughly-floor'-test
  (t/with-instrument `m/roughly-floor'
    (t/is (t/spec-check m/roughly-floor')))
  (t/with-instrument :all
    (t/is= 1 (m/roughly-floor' 0.99 0.02))))

(deftest roughly-ceil-test
  (t/with-instrument `m/roughly-ceil
    (t/is (t/spec-check m/roughly-ceil)))
  (t/with-instrument :all
    (t/is (zero? (m/roughly-ceil 0.01 0.02)))
    (t/is= 1.0 (m/roughly-ceil 0.01 0.005))
    (t/is= 2.3423423423423425E26 (m/roughly-ceil 234234234234234234234343242N 0.02))
    (t/is= 2.3423423423423425E26 (m/roughly-ceil 2.3423423423423425E26 0.02))
    (t/is= -1.0 (m/roughly-ceil -0.99 0.02))
    (t/is= m/inf+ (m/roughly-ceil m/inf+ 0.02))
    (t/is= m/inf- (m/roughly-ceil m/inf- 0.02))
    (t/is (m/nan? (m/roughly-ceil m/nan 0.02)))))

(deftest roughly-ceil'-test
  (t/with-instrument `m/roughly-ceil'
    (t/is (t/spec-check m/roughly-ceil')))
  (t/with-instrument :all
    (t/is= 1 (m/roughly-ceil' 0.01 0.005))))

(deftest roughly?-test
  (t/with-instrument `m/roughly?
    (t/is (t/spec-check m/roughly?)))
  (t/with-instrument :all
    (t/is-not (m/roughly? 0.01 0.02 0.005))
    (t/is (m/roughly? 0.01 0.02 0.01))
    (t/is (m/roughly? 0.01 0.02 0.02))
    (t/is (m/roughly? 2.3423423423423425E26 2.3423423423423425E26 0.03))
    (t/is (m/roughly? 2.3423423423423425E26 2.3423423423423425E26 0.005))
    (t/is-not (m/roughly? m/inf+ m/inf+ 0.01))
    (t/is (m/roughly? m/inf- 0.02 m/inf+))
    (t/is-not (m/roughly? m/nan 0.02 0.01))))

(deftest roughly-round?-test
  (t/with-instrument `m/roughly-round?
    (t/is (t/spec-check m/roughly-round?)))
  (t/with-instrument :all
    (t/is (m/roughly-round? 0.01 0.02))
    (t/is-not (m/roughly-round? 0.01 0.005))
    (t/is (m/roughly-round? 2.3423423423423425E26 0.03))
    (t/is (m/roughly-round? 2.3423423423423425E26 0.005))
    (t/is (m/roughly-round? m/inf+ m/inf+))
    (t/is-not (m/roughly-round? m/inf- 0.4))
    (t/is-not (m/roughly-round? m/nan 0.01))))

(deftest roughly-round-non-?-test
  (t/with-instrument `m/roughly-round-non-?
    (t/is (t/spec-check m/roughly-round-non-?)))
  (t/with-instrument :all
    (t/is (m/roughly-round-non-? 0 0.02))
    (t/is-not (m/roughly-round-non-? -0.01 0.02))
    (t/is (m/roughly-round-non-? 0.01 0.02))
    (t/is-not (m/roughly-round-non-? 0.01 0.005))
    (t/is (m/roughly-round-non-? 2.3423423423423425E26 0.03))
    (t/is (m/roughly-round-non-? 2.3423423423423425E26 0.005))
    (t/is (m/roughly-round-non-? m/inf+ m/inf+))
    (t/is-not (m/roughly-round-non-? m/inf- m/inf+))
    (t/is-not (m/roughly-round-non-? m/inf+ 0.4))
    (t/is-not (m/roughly-round-non-? m/nan 0.01))))

(deftest roughly-round-non+?-test
  (t/with-instrument `m/roughly-round-non+?
    (t/is (t/spec-check m/roughly-round-non+?)))
  (t/with-instrument :all
    (t/is (m/roughly-round-non+? 0 0.02))
    (t/is-not (m/roughly-round-non+? 0.01 0.02))
    (t/is (m/roughly-round-non+? -0.01 0.02))
    (t/is-not (m/roughly-round-non+? -0.01 0.005))
    (t/is (m/roughly-round-non+? -2.3423423423423425E26 0.03))
    (t/is (m/roughly-round-non+? -2.3423423423423425E26 0.005))
    (t/is-not (m/roughly-round-non+? m/inf+ m/inf+))
    (t/is (m/roughly-round-non+? m/inf- m/inf+))
    (t/is-not (m/roughly-round-non+? m/inf- 0.4))
    (t/is-not (m/roughly-round-non+? m/nan 0.01))))

(deftest roughly-round+?-test
  (t/with-instrument `m/roughly-round+?
    (t/is (t/spec-check m/roughly-round+?)))
  (t/with-instrument :all
    (t/is-not (m/roughly-round+? 0 0.02))
    (t/is-not (m/roughly-round+? -0.01 0.02))
    (t/is (m/roughly-round+? 0.01 0.02))
    (t/is-not (m/roughly-round+? 0.01 0.005))
    (t/is (m/roughly-round+? 2.3423423423423425E26 0.03))
    (t/is (m/roughly-round+? 2.3423423423423425E26 0.005))
    (t/is (m/roughly-round+? m/inf+ m/inf+))
    (t/is-not (m/roughly-round+? m/inf- m/inf+))
    (t/is-not (m/roughly-round+? m/inf+ 0.4))
    (t/is-not (m/roughly-round+? m/nan 0.01))))

(deftest roughly-round-?-test
  (t/with-instrument `m/roughly-round-?
    (t/is (t/spec-check m/roughly-round-?)))
  (t/with-instrument :all
    (t/is-not (m/roughly-round-? 0 0.02))
    (t/is-not (m/roughly-round-? 0.01 0.02))
    (t/is (m/roughly-round-? -0.01 0.02))
    (t/is-not (m/roughly-round-? -0.01 0.005))
    (t/is (m/roughly-round-? -2.3423423423423425E26 0.03))
    (t/is (m/roughly-round-? -2.3423423423423425E26 0.005))
    (t/is-not (m/roughly-round-? m/inf+ m/inf+))
    (t/is (m/roughly-round-? m/inf- m/inf+))
    (t/is-not (m/roughly-round-? m/inf- 0.4))
    (t/is-not (m/roughly-round-? m/nan 0.01))))

(deftest roughly-non-?-test
  (t/with-instrument `m/roughly-non-?
    (t/is (t/spec-check m/roughly-non-?)))
  (t/with-instrument :all
    (t/is-not (m/roughly-non-? -0.01 0.005))
    (t/is (m/roughly-non-? -0.02 0.02))
    (t/is (m/roughly-non-? 0.01 0.001))
    (t/is (m/roughly-non-? m/inf+ m/inf+))
    (t/is (m/roughly-non-? m/inf- m/inf+))
    (t/is-not (m/roughly-non-? m/inf- 0.4))
    (t/is (m/roughly-non-? m/inf+ 0.4))
    (t/is-not (m/roughly-non-? m/nan 0.01))))

(deftest roughly-non+?-test
  (t/with-instrument `m/roughly-non+?
    (t/is (t/spec-check m/roughly-non+?)))
  (t/with-instrument :all
    (t/is-not (m/roughly-non+? 0.01 0.005))
    (t/is (m/roughly-non+? 0.02 0.02))
    (t/is (m/roughly-non+? -0.01 0.001))
    (t/is (m/roughly-non+? m/inf+ m/inf+))
    (t/is (m/roughly-non+? m/inf- m/inf+))
    (t/is (m/roughly-non+? m/inf- 0.4))
    (t/is-not (m/roughly-non+? m/inf+ 0.4))
    (t/is-not (m/roughly-non+? m/nan 0.01))))

(deftest roughly-prob?-test
  (t/with-instrument `m/roughly-prob?
    (t/is (t/spec-check m/roughly-prob?)))
  (t/with-instrument :all
    (t/is (m/roughly-prob? 0.01 0.005))
    (t/is (m/roughly-prob? 0.02 0.02))
    (t/is-not (m/roughly-prob? -0.01 0.001))
    (t/is (m/roughly-prob? 1.01 0.01))
    (t/is (m/roughly-prob? 1.01 0.01))
    (t/is (m/roughly-prob? m/inf+ m/inf+))
    (t/is (m/roughly-prob? m/inf- m/inf+))
    (t/is-not (m/roughly-prob? m/inf- 0.4))
    (t/is-not (m/roughly-prob? m/inf+ 0.4))
    (t/is-not (m/roughly-prob? m/nan 0.01))))

(deftest roughly-corr?-test
  (t/with-instrument `m/roughly-corr?
    (t/is (t/spec-check m/roughly-corr?)))
  (t/with-instrument :all
    (t/is-not (m/roughly-corr? -1.01 0.005))
    (t/is (m/roughly-corr? -1.02 0.02))
    (t/is-not (m/roughly-corr? -1.01 0.001))
    (t/is (m/roughly-corr? 1.01 0.01))
    (t/is (m/roughly-corr? 1.01 0.01))
    (t/is (m/roughly-corr? m/inf+ m/inf+))
    (t/is (m/roughly-corr? m/inf- m/inf+))
    (t/is-not (m/roughly-corr? m/inf- 0.4))
    (t/is-not (m/roughly-corr? m/inf+ 0.4))
    (t/is-not (m/roughly-corr? m/nan 0.01))))

;;;QUOTIENTS
(deftest quot'-test
  (t/with-instrument `m/quot'
    (t/is (t/spec-check m/quot')))
  (t/with-instrument :all
    (t/is= 1 (m/quot' 3 2))
    (t/is= -1 (m/quot' -3 2))
    (t/is= -1 (m/quot' 3 -2))
    (t/is= 1 (m/quot' -3 -2))
    (t/is= 1 (m/quot' 3.0 2))
    (t/is= 1 (m/quot' 3 2.0))
    (t/is= 1 (m/quot' 3 2.12))
    (t/is= 1.4150943396226415E40 (m/quot' 3.0E40 2.12))
    (t/is (m/nan? (m/quot' m/inf+ 3)))
    (t/is (m/nan? (m/quot' m/inf- 4)))
    (t/is (m/nan? (m/quot' m/inf+ -3)))
    (t/is (m/nan? (m/quot' m/inf- -4)))
    (t/is (m/nan? (m/quot' m/nan 2)))
    (t/is (m/nan? (m/quot' 3 m/inf+)))
    (t/is (m/nan? (m/quot' 4 m/inf-)))
    (t/is (m/nan? (m/quot' 2 m/nan)))))

(deftest mod'-test
  (t/with-instrument `m/mod'
    (t/is (t/spec-check m/mod')))
  (t/with-instrument :all
    (t/is= 1 (m/mod' 3 2))
    (t/is= 1 (m/mod' -3 2))
    (t/is= -1 (m/mod' 3 -2))
    (t/is= -1 (m/mod' -3 -2))
    (t/is= 1 (m/mod' 3.0 2))
    (t/is= 1 (m/mod' 3 2.0))
    (t/is= 0.8799999999999999 (m/mod' 3 2.12))
    (t/is (zero? (m/mod' 3.0E40 2.12)))
    (t/is (m/nan? (m/mod' m/inf+ 3)))
    (t/is (m/nan? (m/mod' m/inf- 4)))
    (t/is (m/nan? (m/mod' m/nan 2)))
    (t/is (m/nan? (m/mod' 3 m/inf+)))
    (t/is (m/nan? (m/mod' 4 m/inf-)))
    (t/is (m/nan? (m/mod' -3 m/inf+)))
    (t/is (m/nan? (m/mod' -4 m/inf-)))
    (t/is (m/nan? (m/mod' 2 m/nan)))))

(deftest rem'-test
  (t/with-instrument `m/rem'
    (t/is (t/spec-check m/rem')))
  (t/with-instrument :all
    (t/is= 1 (m/rem' 3 2))
    (t/is= -1 (m/rem' -3 2))
    (t/is= 1 (m/rem' 3 -2))
    (t/is= -1 (m/rem' -3 -2))
    (t/is= 1 (m/rem' 3.0 2))
    (t/is= 1 (m/rem' 3 2.0))
    (t/is= 0.8799999999999999 (m/rem' 3 2.12))
    (t/is (zero? (m/rem' 3.0E40 2.12)))
    (t/is (m/nan? (m/rem' m/inf+ 3)))
    (t/is (m/nan? (m/rem' m/inf- 4)))
    (t/is (m/nan? (m/rem' m/nan 2)))
    (t/is (m/nan? (m/rem' 3 m/inf+)))
    (t/is (m/nan? (m/rem' 4 m/inf-)))
    (t/is (m/nan? (m/rem' -3 m/inf+)))
    (t/is (m/nan? (m/rem' -4 m/inf-)))
    (t/is (m/nan? (m/rem' 2 m/nan)))))

(deftest quot-and-rem'-test
  (t/with-instrument `m/quot-and-rem'
    (t/is (t/spec-check m/quot-and-rem')))
  (t/with-instrument :all
    (t/is= [4 0] (m/quot-and-rem' 16 4))
    (t/is= [1 1] (m/quot-and-rem' 3 2))
    (t/is= [-1 -1] (m/quot-and-rem' -3 2))
    (t/is= [-1 1] (m/quot-and-rem' 3 -2))
    (t/is= [1 -1] (m/quot-and-rem' -3 -2))
    (t/is= [0 3] (m/quot-and-rem' 3 4))
    (t/is= [0 -3] (m/quot-and-rem' -3 4))
    (t/is= [0 3] (m/quot-and-rem' 3 -4))
    (t/is= [0 -3] (m/quot-and-rem' -3 -4))
    (t/is= [1 1] (m/quot-and-rem' 3.0 2))
    (t/is= [1 1] (m/quot-and-rem' 3 2.0))
    (t/is= [1 0.8799999999999999] (m/quot-and-rem' 3 2.12))
    (t/is= [1.4150943396226415E40 0] (m/quot-and-rem' 3.0E40 2.12))
    (t/is (every? m/nan? (m/quot-and-rem' m/inf+ 3)))
    (t/is (every? m/nan? (m/quot-and-rem' m/inf- 4)))
    (t/is (every? m/nan? (m/quot-and-rem' m/nan 2)))
    (t/is (every? m/nan? (m/quot-and-rem' 3 m/inf+)))
    (t/is (every? m/nan? (m/quot-and-rem' 2 m/nan)))))

(deftest quot-and-mod'-test
  (t/with-instrument `m/quot-and-mod'
    (t/is (t/spec-check m/quot-and-mod')))
  (t/with-instrument :all
    (t/is= [4 0] (m/quot-and-mod' 16 4))
    (t/is= [0 0] (m/quot-and-mod' 0 4))
    (t/is= [0 0] (m/quot-and-mod' 0 -4))
    (t/is (every? m/nan? (m/quot-and-mod' 4 0)))
    (t/is (every? m/nan? (m/quot-and-mod' -4 0)))
    (t/is (every? m/nan? (m/quot-and-mod' 0 0)))
    (t/is= [1 1] (m/quot-and-mod' 3 2))
    (t/is= [-2 1] (m/quot-and-mod' -3 2))
    (t/is= [-2 -1] (m/quot-and-mod' 3 -2))
    (t/is= [1 -1] (m/quot-and-mod' -3 -2))
    (t/is= [0 3] (m/quot-and-mod' 3 4))
    (t/is= [-1 1] (m/quot-and-mod' -3 4))
    (t/is= [-1 -1] (m/quot-and-mod' 3 -4))
    (t/is= [0 -3] (m/quot-and-mod' -3 -4))
    (t/is= [1 1] (m/quot-and-mod' 3.0 2))
    (t/is= [1 1] (m/quot-and-mod' 3 2.0))
    (t/is= [1 0.8799999999999999] (m/quot-and-mod' 3 2.12))
    (t/is= [1.4150943396226415E40 0] (m/quot-and-mod' 3.0E40 2.12))
    (t/is (every? m/nan? (m/quot-and-mod' m/inf+ 3)))
    (t/is (every? m/nan? (m/quot-and-mod' m/inf- 4)))
    (t/is (every? m/nan? (m/quot-and-mod' m/nan 2)))
    (t/is (every? m/nan? (m/quot-and-mod' 3 m/inf+)))
    (t/is (every? m/nan? (m/quot-and-mod' 4 m/inf-)))
    (t/is (every? m/nan? (m/quot-and-mod' -3 m/inf+)))
    (t/is (every? m/nan? (m/quot-and-mod' -4 m/inf-)))
    (t/is (every? m/nan? (m/quot-and-mod' 2 m/nan)))))

(deftest gcd-test
  (t/with-instrument `m/gcd
    (t/is (t/spec-check m/gcd)))
  (t/with-instrument :all
    (t/is= 7 (m/gcd 271284701247 12467364728))
    (t/is= 6 (m/gcd 12 18))
    (t/is= 6 (m/gcd -12 18))
    (t/is= 6 (m/gcd 12 -18))
    (t/is= 6 (m/gcd -12 -18))
    (t/is= 5 (m/gcd 0 5))
    (t/is= 5 (m/gcd 5 0))
    (t/is= 0 (m/gcd 0 0))))

(deftest lcm'-test
  (t/with-instrument `m/lcm'
    (t/is (t/spec-check m/lcm')))
  (t/with-instrument :all
    (t/is= 36 (m/lcm' 12 18))
    (t/is= 36 (m/lcm' -12 18))
    (t/is= 36 (m/lcm' 12 -18))
    (t/is= 36 (m/lcm' -12 -18))
    (t/is= 0 (m/lcm' 0 5))
    (t/is= 0 (m/lcm' 5 0))
    (t/is= 0 (m/lcm' 0 0))
    (t/is= 12 (m/lcm' 4 6))
    (t/is= 15 (m/lcm' 5 15))))

;;;ANGLES
(deftest reduce-angle'-test
  (t/with-instrument `m/reduce-angle'
    (t/is (t/spec-check m/reduce-angle')))
  (t/with-instrument :all
    (t/is= 30.4 (m/reduce-angle' 30.4))
    (t/is= 350.2 (m/reduce-angle' -9.8))
    (t/is= 118 (m/reduce-angle' 478.0))
    (t/is= 26 (m/reduce-angle' -8399494))
    (t/is (m/nan? (m/reduce-angle' m/nan)))
    (t/is (m/nan? (m/reduce-angle' m/inf+)))
    (t/is (m/nan? (m/reduce-angle' m/inf-)))))

(deftest reduce-radians'-test
  (t/with-instrument `m/reduce-radians'
    (t/is (t/spec-check m/reduce-radians')))
  (t/with-instrument :all
    (t/is= 5.267258771281654 (m/reduce-radians' 30.4))
    (t/is (zero? (m/reduce-radians' m/two-pi)))
    (t/is= m/PI (m/reduce-radians' m/PI))
    (t/is= 0.06552912132908517 (m/reduce-radians' -8399494))
    (t/is (m/nan? (m/reduce-radians' m/nan)))
    (t/is (m/nan? (m/reduce-radians' m/inf+)))
    (t/is (m/nan? (m/reduce-radians' m/inf-)))))

(deftest radians->angle'-test
  (t/with-instrument `m/radians->angle'
    (t/is (t/spec-check m/radians->angle')))
  (t/with-instrument :all
    (t/is (zero? (m/radians->angle' 0)))
    (t/is= 194.8056503444799 (m/radians->angle' 3.4))
    (t/is (zero? (m/radians->angle' m/two-pi)))
    (t/is= 165.1943496555201 (m/radians->angle' -3.4))
    (t/is= 58.31007808870436 (m/radians->angle' 45))
    (t/is (m/nan? (m/radians->angle' m/nan)))
    (t/is= m/inf+ (m/radians->angle' m/inf+))
    (t/is= m/inf- (m/radians->angle' m/inf-))))

(deftest angle->radians'-test
  (t/with-instrument `m/angle->radians'
    (t/is (t/spec-check m/angle->radians')))
  (t/with-instrument :all
    (t/is (zero? (m/angle->radians' 0)))
    (t/is= 0.059341194567807204 (m/angle->radians' 3.4))
    (t/is= 0.002777777777777778 (m/angle->radians' m/inv-two-pi))
    (t/is= 6.223844112611779 (m/angle->radians' -3.4))
    (t/is= 0.7853981633974483 (m/angle->radians' 45))
    (t/is (m/nan? (m/angle->radians' m/nan)))
    (t/is= m/inf+ (m/angle->radians' m/inf+))
    (t/is= m/inf- (m/angle->radians' m/inf-))))
