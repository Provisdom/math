(ns provisdom.math.core-test
  (:refer-clojure :exclude [pos? neg? int?])
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.core :as m]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;;7 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

;;TYPE TESTS
(deftest numbers?-test
  (is-not (m/numbers? 1))
  (is (m/numbers? '(1)))
  (is (m/numbers? []))
  (is (m/numbers? [2 3]))
  (is-not (m/numbers? [[2]])))

(deftest num?-test
  (is-not (m/num? "A"))
  (is (m/num? 3.3E30))
  (is (m/num? -3.3E30))
  (is (m/num? m/inf+))
  (is (m/num? m/inf-))
  (is-not (m/num? m/nan)))

(deftest nan?-test
  (is-not (m/nan? 3.3))
  (is-not (m/nan? "A"))
  (is-not (m/nan? m/inf+))
  (is-not (m/nan? m/inf-))
  (is (m/nan? m/nan)))

(deftest pos?-test
  (is-not (m/pos? "A"))
  (is (m/pos? 3.3E30))
  (is-not (m/pos? -3.3E30))
  (is (m/pos? m/inf+))
  (is-not (m/pos? m/inf-))
  (is-not (m/pos? m/nan)))

(deftest neg?-test
  (is-not (m/neg? "A"))
  (is-not (m/neg? 3.3E30))
  (is (m/neg? -3.3E30))
  (is-not (m/neg? m/inf+))
  (is (m/neg? m/inf-))
  (is-not (m/neg? m/nan)))

(deftest non-?-test
  (is (m/non-? 0))
  (is (m/non-? 1))
  (is-not (m/non-? -1))
  (is (m/non-? m/inf+))
  (is-not (m/non-? m/inf-))
  (is-not (m/non-? m/nan))
  (is-not (m/non-? "A")))

(deftest non+?-test
  (is (m/non+? 0))
  (is-not (m/non+? 1))
  (is (m/non+? -1))
  (is-not (m/non+? m/inf+))
  (is (m/non+? m/inf-))
  (is-not (m/non+? m/nan))
  (is-not (m/non+? "A")))

(deftest finite?-test
  (is-not (m/finite? "A"))
  (is (m/finite? 3.3E30))
  (is (m/finite? -3.3E30))
  (is-not (m/finite? m/inf+))
  (is-not (m/finite? m/inf-))
  (is-not (m/finite? m/nan)))

(deftest finite+?-test
  (is-not (m/finite+? "A"))
  (is (m/finite+? 3.3E30))
  (is-not (m/finite+? -3.3E30))
  (is-not (m/finite+? m/inf+))
  (is-not (m/finite+? m/inf-))
  (is-not (m/finite+? m/nan)))

(deftest finite-?-test
  (is-not (m/finite-? "A"))
  (is-not (m/finite-? 3.3E30))
  (is (m/finite-? -3.3E30))
  (is-not (m/finite-? m/inf+))
  (is-not (m/finite-? m/inf-))
  (is-not (m/finite-? m/nan)))

(deftest finite-non-?-test
  (is (m/finite-non-? 0))
  (is (m/finite-non-? 1))
  (is-not (m/finite-non-? -1))
  (is-not (m/finite-non-? m/inf+))
  (is-not (m/finite-non-? m/inf-))
  (is-not (m/finite-non-? m/nan))
  (is-not (m/finite-non-? "A")))

(deftest finite-non+?-test
  (is (m/finite-non+? 0))
  (is-not (m/finite-non+? 1))
  (is (m/finite-non+? -1))
  (is-not (m/finite-non+? m/inf+))
  (is-not (m/finite-non+? m/inf-))
  (is-not (m/finite-non+? m/nan))
  (is-not (m/finite-non+? "A")))

(deftest double-finite?-test
  (is-not (m/double-finite? "A"))
  (is-not (m/double-finite? 0))
  (is (m/double-finite? 0.0))
  (is (m/double-finite? 3.3E300))
  (is (m/double-finite? -3.3E300))
  (is-not (m/double-finite? m/inf+))
  (is-not (m/double-finite? m/inf-))
  (is-not (m/double-finite? m/nan)))

(deftest double-finite+?-test
  (is-not (m/double-finite? "A"))
  (is-not (m/double-finite? 0))
  (is-not (m/double-finite? 0.0))
  (is (m/double-finite? 3.3E300))
  (is-not (m/double-finite? -3.3E300))
  (is-not (m/double-finite? m/inf+))
  (is-not (m/double-finite? m/inf-))
  (is-not (m/double-finite? m/nan)))

(deftest single?-test
  (is-not (m/single? "A"))
  (is-not (m/single? 0))
  (is (m/single? 0.0))
  (is (m/single? 3.3E30))
  (is (m/single? -3.3E30))
  (is-not (m/single? 3.3E300))
  (is-not (m/single? -3.3E300))
  (is (m/single? m/inf+))
  (is (m/single? m/inf-))
  (is (m/single? m/nan)))

(deftest single-finite?-test
  (is-not (m/single-finite? "A"))
  (is-not (m/single-finite? 0))
  (is (m/single-finite? 0.0))
  (is (m/single-finite? 3.3E30))
  (is (m/single-finite? -3.3E30))
  (is-not (m/single-finite? 3.3E300))
  (is-not (m/single-finite? -3.3E300))
  (is-not (m/single-finite? m/inf+))
  (is-not (m/single-finite? m/inf-))
  (is-not (m/single-finite? m/nan)))

(deftest long?-test
  (is-not (m/long? 3.3))
  (is (m/long? 3))
  (is-not (m/long? 3.0))
  (is-not (m/long? "A"))
  (is-not (m/long? 3.4E15))
  (is-not (m/long? 3.3E30))
  (is-not (m/long? -3.3E30))
  (is-not (m/long? m/nan)))

(deftest long+?-test
  (is-not (m/long+? 3.3))
  (is (m/long+? 3))
  (is-not (m/long+? -3))
  (is-not (m/long+? 3.0))
  (is-not (m/long+? "A"))
  (is-not (m/long+? 3.4E15))
  (is-not (m/long+? 3.3E30))
  (is-not (m/long+? -3.3E30))
  (is-not (m/long+? m/nan)))

(deftest long-?-test
  (is-not (m/long-? 3.3))
  (is-not (m/long-? 3))
  (is (m/long-? -3))
  (is-not (m/long-? 3.0))
  (is-not (m/long-? "A"))
  (is-not (m/long-? -3.4E15))
  (is-not (m/long-? 3.3E30))
  (is-not (m/long-? -3.3E30))
  (is-not (m/long-? m/nan)))

(deftest long-non-?-test
  (is-not (m/long-non-? 3.3))
  (is (m/long-non-? 3))
  (is-not (m/long-non-? -3))
  (is (m/long-non-? 0))
  (is-not (m/long-non-? 3.0))
  (is-not (m/long-non-? "A"))
  (is-not (m/long-non-? 3.4E15))
  (is-not (m/long-non-? 3.3E30))
  (is-not (m/long-non-? -3.3E30))
  (is-not (m/long-non-? m/nan)))

(deftest long-non+?-test
  (is-not (m/long-non+? 3.3))
  (is-not (m/long-non+? 3))
  (is (m/long-non+? -3))
  (is (m/long-non+? 0))
  (is-not (m/long-non+? 3.0))
  (is-not (m/long-non+? "A"))
  (is-not (m/long-non+? 3.4E15))
  (is-not (m/long-non+? 3.3E30))
  (is-not (m/long-non+? -3.3E30))
  (is-not (m/long-non+? m/nan)))

(deftest int?-test
  (is-not (m/int? 3.3))
  (is (m/int? 3))
  (is-not (m/int? 3.0))
  (is-not (m/int? "A"))
  (is-not (m/int? 3.4E15))
  (is-not (m/int? 3.3E30))
  (is-not (m/int? -3.3E30))
  (is-not (m/int? m/nan)))

(deftest int+?-test
  (is-not (m/int+? 3.3))
  (is (m/int+? 3))
  (is-not (m/int+? -3))
  (is-not (m/int+? 3.0))
  (is-not (m/int+? "A"))
  (is-not (m/int+? 3.4E15))
  (is-not (m/int+? 3.3E30))
  (is-not (m/int+? -3.3E30))
  (is-not (m/int+? m/nan)))

(deftest int-?-test
  (is-not (m/int-? 3.3))
  (is-not (m/int-? 3))
  (is (m/int-? -3))
  (is-not (m/int-? 3.0))
  (is-not (m/int-? "A"))
  (is-not (m/int-? -3.4E15))
  (is-not (m/int-? 3.3E30))
  (is-not (m/int-? -3.3E30))
  (is-not (m/int-? m/nan)))

(deftest int-non-?-test
  (is-not (m/int-non-? 3.3))
  (is (m/int-non-? 3))
  (is-not (m/int-non-? -3))
  (is (m/int-non-? 0))
  (is-not (m/int-non-? 3.0))
  (is-not (m/int-non-? "A"))
  (is-not (m/int-non-? 3.4E15))
  (is-not (m/int-non-? 3.3E30))
  (is-not (m/int-non-? -3.3E30))
  (is-not (m/int-non-? m/nan)))

(deftest int-non+?-test
  (is-not (m/int-non+? 3.3))
  (is-not (m/int-non+? 3))
  (is (m/int-non+? -3))
  (is (m/int-non+? 0))
  (is-not (m/int-non+? 3.0))
  (is-not (m/int-non+? "A"))
  (is-not (m/int-non+? 3.4E15))
  (is-not (m/int-non+? 3.3E30))
  (is-not (m/int-non+? -3.3E30))
  (is-not (m/int-non+? m/nan)))

(deftest long-able?-test
  (is-not (m/long-able? 3.3))
  (is (m/long-able? 3))
  (is (m/long-able? 3.0))
  (is-not (m/long-able? "A"))
  (is (m/long-able? 3.4E15))
  (is-not (m/long-able? 3.3E30))
  (is-not (m/long-able? -3.3E30))
  (is-not (m/long-able? m/nan)))

(deftest long-able+?-test
  (is-not (m/long-able+? 3.3))
  (is-not (m/long-able+? 0.0))
  (is (m/long-able+? 1.0))
  (is-not (m/long-able+? -1.0))
  (is-not (m/long-able+? -3.3)))

(deftest long-able-?-test
  (is-not (m/long-able-? 3.3))
  (is-not (m/long-able-? 0.0))
  (is-not (m/long-able-? 1.0))
  (is (m/long-able-? -1.0))
  (is-not (m/long-able-? -3.3)))

(deftest long-able-non+?-test
  (is-not (m/long-able-non+? 3.3))
  (is (m/long-able-non+? 0.0))
  (is-not (m/long-able-non+? 1.0))
  (is (m/long-able-non+? -1.0))
  (is-not (m/long-able-non+? -3.3)))

(deftest long-able-non-?-test
  (is-not (m/long-able-non-? 3.3))
  (is (m/long-able-non-? 0.0))
  (is (m/long-able-non-? 1.0))
  (is-not (m/long-able-non-? -1.0))
  (is-not (m/long-able-non-? -3.3)))

(deftest inf+?-test
  (is-not (m/inf+? 3.3))
  (is-not (m/inf+? "A"))
  (is (m/inf+? m/inf+))
  (is-not (m/inf+? m/inf-))
  (is-not (m/inf+? m/nan)))

(deftest inf-?-test
  (is-not (m/inf-? 3.3))
  (is-not (m/inf-? "A"))
  (is-not (m/inf-? m/inf+))
  (is (m/inf-? m/inf-))
  (is-not (m/inf-? m/nan)))

(deftest inf?-test
  (is-not (m/inf? 3.3))
  (is-not (m/inf? "A"))
  (is (m/inf? m/inf+))
  (is (m/inf? m/inf-))
  (is-not (m/inf? m/nan)))

(deftest one?-test
  (is (m/one? 1))
  (is (m/one? 1.0))
  (is-not (m/one? "A"))
  (is-not (m/one? -1))
  (is-not (m/one? m/nan)))

(deftest prob?-test
  (is-not (m/prob? -0.5))
  (is (m/prob? 0))
  (is (m/prob? 0.5))
  (is (m/prob? 1))
  (is-not (m/prob? 1.5))
  (is-not (m/prob? m/inf+))
  (is-not (m/prob? m/inf-))
  (is-not (m/prob? "A"))
  (is-not (m/prob? m/nan)))

(deftest open-prob?-test
  (is-not (m/open-prob? -0.5))
  (is-not (m/open-prob? 0))
  (is (m/open-prob? 0.5))
  (is-not (m/open-prob? 1))
  (is-not (m/open-prob? 1.5))
  (is-not (m/open-prob? m/inf+))
  (is-not (m/open-prob? m/inf-))
  (is-not (m/open-prob? m/nan))
  (is-not (m/open-prob? "A")))

(deftest corr?-test
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
  (is-not (m/corr? "A")))

(deftest open-corr?-test
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
  (is-not (m/open-corr? "A")))

(deftest maybe-long-able-test
  (is (zero? (m/maybe-long-able 0.0)))
  (is (zero? (m/maybe-long-able 0)))
  (is= 23423423 (m/maybe-long-able 2.3423423E7))
  (is= 234234324234234234234N (m/maybe-long-able 234234324234234234234N))
  (is= 2.3423432423423423E20 (m/maybe-long-able 2.3423432423423423E20))
  (is= m/inf+ (m/maybe-long-able m/inf+))
  (is= m/inf- (m/maybe-long-able m/inf-))
  (is (m/nan? (m/maybe-long-able m/nan)))
  (is (nil? (m/maybe-long-able nil))))

;;BASIC MATH TESTS
(deftest ===-test
  (is (spec-check m/===))
  (is (m/=== m/nan))
  (is (m/=== m/nan m/nan))
  (is (m/=== m/nan m/nan m/nan))
  (is (m/=== 3 3 3 3))
  (is-not (m/=== 3 m/nan)))

(deftest next-up-test
  (is (spec-check m/next-up))
  (is= 3.0000000000000004 (m/next-up 3))
  (is= -2.9999999999999996 (m/next-up -3))
  (is (m/nan? (m/next-up m/nan)))
  (is= m/inf+ (m/next-up m/inf+))
  (is= m/min-dbl (m/next-up m/inf-))
  (is= -2.9999999999999996 (m/next-up -3.0)))

(deftest next-down-test
  (is (spec-check m/next-down))
  (is= 2.9999999999999996 (m/next-down 3))
  (is= -3.0000000000000004 (m/next-down -3))
  (is (m/nan? (m/next-down m/nan)))
  (is= m/max-dbl (m/next-down m/inf+))
  (is= m/inf- (m/next-down m/inf-))
  (is= 2.9999999999999996 (m/next-down 3.0)))

(deftest div-test
  (is (spec-check m/div))
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
  (is (m/nan? (m/div 0 0 m/nan))))

(deftest one--test
  (is (spec-check m/one-))
  (is= -2 (m/one- 3))
  (is= 0.0 (m/one- 3 -2))
  (is= -16.0 (m/one- 3 4 2 8))
  (is= 4 (m/one- -3))
  (is (m/nan? (m/one- m/nan)))
  (is= m/inf- (m/one- m/inf+))
  (is= m/inf+ (m/one- m/inf-))
  (is= 4.0 (m/one- -3.0)))

(deftest sq'-test
  (is (spec-check m/sq'))
  (is= 9 (m/sq' 3))
  (is= 9 (m/sq' -3))
  (is (m/nan? (m/sq' m/nan)))
  (is= m/inf+ (m/sq' m/inf+))
  (is= m/inf+ (m/sq' m/inf-))
  (is= 9 (m/sq' -3.0)))

(deftest cube'-test
  (is (spec-check m/cube'))
  (is= 27 (m/cube' 3))
  (is (m/nan? (m/cube' m/nan)))
  (is= m/inf+ (m/cube' m/inf+))
  (is= m/inf- (m/cube' m/inf-))
  (is= -27 (m/cube' -3))
  (is= -27 (m/cube' -3.0)))

(deftest sgn-test
  (is (spec-check m/sgn))
  (is= 1 (m/sgn 3))
  (is= -1 (m/sgn -3))
  (is (m/nan? (m/sgn m/nan)))
  (is= 1 (m/sgn m/inf+))
  (is= -1 (m/sgn m/inf-))
  (is (zero? (m/sgn 0)))
  (is (zero? (m/sgn 0.0)))
  (is= -1 (m/sgn -3.0)))

(deftest log2-test
  (is (spec-check m/log2))
  (is= 1.5849625007211563 (m/log2 3))
  (is= m/inf- (m/log2 0))
  (is= m/inf+ (m/log2 m/inf+))
  (is (m/nan? (m/log2 m/nan)))
  (is (m/nan? (m/log2 -3.0)))
  (is= 0.0 (m/log2 1.0))
  (is= -0.15200309344504997 (m/log2 0.9)))

(deftest logn-test
  (is (spec-check m/logn))
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
  (is= -0.0 (m/logn 0.9 m/inf+)))

(deftest abs'-test
  (is (spec-check m/abs'))
  (is= 3.3 (m/abs' -3.3))
  (is= 3 (m/abs' -3))
  (is= 300000000 (m/abs' 3.0E8))
  (is (zero? (m/abs' 0)))
  (is (zero? (m/abs' 0.0)))
  (is= m/inf+ (m/abs' m/inf+))
  (is= m/inf+ (m/abs' m/inf-))
  (is (m/nan? (m/abs' m/nan))))

(deftest cbrt-test
  (is (spec-check m/cbrt))
  (is= 0.0 (m/cbrt 0.0))
  (is= 1.0 (m/cbrt 1.0))
  (is= -1.0 (m/cbrt -1.0))
  (is= -2.0 (m/cbrt -8))
  (is= m/inf+ (m/cbrt m/inf+))
  (is= m/inf- (m/cbrt m/inf-))
  (is (m/nan? (m/cbrt m/nan))))

;;TRIGONOMETRY
(deftest sin-test
  (is (spec-check m/sin)))

(deftest asin-test
  (is (spec-check m/asin)))

(deftest asinh-test
  (is (spec-check m/asinh))
  (is= 0.0 (m/asinh 0.0))
  (is= 0.48121182505960347 (m/asinh 0.5))
  (is= -0.8813735870195428 (m/asinh -1.0))
  (is= 0.8813735870195429 (m/asinh 1.0))
  (is= -1.4436354751788099 (m/asinh -2.0))
  (is= 1.4436354751788103 (m/asinh 2.0))
  (is= m/inf+ (m/asinh m/inf+))
  (is (m/nan? (m/asinh m/inf-)))
  (is (m/nan? (m/asinh m/nan))))

(deftest cos-test
  (is (spec-check m/cos)))

(deftest acos-test
  (is (spec-check m/acos)))

(deftest acosh-test
  (is (spec-check m/acosh))
  (is (m/nan? (m/acosh 0.0)))
  (is= 0.0 (m/acosh 1.0))
  (is= 1.3169578969248166 (m/acosh 2.0))
  (is= m/inf+ (m/acosh m/inf+))
  (is (m/nan? (m/acosh m/nan))))

(deftest tan-test
  (is (spec-check m/tan)))

(deftest atan-test
  (is (spec-check m/atan)))

(deftest atan2-test
  (is (spec-check m/atan2)))

(deftest atanh-test
  (is (spec-check m/atanh))
  (is= 0.0 (m/atanh 0.0))
  (is= -0.2027325540540822 (m/atanh 0.5))
  (is= m/inf- (m/atanh -1.0))
  (is= m/inf+ (m/atanh 1.0))
  (is (m/nan? (m/atanh -2.0)))
  (is (m/nan? (m/atanh m/nan))))

(deftest hypot-test
  (is (spec-check m/hypot)))

;;ROUNDING
(deftest round-test
  (is (spec-check m/round))
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
  (is (m/nan? (m/round m/nan :up))))

(deftest floor-test
  (is (spec-check m/floor))
  (is (zero? (m/floor 0.4)))
  (is= 2.3423423423423425E26 (m/floor 234234234234234234234343242N))
  (is= -1.0(m/floor -0.4))
  (is= m/inf+ (m/floor m/inf+))
  (is= m/inf- (m/floor m/inf-))
  (is (m/nan? (m/floor m/nan))))

(deftest floor'-test
  (is (spec-check m/floor'))
  (is= -1 (m/floor' -0.4)))

(deftest ceil-test
  (is (spec-check m/ceil))
  (is= 1.0 (m/ceil 0.4))
  (is= 2.3423423423423425E26 (m/ceil 234234234234234234234343242N))
  (is (zero? (m/ceil -0.4)))
  (is= m/inf+ (m/ceil m/inf+))
  (is= m/inf- (m/ceil m/inf-))
  (is (m/nan? (m/ceil m/nan))))

(deftest ceil'-test
  (is (spec-check m/ceil'))
  (is= 1 (m/ceil' 0.4)))

(deftest roughly-floor-test
  (is (spec-check m/roughly-floor))
  (is= 1.0 (m/roughly-floor 0.99 0.02))
  (is (zero? (m/roughly-floor 0.99 0.005)))
  (is= 2.3423423423423425E26 (m/roughly-floor 234234234234234234234343242N 0.02))
  (is= 2.3423423423423425E26 (m/roughly-floor 2.3423423423423425E26 0.02))
  (is (zero? (m/roughly-floor -0.01 0.02)))
  (is= m/inf+ (m/roughly-floor m/inf+ 0.02))
  (is= m/inf- (m/roughly-floor m/inf- 0.02))
  (is (m/nan? (m/roughly-floor m/nan 0.02))))

(deftest roughly-floor'-test
  (is (spec-check m/roughly-floor'))
  (is= 1 (m/roughly-floor' 0.99 0.02)))

(deftest roughly-ceil-test
  (is (spec-check m/roughly-ceil))
  (is (zero? (m/roughly-ceil 0.01 0.02)))
  (is= 1.0 (m/roughly-ceil 0.01 0.005))
  (is= 2.3423423423423425E26 (m/roughly-ceil 234234234234234234234343242N 0.02))
  (is= 2.3423423423423425E26 (m/roughly-ceil 2.3423423423423425E26 0.02))
  (is= -1.0 (m/roughly-ceil -0.99 0.02))
  (is= m/inf+ (m/roughly-ceil m/inf+ 0.02))
  (is= m/inf- (m/roughly-ceil m/inf- 0.02))
  (is (m/nan? (m/roughly-ceil m/nan 0.02))))

(deftest roughly-ceil'-test
  (is (spec-check m/roughly-ceil'))
  (is= 1 (m/roughly-ceil' 0.01 0.005)))

(deftest roughly?-test
  (is (spec-check m/roughly?))
  (is-not (m/roughly? 0.01 0.02 0.005))
  (is (m/roughly? 0.01 0.02 0.01))
  (is (m/roughly? 0.01 0.02 0.02))
  (is (m/roughly? 2.3423423423423425E26 2.3423423423423425E26 0.03))
  (is (m/roughly? 2.3423423423423425E26 2.3423423423423425E26 0.005))
  (is-not (m/roughly? m/inf+ m/inf+ 0.01))
  (is (m/roughly? m/inf- 0.02 m/inf+))
  (is-not (m/roughly? m/nan 0.02 0.01)))

(deftest roughly-round?-test
  (is (spec-check m/roughly-round?))
  (is (m/roughly-round? 0.01 0.02))
  (is-not (m/roughly-round? 0.01 0.005))
  (is (m/roughly-round? 2.3423423423423425E26 0.03))
  (is (m/roughly-round? 2.3423423423423425E26 0.005))
  (is (m/roughly-round? m/inf+ m/inf+))
  (is-not (m/roughly-round? m/inf- 0.4))
  (is-not (m/roughly-round? m/nan 0.01)))

(deftest roughly-round-non-?-test
  (is (spec-check m/roughly-round-non-?))
  (is (m/roughly-round-non-? 0 0.02))
  (is-not (m/roughly-round-non-? -0.01 0.02))
  (is (m/roughly-round-non-? 0.01 0.02))
  (is-not (m/roughly-round-non-? 0.01 0.005))
  (is (m/roughly-round-non-? 2.3423423423423425E26 0.03))
  (is (m/roughly-round-non-? 2.3423423423423425E26 0.005))
  (is (m/roughly-round-non-? m/inf+ m/inf+))
  (is-not (m/roughly-round-non-? m/inf- m/inf+))
  (is-not (m/roughly-round-non-? m/inf+ 0.4))
  (is-not (m/roughly-round-non-? m/nan 0.01)))

(deftest roughly-round-non+?-test
  (is (spec-check m/roughly-round-non+?))
  (is (m/roughly-round-non+? 0 0.02))
  (is-not (m/roughly-round-non+? 0.01 0.02))
  (is (m/roughly-round-non+? -0.01 0.02))
  (is-not (m/roughly-round-non+? -0.01 0.005))
  (is (m/roughly-round-non+? -2.3423423423423425E26 0.03))
  (is (m/roughly-round-non+? -2.3423423423423425E26 0.005))
  (is-not (m/roughly-round-non+? m/inf+ m/inf+))
  (is (m/roughly-round-non+? m/inf- m/inf+))
  (is-not (m/roughly-round-non+? m/inf- 0.4))
  (is-not (m/roughly-round-non+? m/nan 0.01)))

(deftest roughly-round+?-test
  (is (spec-check m/roughly-round+?))
  (is-not (m/roughly-round+? 0 0.02))
  (is-not (m/roughly-round+? -0.01 0.02))
  (is (m/roughly-round+? 0.01 0.02))
  (is-not (m/roughly-round+? 0.01 0.005))
  (is (m/roughly-round+? 2.3423423423423425E26 0.03))
  (is (m/roughly-round+? 2.3423423423423425E26 0.005))
  (is (m/roughly-round+? m/inf+ m/inf+))
  (is-not (m/roughly-round+? m/inf- m/inf+))
  (is-not (m/roughly-round+? m/inf+ 0.4))
  (is-not (m/roughly-round+? m/nan 0.01)))

(deftest roughly-round-?-test
  (is (spec-check m/roughly-round-?))
  (is-not (m/roughly-round-? 0 0.02))
  (is-not (m/roughly-round-? 0.01 0.02))
  (is (m/roughly-round-? -0.01 0.02))
  (is-not (m/roughly-round-? -0.01 0.005))
  (is (m/roughly-round-? -2.3423423423423425E26 0.03))
  (is (m/roughly-round-? -2.3423423423423425E26 0.005))
  (is-not (m/roughly-round-? m/inf+ m/inf+))
  (is (m/roughly-round-? m/inf- m/inf+))
  (is-not (m/roughly-round-? m/inf- 0.4))
  (is-not (m/roughly-round-? m/nan 0.01)))

(deftest roughly-non-?-test
  (is (spec-check m/roughly-non-?))
  (is-not (m/roughly-non-? -0.01 0.005))
  (is (m/roughly-non-? -0.02 0.02))
  (is (m/roughly-non-? 0.01 0.001))
  (is (m/roughly-non-? m/inf+ m/inf+))
  (is (m/roughly-non-? m/inf- m/inf+))
  (is-not (m/roughly-non-? m/inf- 0.4))
  (is (m/roughly-non-? m/inf+ 0.4))
  (is-not (m/roughly-non-? m/nan 0.01)))

(deftest roughly-non+?-test
  (is (spec-check m/roughly-non+?))
  (is-not (m/roughly-non+? 0.01 0.005))
  (is (m/roughly-non+? 0.02 0.02))
  (is (m/roughly-non+? -0.01 0.001))
  (is (m/roughly-non+? m/inf+ m/inf+))
  (is (m/roughly-non+? m/inf- m/inf+))
  (is (m/roughly-non+? m/inf- 0.4))
  (is-not (m/roughly-non+? m/inf+ 0.4))
  (is-not (m/roughly-non+? m/nan 0.01)))

(deftest roughly-prob?-test
  (is (spec-check m/roughly-prob?))
  (is (m/roughly-prob? 0.01 0.005))
  (is (m/roughly-prob? 0.02 0.02))
  (is-not (m/roughly-prob? -0.01 0.001))
  (is (m/roughly-prob? 1.01 0.01))
  (is (m/roughly-prob? 1.01 0.01))
  (is (m/roughly-prob? m/inf+ m/inf+))
  (is (m/roughly-prob? m/inf- m/inf+))
  (is-not (m/roughly-prob? m/inf- 0.4))
  (is-not (m/roughly-prob? m/inf+ 0.4))
  (is-not (m/roughly-prob? m/nan 0.01)))

(deftest roughly-corr?-test
  (is (spec-check m/roughly-corr?))
  (is-not (m/roughly-corr? -1.01 0.005))
  (is (m/roughly-corr? -1.02 0.02))
  (is-not (m/roughly-corr? -1.01 0.001))
  (is (m/roughly-corr? 1.01 0.01))
  (is (m/roughly-corr? 1.01 0.01))
  (is (m/roughly-corr? m/inf+ m/inf+))
  (is (m/roughly-corr? m/inf- m/inf+))
  (is-not (m/roughly-corr? m/inf- 0.4))
  (is-not (m/roughly-corr? m/inf+ 0.4))
  (is-not (m/roughly-corr? m/nan 0.01)))

;;;QUOTIENTS
(deftest quot'-test
  (is (spec-check m/quot'))
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
  (is (m/nan? (m/quot' 2 m/nan))))

(deftest mod'-test
  (is (spec-check m/mod'))
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
  (is (m/nan? (m/mod' 2 m/nan))))

(deftest rem'-test
  (is (spec-check m/rem'))
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
  (is (m/nan? (m/rem' 2 m/nan))))

(deftest quot-and-rem'-test
  (is (spec-check m/quot-and-rem'))
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
  (is (every? m/nan? (m/quot-and-rem' 2 m/nan))))

(deftest quot-and-mod'-test
  (is (spec-check m/quot-and-mod'))
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
  (is (every? m/nan? (m/quot-and-mod' 2 m/nan))))

(deftest gcd-test
  (is (spec-check m/gcd))
  (is= 7 (m/gcd 271284701247 12467364728)))

;;;ANGLES
(deftest reduce-angle'-test
  (is (spec-check m/reduce-angle'))
  (is= 30.4 (m/reduce-angle' 30.4))
  (is= 350.2 (m/reduce-angle' -9.8))
  (is= 118 (m/reduce-angle' 478.0))
  (is= 26 (m/reduce-angle' -8399494))
  (is (m/nan? (m/reduce-angle' m/nan)))
  (is (m/nan? (m/reduce-angle' m/inf+)))
  (is (m/nan? (m/reduce-angle' m/inf-))))

(deftest reduce-radians'-test
  (is (spec-check m/reduce-radians'))
  (is= 5.267258771281654 (m/reduce-radians' 30.4))
  (is (zero? (m/reduce-radians' m/two-pi)))
  (is= m/PI (m/reduce-radians' m/PI))
  (is= 0.06552912132908517 (m/reduce-radians' -8399494))
  (is (m/nan? (m/reduce-radians' m/nan)))
  (is (m/nan? (m/reduce-radians' m/inf+)))
  (is (m/nan? (m/reduce-radians' m/inf-))))

(deftest radians->angle'-test
  (is (spec-check m/radians->angle'))
  (is (zero? (m/radians->angle' 0)))
  (is= 194.8056503444799 (m/radians->angle' 3.4))
  (is (zero? (m/radians->angle' m/two-pi)))
  (is= 165.1943496555201 (m/radians->angle' -3.4))
  (is= 58.31007808870436 (m/radians->angle' 45))
  (is (m/nan? (m/radians->angle' m/nan)))
  (is= m/inf+ (m/radians->angle' m/inf+))
  (is= m/inf- (m/radians->angle' m/inf-)))

(deftest angle->radians'-test
  (is (spec-check m/angle->radians'))
  (is (zero? (m/angle->radians' 0)))
  (is= 0.059341194567807204 (m/angle->radians' 3.4))
  (is= 0.002777777777777778 (m/angle->radians' m/inv-two-pi))
  (is= 6.223844112611779 (m/angle->radians' -3.4))
  (is= 0.7853981633974483 (m/angle->radians' 45))
  (is (m/nan? (m/angle->radians' m/nan)))
  (is= m/inf+ (m/angle->radians' m/inf+))
  (is= m/inf- (m/angle->radians' m/inf-)))

#_(ost/unstrument)
