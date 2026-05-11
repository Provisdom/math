(ns provisdom.math.core-test
  (:require
    [clojure.spec.alpha :as s]
    [provisdom.math.core :as m]
    [provisdom.test.core :as t]))

;;6 seconds

(set! *warn-on-reflection* true)

;;;TEST FOR NUMERIC TYPES
(t/deftest numbers?-test
  (t/with-instrument `m/numbers?
    (t/is-spec-check m/numbers?))
  (t/with-instrument :all
    (t/is-not (m/numbers? 1))
    (t/is (m/numbers? '(1)))
    (t/is (m/numbers? []))
    (t/is (m/numbers? [2 3]))
    (t/is-not (m/numbers? [[2]]))))

(t/deftest num?-test
  (t/with-instrument `m/num?
    (t/is-spec-check m/num?))
  (t/with-instrument :all
    (t/is-not (m/num? "A"))
    (t/is (m/num? 3.3E30))
    (t/is (m/num? -3.3E30))
    (t/is (m/num? m/inf+))
    (t/is (m/num? m/inf-))
    (t/is-not (m/num? m/nan))))

(t/deftest nan?-test
  (t/with-instrument `m/nan?
    (t/is-spec-check m/nan?))
  (t/with-instrument :all
    (t/is-not (m/nan? 3.3))
    (t/is-not (m/nan? "A"))
    (t/is-not (m/nan? m/inf+))
    (t/is-not (m/nan? m/inf-))
    (t/is (m/nan? m/nan))))

(t/deftest pos?-test
  (t/with-instrument `m/pos?
    (t/is-spec-check m/pos?))
  (t/with-instrument :all
    (t/is-not (m/pos? "A"))
    (t/is (m/pos? 3.3E30))
    (t/is-not (m/pos? -3.3E30))
    (t/is (m/pos? m/inf+))
    (t/is-not (m/pos? m/inf-))
    (t/is-not (m/pos? m/nan))))

(t/deftest neg?-test
  (t/with-instrument `m/neg?
    (t/is-spec-check m/neg?))
  (t/with-instrument :all
    (t/is-not (m/neg? "A"))
    (t/is-not (m/neg? 3.3E30))
    (t/is (m/neg? -3.3E30))
    (t/is-not (m/neg? m/inf+))
    (t/is (m/neg? m/inf-))
    (t/is-not (m/neg? m/nan))))

(t/deftest non-?-test
  (t/with-instrument `m/non-?
    (t/is-spec-check m/non-?))
  (t/with-instrument :all
    (t/is (m/non-? 0))
    (t/is (m/non-? 1))
    (t/is-not (m/non-? -1))
    (t/is (m/non-? m/inf+))
    (t/is-not (m/non-? m/inf-))
    (t/is-not (m/non-? m/nan))
    (t/is-not (m/non-? "A"))))

(t/deftest non+?-test
  (t/with-instrument `m/non+?
    (t/is-spec-check m/non+?))
  (t/with-instrument :all
    (t/is (m/non+? 0))
    (t/is-not (m/non+? 1))
    (t/is (m/non+? -1))
    (t/is-not (m/non+? m/inf+))
    (t/is (m/non+? m/inf-))
    (t/is-not (m/non+? m/nan))
    (t/is-not (m/non+? "A"))))

(t/deftest finite?-test
  (t/with-instrument `m/finite?
    (t/is-spec-check m/finite?))
  (t/with-instrument :all
    (t/is-not (m/finite? "A"))
    (t/is (m/finite? 3.3E30))
    (t/is (m/finite? -3.3E30))
    (t/is-not (m/finite? m/inf+))
    (t/is-not (m/finite? m/inf-))
    (t/is-not (m/finite? m/nan))))

(t/deftest finite-spec-test
  ;;macro test - no spec-check
  (let [test-spec (m/finite-spec {:min 0 :max 10})]
    (t/is (s/valid? test-spec 5.0))
    (t/is (s/valid? test-spec 0))
    (t/is (s/valid? test-spec 10))
    (t/is-not (s/valid? test-spec -1))
    (t/is-not (s/valid? test-spec 15.0))
    (t/is-not (s/valid? test-spec m/inf+))
    (t/is-not (s/valid? test-spec m/nan))))

(t/deftest finite+?-test
  (t/with-instrument `m/finite+?
    (t/is-spec-check m/finite+?))
  (t/with-instrument :all
    (t/is-not (m/finite+? "A"))
    (t/is (m/finite+? 3.3E30))
    (t/is-not (m/finite+? -3.3E30))
    (t/is-not (m/finite+? m/inf+))
    (t/is-not (m/finite+? m/inf-))
    (t/is-not (m/finite+? m/nan))))

(t/deftest finite+-spec-test
  ;;macro test - no spec-check
  (let [test-spec (m/finite+-spec 100)]
    (t/is (s/valid? test-spec 5.0))
    (t/is (s/valid? test-spec 1))
    (t/is (s/valid? test-spec 100))
    (t/is-not (s/valid? test-spec 0))
    (t/is-not (s/valid? test-spec -1))
    (t/is-not (s/valid? test-spec 101))
    (t/is-not (s/valid? test-spec m/inf+))
    (t/is-not (s/valid? test-spec m/nan))))

(t/deftest finite-?-test
  (t/with-instrument `m/finite-?
    (t/is-spec-check m/finite-?))
  (t/with-instrument :all
    (t/is-not (m/finite-? "A"))
    (t/is-not (m/finite-? 3.3E30))
    (t/is (m/finite-? -3.3E30))
    (t/is-not (m/finite-? m/inf+))
    (t/is-not (m/finite-? m/inf-))
    (t/is-not (m/finite-? m/nan))))

(t/deftest finite-non-?-test
  (t/with-instrument `m/finite-non-?
    (t/is-spec-check m/finite-non-?))
  (t/with-instrument :all
    (t/is (m/finite-non-? 0))
    (t/is (m/finite-non-? 1))
    (t/is-not (m/finite-non-? -1))
    (t/is-not (m/finite-non-? m/inf+))
    (t/is-not (m/finite-non-? m/inf-))
    (t/is-not (m/finite-non-? m/nan))
    (t/is-not (m/finite-non-? "A"))))

(t/deftest finite-non--spec-test
  ;;macro test - no spec-check
  (let [test-spec (m/finite-non--spec 100)]
    (t/is (s/valid? test-spec 0))
    (t/is (s/valid? test-spec 50))
    (t/is (s/valid? test-spec 100))
    (t/is-not (s/valid? test-spec -1))
    (t/is-not (s/valid? test-spec 101))
    (t/is-not (s/valid? test-spec m/inf+))
    (t/is-not (s/valid? test-spec m/nan))))

(t/deftest finite-non+?-test
  (t/with-instrument `m/finite-non+?
    (t/is-spec-check m/finite-non+?))
  (t/with-instrument :all
    (t/is (m/finite-non+? 0))
    (t/is-not (m/finite-non+? 1))
    (t/is (m/finite-non+? -1))
    (t/is-not (m/finite-non+? m/inf+))
    (t/is-not (m/finite-non+? m/inf-))
    (t/is-not (m/finite-non+? m/nan))
    (t/is-not (m/finite-non+? "A"))))

(t/deftest double-finite?-test
  (t/with-instrument `m/double-finite?
    (t/is-spec-check m/double-finite?))
  (t/with-instrument :all
    (t/is-not (m/double-finite? "A"))
    (t/is-not (m/double-finite? 0))
    (t/is (m/double-finite? 0.0))
    (t/is (m/double-finite? 3.3E300))
    (t/is (m/double-finite? -3.3E300))
    (t/is-not (m/double-finite? m/inf+))
    (t/is-not (m/double-finite? m/inf-))
    (t/is-not (m/double-finite? m/nan))))

(t/deftest double-finite+?-test
  (t/with-instrument `m/double-finite+?
    (t/is-spec-check m/double-finite+?))
  (t/with-instrument :all
    (t/is-not (m/double-finite+? "A"))
    (t/is-not (m/double-finite+? 0))
    (t/is-not (m/double-finite+? 0.0))
    (t/is (m/double-finite+? 3.3E300))
    (t/is-not (m/double-finite+? -3.3E300))
    (t/is-not (m/double-finite+? m/inf+))
    (t/is-not (m/double-finite+? m/inf-))
    (t/is-not (m/double-finite+? m/nan))))

(t/deftest single?-test
  (t/with-instrument `m/single?
    (t/is-spec-check m/single?))
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

(t/deftest single-finite?-test
  (t/with-instrument `m/single-finite?
    (t/is-spec-check m/single-finite?))
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

(t/deftest long?-test
  (t/with-instrument `m/long?
    (t/is-spec-check m/long?))
  (t/with-instrument :all
    (t/is-not (m/long? 3.3))
    (t/is (m/long? 3))
    (t/is-not (m/long? 3.0))
    (t/is-not (m/long? "A"))
    (t/is-not (m/long? 3.4E15))
    (t/is-not (m/long? 3.3E30))
    (t/is-not (m/long? -3.3E30))
    (t/is-not (m/long? m/nan))))

(t/deftest long-spec-test
  ;;macro test - no spec-check
  (let [test-spec (m/long-spec {:min -10 :max 10})]
    (t/is (s/valid? test-spec 0))
    (t/is (s/valid? test-spec -10))
    (t/is (s/valid? test-spec 10))
    (t/is-not (s/valid? test-spec -11))
    (t/is-not (s/valid? test-spec 11))
    (t/is-not (s/valid? test-spec 5.0))))

(t/deftest long+?-test
  (t/with-instrument `m/long+?
    (t/is-spec-check m/long+?))
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

(t/deftest long+-spec-test
  ;;macro test - no spec-check
  (let [test-spec (m/long+-spec 100)]
    (t/is (s/valid? test-spec 1))
    (t/is (s/valid? test-spec 50))
    (t/is (s/valid? test-spec 100))
    (t/is-not (s/valid? test-spec 0))
    (t/is-not (s/valid? test-spec -1))
    (t/is-not (s/valid? test-spec 101))))

(t/deftest long-?-test
  (t/with-instrument `m/long-?
    (t/is-spec-check m/long-?))
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

(t/deftest long-non-?-test
  (t/with-instrument `m/long-non-?
    (t/is-spec-check m/long-non-?))
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

(t/deftest long-non--spec-test
  ;;macro test - no spec-check
  (let [test-spec (m/long-non--spec 100)]
    (t/is (s/valid? test-spec 0))
    (t/is (s/valid? test-spec 50))
    (t/is (s/valid? test-spec 100))
    (t/is-not (s/valid? test-spec -1))
    (t/is-not (s/valid? test-spec 101))))

(t/deftest long-non+?-test
  (t/with-instrument `m/long-non+?
    (t/is-spec-check m/long-non+?))
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

(t/deftest int?-test
  (t/with-instrument `m/int?
    (t/is-spec-check m/int?))
  (t/with-instrument :all
    (t/is-not (m/int? 3.3))
    (t/is (m/int? 3))
    (t/is-not (m/int? 3.0))
    (t/is-not (m/int? "A"))
    (t/is-not (m/int? 3.4E15))
    (t/is-not (m/int? 3.3E30))
    (t/is-not (m/int? -3.3E30))
    (t/is-not (m/int? m/nan))))

(t/deftest int-spec-test
  ;;macro test - no spec-check
  (let [test-spec (m/int-spec {:min -10 :max 10})]
    (t/is (s/valid? test-spec 0))
    (t/is (s/valid? test-spec -10))
    (t/is (s/valid? test-spec 10))
    (t/is-not (s/valid? test-spec -11))
    (t/is-not (s/valid? test-spec 11))
    (t/is-not (s/valid? test-spec 5.0))))

(t/deftest int+?-test
  (t/with-instrument `m/int+?
    (t/is-spec-check m/int+?))
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

(t/deftest int+-spec-test
  ;;macro test - no spec-check
  (let [test-spec (m/int+-spec 100)]
    (t/is (s/valid? test-spec 1))
    (t/is (s/valid? test-spec 50))
    (t/is (s/valid? test-spec 100))
    (t/is-not (s/valid? test-spec 0))
    (t/is-not (s/valid? test-spec -1))
    (t/is-not (s/valid? test-spec 101))))

(t/deftest int-?-test
  (t/with-instrument `m/int-?
    (t/is-spec-check m/int-?))
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

(t/deftest int-non-?-test
  (t/with-instrument `m/int-non-?
    (t/is-spec-check m/int-non-?))
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

(t/deftest int-non--spec-test
  ;;macro test - no spec-check
  (let [test-spec (m/int-non--spec 100)]
    (t/is (s/valid? test-spec 0))
    (t/is (s/valid? test-spec 50))
    (t/is (s/valid? test-spec 100))
    (t/is-not (s/valid? test-spec -1))
    (t/is-not (s/valid? test-spec 101))))

(t/deftest int-non+?-test
  (t/with-instrument `m/int-non+?
    (t/is-spec-check m/int-non+?))
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

(t/deftest long-able?-test
  (t/with-instrument `m/long-able?
    (t/is-spec-check m/long-able?))
  (t/with-instrument :all
    (t/is-not (m/long-able? 3.3))
    (t/is (m/long-able? 3))
    (t/is (m/long-able? 3.0))
    (t/is-not (m/long-able? "A"))
    (t/is (m/long-able? 3.4E15))
    (t/is-not (m/long-able? 3.3E30))
    (t/is-not (m/long-able? -3.3E30))
    (t/is-not (m/long-able? m/nan))))

(t/deftest long-able+?-test
  (t/with-instrument `m/long-able+?
    (t/is-spec-check m/long-able+?))
  (t/with-instrument :all
    (t/is-not (m/long-able+? 3.3))
    (t/is-not (m/long-able+? 0.0))
    (t/is (m/long-able+? 1.0))
    (t/is-not (m/long-able+? -1.0))
    (t/is-not (m/long-able+? -3.3))))

(t/deftest long-able-?-test
  (t/with-instrument `m/long-able-?
    (t/is-spec-check m/long-able-?))
  (t/with-instrument :all
    (t/is-not (m/long-able-? 3.3))
    (t/is-not (m/long-able-? 0.0))
    (t/is-not (m/long-able-? 1.0))
    (t/is (m/long-able-? -1.0))
    (t/is-not (m/long-able-? -3.3))))

(t/deftest long-able-non+?-test
  (t/with-instrument `m/long-able-non+?
    (t/is-spec-check m/long-able-non+?))
  (t/with-instrument :all
    (t/is-not (m/long-able-non+? 3.3))
    (t/is (m/long-able-non+? 0.0))
    (t/is-not (m/long-able-non+? 1.0))
    (t/is (m/long-able-non+? -1.0))
    (t/is-not (m/long-able-non+? -3.3))))

(t/deftest long-able-non-?-test
  (t/with-instrument `m/long-able-non-?
    (t/is-spec-check m/long-able-non-?))
  (t/with-instrument :all
    (t/is-not (m/long-able-non-? 3.3))
    (t/is (m/long-able-non-? 0.0))
    (t/is (m/long-able-non-? 1.0))
    (t/is-not (m/long-able-non-? -1.0))
    (t/is-not (m/long-able-non-? -3.3))))

(t/deftest inf+?-test
  (t/with-instrument `m/inf+?
    (t/is-spec-check m/inf+?))
  (t/with-instrument :all
    (t/is-not (m/inf+? 3.3))
    (t/is-not (m/inf+? "A"))
    (t/is (m/inf+? m/inf+))
    (t/is-not (m/inf+? m/inf-))
    (t/is-not (m/inf+? m/nan))))

(t/deftest inf-?-test
  (t/with-instrument `m/inf-?
    (t/is-spec-check m/inf-?))
  (t/with-instrument :all
    (t/is-not (m/inf-? 3.3))
    (t/is-not (m/inf-? "A"))
    (t/is-not (m/inf-? m/inf+))
    (t/is (m/inf-? m/inf-))
    (t/is-not (m/inf-? m/nan))))

(t/deftest inf?-test
  (t/with-instrument `m/inf?
    (t/is-spec-check m/inf?))
  (t/with-instrument :all
    (t/is-not (m/inf? 3.3))
    (t/is-not (m/inf? "A"))
    (t/is (m/inf? m/inf+))
    (t/is (m/inf? m/inf-))
    (t/is-not (m/inf? m/nan))))

(t/deftest one?-test
  (t/with-instrument `m/one?
    (t/is-spec-check m/one?))
  (t/with-instrument :all
    (t/is (m/one? 1))
    (t/is (m/one? 1.0))
    (t/is-not (m/one? "A"))
    (t/is-not (m/one? -1))
    (t/is-not (m/one? m/nan))))

(t/deftest prob?-test
  (t/with-instrument `m/prob?
    (t/is-spec-check m/prob?))
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

(t/deftest open-prob?-test
  (t/with-instrument `m/open-prob?
    (t/is-spec-check m/open-prob?))
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

(t/deftest corr?-test
  (t/with-instrument `m/corr?
    (t/is-spec-check m/corr?))
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

(t/deftest open-corr?-test
  (t/with-instrument `m/open-corr?
    (t/is-spec-check m/open-corr?))
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

(t/deftest maybe-long-able-test
  (t/with-instrument `m/maybe-long-able
    (t/is-spec-check m/maybe-long-able))
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

;;;BASIC MATH
(t/deftest ===-test
  (t/with-instrument `m/===
    (t/is-spec-check m/===))
  (t/with-instrument :all
    (t/is (m/=== m/nan))
    (t/is (m/=== m/nan m/nan))
    (t/is (m/=== m/nan m/nan m/nan))
    (t/is (m/=== 3 3 3 3))
    (t/is-not (m/=== 3 m/nan))))

(t/deftest next-up-test
  (t/with-instrument `m/next-up
    (t/is-spec-check m/next-up))
  (t/with-instrument :all
    (t/is= 3.0000000000000004 (m/next-up 3))
    (t/is= -2.9999999999999996 (m/next-up -3))
    (t/is (m/nan? (m/next-up m/nan)))
    (t/is= m/inf+ (m/next-up m/inf+))
    (t/is= m/min-dbl (m/next-up m/inf-))
    (t/is= -2.9999999999999996 (m/next-up -3.0))))

(t/deftest next-down-test
  (t/with-instrument `m/next-down
    (t/is-spec-check m/next-down))
  (t/with-instrument :all
    (t/is= 2.9999999999999996 (m/next-down 3))
    (t/is= -3.0000000000000004 (m/next-down -3))
    (t/is (m/nan? (m/next-down m/nan)))
    (t/is= m/max-dbl (m/next-down m/inf+))
    (t/is= m/inf- (m/next-down m/inf-))
    (t/is= 2.9999999999999996 (m/next-down 3.0))))

(t/deftest div-test
  (t/with-instrument `m/div
    (t/is-spec-check m/div))
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

(t/deftest one--test
  (t/with-instrument `m/one-
    (t/is-spec-check m/one-))
  (t/with-instrument :all
    (t/is= -2 (m/one- 3))
    (t/is= 0.0 (m/one- 3 -2))
    (t/is= -16.0 (m/one- 3 4 2 8))
    (t/is= 4 (m/one- -3))
    (t/is (m/nan? (m/one- m/nan)))
    (t/is= m/inf- (m/one- m/inf+))
    (t/is= m/inf+ (m/one- m/inf-))
    (t/is= 4.0 (m/one- -3.0))))

(t/deftest sq-test
  (t/with-instrument `m/sq
    (t/is-spec-check m/sq))
  (t/with-instrument :all
    (t/is= 9.0 (m/sq 3))
    (t/is= 9.0 (m/sq -3))
    (t/is (m/nan? (m/sq m/nan)))
    (t/is= m/inf+ (m/sq m/inf+))
    (t/is= m/inf+ (m/sq m/inf-))
    (t/is= 0.0 (m/sq 0))
    (t/is= 2.25 (m/sq 1.5))))

(t/deftest sq'-test
  (t/with-instrument `m/sq'
    (t/is-spec-check m/sq'))
  (t/with-instrument :all
    (t/is= 9 (m/sq' 3))
    (t/is= 9 (m/sq' -3))
    (t/is (m/nan? (m/sq' m/nan)))
    (t/is= m/inf+ (m/sq' m/inf+))
    (t/is= m/inf+ (m/sq' m/inf-))
    (t/is= 9 (m/sq' -3.0))))

(t/deftest cube-test
  (t/with-instrument `m/cube
    (t/is-spec-check m/cube))
  (t/with-instrument :all
    (t/is= 27.0 (m/cube 3))
    (t/is= -27.0 (m/cube -3))
    (t/is (m/nan? (m/cube m/nan)))
    (t/is= m/inf+ (m/cube m/inf+))
    (t/is= m/inf- (m/cube m/inf-))
    (t/is= 0.0 (m/cube 0))
    (t/is= 3.375 (m/cube 1.5))))

(t/deftest cube'-test
  (t/with-instrument `m/cube'
    (t/is-spec-check m/cube'))
  (t/with-instrument :all
    (t/is= 27 (m/cube' 3))
    (t/is (m/nan? (m/cube' m/nan)))
    (t/is= m/inf+ (m/cube' m/inf+))
    (t/is= m/inf- (m/cube' m/inf-))
    (t/is= -27 (m/cube' -3))
    (t/is= -27 (m/cube' -3.0))))

(t/deftest sgn-test
  (t/with-instrument `m/sgn
    (t/is-spec-check m/sgn))
  (t/with-instrument :all
    (t/is= 1 (m/sgn 3))
    (t/is= -1 (m/sgn -3))
    (t/is (m/nan? (m/sgn m/nan)))
    (t/is= 1 (m/sgn m/inf+))
    (t/is= -1 (m/sgn m/inf-))
    (t/is (zero? (m/sgn 0)))
    (t/is (zero? (m/sgn 0.0)))
    (t/is= -1 (m/sgn -3.0))))

(t/deftest exp-test
  (t/with-instrument `m/exp
    (t/is-spec-check m/exp))
  (t/with-instrument :all
    (t/is= 1.0 (m/exp 0))
    (t/is-approx= m/E (m/exp 1))
    (t/is-approx= (/ m/E) (m/exp -1))
    (t/is= m/inf+ (m/exp m/inf+))
    (t/is= 0.0 (m/exp m/inf-))
    (t/is (m/nan? (m/exp m/nan)))
    ;;SciPy 20.085
    (t/is= 20.085536923187668 (m/exp 3))))

(t/deftest dec-exp-test
  (t/with-instrument `m/dec-exp
    (t/is-spec-check m/dec-exp))
  (t/with-instrument :all
    (t/is= 0.0 (m/dec-exp 0))
    (t/is-approx= (dec m/E) (m/dec-exp 1))
    (t/is= m/inf+ (m/dec-exp m/inf+))
    (t/is= -1.0 (m/dec-exp m/inf-))
    (t/is (m/nan? (m/dec-exp m/nan)))
    ;;useful for small values
    ;;SciPy 1.0000
    (t/is= 1.0000000000000007E-15 (m/dec-exp 1e-15))))

(t/deftest dec-exp-over-x-test
  (t/with-instrument `m/dec-exp-over-x
    (t/is-spec-check m/dec-exp-over-x))
  (t/with-instrument :all
    ;; lim_{x→0} (e^x - 1)/x = 1
    (t/is= 1.0 (m/dec-exp-over-x 0.0))
    ;; (e^1 - 1)/1 = e - 1
    (t/is= (dec m/E) (m/dec-exp-over-x 1.0))
    ;; Small value: Taylor series used
    (t/is= 1.0000000000000004 (m/dec-exp-over-x 1e-15))
    ;; Negative input
    ;;SciPy 0.63212
    (t/is-approx= 0.6321205588285577 (m/dec-exp-over-x -1.0) :tolerance 1e-15)))

(t/deftest safe-exp-test
  (t/with-instrument `m/safe-exp
    (t/is-spec-check m/safe-exp))
  (t/with-instrument :all
    ;; Normal cases
    (t/is= 1.0 (m/safe-exp 0.0))
    (t/is-approx= m/E (m/safe-exp 1.0))
    (t/is-approx= (/ m/E) (m/safe-exp -1.0))
    ;; Underflow protection
    (t/is= 0.0 (m/safe-exp -701.0))
    (t/is= 0.0 (m/safe-exp -1000.0))
    ;; Overflow protection
    (t/is= m/inf+ (m/safe-exp 701.0))
    (t/is= m/inf+ (m/safe-exp 1000.0))
    ;; Edge of threshold
    (t/is (m/pos? (m/safe-exp -699.0)))
    (t/is (m/finite? (m/safe-exp 699.0)))))

(t/deftest log-test
  (t/with-instrument `m/log
    (t/is-spec-check m/log))
  (t/with-instrument :all
    (t/is= 0.0 (m/log 1))
    (t/is= 1.0 (m/log m/E))
    (t/is= m/inf- (m/log 0))
    (t/is= m/inf+ (m/log m/inf+))
    (t/is (m/nan? (m/log -1)))
    (t/is (m/nan? (m/log m/nan)))
    ;;SciPy 1.0986
    (t/is-approx= 1.0986122886681098 (m/log 3) :tolerance 1e-15)))

(t/deftest log-inc-test
  (t/with-instrument `m/log-inc
    (t/is-spec-check m/log-inc))
  (t/with-instrument :all
    (t/is= 0.0 (m/log-inc 0))
    (t/is= 1.0 (m/log-inc (dec m/E)))
    (t/is= m/inf- (m/log-inc -1))
    (t/is= m/inf+ (m/log-inc m/inf+))
    (t/is (m/nan? (m/log-inc -2)))
    (t/is (m/nan? (m/log-inc m/nan)))
    ;;useful for small values
    ;;SciPy 9.9999
    (t/is= 9.999999999999995E-16 (m/log-inc 1e-15))))

(t/deftest log-inc-over-x-test
  (t/with-instrument `m/log-inc-over-x
    (t/is-spec-check m/log-inc-over-x))
  (t/with-instrument :all
    ;; lim_{x→0} log(1+x)/x = 1
    (t/is= 1.0 (m/log-inc-over-x 0.0))
    ;; log(2)/1 = ln(2)
    (t/is= 0.6931471805599453 (m/log-inc-over-x 1.0))
    ;; Small value: Taylor series used
    (t/is= 0.9999999999999994 (m/log-inc-over-x 1e-15))
    ;; Negative input (valid for x > -1)
    ;;SciPy 1.38629
    (t/is-approx= 1.3862943611198906 (m/log-inc-over-x -0.5) :tolerance 1e-15)))

(t/deftest log-sum-exp-test
  (t/with-instrument `m/log-sum-exp
    (t/is-spec-check m/log-sum-exp))
  (t/with-instrument :all
    ;; Empty vector returns -inf
    (t/is= m/inf- (m/log-sum-exp []))
    ;; Single element
    (t/is= 5.0 (m/log-sum-exp [5.0]))
    ;; Two equal elements: log(2*exp(x)) = x + log(2)
    (t/is-approx= (+ 3.0 m/log-two) (m/log-sum-exp [3.0 3.0]))
    ;; General case: log(exp(1) + exp(2)) = 2 + log(1 + exp(-1))
    (t/is-approx= (+ 2.0 (m/log (inc (m/exp -1.0)))) (m/log-sum-exp [1.0 2.0]))
    ;; Large values (should not overflow)
    ;;SciPy -1199.9
    (t/is= -1199.9999546011009 (m/log-sum-exp [-1200.0 -1210.0]))
    ;;SciPy 1210.0
    (t/is= 1210.0000453988991 (m/log-sum-exp [1200.0 1210.0]))
    (t/is= 1210.0 (m/log-sum-exp [-1200.0 1210.0]))
    (t/is-approx= 1000.0 (m/log-sum-exp [1000.0 900.0]) :tolerance 0.001)
    ;; All -inf
    (t/is= m/inf- (m/log-sum-exp [m/inf- m/inf-]))
    ;; Contains +inf
    (t/is= m/inf+ (m/log-sum-exp [1.0 2.0 m/inf+]))))

(t/deftest log2-test
  (t/with-instrument `m/log2
    (t/is-spec-check m/log2))
  (t/with-instrument :all
    ;;SciPy 1.5849
    (t/is-approx= 1.5849625007211563 (m/log2 3))
    (t/is= m/inf- (m/log2 0))
    (t/is= m/inf+ (m/log2 m/inf+))
    (t/is (m/nan? (m/log2 m/nan)))
    (t/is (m/nan? (m/log2 -3.0)))
    (t/is= 0.0 (m/log2 1.0))
    ;;SciPy -0.15200
    (t/is= -0.15200309344504997 (m/log2 0.9))))

(t/deftest log10-test
  (t/with-instrument `m/log10
    (t/is-spec-check m/log10))
  (t/with-instrument :all
    (t/is= 0.0 (m/log10 1))
    (t/is= 1.0 (m/log10 10))
    (t/is= 2.0 (m/log10 100))
    (t/is= m/inf- (m/log10 0))
    (t/is= m/inf+ (m/log10 m/inf+))
    (t/is (m/nan? (m/log10 -1)))
    (t/is (m/nan? (m/log10 m/nan)))
    ;;SciPy 0.47712
    (t/is= 0.47712125471966244 (m/log10 3))))

(t/deftest logn-test
  (t/with-instrument `m/logn
    (t/is-spec-check m/logn))
  (t/with-instrument :all
    (t/is= 1.0 (m/logn 3 3))
    (t/is= m/inf- (m/logn 0 3))
    (t/is= m/inf+ (m/logn m/inf+ 3))
    (t/is (m/nan? (m/logn -3.0 3)))
    (t/is (m/nan? (m/logn m/nan 3)))
    (t/is= 0.0 (m/logn 1.0 3))
    ;;SciPy -0.095903
    (t/is-approx= -0.09590327428938458 (m/logn 0.9 3))
    (t/is= m/inf- (m/logn 0.9 1))
    ;;SciPy 0.15200
    (t/is= 0.15200309344504997 (m/logn 0.9 0.5))
    (t/is= 0.0 (m/logn 0.9 0))
    (t/is= -0.0 (m/logn 0.9 m/inf+))))

(t/deftest pow-test
  (t/with-instrument `m/pow
    (t/is-spec-check m/pow))
  (t/with-instrument :all
    (t/is= 1.0 (m/pow 1 100))
    (t/is= 8.0 (m/pow 2 3))
    (t/is= 0.125 (m/pow 2 -3))
    (t/is= 1.0 (m/pow 2 0))
    (t/is= 0.0 (m/pow 0 1))
    (t/is= 1.0 (m/pow 0 0))
    (t/is= m/inf+ (m/pow m/inf+ 1))
    (t/is= 0.0 (m/pow m/inf+ -1))
    (t/is (m/nan? (m/pow m/nan 1)))
    (t/is (m/nan? (m/pow -1 0.5)))
    (t/is= 27.0 (m/pow 3 3))))

(t/deftest abs-test
  (t/with-instrument `m/abs
    (t/is-spec-check m/abs))
  (t/with-instrument :all
    (t/is= 3.3 (m/abs -3.3))
    (t/is= 3.0 (m/abs -3))
    (t/is= 0.0 (m/abs 0))
    (t/is= 0.0 (m/abs 0.0))
    (t/is= m/inf+ (m/abs m/inf+))
    (t/is= m/inf+ (m/abs m/inf-))
    (t/is (m/nan? (m/abs m/nan)))))

(t/deftest abs'-test
  (t/with-instrument `m/abs'
    (t/is-spec-check m/abs'))
  (t/with-instrument :all
    (t/is= 3.3 (m/abs' -3.3))
    (t/is= 3 (m/abs' -3))
    (t/is= 300000000 (m/abs' 3.0E8))
    (t/is (zero? (m/abs' 0)))
    (t/is (zero? (m/abs' 0.0)))
    (t/is= m/inf+ (m/abs' m/inf+))
    (t/is= m/inf+ (m/abs' m/inf-))
    (t/is (m/nan? (m/abs' m/nan)))))

(t/deftest sqrt-test
  (t/with-instrument `m/sqrt
    (t/is-spec-check m/sqrt))
  (t/with-instrument :all
    (t/is= 3.0 (m/sqrt 9))
    ;;SciPy 1.4142
    (t/is= 1.4142135623730951 (m/sqrt 2))
    (t/is= 0.0 (m/sqrt 0))
    (t/is= m/inf+ (m/sqrt m/inf+))
    (t/is (m/nan? (m/sqrt -1)))
    (t/is (m/nan? (m/sqrt m/nan)))
    (t/is= 10.0 (m/sqrt 100))))

(t/deftest cbrt-test
  (t/with-instrument `m/cbrt
    (t/is-spec-check m/cbrt))
  (t/with-instrument :all
    (t/is= 0.0 (m/cbrt 0.0))
    (t/is= 1.0 (m/cbrt 1.0))
    (t/is= -1.0 (m/cbrt -1.0))
    (t/is= -2.0 (m/cbrt -8))
    (t/is= m/inf+ (m/cbrt m/inf+))
    (t/is= m/inf- (m/cbrt m/inf-))
    (t/is (m/nan? (m/cbrt m/nan)))))

;;;TRIGONOMETRY
(t/deftest sin-test
  (t/with-instrument `m/sin
    (t/is-spec-check m/sin)))

(t/deftest sinh-test
  (t/with-instrument `m/sinh
    (t/is-spec-check m/sinh)))

(t/deftest asin-test
  (t/with-instrument `m/asin
    (t/is-spec-check m/asin)))

(t/deftest asinh-test
  (t/with-instrument `m/asinh
    (t/is-spec-check m/asinh))
  (t/with-instrument :all
    (t/is= 0.0 (m/asinh 0.0))
    ;;SciPy 0.48121
    (t/is= 0.48121182505960347 (m/asinh 0.5))
    ;;SciPy -0.88137
    (t/is= -0.8813735870195429 (m/asinh -1.0))
    ;;SciPy 0.88137
    (t/is= 0.8813735870195429 (m/asinh 1.0))
    ;;SciPy -1.4436
    (t/is= -1.4436354751788103 (m/asinh -2.0))
    ;;SciPy 1.4436
    (t/is= 1.4436354751788103 (m/asinh 2.0))
    (t/is= m/inf+ (m/asinh m/inf+))
    (t/is= m/inf- (m/asinh m/inf-))
    (t/is (m/nan? (m/asinh m/nan)))))

(t/deftest cos-test
  (t/with-instrument `m/cos
    (t/is-spec-check m/cos)))

(t/deftest cosh-test
  (t/with-instrument `m/cosh
    (t/is-spec-check m/cosh)))

(t/deftest acos-test
  (t/with-instrument `m/acos
    (t/is-spec-check m/acos)))

(t/deftest acosh-test
  (t/with-instrument `m/acosh
    (t/is-spec-check m/acosh))
  (t/with-instrument :all
    (t/is (m/nan? (m/acosh 0.0)))
    (t/is= 0.0 (m/acosh 1.0))
    ;;SciPy 1.3169
    (t/is= 1.3169578969248166 (m/acosh 2.0))
    (t/is= m/inf+ (m/acosh m/inf+))
    (t/is (m/nan? (m/acosh m/nan)))))

(t/deftest tan-test
  (t/with-instrument `m/tan
    (t/is-spec-check m/tan)))

(t/deftest tanh-test
  (t/with-instrument `m/tanh
    (t/is-spec-check m/tanh)))

(t/deftest atan-test
  (t/with-instrument `m/atan
    (t/is-spec-check m/atan)))

(t/deftest atan2-test
  (t/with-instrument `m/atan2
    (t/is-spec-check m/atan2)))

(t/deftest atanh-test
  (t/with-instrument `m/atanh
    (t/is-spec-check m/atanh))
  (t/with-instrument :all
    (t/is= 0.0 (m/atanh 0.0))
    ;;SciPy 0.54930
    (t/is-approx= 0.5493061443340549 (m/atanh 0.5) :tolerance 1e-14)
    ;;SciPy -0.54930
    (t/is-approx= -0.5493061443340549 (m/atanh -0.5) :tolerance 1e-14)
    (t/is= m/inf- (m/atanh -1.0))
    (t/is= m/inf+ (m/atanh 1.0))
    (t/is (m/nan? (m/atanh -2.0)))
    (t/is (m/nan? (m/atanh m/nan)))))

(t/deftest hypot-test
  (t/with-instrument `m/hypot
    (t/is-spec-check m/hypot)))

;;;ROUNDING
(t/deftest round'-test
  (t/with-instrument `m/round'
    (t/is-spec-check m/round'))
  (t/with-instrument :all
    (t/is= 1 (m/round' 0.5 :up))
    (t/is= 2.342342342342342E22 (m/round' 2.342342342342342E22 :up))
    (t/is (zero? (m/round' -0.5 :up)))
    (t/is= -1 (m/round' -0.5 :down))
    (t/is= -1 (m/round' -0.5 :away-from-zero))
    (t/is (zero? (m/round' -0.5 :toward-zero)))
    (t/is (zero? (m/round' 0.5 :down)))
    (t/is= 1 (m/round' 0.5 :away-from-zero))
    (t/is (zero? (m/round' 0.5 :toward-zero)))
    (t/is (zero? (m/round' 0.5 :toward-even)))
    (t/is= 2 (m/round' 1.5 :toward-even))
    (t/is= 2 (m/round' 2.5 :toward-even))
    (t/is= 4 (m/round' 3.5 :toward-even))
    (t/is (zero? (m/round' -0.5 :toward-even)))
    (t/is= -2 (m/round' -1.5 :toward-even))
    (t/is= m/inf+ (m/round' m/inf+ :up))
    (t/is= m/inf- (m/round' m/inf- :up))
    (t/is (m/nan? (m/round' m/nan :up)))))

(t/deftest round-significant-test
  (t/with-instrument `m/round-significant
    (t/is-spec-check m/round-significant))
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

(t/deftest floor-test
  (t/with-instrument `m/floor
    (t/is-spec-check m/floor))
  (t/with-instrument :all
    (t/is (zero? (m/floor 0.4)))
    (t/is= 2.3423423423423425E26 (m/floor 234234234234234234234343242N))
    (t/is= -1.0 (m/floor -0.4))
    (t/is= m/inf+ (m/floor m/inf+))
    (t/is= m/inf- (m/floor m/inf-))
    (t/is (m/nan? (m/floor m/nan)))))

(t/deftest floor'-test
  (t/with-instrument `m/floor'
    (t/is-spec-check m/floor'))
  (t/with-instrument :all
    (t/is= -1 (m/floor' -0.4))))

(t/deftest ceil-test
  (t/with-instrument `m/ceil
    (t/is-spec-check m/ceil))
  (t/with-instrument :all
    (t/is= 1.0 (m/ceil 0.4))
    (t/is= 2.3423423423423425E26 (m/ceil 234234234234234234234343242N))
    (t/is (zero? (m/ceil -0.4)))
    (t/is= m/inf+ (m/ceil m/inf+))
    (t/is= m/inf- (m/ceil m/inf-))
    (t/is (m/nan? (m/ceil m/nan)))))

(t/deftest ceil'-test
  (t/with-instrument `m/ceil'
    (t/is-spec-check m/ceil'))
  (t/with-instrument :all
    (t/is= 1 (m/ceil' 0.4))))

(t/deftest roughly-floor-test
  (t/with-instrument `m/roughly-floor
    (t/is-spec-check m/roughly-floor))
  (t/with-instrument :all
    (t/is= 1.0 (m/roughly-floor 0.99 0.02))
    (t/is (zero? (m/roughly-floor 0.99 0.005)))
    (t/is= 2.3423423423423425E26 (m/roughly-floor 234234234234234234234343242N 0.02))
    (t/is= 2.3423423423423425E26 (m/roughly-floor 2.3423423423423425E26 0.02))
    (t/is (zero? (m/roughly-floor -0.01 0.02)))
    (t/is= m/inf+ (m/roughly-floor m/inf+ 0.02))
    (t/is= m/inf- (m/roughly-floor m/inf- 0.02))
    (t/is (m/nan? (m/roughly-floor m/nan 0.02)))))

(t/deftest roughly-floor'-test
  (t/with-instrument `m/roughly-floor'
    (t/is-spec-check m/roughly-floor'))
  (t/with-instrument :all
    (t/is= 1 (m/roughly-floor' 0.99 0.02))))

(t/deftest roughly-ceil-test
  (t/with-instrument `m/roughly-ceil
    (t/is-spec-check m/roughly-ceil))
  (t/with-instrument :all
    (t/is (zero? (m/roughly-ceil 0.01 0.02)))
    (t/is= 1.0 (m/roughly-ceil 0.01 0.005))
    (t/is= 2.3423423423423425E26 (m/roughly-ceil 234234234234234234234343242N 0.02))
    (t/is= 2.3423423423423425E26 (m/roughly-ceil 2.3423423423423425E26 0.02))
    (t/is= -1.0 (m/roughly-ceil -0.99 0.02))
    (t/is= m/inf+ (m/roughly-ceil m/inf+ 0.02))
    (t/is= m/inf- (m/roughly-ceil m/inf- 0.02))
    (t/is (m/nan? (m/roughly-ceil m/nan 0.02)))))

(t/deftest roughly-ceil'-test
  (t/with-instrument `m/roughly-ceil'
    (t/is-spec-check m/roughly-ceil'))
  (t/with-instrument :all
    (t/is= 1 (m/roughly-ceil' 0.01 0.005))))

(t/deftest roughly?-test
  (t/with-instrument `m/roughly?
    (t/is-spec-check m/roughly?))
  (t/with-instrument :all
    (t/is-not (m/roughly? 0.01 0.02 0.005))
    (t/is (m/roughly? 0.01 0.02 0.01))
    (t/is (m/roughly? 0.01 0.02 0.02))
    (t/is (m/roughly? 2.3423423423423425E26 2.3423423423423425E26 0.03))
    (t/is (m/roughly? 2.3423423423423425E26 2.3423423423423425E26 0.005))
    (t/is-not (m/roughly? m/inf+ m/inf+ 0.01))
    (t/is (m/roughly? m/inf- 0.02 m/inf+))
    (t/is-not (m/roughly? m/nan 0.02 0.01))))

(t/deftest roughly-round?-test
  (t/with-instrument `m/roughly-round?
    (t/is-spec-check m/roughly-round?))
  (t/with-instrument :all
    (t/is (m/roughly-round? 0.01 0.02))
    (t/is-not (m/roughly-round? 0.01 0.005))
    (t/is (m/roughly-round? 2.3423423423423425E26 0.03))
    (t/is (m/roughly-round? 2.3423423423423425E26 0.005))
    (t/is (m/roughly-round? m/inf+ m/inf+))
    (t/is-not (m/roughly-round? m/inf- 0.4))
    (t/is-not (m/roughly-round? m/nan 0.01))))

(t/deftest roughly-round-non-?-test
  (t/with-instrument `m/roughly-round-non-?
    (t/is-spec-check m/roughly-round-non-?))
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

(t/deftest roughly-round-non+?-test
  (t/with-instrument `m/roughly-round-non+?
    (t/is-spec-check m/roughly-round-non+?))
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

(t/deftest roughly-round+?-test
  (t/with-instrument `m/roughly-round+?
    (t/is-spec-check m/roughly-round+?))
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

(t/deftest roughly-round-?-test
  (t/with-instrument `m/roughly-round-?
    (t/is-spec-check m/roughly-round-?))
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

(t/deftest roughly-non-?-test
  (t/with-instrument `m/roughly-non-?
    (t/is-spec-check m/roughly-non-?))
  (t/with-instrument :all
    (t/is-not (m/roughly-non-? -0.01 0.005))
    (t/is (m/roughly-non-? -0.02 0.02))
    (t/is (m/roughly-non-? 0.01 0.001))
    (t/is (m/roughly-non-? m/inf+ m/inf+))
    (t/is (m/roughly-non-? m/inf- m/inf+))
    (t/is-not (m/roughly-non-? m/inf- 0.4))
    (t/is (m/roughly-non-? m/inf+ 0.4))
    (t/is-not (m/roughly-non-? m/nan 0.01))))

(t/deftest roughly-non+?-test
  (t/with-instrument `m/roughly-non+?
    (t/is-spec-check m/roughly-non+?))
  (t/with-instrument :all
    (t/is-not (m/roughly-non+? 0.01 0.005))
    (t/is (m/roughly-non+? 0.02 0.02))
    (t/is (m/roughly-non+? -0.01 0.001))
    (t/is (m/roughly-non+? m/inf+ m/inf+))
    (t/is (m/roughly-non+? m/inf- m/inf+))
    (t/is (m/roughly-non+? m/inf- 0.4))
    (t/is-not (m/roughly-non+? m/inf+ 0.4))
    (t/is-not (m/roughly-non+? m/nan 0.01))))

(t/deftest roughly-prob?-test
  (t/with-instrument `m/roughly-prob?
    (t/is-spec-check m/roughly-prob?))
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

(t/deftest roughly-corr?-test
  (t/with-instrument `m/roughly-corr?
    (t/is-spec-check m/roughly-corr?))
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

;;;CLAMP
(t/deftest clamp-test
  (t/with-instrument `m/clamp
    (t/is-spec-check m/clamp))
  (t/with-instrument :all
    (t/is= 5 (m/clamp 5 0 10))
    (t/is= 10 (m/clamp 15 0 10))
    (t/is= 0 (m/clamp -3 0 10))
    (t/is= 0 (m/clamp 0 0 10))
    (t/is= 10 (m/clamp 10 0 10))
    (t/is= nil (m/clamp m/nan 0.0 1.0))
    (t/is= 1.0 (m/clamp 1.0001 0.0 1.0 1e-3))
    (t/is= 0.0 (m/clamp -0.0001 0.0 1.0 1e-3))
    (t/is= 0.5 (m/clamp 0.5 0.0 1.0 1e-3))
    (t/is= nil (m/clamp 1.5 0.0 1.0 1e-3))
    (t/is= nil (m/clamp -0.5 0.0 1.0 1e-3))
    (t/is= nil (m/clamp m/nan 0.0 1.0 1e-3))))

(t/deftest clamp-long'-test
  (t/with-instrument `m/clamp-long'
    (t/is-spec-check m/clamp-long'))
  (t/with-instrument :all
    (t/is= 5 (m/clamp-long' 5.7))
    (t/is= 0 (m/clamp-long' 0))
    (t/is= -3 (m/clamp-long' -3.2))
    (t/is= m/max-long (m/clamp-long' m/inf+))
    (t/is= m/min-long (m/clamp-long' m/inf-))
    (t/is= m/max-long (m/clamp-long' 1e100))
    (t/is= m/min-long (m/clamp-long' -1e100))
    (t/is= m/max-long (m/clamp-long' (double m/max-long)))
    (t/is= nil (m/clamp-long' m/nan))))

(t/deftest clamp-finite-test
  (t/with-instrument `m/clamp-finite
    (t/is-spec-check m/clamp-finite))
  (t/with-instrument :all
    (t/is= 0.5 (m/clamp-finite 0.5))
    (t/is= 0 (m/clamp-finite 0))
    (t/is= -3.0 (m/clamp-finite -3.0))
    (t/is= m/max-dbl (m/clamp-finite m/inf+))
    (t/is= m/min-dbl (m/clamp-finite m/inf-))
    (t/is= nil (m/clamp-finite m/nan))))

(t/deftest clamp-finite+-test
  (t/with-instrument `m/clamp-finite+
    (t/is-spec-check m/clamp-finite+))
  (t/with-instrument :all
    (t/is= 0.5 (m/clamp-finite+ 0.5))
    (t/is= (m/next-up 0.0) (m/clamp-finite+ 0.0))
    (t/is= (m/next-up 0.0) (m/clamp-finite+ 0))
    (t/is= (m/next-up 0.0) (m/clamp-finite+ -3.0))
    (t/is= (m/next-up 0.0) (m/clamp-finite+ m/inf-))
    (t/is= m/max-dbl (m/clamp-finite+ m/inf+))
    (t/is= m/max-dbl (m/clamp-finite+ m/max-dbl))
    (t/is= nil (m/clamp-finite+ m/nan))))

(t/deftest clamp-finite-non--test
  (t/with-instrument `m/clamp-finite-non-
    (t/is-spec-check m/clamp-finite-non-))
  (t/with-instrument :all
    (t/is= 0.5 (m/clamp-finite-non- 0.5))
    (t/is= 0.0 (m/clamp-finite-non- 0.0))
    (t/is= 0 (m/clamp-finite-non- 0))
    (t/is= 0.0 (m/clamp-finite-non- -3.0))
    (t/is= 0.0 (m/clamp-finite-non- m/inf-))
    (t/is= m/max-dbl (m/clamp-finite-non- m/inf+))
    (t/is= m/max-dbl (m/clamp-finite-non- m/max-dbl))
    (t/is= nil (m/clamp-finite-non- m/nan))))

(t/deftest clamp-prob-test
  (t/with-instrument `m/clamp-prob
    (t/is-spec-check m/clamp-prob))
  (t/with-instrument :all
    (t/is= 0.5 (m/clamp-prob 0.5))
    (t/is= 1.0 (m/clamp-prob 1.5))
    (t/is= 0.0 (m/clamp-prob -0.5))
    (t/is= nil (m/clamp-prob m/nan))
    (t/is= 1.0 (m/clamp-prob 1.0001 1e-3))
    (t/is= nil (m/clamp-prob 1.5 1e-3))
    (t/is= nil (m/clamp-prob m/nan 1e-3))))

(t/deftest clamp-open-prob-test
  (t/with-instrument `m/clamp-open-prob
    (t/is-spec-check m/clamp-open-prob))
  (t/with-instrument :all
    (t/is= 0.5 (m/clamp-open-prob 0.5))
    (t/is= (m/next-up 0.0) (m/clamp-open-prob 0.0))
    (t/is= (m/next-down 1.0) (m/clamp-open-prob 1.0))
    (t/is= (m/next-up 0.0) (m/clamp-open-prob -0.5))
    (t/is= (m/next-down 1.0) (m/clamp-open-prob 1.5))
    (t/is= nil (m/clamp-open-prob m/nan))
    (t/is= (m/next-up 0.0) (m/clamp-open-prob -0.0001 1e-3))
    (t/is= (m/next-down 1.0) (m/clamp-open-prob 1.0001 1e-3))
    (t/is= nil (m/clamp-open-prob -0.5 1e-3))
    (t/is= nil (m/clamp-open-prob m/nan 1e-3))))

(t/deftest clamp-prob+-test
  (t/with-instrument `m/clamp-prob+
    (t/is-spec-check m/clamp-prob+))
  (t/with-instrument :all
    (t/is= 0.5 (m/clamp-prob+ 0.5))
    (t/is= 1.0 (m/clamp-prob+ 1.0))
    (t/is= 1.0 (m/clamp-prob+ 1.5))
    (t/is= (m/next-up 0.0) (m/clamp-prob+ 0.0))
    (t/is= (m/next-up 0.0) (m/clamp-prob+ -0.5))
    (t/is= nil (m/clamp-prob+ m/nan))
    (t/is= (m/next-up 0.0) (m/clamp-prob+ -0.0001 1e-3))
    (t/is= 1.0 (m/clamp-prob+ 1.0001 1e-3))
    (t/is= nil (m/clamp-prob+ 1.5 1e-3))
    (t/is= nil (m/clamp-prob+ m/nan 1e-3))))

(t/deftest clamp-corr-test
  (t/with-instrument `m/clamp-corr
    (t/is-spec-check m/clamp-corr))
  (t/with-instrument :all
    (t/is= 0.5 (m/clamp-corr 0.5))
    (t/is= 1.0 (m/clamp-corr 1.5))
    (t/is= -1.0 (m/clamp-corr -1.5))
    (t/is= nil (m/clamp-corr m/nan))
    (t/is= 1.0 (m/clamp-corr 1.0001 1e-3))
    (t/is= -1.0 (m/clamp-corr -1.0001 1e-3))
    (t/is= nil (m/clamp-corr 1.5 1e-3))
    (t/is= nil (m/clamp-corr m/nan 1e-3))))

(t/deftest clamp-open-corr-test
  (t/with-instrument `m/clamp-open-corr
    (t/is-spec-check m/clamp-open-corr))
  (t/with-instrument :all
    (t/is= 0.0 (m/clamp-open-corr 0.0))
    (t/is= (m/next-up -1.0) (m/clamp-open-corr -1.0))
    (t/is= (m/next-down 1.0) (m/clamp-open-corr 1.0))
    (t/is= (m/next-up -1.0) (m/clamp-open-corr -1.5))
    (t/is= (m/next-down 1.0) (m/clamp-open-corr 1.5))
    (t/is= nil (m/clamp-open-corr m/nan))
    (t/is= (m/next-up -1.0) (m/clamp-open-corr -1.0001 1e-3))
    (t/is= nil (m/clamp-open-corr 1.5 1e-3))
    (t/is= nil (m/clamp-open-corr m/nan 1e-3))))

;;;QUOTIENTS
(t/deftest quot'-test
  (t/with-instrument `m/quot'
    (t/is-spec-check m/quot'))
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

(t/deftest mod'-test
  (t/with-instrument `m/mod'
    (t/is-spec-check m/mod'))
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

(t/deftest rem'-test
  (t/with-instrument `m/rem'
    (t/is-spec-check m/rem'))
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

(t/deftest quot-and-rem'-test
  (t/with-instrument `m/quot-and-rem'
    (t/is-spec-check m/quot-and-rem'))
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

(t/deftest quot-and-mod'-test
  (t/with-instrument `m/quot-and-mod'
    (t/is-spec-check m/quot-and-mod'))
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

(t/deftest gcd-test
  (t/with-instrument `m/gcd
    (t/is-spec-check m/gcd))
  (t/with-instrument :all
    (t/is= 7 (m/gcd 271284701247 12467364728))
    (t/is= 6 (m/gcd 12 18))
    (t/is= 6 (m/gcd -12 18))
    (t/is= 6 (m/gcd 12 -18))
    (t/is= 6 (m/gcd -12 -18))
    (t/is= 5 (m/gcd 0 5))
    (t/is= 5 (m/gcd 5 0))
    (t/is= 0 (m/gcd 0 0))))

(t/deftest lcm'-test
  (t/with-instrument `m/lcm'
    (t/is-spec-check m/lcm'))
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
(t/deftest reduce-angle'-test
  (t/with-instrument `m/reduce-angle'
    (t/is-spec-check m/reduce-angle'))
  (t/with-instrument :all
    (t/is= 30.4 (m/reduce-angle' 30.4))
    (t/is= 350.2 (m/reduce-angle' -9.8))
    (t/is= 118 (m/reduce-angle' 478.0))
    (t/is= 26 (m/reduce-angle' -8399494))
    (t/is (m/nan? (m/reduce-angle' m/nan)))
    (t/is (m/nan? (m/reduce-angle' m/inf+)))
    (t/is (m/nan? (m/reduce-angle' m/inf-)))))

(t/deftest reduce-radians'-test
  (t/with-instrument `m/reduce-radians'
    (t/is-spec-check m/reduce-radians'))
  (t/with-instrument :all
    ;;SciPy 5.2672
    (t/is= 5.267258771281654 (m/reduce-radians' 30.4))
    (t/is (zero? (m/reduce-radians' m/two-pi)))
    (t/is= m/PI (m/reduce-radians' m/PI))
    (t/is= 0.06552912132908517 (m/reduce-radians' -8399494))
    (t/is (m/nan? (m/reduce-radians' m/nan)))
    (t/is (m/nan? (m/reduce-radians' m/inf+)))
    (t/is (m/nan? (m/reduce-radians' m/inf-)))))

(t/deftest radians->angle'-test
  (t/with-instrument `m/radians->angle'
    (t/is-spec-check m/radians->angle'))
  (t/with-instrument :all
    (t/is (zero? (m/radians->angle' 0)))
    ;;SciPy 194.80
    (t/is= 194.8056503444799 (m/radians->angle' 3.4))
    (t/is (zero? (m/radians->angle' m/two-pi)))
    ;;SciPy 165.19
    (t/is= 165.1943496555201 (m/radians->angle' -3.4))
    ;;SciPy 58.310
    (t/is= 58.31007808870436 (m/radians->angle' 45))
    (t/is (m/nan? (m/radians->angle' m/nan)))
    (t/is= m/inf+ (m/radians->angle' m/inf+))
    (t/is= m/inf- (m/radians->angle' m/inf-))))

(t/deftest angle->radians'-test
  (t/with-instrument `m/angle->radians'
    (t/is-spec-check m/angle->radians'))
  (t/with-instrument :all
    (t/is (zero? (m/angle->radians' 0)))
    ;;SciPy 0.059341
    (t/is= 0.059341194567807204 (m/angle->radians' 3.4))
    (t/is= 0.002777777777777778 (m/angle->radians' m/inv-two-pi))
    ;;SciPy 6.2238
    (t/is= 6.223844112611779 (m/angle->radians' -3.4))
    ;;SciPy 0.78539
    (t/is= 0.7853981633974483 (m/angle->radians' 45))
    (t/is (m/nan? (m/angle->radians' m/nan)))
    (t/is= m/inf+ (m/angle->radians' m/inf+))
    (t/is= m/inf- (m/angle->radians' m/inf-))))
