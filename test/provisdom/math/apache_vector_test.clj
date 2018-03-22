(ns provisdom.math.apache-vector-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.apache-vector :as apache-v]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;;;2 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

;;;TYPES
(deftest apache-vector?-test
  (is (spec-check apache-v/apache-vector?))
  (is (apache-v/apache-vector? (apache-v/apache-vector [])))
  (is (apache-v/apache-vector? (apache-v/apache-vector [1])))
  (is-not (apache-v/apache-vector? "A"))
  (is-not (apache-v/apache-vector? [1 2])))

;;;CONSTRUCTORS
(deftest apache-vector-&-apache-vector->vector-test
  (is (spec-check apache-v/apache-vector))
  (is (spec-check apache-v/apache-vector->vector))
  (is= [] (apache-v/apache-vector->vector (apache-v/apache-vector [])))
  (is= [1.0] (apache-v/apache-vector->vector (apache-v/apache-vector [1.0])))
  (is= [1.0 2.0] (apache-v/apache-vector->vector (apache-v/apache-vector [1.0 2.0]))))

#_(ost/unstrument)