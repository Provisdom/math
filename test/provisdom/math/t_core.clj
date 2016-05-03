(ns provisdom.math.t-core
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test :refer [deftest is]]
            [midje.sweet :as midj]
            [midje.parsing.1-to-explicit-form.metadata :as mparse]
            [midje.parsing.1-to-explicit-form.parse-background :as mbparse]
            [provisdom.math.core :as m]))

(defmacro convert-midje
  [facts]
  (let [[metadata forms] (mparse/separate-metadata facts)
        separate-forms (partition 3 forms)]
    ))

(deftest long-able?
  (is ()))