(ns provisdom.math.t-core
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [provisdom.math.core :as m]))

(def simple-type
  (gen/one-of [gen/int gen/large-integer gen/double gen/char
               gen/string gen/ratio gen/boolean gen/keyword gen/symbol]))

(defn- long-able*
  [f v & tests]
  (let [v? (f v)]
    (= v?
       (if (number? v)
         (if (every? identity (apply list (m/roughly-round? v 0.0) (#'m/long-range? v) (map #(% v) tests)))
           true
           false)
         false))))

(defspec long-able?-check
         100
         (prop/for-all [v simple-type] (long-able* m/long-able? v)))

(defspec long-able+?-check
         100
         (prop/for-all [v simple-type] (long-able* m/long-able+? v #(pos? %))))

(defspec long-able-?-check
         100
         (prop/for-all [v simple-type] (long-able* m/long-able-? v #(neg? %))))

(defspec long-able-non+?-check
         100
         (prop/for-all [v simple-type] (long-able* m/long-able-non+? v #(or (neg? %) (zero? %)))))

(defspec long-able-non-?-check
         100
         (prop/for-all [v simple-type] (long-able* m/long-able-non-? v #(or (pos? %) (zero? %)))))

(defspec maybe-long-able-check
         100
         (prop/for-all [v simple-type]
                       (let [res (m/maybe-long-able v)]
                         (if (m/long-able? v)
                           (= (type res) Long)
                           (= (type res) (type v))))))