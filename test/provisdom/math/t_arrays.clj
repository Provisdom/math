(ns provisdom.math.t-arrays
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.arrays :as a]))

(deftest deep-aget-test
  (let [arr (a/jagged-2D-array :long [[1 2 3]])]
    (is= 3 (a/deep-aget arr 0 2))))