(ns provisdom.math.neanderthal-matrix-test
  (:require
    [clojure.test :refer :all]
    [criterium.core :as criterium]
    [provisdom.test.core :refer :all]
    [provisdom.math.neanderthal-matrix :as neanderthal-mx]
    [provisdom.math.core :as m]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;? seconds

(set! *warn-on-reflection* true)

(ost/instrument)

(deftest lls-with-error-test
  (is (spec-check neanderthal-mx/lls-with-error))
  (is (data-approx=
        #::neanderthal-mx
            {:standard-squared-errors [[3.3684210526315392]]
             :mean-squared-errors     [[1.6842105263157696]]
             :annihilator             [[0.37969924812029987 -0.25187969924812054
                                        0.30075187969924816 -0.2857142857142859]
                                       [-0.25187969924812054 0.8007518796992481
                                        -0.12030075187969944 -0.2857142857142858]
                                       [0.30075187969924816 -0.12030075187969944
                                        0.24812030075187974 -0.285714285714286]
                                       [-0.2857142857142859 -0.2857142857142858
                                        -0.285714285714286 0.5714285714285714]]
             :solution                [[-0.6842105263157898] [3.473684210526316]]
             :projection              [[0.6203007518797001 0.25187969924812054
                                        -0.30075187969924816 0.2857142857142859]
                                       [0.25187969924812054 0.19924812030075195
                                        0.12030075187969944 0.2857142857142858]
                                       [-0.30075187969924816 0.12030075187969944
                                        0.7518796992481203 0.285714285714286]
                                       [0.2857142857142859 0.2857142857142858
                                        0.285714285714286 0.4285714285714286]]}
        (apply hash-map
               (mapcat
                 (fn [[k v]]
                   (let [m (neanderthal-mx/neanderthal-matrix->matrix v)]
                     [k m]))
                 (neanderthal-mx/lls-with-error
                   (neanderthal-mx/matrix->neanderthal-matrix
                     [[1.0 2.0] [3.0 2.0] [6.0 2.0] [5.0 3.0]])
                   (neanderthal-mx/matrix->neanderthal-matrix
                     [[5.0] [7.0] [2.0] [7.0]])))))))

#_(ost/unstrument)