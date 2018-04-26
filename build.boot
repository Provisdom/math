(def project 'provisdom/math)
(def version "1.2.0-alpha3")

(set-env!
  :resource-paths #{"src"}
  :source-paths #{"test"}
  :wagons '[[s3-wagon-private "1.2.0"]]
  :repositories #(conj % ["private" {:url "s3p://provisdom-artifacts/releases"}])
  :dependencies '[[org.clojure/tools.nrepl "0.2.13" :scope "test"]
                  [org.clojure/test.check "0.10.0-alpha2"]
                  [adzerk/boot-test "1.2.0" :scope "test"]
                  [criterium "0.4.4" :scope "test"]
                  [provisdom/boot-tasks "1.4" :scope "test"]
                  [provisdom/test "0.3.7" :scope "test"]

                  ;;project deps
                  [org.clojure/clojure "1.9.0" :scope "provided"]
                  [org.clojure/spec.alpha "0.1.143"]
                  [orchestra "2017.11.12-1"]
                  [provisdom/utility-belt "0.3.0-alpha1"]
                  [org.apache.commons/commons-math3 "3.6.1"]
                  [apache-commons-matrix "0.4.1"]])

(require
  '[adzerk.boot-test :refer [test]]
  '[provisdom.boot-tasks.core :refer [build push-jar]])

(task-options!
  pom {:project     project
       :version     version
       :description "Provisdom math"
       :url         "https://gitlab.com/provisdom/math"
       :scm         {:url "https://github.com/Provisdom/math"}
       :license     {"Provisdom" "(c) 2015-2018 Provisdom Corporation"}}
  test {:namespaces '#{
                       provisdom.math.apache-matrix-test
                       provisdom.math.apache-vector-test
                       provisdom.math.arrays-test
                       provisdom.math.combinatorics-test
                       provisdom.math.core-test
                       provisdom.math.derivatives-test
                       provisdom.math.format-test
                       ;provisdom.math.integrals-test ;need to speed this up first
                       ;provisdom.math.internal-splittable-random-test ;not for CI
                       provisdom.math.intervals-test
                       provisdom.math.matrix-test
                       provisdom.math.random-test
                       provisdom.math.series-test
                       provisdom.math.special-functions-test
                       provisdom.math.tensor-test
                       provisdom.math.vector-test
                       }})
