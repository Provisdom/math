(def project 'provisdom/math)
(def version "1.1.0-alpha3")

(set-env!
  :resource-paths #{"src"}
  :source-paths #{"test"}
  :wagons '[[s3-wagon-private "1.2.0"]]
  :repositories #(conj % ["private" {:url "s3p://provisdom-artifacts/releases"}])
  :dependencies '[[org.clojure/tools.nrepl "0.2.13" :scope "test"]
                  [org.clojure/test.check "0.10.0-alpha2"]
                  [adzerk/boot-test "1.2.0" :scope "test"]
                  [midje "1.9.0-alpha11" :exclusions [org.clojure/clojure] :scope "test"]
                  [criterium "0.4.4" :scope "test"]
                  [provisdom/boot-tasks "1.4" :scope "test"]
                  [provisdom/test "0.3.3-alpha1" :scope "test"]
                  ;;project deps
                  [org.clojure/clojure "1.9.0-RC1" :scope "provided"]
                  [org.clojure/spec.alpha "0.1.143"]
                  [orchestra "2017.11.12-1"]
                  [provisdom/utility-belt "0.2.0-alpha2"]
                  [org.apache.commons/commons-math3 "3.6.1"]
                  [apache-commons-matrix "0.4.1"]
                  [clatrix "0.5.0"]])

(require
  '[adzerk.boot-test :refer [test]]
  '[provisdom.boot-tasks.core :refer [build push-jar]])

(task-options!
  pom {:project     project
       :version     version
       :description "Provisdom math"
       :url         "https://gitlab.com/provisdom/math"
       :scm         {:url "https://github.com/Provisdom/math"}
       :license     {"Provisdom" "(c) 2015-2017 Provisdom Corporation"}}
  ;; TODO: replace this when all namespaces are converted to clojure.test
  test {:namespaces '#{provisdom.math.t-apache-matrix
                       provisdom.math.t-apache-vector
                       provisdom.math.t-arrays
                       provisdom.math.t-calculus
                       provisdom.math.t-clatrix
                       provisdom.math.t-combinatorics
                       provisdom.math.t-core
                       provisdom.math.t-format
                       provisdom.math.t-intervals
                       provisdom.math.t-matrix
                       provisdom.math.t-random
                       provisdom.math.t-series
                       provisdom.math.t-special-functions
                       ;provisdom.math.t-splittable-random
                       provisdom.math.t-tensor
                       provisdom.math.t-vector
                       }})
