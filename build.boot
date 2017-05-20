(def project 'provisdom/math)
(def version "1.0-alpha10")

(set-env! :resource-paths #{"src"}
          :source-paths #{"test"}
          :dependencies '[[org.clojure/tools.nrepl "0.2.12" :scope "test"]
                          [org.clojure/test.check "0.9.0"]
                          [adzerk/boot-test "1.1.2" :scope "test"]
                          [midje "1.9.0-alpha6" :exclusions [org.clojure/clojure] :scope "test"]
                          [criterium "0.4.4" :scope "test"]

                          [provisdom/boot-tasks "1.4" :scope "test"]
                          [provisdom/test "0.2.4" :scope "test"]
                          ;;project deps
                          [org.clojure/clojure "1.9.0-alpha15" :scope "provided"]
                          [org.clojure/core.async "0.3.442"]
                          [provisdom/utility-belt "0.1.1"]
                          [com.taoensso/truss "1.3.6"]
                          [org.clojure/math.numeric-tower "0.0.4"]
                          [org.apache.commons/commons-math3 "3.6.1"]
                          [apache-commons-matrix "0.4.1"]
                          [clatrix "0.5.0"]
                          [net.mikera/core.matrix "0.57.0"]
                          [net.sourceforge.parallelcolt/parallelcolt "0.10.1"]])

(require
  '[adzerk.boot-test :refer [test]]
  '[provisdom.boot-tasks.core :refer [build push-jar]])

(task-options!
  pom {:project     project
       :version     version
       :description "FIXME: write description"
       :url         "http://example/FIXME"
       :scm         {:url "https://github.com/Provisdom/math"}
       :license     {"Eclipse Public License"
                     "http://www.eclipse.org/legal/epl-v10.html"}}
  ;; TODO: replace this when all namespaces are converted to clojure.test
  test {:namespaces '#{provisdom.math.t-arrays provisdom.math.t-core provisdom.math.t-format
                       provisdom.math.random2}})
