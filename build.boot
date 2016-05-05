(def project 'provisdom/math)
(def version "0.1.1")

(set-env! :resource-paths #{"resources" "src" "scss"}
          :source-paths #{"test"}
          :dependencies '[[org.clojure/clojure "1.8.0"]
                          [adzerk/boot-test "1.1.1" :scope "test"]
                          [org.clojure/tools.nrepl "0.2.12" :scope "test"]
                          [provisdom/boot-tasks "0.6.0" :scope "test"]
                          [org.clojure/test.check "0.9.0"]
                          [provisdom/test "0.2.0" :scope "test"]
                          [midje "1.8.3" :exclusions [org.clojure/clojure] :scope "test"]
                          ;;project deps
                          [provisdom/utility-belt "0.1.0"]
                          [com.taoensso/truss "1.0.0"]
                          [org.clojure/math.numeric-tower "0.0.4"]
                          [org.apache.commons/commons-math3 "3.6"]
                          [apache-commons-matrix "0.4.0"]
                          [clatrix "0.5.0"]
                          [net.mikera/core.matrix "0.50.0"]
                          [net.sourceforge.parallelcolt/parallelcolt "0.10.1"]])

(require
  '[adzerk.boot-test :refer [test]]
  '[provisdom.boot-tasks.core :refer [build release]])

(task-options!
  pom {:project     project
       :version     version
       :description "FIXME: write description"
       :url         "http://example/FIXME"
       :scm         {:url "https://github.com/Provisdom/math"}
       :license     {"Eclipse Public License"
                     "http://www.eclipse.org/legal/epl-v10.html"}})
