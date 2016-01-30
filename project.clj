(defproject provisdom/math "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts [~(str "-Djava.library.path=native/:"
                (System/getProperty "java.library.path"))]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [provisdom/utility-belt "0.1.0"]
                 [com.taoensso/truss "1.0.0"]

                 [provisdom/test "0.1.0" :scope "test"]
                 [midje "1.6.3" :exclusions [org.clojure/clojure] :scope "test"]

                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.apache.commons/commons-math3 "3.6"]
                 [apache-commons-matrix "0.4.0"]
                 [clatrix "0.5.0"]
                 [net.mikera/core.matrix "0.49.0"]]
  :plugins [[lein-midje "3.1.3"]])
