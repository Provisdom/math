(defproject
  provisdom/math
  "0.1.1"
  :plugins
  [[s3-wagon-private "1.2.0"]]
  :dependencies
  [[org.clojure/clojure "1.8.0"]
   [adzerk/boot-test "1.1.1" :scope "test"]
   [org.clojure/tools.nrepl "0.2.12" :scope "test"]
   [provisdom/boot-tasks "0.6.0" :scope "test"]
   [org.clojure/test.check "0.9.0"]
   [provisdom/test "0.2.0" :scope "test"]
   [midje "1.8.3" :exclusions [org.clojure/clojure] :scope "test"]
   [provisdom/translate-midje "0.1.0" :scope "test"]
   [criterium "0.4.4" :scope "test"]
   [provisdom/utility-belt "0.1.0"]
   [com.taoensso/truss "1.0.0"]
   [org.clojure/math.numeric-tower "0.0.4"]
   [org.apache.commons/commons-math3 "3.6"]
   [apache-commons-matrix "0.4.0"]
   [clatrix "0.5.0"]
   [net.mikera/core.matrix "0.50.0"]
   [net.sourceforge.parallelcolt/parallelcolt "0.10.1"]]
  :repositories
  [["clojars" "http://clojars.org/repo/"]
   ["maven-central" "http://repo1.maven.org/maven2/"]
   ["provisdom"
    {:username :env/aws_access_key,
     :passphrase :env/aws_secret_key,
     :url "s3p://provisdom-artifacts/releases/"}]]
  :source-paths
  ["test" "src" "scss" "resources"])