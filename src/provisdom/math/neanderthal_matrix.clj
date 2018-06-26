(ns provisdom.math.neanderthal-matrix
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.utility-belt.anomalies :as anomalies]
    [uncomplicate.neanderthal.core :as neanderthal]
    [uncomplicate.neanderthal.linalg :as linear-algebra]
    [uncomplicate.neanderthal.native :as native]))

(declare )

(defn matrix
  "`v` is a vector of data."
  [rows columns v]
  (native/dge rows columns v))

(defn mx*
  "Matrix multiplication."
  [mx1 mx2]
  (neanderthal/mm mx1 mx2))

(defn lls
  "Linear Linear Squares, solving for 'x', where `a` × x = `b`."
  [a b]
  (linear-algebra/ls a b))

(defn lls!
  "Linear Linear Squares, solving for 'x', where `a` × x = `b`. After
  destruction, `a` will contain factorization data, and `b` will contain matrix
  solution. Also returns solution."
  [a b]
  (linear-algebra/ls! a b))


