(ns provisdom.math.neanderthal-matrix
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.utility-belt.anomalies :as anomalies]
    [uncomplicate.fluokitten.core :as fluokitten]
    [uncomplicate.neanderthal.core :as neanderthal]
    [uncomplicate.neanderthal.linalg :as linear-algebra]
    [uncomplicate.neanderthal.native :as native]))

(declare neanderthal-rows)

(defn vector->neanderthal-matrix
  "`v` is a vector of data."
  [rows columns v by-column?]
  (native/dge rows columns v {:layout (if by-column? :column :row)}))

(defn matrix->neanderthal-matrix
  ""
  [m]
  (native/dge m))

(defn neanderthal-matrix->matrix
  ""
  ([neanderthal-mx]
   (mapv (comp vec seq) (neanderthal-rows neanderthal-mx)))
  ([neanderthal-mx take-nrows]
   (mapv (comp vec seq) (take take-nrows (neanderthal-rows neanderthal-mx)))))

(defn neanderthal-rows
  "Returns the rows as neanderthal vectors."
  [neanderthal-mx]
  (neanderthal/rows neanderthal-mx))

(defn neanderthal-columns
  "Returns the columns as neanderthal vectors."
  [neanderthal-mx]
  (neanderthal/cols neanderthal-mx))

(defn rows
  "Returns the number of rows."
  [neanderthal-mx]
  (neanderthal/mrows neanderthal-mx))

(defn columns
  "Returns the number of columns."
  [neanderthal-mx]
  (neanderthal/ncols neanderthal-mx))

(defn mx*
  "Matrix multiplication."
  [neanderthal-mx1 neanderthal-mx2]
  (neanderthal/mm neanderthal-mx1 neanderthal-mx2))

(defn fmap
  "Maps a function onto a functor."
  [f a]
  (fluokitten/fmap f a))

(defn lls
  "Linear Linear Squares, solving for 'x', where `a` × x = `b`."
  [a b]
  (try (linear-algebra/ls a b)
       (catch Exception _ {::anomalies/category ::anomalies/no-solve
                           ::anomalies/message "No LLS Solution"
                           ::anomalies/fn (var lls)})))

(defn lls!
  "Linear Linear Squares, solving for 'x', where `a` × x = `b`. After
  destruction, `a` will contain factorization data, and `b` will contain matrix
  solution. Also returns solution."
  [a b]
  (try (linear-algebra/ls! a b)
       (catch Exception _ {::anomalies/category ::anomalies/no-solve
                           ::anomalies/message "No LLS Solution"
                           ::anomalies/fn (var lls!)})))


