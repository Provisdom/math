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

(defn ->neanderthal-matrix
  ""
  [nrows ncols]
  (native/dge nrows ncols))

(defn ->identity-neanderthal-matrix
  ""
  [size]
  (let [zero (->neanderthal-matrix size size)
        _ (neanderthal/entry! (neanderthal/dia zero) 1.0)]
    zero))

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

(defn transpose
  ""
  [neanderthal-mx]
  (neanderthal/trans neanderthal-mx))

(defn mx*
  "Matrix multiplication."
  [neanderthal-mx1 neanderthal-mx2]
  (neanderthal/mm neanderthal-mx1 neanderthal-mx2))

(defn fmap
  "Maps a function onto a functor."
  [f a]
  (fluokitten/fmap f a))

(defn lls
  "Linear Linear Squares, solving for 'x', where `a` × x = `b`.  Returns
  solution."
  [a b]
  (try (let [cols (columns a)
             sol (linear-algebra/ls a b)
             cols-sol (columns sol)]
         (neanderthal/submatrix sol cols cols-sol))
       (catch Exception _ {::anomalies/category ::anomalies/no-solve
                           ::anomalies/message  "No LLS Solution"
                           ::anomalies/fn       (var lls)})))

(defn lls!
  "Linear Linear Squares, solving for 'x', where `a` × x = `b`.  After
  destruction, `a` will contain factorization data, and `b` will contain
  solution.  Also returns solution."
  [a b]
  (try (let [cols (columns a)
             sol (linear-algebra/ls! a b)
             cols-sol (columns sol)]
         (neanderthal/submatrix sol cols cols-sol))
       (catch Exception _ {::anomalies/category ::anomalies/no-solve
                           ::anomalies/message  "No LLS Solution"
                           ::anomalies/fn       (var lls!)})))

(defn lls-with-projection-and-annihilation
  "Linear Linear Squares, solving for 'x', where `a` × x = `b`.  Returns map
  of solution, projection matrix, and annihilation matrix."
  [a b]
  (try (let [qr (linear-algebra/qrf a)
             q (linear-algebra/org qr)
             r1 (neanderthal/view-tr (:or qr) {:uplo :upper})
             solution (linear-algebra/sv! r1 (mx* (transpose q) b))
             projection (mx* q (transpose q))
             cols (columns projection)
             identity (->identity-neanderthal-matrix cols)
             annihilation (neanderthal/axpy -1.0 projection identity)]
         {:solution     solution
          :projection   projection
          :annihilation annihilation})
       (catch Exception _ {::anomalies/category ::anomalies/no-solve
                           ::anomalies/message  "No LLS Solution"
                           ::anomalies/fn       (var lls-with-projection-and-annihilation)})))


