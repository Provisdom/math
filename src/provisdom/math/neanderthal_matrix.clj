(ns provisdom.math.neanderthal-matrix
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.utility-belt.anomalies :as anomalies]
    [provisdom.math.core :as m]
    [provisdom.math.matrix :as mx]
    [uncomplicate.fluokitten.core :as fluokitten]
    [uncomplicate.neanderthal.core :as neanderthal]
    [uncomplicate.neanderthal.linalg :as linear-algebra]
    [uncomplicate.neanderthal.native :as native]))

(declare neanderthal-rows neanderthal-matrix? matrix->neanderthal-matrix)

;;;MATRIX TYPES
(defn neanderthal-matrix?
  "Returns true if a Neanderthal matrix."
  [x]
  (neanderthal/matrix? x))

(s/fdef neanderthal-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::neanderthal-matrix
  (s/with-gen
    neanderthal-matrix?
    #(gen/fmap matrix->neanderthal-matrix (s/gen ::mx/matrix))))

;;;CONVERSIONS
(defn neanderthal-vector->vector
  ""
  [neanderthal-v]
  (apply vector neanderthal-v))

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
   (mapv neanderthal-vector->vector
         (neanderthal-rows neanderthal-mx)))
  ([neanderthal-mx take-nrows]
   (mapv neanderthal-vector->vector
         (take take-nrows (neanderthal-rows neanderthal-mx)))))

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

(defn diagonal
  ""
  [neanderthal-mx]
  (neanderthal/dia neanderthal-mx))

(defn transpose
  ""
  [neanderthal-mx]
  (neanderthal/trans neanderthal-mx))

(defn mx*
  "Matrix multiplication."
  ([neanderthal-mx1 neanderthal-mx2]
   (neanderthal/mm neanderthal-mx1 neanderthal-mx2))
  ([neanderthal-mx1 neanderthal-mx2 & more]
   (apply neanderthal/mm neanderthal-mx1 neanderthal-mx2 more)))

(defn fmap
  "Maps a function onto a functor."
  [f a]
  (fluokitten/fmap f a))

(defn lower-cholesky
  "Lower * Upper = `neanderthal-mx`, using the symmetric view of a Neanderthal
  matrix."
  [neanderthal-mx]
  (try (let [a-sy (neanderthal/view-sy (neanderthal/copy a) {:uplo :lower})
             lower-tr (neanderthal/view-tr (:lu (linear-algebra/ptrf! a-sy)))]
         (native/dge lower-tr))
       (catch Exception _ {::anomalies/category ::anomalies/no-solve
                           ::anomalies/message  "No Cholesky Solution"
                           ::anomalies/fn       (var lower-cholesky)})))

(defn inverse-triangular
  "Inverts the triangular view of a Neanderthal matrix, and returns a
  triangular-neanderthal-mx."
  [neanderthal-mx]
  (try (let [a-tr (neanderthal/view-tr (neanderthal/copy neanderthal-mx))
             inv-a (linear-algebra/tri! a-tr)]
         inv-a)
       (catch Exception _ {::anomalies/category ::anomalies/no-solve
                           ::anomalies/message  "No Inverse"
                           ::anomalies/fn       (var inverse-triangular)})))

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

(s/def ::solution ::neanderthal-matrix)
(s/def ::projection ::neanderthal-matrix)
(s/def ::annihilator ::neanderthal-matrix)
(s/def ::mean-squared-errors ::neanderthal-matrix)
(s/def ::standard-squared-errors ::neanderthal-matrix)

(defn lls-with-error
  "Linear Linear Squares, solving for 'x', where `a` × x = `b`.  Returns map
  of solution, projection matrix, annihilator matrix, standard-squared-errors
  (unbiased), and mean-squared-errors (maximum likelihood errors).
  See https://en.wikipedia.org/wiki/Ordinary_least_squares."
  [a b]
  (try (let [qr (linear-algebra/qrf a)
             q (linear-algebra/org qr)
             r1 (neanderthal/view-tr (:or qr) {:uplo :upper})
             qt (transpose q)
             solution (linear-algebra/sv! r1 (mx* qt b))
             projection (mx* q qt)
             cols (columns projection)
             identity (->identity-neanderthal-matrix cols)
             annihilator (neanderthal/axpy -1.0 projection identity)
             n (rows b)
             p (columns a)
             sum-squared-errors (mx* (transpose b) annihilator b)
             mean-squared-errors (neanderthal/scal (/ 1.0 n) sum-squared-errors)
             standard-squared-errors (neanderthal/scal (/ n (- n p)) mean-squared-errors)]
         {::solution                solution
          ::projection              projection
          ::annihilator             annihilator
          ::mean-squared-errors     mean-squared-errors
          ::standard-squared-errors standard-squared-errors})
       (catch Exception _ {::anomalies/category ::anomalies/no-solve
                           ::anomalies/message  "No LLS Solution"
                           ::anomalies/fn       (var lls-with-error)})))

(s/fdef lls-with-error
        :args (s/cat :a ::neanderthal-matrix
                     :b ::neanderthal-matrix)
        :ret (s/or :anomaly ::anomalies/anomaly
                   :sol (s/keys :req [::solution
                                      ::projection
                                      ::annihilator
                                      ::mean-squared-errors
                                      ::standard-squared-errors])))

(s/def ::svd-left ::neanderthal-matrix)
(s/def ::svd-right ::neanderthal-matrix)
(s/def ::singular-values ::neanderthal-matrix)              ;;diagonal-mx
(s/def ::rank ::m/int-non-)

(defn sv-decomposition
  "Calculates the compact Singular Value Decomposition of a Neanderthal
  matrix. The Singular Value Decomposition of `neanderthal-m` is a set of three
   matrices: `svd-left`, `singular-values`, and `svd-right` such that:
    `neanderthal-m` = `svd-left` × `singular-values` × `svd-right`.
   Let `neanderthal-m` be a m × n matrix, then `svd-left` is a m × p orthogonal
   matrix of the left singular vectors, `singular-values` is a p × p diagonal
   matrix of singular values with positive or nil elements, and are ordered from
   largest to smallest. `svd-right` is a p × n orthogonal matrix of the right
   singular vectors where p = min(m,n). Note that:
   Identity Matrix = (transpose `svd-left`) × `svd-left` =
    `svd-right` × (transpose `svd-right`).
   Returns a map containing:
      `::svd-left` -- Neanderthal matrix of left singular vectors
      `::singular-values` -- diagonal Neanderthal Commons matrix
      `::svd-right` -- transpose of Neanderthal matrix of right singular
        vectors
      `::rank` -- rank."
  [neanderthal-m]
  (try (let [sol (linear-algebra/svd neanderthal-m true true)
             svd-left (:u sol)
             svd-right (:vt sol)
             singular-values (:sigma sol)]
         {::svd-left        svd-left
          ::svd-right       svd-right
          ::singular-values singular-values
          ::rank            (count (filter pos? (diagonal singular-values)))})
       (catch Exception _ {::anomalies/category ::anomalies/no-solve
                           ::anomalies/message  "No SVD Solution"
                           ::anomalies/fn       (var sv-decomposition)})))


