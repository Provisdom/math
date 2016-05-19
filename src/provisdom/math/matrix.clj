(ns provisdom.math.matrix
  (:require [provisdom.utility-belt.core :as co]
            [provisdom.math
             [core :as m]
             [arrays :as ar]]
            [clatrix.core :as clx]
            [clojure.core.matrix :as mxc]
            [clojure.core.matrix.protocols :as mp]
            [apache-commons-matrix.core :as acm]
            [taoensso.truss :refer [have have?]]
    ;[kublai.core :as ku]
            )
  (:import [clatrix.core Matrix]
           [org.apache.commons.math3.linear RealVector RealMatrix
                                            QRDecomposition LUDecomposition CholeskyDecomposition
                                            RectangularCholeskyDecomposition Array2DRowRealMatrix
                                            EigenDecomposition SingularValueDecomposition RRQRDecomposition
                                            DecompositionSolver ConjugateGradient SymmLQ
                                            PreconditionedIterativeLinearSolver RealLinearOperator]))

(set! *warn-on-reflection* true)

;;;;(defmethod print-method DoubleMatrix [mat ^java.io.Writer w] 
;;;;(print-method (cl mat) w))

;;;;1. move double-layered sequences and arrays code into this ns? 
;;;;Neither of these are handled by the core.
;;;;;2. during testing, test scalars, seqences, nested sequences, vectors, 
;;;;nested vectors, double-arrays, 2D double-arrays, clatrix, 
;;;;and maybe Apache (and combos where possible)

(comment "
(def KNOWN-IMPLEMENTATIONS
  (array-map
   :vectorz 'mikera.vectorz.matrix-api
   :ndarray 'clojure.core.matrix.impl.ndarray
   :ndarray-double 'clojure.core.matrix.impl.ndarray
   :ndarray-float 'clojure.core.matrix.impl.ndarray
   :ndarray-long 'clojure.core.matrix.impl.ndarray
   :persistent-vector 'clojure.core.matrix.impl.persistent-vector
   :persistent-map 'clojure.core.matrix.impl.sparse-map
   :sequence 'clojure.core.matrix.impl.sequence
   :double-array 'clojure.core.matrix.impl.double-array
   :scalar-wrapper 'clojure.core.matrix.impl.wrappers
   :slice-wrapper 'clojure.core.matrix.impl.wrappers
   :nd-wrapper 'clojure.core.matrix.impl.wrappers
   :jblas :TODO
   :clatrix 'clatrix.core
   :parallel-colt :TODO
   :ejml :TODO
   :ujmp :TODO
   :apache-commons 'apache-commons-matrix.core))")

;;;DECLARATIONS
(declare eigenvalues column-matrix transpose rrqr-decomposition diagonal
         get-slices-as-matrix esome)

;===========================================
; APACHE MATRIX
;===========================================
(extend-protocol mp/PComputeMatrix
  RealMatrix
  (compute-matrix [_ shape f]
    (Array2DRowRealMatrix.
      ^"[[D" (ar/jagged-2D-array :d (co/create-dbl-layered
                                      (first shape) (second shape) f)))))


;===========================================
; CLOJURE.CORE.MATRIX REDUNDANCY
;===========================================

(defn matrix?
  "Returns true if parameter is a valid matrix (dimensionality == 2)"
  [m] (mxc/matrix? m))

(defn mget
  "Gets a scalar value from an array at the specified position. Supports any number of dimensions."
  ([m] (mxc/mget m))
  ([m x] (mxc/mget m x))
  ([m x y] (mxc/mget m x y))
  ([m x y & more] (apply mxc/mget m x y more)))

(defn mset
  "Sets a scalar value in an array at the specified position, returning a new matrix and leaving the
  original unchanged."
  ([m v] (mxc/mset m v))
  ([m x v] (mxc/mset m x v))
  ([m x y v] (mxc/mset m x y v))
  ([m x y v & more] (apply mxc/mset m x y v more)))

(defn mset!
  "Sets a scalar value in an array at the specified position. Supports any number of dimensions. Will throw an
  exception if the matrix is not mutable at the specified position. Note that it is possible for some arrays to be
  mutable in places and immutable in others (e.g. sparse arrays) Returns the modified matrix (it is guaranteed to
  return the same instance)."
  ([m v] (mxc/mset! m v))
  ([m x v] (mxc/mset! m x v))
  ([m x y v] (mxc/mset! m x y v))
  ([m x y v & more] (apply mxc/mset! m x y v more)))

(defn row-matrix?
  "Returns true if a matrix is a row-matrix (i.e. is 2D and has exactly one row)"
  [m] (mxc/row-matrix? m))

(defn column-matrix?
  "Returns true if a matrix is a column-matrix (i.e. is 2D and has has exactly one column)"
  [m] (mxc/column-matrix? m))

(defn square?
  "Returns true if matrix is square (i.e. a 2D array with same number of rows and columns)"
  [m] (mxc/square? m))

(defn zero-matrix?
  "Returns true if all the elements of the parameter are zeros."
  [m] (mxc/zero-matrix? m))

(defn identity-matrix
  "Constructs a 2D identity matrix with the given number of rows. Identity matrices constructed with this function
  may not be fully mutable because they may be implemented with a specialised identity matrix type. Use
  (mutable (identity-matrix ...)) if you need to guarantee a mutable matrix."
  ([dims] (mxc/identity-matrix dims))
  ([implementation dims] (mxc/identity-matrix implementation dims)))

(defn row-count
  "Returns the number of rows in a matrix or vector (array must be 1D or more)"
  [m] (mxc/row-count m))

(defn column-count
  "Returns the number of columns in a matrix (array must be 2D or more)"
  [m] (mxc/column-count m))

(defn get-row
  "Gets a row of a matrix, as a vector. Will return a mutable view if supported by the implementation."
  [m idx] (mxc/get-row m idx))

(defn get-column
  "Gets a column of a matrix, as a vector. Will return a mutable view if supported by the implementation."
  [m idx] (mxc/get-column m idx))

(defn new-vector
  "Constructs a new vector with the given length. New matrix will contain default values as defined by the
  implementation (usually null or zero). If the implementation supports mutable vectors, then the new vector
  will be fully mutable."
  ([length] (mxc/new-vector length))
  ([implementation length] (mxc/new-vector implementation length)))

(defn new-matrix
  "Constructs a new 2D array (matrix) with the given dimensions. The new matrix will contain default values as defined
  by the implementation (usually null or zero). If the implementation supports mutable matrices, then the new matrix
  will be fully mutable."
  ([rows columns] (mxc/new-matrix rows columns))
  ([implementation rows columns] (mxc/new-matrix implementation rows columns)))

(defn compute-matrix
  "Creates a matrix with the specified shape, and each element specified by (f i j k...), where i, j, k... are the
  index positions of each element in the matrix"
  ([shape f] (mxc/compute-matrix shape f))
  ([implementation shape f] (mxc/compute-matrix implementation shape f)))

(defn zero-matrix
  "Constructs a new zero-filled numerical matrix with the given dimensions."
  ([rows columns] (mxc/zero-matrix rows columns))
  ([implementation rows columns]
   (mxc/zero-matrix implementation rows columns)))

(defn div
  "Performs element-wise matrix division for numerical arrays."
  ([a] (mxc/div a))
  ([a b] (mxc/div a b))
  ([a b & more] (apply mxc/div a b more)))

(defn add
  "Performs element-wise addition on one or more numerical arrays."
  ([a] (mxc/add a))
  ([a b] (mxc/add a b))
  ([a b & more] (apply mxc/add a b more)))

(defn sub
  "Performs element-wise subtraction on one or more numerical arrays."
  ([a] (mxc/sub a))
  ([a b] (mxc/sub a b))
  ([a b & more] (apply mxc/sub a b more)))

(defn mul
  "Performs element-wise multiplication with scalars and numerical arrays.  Behaves like clojure.core/* for scalar values."
  ([] (mxc/mul))
  ([a] (mxc/mul a))
  ([a b] (mxc/mul a b))
  ([a b & more] (apply mxc/mul a b more)))

(defn vec?
  "Returns true if the parameter is a vector (1-dimensional array)"
  [m] (mxc/vec? m))

(defn set-row
  "Sets a row in a matrix using a specified vector."
  [m i row] (mxc/set-row m i row))

(defn negate
  "Calculates the negation of a numerical array. Generally equivalent to (scale m -1.0)"
  [m] (mxc/negate m))

(defn emin
  "Gets the minimum element value from a numerical array"
  [m] (mxc/emin m))

(defn emax
  "Gets the maximum element value from a numerical array"
  [m] (mxc/emax m))

(defn ecount
  "Returns the total count of elements in an array. Equal to the product of the lengths of each dimension in the
  array's shape. Returns 1 for a zero-dimensional array or scalar."
  [m] (mxc/ecount m))

(defn eseq
  "Returns all elements of an array as a sequence in row-major order"
  [m] (mxc/eseq m))

(defn square
  "Squares every element of a numerical array."
  [m] (mxc/square m))

(defn to-vector
  "Creates a new array representing the elements of array `m` as a single flattened vector."
  [m] (mxc/to-vector m))

(defn dimensionality
  "Returns the dimensionality of an array. The dimensionality is equal to the number of dimensions in the array's shape."
  [m] (mxc/dimensionality m))

(defn trace
  "Calculates the trace of a 2D numerical matrix (sum of elements on main diagonal). The matrix need not be square."
  [m] (mxc/trace m))

(defn columns
  "Gets the columns of a matrix, as a sequence of 1D vectors.

   If the array has more than 2 dimensions, will return the columns from all slices in order."
  [m]
  (mxc/columns m))

;===========================================
; MATRIX SPECIAL TYPE HELP
;===========================================
;; TODO: Add tests for mempty?
(defn mempty?
  "Returns true if the matrix is an empty matrix."
  [m]
  {:pre [(have? matrix? m)]}
  (-> m (mget 0) empty?))

(defn diagonal?
  "Returns true if the matrix is a diagonal matrix (a matrix (usually a square matrix) in which the entries outside the
  main diagonal are all zero)."
  [m]
  (nil? (esome (fn [i j e]
                 {:pre [(have? number? e)]}
                 (not (or (= i j) (zero? e)))) m true)))

(defn symmetric?
  "Returns true if the matrix is a symmetric matrix."
  [m]
  (= (transpose m) m))

(defn unit-diagonal?
  "Returns true if the matrix has a unit diagonal."
  [m]
  (every? m/one? (diagonal m)))

(defn symmetric-with-unit-diagonal?
  "Returns true if the matrix has a unit diagonal and is symmetric."
  [m]
  (and (unit-diagonal? m) (symmetric? m)))

(defn positive?
  "Returns true if `m` is a positive definite matrix."
  ([m] (positive? m m/*dbl-close*))
  ([m accu] (and (symmetric? m) (every? #(> % accu) (eigenvalues m)))))

(defn positive-with-unit-diagonal?
  "Returns true if `m` has a unit diagonal and is a positive definite matrix."
  ([m] (and (unit-diagonal? m) (positive? m)))
  ([m accu] (and (unit-diagonal? m) (positive? m accu))))

(defn non-negative?
  "Returns true if `m` is a non-negative matrix."
  ([m] (non-negative? m m/*dbl-close*))
  ([m accu]
   (and (symmetric? m) (every? #(m/roughly-non-? % accu) (eigenvalues m)))))

(defn row-or-column-matrix?
  "Returns true if `m` is a row or a column matrix."
  [m]
  (or (column-matrix? m) (row-matrix? m)))

(defn size-symmetric
  "Returns the size of the matrix given `ecount`. `ecount` is the number of independent symmetric matrix elements (the
  number of elements on the diagonal plus the number either above or below the diagonal)."
  ^long [^long ecount]
  (let [s (-> ecount (* 8) inc m/sqrt dec (/ 2.0))]
    (when-not (m/roughly-round? s 1e-6)
      (throw (ex-info "Not a symmetric matrix." {:fn (var size-symmetric)})))
    (long s)))

(defn size-symmetric-with-unit-diagonal
  "Returns the size of the matrix given `ecount`. `ecount` is the number of elements above or below the unit diagonal."
  ^long [^long ecount]
  (-> ecount size-symmetric inc))

(defn ecount-symmetric
  "Returns the element count (Usually referred to as `ecount`) for a symmetric matrix. This is the number of elements on
  the diagonal plus the number of elements above or below the diagonal."
  ^long [^long size]
  (-> size m/sq (+ size) (/ 2)))

(defn ecount-symmetric-with-unit-diagonal
  "Returns the element count (Usually referred to as `ecount`) for a symmetric matrix with a unit diagonal. This is the
  number of elements above or below the diagonal."
  ^long [^long size]
  (-> size m/sq (- size) (/ 2)))

(defn to-vector-from-symmetric
  "Returns a vector that contains the upper (defualt) or lower half of the matrix. `m` doesn't have to be symmetric.
  Options:
    `:byrow?` (default: *true*) Set to false to get lower triangular values instead of upper."
  [m & {:keys [byrow?] :or {byrow? true}}]
  (let [nr (row-count m)
        nc (column-count m)]
    (vec (if byrow?
           (for [r (range nr)
                 c (range r nc)]
             (mget m r c))
           (for [c (range nc)
                 r (range c nr)]
             (mget m r c))))))

(defn to-vector-from-symmetric-with-unit-diagonal
  "Returns a vector that contains the upper (defualt) or lower half of the matrix without the diagonal. `m` doesn't have
   to be symmetric or have a unit diagonal.
   Options:
     `:byrow?` (default: *true*) Set to false to get lower triangular values instead of upper."
  [m & {:keys [byrow?] :or {byrow? true}}]
  (let [nr (row-count m), nc (column-count m)]
    (vec (if byrow?
           (for [r (range nr)
                 c (range (inc r) nc)]
             (mget m r c))
           (for [c (range nc)
                 r (range (inc c) nr)]
             (mget m r c))))))

;===========================================
; MATRIX IMPLEMENTATIONS
;===========================================
(defn apache-commons
  "Returns a matrix using the apache-commoms matrix implementation."
  [data]
  (mxc/matrix :apache-commons data))

;; NOTE: This function was previously written to see if the type of the matrix was = to Array2DRowRealMatrix.
;; That comparison is faster than the instance? check, however it is less general. Same applies for apache-commons-vec?.
(defn apache-commons?
  "Returns true if `m` is an apache commons matrix."
  [m]
  (instance? RealMatrix m))

(defn apache-commons-vec?
  "Returns true if `m` is an apache commons matrix."
  [m]
  (instance? RealVector m))

(defn clatrix
  "Returns a matrix using the Clatrix matrix implementation."
  [data]
  (cond (= data []) (clx/matrix [])
        (nil? data) nil
        :else (mxc/matrix :clatrix data)))

(defn clatrix?
  "Returns true if `m` is a Clatrix matrix."
  [m]
  (clx/matrix? m))

(defn clatrix-impl?
  "Returns true if `impl` is a Clatrix implementation. This can be either :clatrix or a matrix that is an instance of a
  Clatrix matrix."
  [impl]
  (or (= impl :clatrix) (clatrix? impl)))

(defn clatrix-vec?
  "Returns true if `m` is a Clatrix vector."
  [m]
  (clx/vec? m))

;; Not entirely sure why this exists. Clatxi may have problems handling row, column, and zero element matrices
(defn- maybe-convert-clatrix-row-or-column
  [m]
  (cond (not (clatrix? m)) m
        (row-matrix? m) [(to-vector m)]
        (column-matrix? m) (mxc/to-nested-vectors
                             (column-matrix :persistent-vector (to-vector m)))
        (zero? (ecount m)) []
        :else m))

(defn coerce
  "Coerces `param` into the provided implementation or matrix type"
  [matrix-or-implementation param]
  (mxc/coerce matrix-or-implementation
              (maybe-convert-clatrix-row-or-column param)))

(defn to-nested-vectors
  "Converts an array to an idiomatic, immutable nested Clojure vector format. The bottom level of the nested vectors
  will contain the element values. The depth of nesting will be equal to the dimensionality of the array."
  [m]
  (let [m (maybe-convert-clatrix-row-or-column m)]
    (cond (= (dimensionality m) 2) (if (clatrix? m)
                                     (into [] (map to-vector (mxc/rows m)))
                                     (into [] (map to-nested-vectors (mxc/rows m))))
          (>= (dimensionality m) 2) (into [] (map to-nested-vectors m))
          :else (mxc/to-nested-vectors m))))

;===========================================
; CONSTRUCTORS
;===========================================
(defn maybe-to-vector
  "Returns `x` as a vector (by calling [[to-vector]]) unless `x` is `nil`"
  [x]
  (when x (to-vector x)))

(defn create-vector
  "Returns a vector from `data`.
  Optional params:
    `implementation`: The core.matrix implementation to use to create the vector
    `size`: The size of the vector to create
    `f-or-val`: A function or a value used to create the result vector. If passed a function, the function will be called
    like `(f-or-val idx)` => value at the index. If passed a value then that value will be used to create the vector."
  ([data] (create-vector nil data))
  ([implementation data]
   (coerce implementation (if (number? data)
                            (to-vector data)
                            (to-nested-vectors data))))
  ([implementation size f-or-val]
   (coerce implementation (if (number? f-or-val)
                            (repeat size f-or-val)
                            (co/create-seq size f-or-val)))))

(defn sparse-vector
  "Returns a vector after applying `sparse` to `v-or-length`. `sparse` is a seq of seqs, each inner seq having the
  form `[index value]`. Later values will override prior overlapping values. `v-or-length` can be a starting vector or
  the length of the vector that will be created using [[new-vector]]. Optionally takes a core.matrix implementation."
  ([sparse v-or-length] (sparse-vector nil sparse v-or-length))
  ([implementation sparse v-or-length]
   (if (and (not (vec? v-or-length)) (= implementation :clatrix))
     (clx/from-sparse v-or-length 1 (map (fn [[i v]] [i 0 v]) sparse))
     (let [[length v] (if (vec? v-or-length)
                        [(count v-or-length) v-or-length]
                        [v-or-length (new-vector implementation v-or-length)])]
       (reduce (fn [m [i value]]
                 (when (or (>= i length) (neg? i))          ;TODO - replace with assert
                   (throw (ex-info "Sparse out of bounds." {:fn (var sparse-vector)})))
                 (mset m i value))
               v sparse)))))

(defn matrix
  "Constructs a new 2-dimensional matrix from the given numerical data.

   The data may be in one of the following forms:
   - A valid existing numerical array
   - Nested sequences of scalar values, e.g. Clojure vectors
   - A sequence of slices, each of which must be valid matrix data

   If implementation is not specified, uses the current matrix library as specified
   in *matrix-implementation*

   `matrix` works as a synonym for `array`
   NOTE: this function is similar to core.matrix's matrix function but it corrects Clatrix's behavior with nil
         and [] matrices."
  ([data] (matrix nil data))
  ([implementation data]
   (if (or (clatrix? implementation) (= implementation :clatrix))
     (clatrix data)
     (mxc/matrix implementation data))))

(defn constant-matrix
  "Returns a matrix where every element has the value `value`. `shape` is a vector `[n m]` which describes the shape of
  the returned matrix."
  ([shape ^double value] (constant-matrix nil shape value))
  ([implementation shape ^double value]
   (if (clatrix-impl? implementation)
     (let [[r c] shape] (clx/constant r c value))
     (compute-matrix implementation shape (fn [_ _] value)))))

(defn sequence-to-matrix
  "Returns a matrix that is created from `coll`. `coll` is a 1D vector which will be converted into the matrix determined
  by `rows` and `byrow?`. `rows` is the number of rows the returned matrix will have. `byrow?` is true if the elements
  are in row order, false if they are in column order."
  ([coll ^long rows byrow?] (sequence-to-matrix nil coll rows byrow?))
  ([implementation coll ^long rows byrow?]
   {:pre [(have? [:or seq? vec?] coll)]}
   (let [c (if-not (vec? coll) (vec coll) coll)
         e (ecount coll)
         columns (/ e rows)
         c (to-nested-vectors c)]
     (when-not (zero? (rem e rows))                         ;TODO - use assert
       (throw (ex-info "Collection wrong size to fill matrix."
                       {:fn (var sequence-to-matrix)})))
     (if byrow?
       (matrix implementation (partition columns c))
       (matrix implementation (co/flip-dbl-layered (partition rows c)))))))

(defn square-matrix
  "Returns a square matrix. If given a matrix `m` it will return a square version of the given matrix my truncating
  values from the matrix. Otherwise takes a `size`, `f-or-val`, and, optionally, an `implementation`. `size` is the size
  of the square matrix. `f-or-val` is a function or a value. If passed a function the function will be called with
  arguments i,j and should return the element at i,j. If passed a value that value will be placed at every index in the
  matrix."
  ([m]
   (let [size (min (column-count m) (row-count m))]
     (get-slices-as-matrix m :rows (range size) :columns (range size))))
  ([^long size f-or-val] (square-matrix nil size f-or-val))
  ([implementation ^long size f-or-val]
   {:pre [(have? [:or fn? number?] f-or-val)]}
   (cond (fn? f-or-val) (compute-matrix implementation [size size] f-or-val)
         (number? f-or-val) (constant-matrix implementation [size size]
                                             f-or-val))))

(defn column-matrix
  "Returns a column matrix created from `data` or `size` and `f-or-val`. `data` is a 1D vector where each element in the
  vector will be used as a column in the column matrix. `size` is the size of the returned matrix. `f-or-val` is either
  a function or value. If passed a function the function will be called with `i` and should return the element at `i`,
  where `i` is the row index in the matrix."
  ([data]
   (if (apache-commons-vec? data) (column-matrix :apache-commons data)
                                  (mxc/column-matrix data)))
  ([implementation data]
   (mxc/column-matrix implementation (if (apache-commons-vec? data)
                                       (to-nested-vectors data) data)))
  ([implementation ^long size f-or-val]
   {:pre [(have? [:or fn? number?] f-or-val)]}
   (cond (fn? f-or-val) (matrix implementation
                                (partition 1 (co/create-seq size f-or-val)))
         (number? f-or-val) (constant-matrix implementation [size 1] f-or-val))))

(defn row-matrix
  "Returns a row matrix created from `data` or `size` and `f-or-val`. `data` is a 1D vector which will be the row in the
  matrix. `size` is the size of the returned matrix. `f-or-val` is either a function or value. If passed a function the
  function will be passed no arguments and should return a value."
  ([data]
   (if (apache-commons-vec? data) (row-matrix :apache-commons data)
                                  (mxc/row-matrix data)))
  ([implementation data]
   (mxc/row-matrix implementation (if (apache-commons-vec? data)
                                    (to-nested-vectors data) data)))
  ([implementation ^long size f-or-val]
   {:pre [(have? [:or fn? number?] f-or-val)]}
   (cond (fn? f-or-val) (matrix implementation
                                [(co/create-seq size f-or-val)])
         (number? f-or-val) (constant-matrix implementation [1 size] f-or-val))))

(defn diagonal-matrix
  "Returns a diagonal matrix (a matrix with all elements not on the diagonal being 0) with the values on the diagonal
  given by `diagonal-values`, a 1D vector. `size` is the size of the matrix given by a single number. `f-or-val` is
  either a function or value. If given a function, the function will be called with `i` and should return the element
  at `i`, where `i` is the index of the diagonal element."
  ([diagonal-values]
   (if (apache-commons-vec? diagonal-values)
     (diagonal-matrix :apache-commons diagonal-values)
     (mxc/diagonal-matrix diagonal-values)))
  ([implementation diagonal-values]
   (cond (nil? implementation) (mxc/diagonal-matrix diagonal-values)
         (apache-commons-vec? diagonal-values) (mxc/diagonal-matrix
                                                 implementation
                                                 (to-nested-vectors
                                                   diagonal-values))
         :else (mxc/diagonal-matrix implementation diagonal-values)))
  ([implementation ^long size f-or-val]
   {:pre [(have? [:or fn? number?] f-or-val)]}
   (let [impl (or implementation :persistent-vector)]
     (cond (fn? f-or-val) (diagonal-matrix impl (co/create-seq size f-or-val))
           (number? f-or-val) (diagonal-matrix impl (repeat size f-or-val))))))

(defn triangular-matrix
  "Returns a triangular matrix created from `coll`. `coll` is a 1D vector where each element will be used to create the
  triangular matrix. `upper?` is set to true to create an upper triangular matrix, false for a lower triangular matrix.
  `diagonal` is a 1D vector of elements on the diagonal. `off-diagonal` is a 1D vector of upper or lower matrix elements."
  ([coll upper?] (triangular-matrix nil coll upper?))
  ([implementation coll upper?]
   (let [size (size-symmetric (count coll))
         val-fn (fn [r c]
                  (mget coll (+ c (* r size)
                                (* -0.5 (+ r (m/sq r))))))
         f (fn [r c]
             (if (> r c) 0.0 (val-fn r c)))
         f (if upper? f (fn [r c] (f c r)))]
     (compute-matrix implementation [size size] f)))
  ([implementation diagonal off-diagonal upper?]
   (let [size (size-symmetric-with-unit-diagonal (count off-diagonal))
         val-fn (fn [r c]
                  (mget off-diagonal
                        (+ c (* r size)
                           (* -0.5 (+ (inc r) (m/sq (inc r)))))))
         f (fn [r c] (cond (= r c) (nth diagonal r)
                           (> r c) 0.0
                           :else (val-fn r c)))
         f (if upper? f (fn [r c] (f c r)))]
     (compute-matrix implementation [size size] f))))

(defn symmetric-matrix
  "Returns a symmetric matrix created from `coll`. `coll` has the elements on the diagonal and the elements from the
  upper or lower half of the matrix. `byrow?` is true if `coll` is row major order, false if `coll` is col major order.
  You can pass `f` and `size` which will create a symmetric matrix of size `size` by calling `f` with r,c which should
  return the element at r,c. `f` is only called for each element on the diagonal and either the upper or lower half of
  the matrix, depending on `byrow?`."
  ([coll] (symmetric-matrix nil coll))
  ([implementation coll]
   (let [size (size-symmetric (count coll))
         val-fn (fn [r c]
                  (mget coll (+ c (* r size)
                                (* -0.5 (+ r (m/sq r))))))
         f (fn [r c] (if (<= r c) (val-fn r c) (val-fn c r)))]
     (compute-matrix implementation [size size] f)))
  ([f ^long size byrow?] (symmetric-matrix nil f size byrow?))
  ([implementation f ^long size byrow?]
   (compute-matrix
     implementation [size size]
     (fn [r c] (if (or (and (not byrow?) (>= r c)) (and byrow? (<= r c)))
                 (f r c) (f c r))))))

(defn symmetric-with-unit-diagonal-matrix
  "Returns a symmetric matrix created from `coll`. `coll` has the elements from the upper or lower half of the matrix.
  `byrow?` is true if `coll` is row major order, false if `coll` is col major order. You can pass `f` and `size` which
  will create a symmetric matrix of size `size` by calling `f` with r,c which should return the element at r,c. `f` is
  only called for each element on either the upper or lower half of the matrix, depending on `byrow?`."
  ([coll] (symmetric-with-unit-diagonal-matrix nil coll))
  ([implementation coll]
   (let [size (size-symmetric-with-unit-diagonal (count coll))
         val-fn (fn [r c]
                  (mget coll (+ c (* r size)
                                (* -0.5 (+ (inc r) (m/sq (inc r)))))))
         f (fn [r c] (cond (= r c) 1.0
                           (< r c) (val-fn r c)
                           :else (val-fn c r)))]
     (compute-matrix implementation [size size] f)))
  ([implementation ^long size f byrow?]
   (compute-matrix implementation [size size]
                   (fn [r c] (cond (= r c) 1.0
                                   (or (and (not byrow?) (> r c))
                                       (and byrow? (< r c))) (f r c)
                                   :else (f c r))))))

(defn symmetric-by-averaging-matrix
  "Returns a symmetric matrix where each element above or below the diagonal is equal to the average of the matrix `m`
  at r,c and c,r."
  [m]
  {:pre [(have? square? m)]}
  (symmetric-matrix
    m (let [size (row-count m)]
        (for [r (range size)
              c (range r size)]
          (if (== c r)
            (mget m r c)
            (* 0.5 (+ (mget m r c) (mget m c r))))))))

(defn toeplitz-matrix
  "Returns a toeplitz matrix (also called a diagonal-constant matrix) computed by `first-row` and `first-column` where
  `first-row` is the first row in the matrix and `first-column` is the first column in the matrix."
  ([first-row first-column] (toeplitz-matrix nil first-row first-column))
  ([implementation first-row first-column]
   {:pre [(have? (fn [[first-row first-column]]
                   (and (= (first first-row) (first first-column))
                        (= (count first-row) (count first-column)))) [first-row first-column])]}
   (let [size (count first-row)]
     (square-matrix implementation size
                    (fn [r c]
                      (if (<= r c)
                        (mget first-row (- c r))
                        (mget first-column (- r c))))))))

(def ^{:doc "See [[toeplitz-matrix]]"} diagonal-constant toeplitz-matrix)

(defn sparse-matrix
  "Returns a matrix created from a sparse representation. A sparse representation is a seq of seqs, each inner seq
  having the form `[row column value]`. Later values will override prior overlapping values.
  `m-or-shape` can be a starting matrix or a tuple with the number of rows and cols."
  ([sparse m-or-shape] (sparse-matrix nil sparse m-or-shape))
  ([implementation sparse m-or-shape]
   {:pre [(have? (or (matrix? m-or-shape) (and (sequential? m-or-shape) (= (count m-or-shape) 2))))]}
   (if (and (not (matrix? m-or-shape)) (= implementation :clatrix))
     (clx/from-sparse (first m-or-shape) (second m-or-shape) sparse)
     (let [[[rows columns] m] (if (matrix? m-or-shape)
                                [[(row-count m-or-shape) (column-count m-or-shape)]
                                 m-or-shape]
                                [m-or-shape
                                 (new-matrix implementation
                                             (first m-or-shape)
                                             (second m-or-shape))])]
       (reduce (fn [m [r c value]]
                 (let [r (have [:and #(< % rows) m/non-?] r :data "Sparse row idx out of bounds")
                       c (have [:and #(< % columns) m/non-?] c :data "Sparse col idx out of bounds")]
                   (mset m r c value)))
               m sparse)))))

(defn sparse-symmetric-matrix
  "Returns a symmetric matrix (unless `m` is not a symmetric matrix) created from a sparse representation. A sparse
  representation is a seq of seqs, each inner seq having the form `[row column value]`. Later values will override
  prior overlapping values.
  `m-or-shape` can be a starting matrix or a tuple with the number of rows and cols.
  Each off-diagonal inner sparse form is applied twice, with the row and column switched."
  ([sparse m-or-size] (sparse-symmetric-matrix nil sparse m-or-size))
  ([implementation sparse m-or-size]
   (let [[size m] (if (matrix? m-or-size)
                    [(row-count m-or-size) m-or-size]
                    [m-or-size (new-matrix implementation m-or-size
                                           m-or-size)])]
     (reduce (fn [m [r c value]]
               (let [r (have [:and #(< % size) m/non-?] r :data "Sparse row idx out of bounds")
                     c (have [:and #(< % size) m/non-?] c :data "Sparse col idx out of bounds")]
                 (mset (mset m r c value) c r value)))
             m sparse))))

;===========================================
; MATRIX GET
;===========================================
;;(mxc/rows m)) ;mxc/rows outputs 'slice-wrappers' instead of rows
(defn rows
  "Returns a vector of the rows of the matrix, essentially returns the "
  [m]
  {:pre [(have? matrix? m)]}
  (to-nested-vectors m))

(defn columns
  "Returns a vector of vectors containing the columns in matrix `m`."
  [m]
  {:pre [(have? matrix? m)]}
  (mxc/columns m))

(defn get-row-as-matrix
  "Returns row `i` in matrix `m` as a row matrix."
  [m ^long i]
  (row-matrix m (get-row m i)))

(defn get-column-as-matrix
  "Returns column `i` in matrix `m` as a column matrix."
  [m ^long i]
  (column-matrix m (get-column m i)))

(defn diagonal
  "Returns the specified diagonal of a 2D matrix as a vector.
   If `k`>0, returns a diagonal above the main diagonal.
   If `k`<0, returns a diagonal below the main diagonal.
   works on both square and rectangular matrices.
   Returns `nil` if value of `k` is out of range (outside matrix)"
  ([m]
   {:pre [(have? matrix? m)]}
    ;; It is faster to get the diagonal using mget, however, it makes more sense to call the protocol for the
    ;; matrix to get the diagonal because the matrix impl may have special considerations.
   (mxc/main-diagonal m))
  ([m k]
   {:pre [(have? matrix? m)]}
   (let [r (if (neg? k) (- k) 0)
         c (if (pos? k) k 0)
         nc (- (column-count m) c)
         nr (- (row-count m) r)
         n (min nc nr)]
     (when (pos? n)
       (vec (for [i (range n)] (mget m (+ i r) (+ i c))))))))

(defn get-slices-as-matrix
  "Performs a slice on the matrix given by the options.
  Options:
    `:rows` returns all rows by default, can pass a row index or sequence of row indices
    `:columns` returns all columns by default, can pass a column index or sequence of column indices
    `:except-rows` can pass a row index or sequence of row indices to exclude
    `:except-columns` can pass a column index or sequence of column indices to exclude"
  [m & {:keys [rows columns except-rows except-columns]}]
  (let [calc-fn (fn [i except-i n]
                  (cond (and (not i) (not except-i)) true
                        (not except-i) i
                        (number? i) (if (number? except-i)
                                      (if (= except-i i) [] i)
                                      (if (contains? (set except-i) i) [] i))
                        :else (let [i (if i i (range n))]   ; i is a seq of idxs
                                (if (number? except-i)
                                  (remove #(= except-i %) i)
                                  (reduce
                                    (fn [tot e]
                                      (if (first (filter #(= % e) except-i))
                                        tot
                                        (conj tot e)))
                                    [] i)))))
        rows (calc-fn rows except-rows (row-count m))
        columns (calc-fn columns except-columns (column-count m))]
    (cond
      (or (and (coll? rows) (empty? rows))
          (and (coll? columns) (empty? columns))) (matrix m [[]])
      (and (number? rows) (number? columns)) (matrix m [[(mget m rows columns)]])
      (and (number? rows) (coll? columns)) (row-matrix
                                             m (let [r (get-row m rows)]
                                                 (map #(mget r %) columns)))
      (and (number? rows) (true? columns)) (get-row-as-matrix m rows)
      (and (coll? rows) (number? columns)) (column-matrix
                                             m (let [c (get-column m columns)]
                                                 (map #(nth c %) rows)))
      (and (coll? rows) (coll? columns)) (matrix
                                           m (map
                                               (fn [e]
                                                 (nth (co/flip-dbl-layered
                                                        (map #(get-column m %) columns))
                                                      e))
                                               rows))
      (and (coll? rows) (true? columns)) (matrix m (map #(get-row m %) rows))
      (and (true? rows) (number? columns)) (get-column-as-matrix m columns)
      (and (true? rows) (coll? columns)) (matrix m (co/flip-dbl-layered
                                                     (map #(get-column m %)
                                                          columns)))
      (and (true? rows) (true? columns)) m)))

(defn matrix-partition
  "Returns a map containing the four sub-matrices labeled `:top-left`, `:bottom-left`, `:top-right`, and
  `:bottom-right`. `first-bottom-row` is the bottom of where the slice will occur. `first-right-column` is
  the right edge of where the slice will occur."
  [m ^long first-bottom-row ^long first-right-column]
  {:pre [(have? m/non-? first-bottom-row first-right-column)
         (have? (fn [[m first-bottom-row]] (<= first-bottom-row (row-count m))) [m first-bottom-row])
         (have? (fn [[m first-right-column]] (<= first-right-column (column-count m))) [m first-right-column])]}
  {:top-left     (get-slices-as-matrix m
                                       :rows (range first-bottom-row)
                                       :columns (range first-right-column))
   :bottom-left  (get-slices-as-matrix m
                                       :except-rows (range first-bottom-row)
                                       :columns (range first-right-column))
   :top-right    (get-slices-as-matrix m
                                       :rows (range first-bottom-row)
                                       :except-columns (range first-right-column))
   :bottom-right (get-slices-as-matrix m
                                       :except-rows (range first-bottom-row)
                                       :except-columns (range first-right-column))})

;===========================================
; MATRIX MANIPULATION
;===========================================
(defn emap
  "Can also map onto a number, multiple numbers, or a single fn"
  ([f m]
   (cond (clatrix? m) (coerce :clatrix (mxc/emap f (to-nested-vectors m))),
         (fn? m) (f m), :else (mxc/emap f m)))
  ([f m a]
   {:pre [(have? (fn [[m a]] (= (row-count m) (row-count a))) [m a])
          (have? (fn [[m a]] (or (and (vec? m) (vec? a))
                                 (= (column-count m) (column-count a)))) [m a])]}
   (if (clatrix? m)
     (coerce :clatrix (mxc/emap f (to-nested-vectors m)
                                (to-nested-vectors a)))
     (mxc/emap f m a)))
  ([f m a & more]
   {:pre [(have? (fn [[m a more]]
                   (let [v (apply vector m a more)]
                     (and (every? #(= (row-count m) (row-count %)) v)
                          (or (every? vec? v) (every? #(= (column-count m)
                                                          (column-count %)) v)))))
                 [m a more])]}
   (if (clatrix? m)
     (coerce :clatrix (apply mxc/emap f (map to-nested-vectors
                                             (apply vector m a more))))
     (apply mxc/emap f m a more))))

;;clatrix row/column matrices don't keep the type
(defn transpose
  "Transposes a matrix, returning a new matrix. 
For 2D matrices, rows and columns are swapped.
More generally, the dimension indices are reversed for any shape of array. 
Note that 1D vectors and scalars will be returned unchanged."
  ;;If ordering is provided, will re-order dimensions according to the provided
  ;;   order. -- ORDERING not yet implemented in mxc
  ([m] (coerce m (mxc/transpose m))))
;([m ordering] (coerce m (mxc/transpose m ordering))))

(defn conj-rows [& ms]
  (let [cl (map #(if (vec? %) (row-matrix :clatrix %) (clatrix %)) ms)
        cc (map column-count cl)
        mc (apply max cc)]
    (when-not (every? #(= mc %) cc)                         ; TODO - use truss or assert
      (throw (ex-info "Column counts must be the same." {:fn (var conj-rows)})))
    (coerce (first ms) (apply clx/vstack cl))))

(defn conj-columns [& ms]
  (let [cl (map #(if (vec? %) (column-matrix :clatrix %) (clatrix %)) ms),
        rc (map row-count cl), mc (apply max rc)]
    (when-not (every? #(= mc %) rc)                         ; TODO - use truss or assert
      (throw (ex-info "row counts must be the same" {:fn (var conj-columns)})))
    (coerce (first ms) (apply clx/hstack cl))))

(defn merge-matrices
  "Returns a Matrix created by binding four matrices together.  
All four must be matrices, not vectors."
  [top-left bottom-left top-right bottom-right]
  {:pre [(have? matrix? top-left bottom-left top-right bottom-right)]}
  (conj-rows (conj-columns top-left top-right)
             (conj-columns (coerce top-left bottom-left) bottom-right)))

(defn conj-symmetrically [m1 m2]
  {:pre [(have? (fn [[m1 m2]]
                  (let [nr1 (row-count m1), nr2 (row-count m2),
                        nc2 (if (vec? m2) 1 (column-count m2))]
                    (and (square? m1) (or (= nr2 (+ nc2 nr1)) (= nc2 (+ nr2 nr1))))))
                [m1 m2])]}
  (let [nr1 (row-count m1)
        m2 (if (vec? m2) (row-matrix m2) m2)
        c? (> (row-count m2) (column-count m2))
        k (if c? :rows :columns)
        k2 (if c? :except-rows :except-columns)
        m (get-slices-as-matrix m2 k (range nr1))
        mt (transpose m)
        br (get-slices-as-matrix m2 k2 (range nr1))
        bl (if c? mt m)
        tr (if c? m mt)]
    (merge-matrices m1 bl tr br)))

(defn conj-diagonally
  "x can be a number, vector or matrix"
  [m x]
  {:pre [(have? [:or number? vec? matrix?] x)]}
  (cond (number? x) (let [v (conj (vec (repeat (row-count m) 0.0)) x)]
                      (conj-symmetrically m v))
        (vec? x) (let [d (diagonal-matrix x),
                       z (zero-matrix (row-count m) (ecount x)),
                       m2 (conj-columns z d)] (conj-symmetrically m m2))
        (matrix? x) (merge-matrices
                      m (zero-matrix (row-count x) (column-count m))
                      (zero-matrix (row-count m) (column-count x)) x)))

(defn replace-submatrix
  "Returns a Matrix after substituting a 'sub' matrix at top-left 
   location 'row' and 'column'.  
Sub must be a matrix, not a vector.  
row and column can be negative.
Unassigned elements will be 0.0"
  [m sub ^long row ^long column]
  {:pre [(have? matrix? m sub)]}
  (let [sr (row-count sub), sc (column-count sub), tr (+ sr row),
        tc (+ sc column), nr (row-count m), nc (column-count m)]
    (matrix m (for [r (range (min row 0) (max tr nr))]
                (for [c (range (min column 0) (max tc nc))]
                  (cond (and (>= r row) (< r tr) (>= c column)
                             (< c tc)) (mget sub (- r row) (- c column))
                        (and (m/non-? r) (< r nr) (m/non-? c)
                             (< c nr)) (mget m r c)
                        :else 0.0))))))

;;this is in clojure.core.matrix but I can't figure out how that works
(defn permutation-matrix
  "Returns a Matrix with the rows and the columns of a matrix permuted.
    Options:
     :rows is the rowspec providing a seq listing the indices of the 
            permutation.
     :cols is the colspec providing a seq listing the indices of the 
            permutation."
  [m & {:keys [rows columns]}]
  {:pre [(have? matrix? m)]}
  (let [clx (clatrix m), p (clx/permute clx :r rows :c columns)]
    (coerce m p)))

(comment "MATRIX MATH")
(defn mmul
  ([] (mxc/mmul))
  ([a] (mxc/mmul a))
  ;;lazy seq's can be a problem otherwise
  ([a b]
   (coerce a (mxc/mmul (if (and (sequential? a) (not (clatrix? a)))
                         (vec a) a)
                       b)))
  ([a b & more] (apply mmul (mmul a b) more)))

(defn esum
  "Returns the sum of the elements."
  ([m] (mxc/esum m))
  ([f m]
   (if (number? m) (f m)
                   (reduce #(+ % (f %2)) 0 (flatten (to-nested-vectors m))))))

(defn eaverage
  "Returns the average of the elements"
  [m] (/ (esum m) (ecount m)))

(defn esum-squares [m]
  (if (number? m) (m/sq m)
                  (reduce #(+ % (m/sq %2)) 0 (flatten (to-nested-vectors m)))))

(defn eproduct
  ([m] (reduce * 1 (flatten (to-nested-vectors m))))
  ([f m]
   (if (number? m) (f m)
                   (reduce #(* % (f %2)) 1 (flatten (to-nested-vectors m))))))

(defn norm
  "This is the standard norm2"
  ^double [m]
  (if (or (clatrix? m) (clatrix-vec? m)) (clx/norm m)
                                         (m/sqrt (esum-squares (flatten (to-nested-vectors m))))))

(defn norm1 ^double [m] (esum (map m/abs (flatten (to-nested-vectors m)))))

(defn normp ^double [m ^double p]
  {:pre [(have? #(>= % 1.0) p)]}
  (m/pow (esum (map #(m/pow (m/abs %) p) (flatten (to-nested-vectors m))))
         (/ p)))

(defn normalise
  "Returns as length one in norm2."
  ;;mxc/normalise only works for matrices, 
  ;;w/ Clatrix it works like normalise! instead
  [m] (coerce m (let [s (norm m)] (emap #(/ % s) m))))

(defn normalise1
  "Returns as length one in norm1."
  [m]
  (coerce m (let [s (norm1 m),
                  ser (emap #(/ % s) m),
                  diff (m/one- (esum ser))]
              (if (zero? diff)
                ser
                (assoc (vec ser) 0 (+ diff (first ser)))))))

(defn normalisep
  "Returns as length one in normp."
  [m ^double p] (coerce m (let [s (normp m p)] (emap #(/ % s) m))))

(defn inner-product
  "Computes the inner product of numerical arrays.
For matrix/matrix and matrix/vector arguments, this is equivalent to matrix 
   multiplication.
The inner product of two arrays with indexed dimensions {..i j} and {j k..} 
   has dimensions {..i k..}. 
The inner-product of two vectors will be scalar."
  ([a] (mxc/inner-product a))
  ([a b]
   (let [i (mxc/inner-product (to-nested-vectors a) (to-nested-vectors b))]
     (if (number? i) i (coerce a i))))
  ([a b & more]
   (let [i (apply mxc/inner-product (map to-nested-vectors
                                         (apply vector a b more)))]
     (if (number? i) i (coerce a i)))))

(defn dot-product
  "Same as inner-product."
  ([a] (inner-product a))
  ([a b] (inner-product a b))
  ([a b & more] (apply inner-product a b more)))

(defn kronecker-product [m & ms]
  {:pre [(have? matrix? m) (have? (partial every? matrix?) ms)]}
  (coerce m (reduce (fn [a b]
                      (let [arows (row-count a), acols (column-count a)]
                        (apply conj-rows (for [i (range arows)]
                                           (apply conj-columns
                                                  (for [j (range acols)]
                                                    (mul (mget a i j) b)))))))
                    m ms)))

(defn matrix-pow
  "Returns the xth matrix power of the square matrix.  
m using Eigendecomposition. 
x need not be an integer."
  [m x]
  {:pre [(have? square? m)]}
  (clx/pow (clx/maybe-symmetric (clatrix m)) x))

(defn det
  "Calculates the determinant of a 2D square numerical matrix."
  [m]
  (let [m (if (or (clatrix? m) (apache-commons? m)) m (apache-commons m))]
    (mxc/det m)))

(comment "VECTOR MATH")
(defn outer-product
  ([v] (if (number? v) (m/sq v) (outer-product nil v)))
  ([implementation v]
   {:pre [(have? vec? v)]}
   (matrix implementation
           (let [s (ecount v)]
             (for [r (range s)]
               (for [c (range s)] (* (mget v r) (mget v c)))))))
  ([implementation f v]
   {:pre [(have? vec? v)]}
   (matrix implementation
           (let [s (ecount v)]
             (for [r (range s)]
               (for [c (range s)] (f (* (mget v r) (mget v c))))))))
  ([implementation f v & ms]
   {:pre [(have? vec? v)]}
   (matrix implementation
           (let [s (ecount v)]
             (for [r (range s)]
               (for [c (range s)]
                 (apply f (* (mget v r) (mget v c))
                        (map #(mget % r c) ms))))))))

(defn cross-product [v1 v2]
  {:pre [(have? vec? v1 v2)]}
  (let [f1 (mget v1 0), f2 (mget v2 0), s1 (mget v1 1), s2 (mget v2 1),
        t (- (* f1 s2) (* f2 s1))]
    (cond
      (= (ecount v1) (ecount v2) 3) (let [t1 (mget v1 2), t2 (mget v2 2)]
                                      [(- (* s1 t2) (* s2 t1))
                                       (- (* t1 f2) (* t2 f1)) t])
      (= (ecount v1) (ecount v2) 2) t
      :else (throw (ex-info "Vectors must be of equal length of 2 or 3" ; TODO - use truss or assert
                            (var cross-product))))))

(defn projection
  "Returns vector of v1 projected onto v2."
  [v1 v2]
  {:pre [(have? vec? v1 v2)]}
  (coerce v1 (let [s (/ (inner-product v1 v2) (esum-squares v2))]
               (emap #(* s %) v2))))

(defn cumulative-sum
  "Returns vector with cumulative sum."
  [v]
  {:pre [(have? vec? v)]}
  (let [vm (coerce [] v)] (coerce v (rest (reductions + 0 vm)))))

(defn differences
  "Returns vector with differences."
  ([v init]
   {:pre [(have? vec? v)]}
   (let [vm (coerce [] v)] (coerce v (sub vm (cons init (pop vm))))))
  ([v init last]
   {:pre [(have? vec? v)]}
   (let [vm (coerce [] v)] (coerce v (sub (conj vm last) (cons init vm))))))

(comment "REDUCE MATRIX")
(defn ereduce-kv
  "Function f takes a result, two indexes, and element(s)."
  ([f init m byrow?]
   {:pre [(have? matrix? m)]}
   (let [mt (if byrow? m (transpose m)), nr (row-count mt)]
     (loop [c 0, val init, s (rows mt)]
       (let [g (if byrow? #(f % c %2 %3) #(f % %2 c %3))]
         (if (>= c nr)
           val
           (recur (inc c) (reduce-kv g val (first s)) (rest s)))))))
  ([f init m1 m2 byrow?]
   {:pre [(have? matrix? m1 m2)]}
   (let [mt1 (if byrow? m1 (transpose m1)),
         mt2 (if byrow? m2 (transpose m2)),
         l (min (row-count mt1) (row-count mt2))]
     (loop [c 0, val init, s1 (rows mt1), s2 (rows mt2)]
       (let [g (if byrow? #(f % c %2 %3 %4) #(f % %2 c %3 %4))]
         (if (>= c l)
           val
           (recur (inc c) (co/reduce-kv-ext g val (first s1) (first s2))
                  (rest s1) (rest s2)))))))
  ([f init m1 m2 m3 byrow?]
   {:pre [(have? matrix? m1 m2 m3)]}
   (let [mt1 (if byrow? m1 (transpose m1))
         mt2 (if byrow? m2 (transpose m2))
         mt3 (if byrow? m3 (transpose m3))
         l (min (row-count mt1) (row-count mt2) (row-count mt3))]
     (loop [c 0, val init, s1 (rows mt1), s2 (rows mt2), s3 (rows mt3)]
       (let [g (if byrow? #(f % c %2 %3 %4 %5) #(f % %2 c %3 %4 %5))]
         (if (>= c l)
           val
           (recur (inc c) (co/reduce-kv-ext
                            g val (first s1) (first s2) (first s3)) (rest s1)
                  (rest s2) (rest s3))))))))

(defn every-kv?
  "Returns true if (pred index e) is logical true for every element in coll, 
   else false."
  [pred coll]
  (loop [c 0, s coll]
    (cond (nil? (seq s)) true
          (pred c (first s)) (recur (inc c) (rest s))
          :else false)))

(defn eevery?
  "Returns true if (pred row col e) is logical true for every element in m, 
   else false."
  [pred m]
  {:pre [(have? matrix? m)]}
  (let [nr (row-count m)]
    (loop [c 0, s (rows m)]
      (cond (>= c nr) true
            (every-kv? #(pred c % %2) (first s)) (recur (inc c) (rest s))
            :else false))))

(defn some-kv
  "Returns the first logical true value of (pred index x) for any x in coll, else nil."
  [pred coll]
  (loop [idx 0
         s coll]
    (when (seq s)
      (or (pred idx (first s)) (recur (inc idx) (next s))))))

(defn esome
  "Returns the first logical true value of (pred row col e) for any e in matrix, else nil."
  [pred m byrow?]
  {:pre [(have? matrix? m)]}
  (let [mt (if byrow? m (transpose m))
        num-rows (row-count mt)]
    (loop [c 0
           s (rows mt)]
      (if (>= c num-rows)
        nil
        (or (some-kv #(pred c % %2) (first s)) (recur (inc c) (next s)))))))


(comment "FILTER MATRICES")
(defn filter-kv
  "Returns a vector of the items in coll for which (pred item) returns true. pred must be free of side-effects."
  [pred coll]
  (persistent! (reduce-kv #(if (pred %2 %3) (conj! % %3) %) (transient [])
                          (vec coll))))

(defn efilter
  "Returns a sequence of filtered values.  pred takes an element"
  [m pred & {:keys [byrow?] :or {byrow? true}}]
  {:pre [(have? matrix? m)]}
  (let [mt (if byrow? m (transpose m))] (filter pred (eseq mt))))

(defn efilter-kv
  "Returns a sequence of filtered values. pred takes two indexes and an element"
  [m pred & {:keys [byrow?] :or {byrow? true}}]
  {:pre [(have? matrix? m)]}
  (ereduce-kv #(if (pred %2 %3 %4) (conj % %4) %) [] m byrow?))

(defn sparse-efilter
  "Returns a vector of [row column value]. pred takes an element"
  [m pred & {:keys [byrow?] :or {byrow? true}}]
  {:pre [(have? matrix? m)]}
  (ereduce-kv #(if (pred %4) (conj % [%2 %3 %4]) %) [] m byrow?))

(defn sparse-symmetric-efilter
  "Returns a vector of [row column value].  
pred takes an element and will be evaluated only for upper-right or lower-left 
   triangle of m."
  [m pred & {:keys [byrow?] :or {byrow? true}}]
  {:pre [(have? matrix? m)]}
  (let [f (if byrow? <= >=)]
    (ereduce-kv #(if (and (f %2 %3) (pred %4)) (conj % [%2 %3 %4]) %)
                [] m byrow?)))

(defn filter-by-row
  "Returns a matrix.  pred takes a row"
  [m pred]
  {:pre [(have? matrix? m)]}
  (matrix m (filter pred (rows m))))

(defn sparse-filter-by-row
  "Returns a vector of [row row-value].  pred takes a row"
  [m pred]
  {:pre [(have? matrix? m)]}
  (reduce-kv #(if (pred %3) (conj % [%2 %3]) %) [] (rows m)))

(defn filter-by-column
  "Returns a matrix. pred takes a column"
  [m pred]
  {:pre [(have? matrix? m)]}
  (matrix m (transpose (filter pred (columns m)))))

(defn sparse-filter-by-column
  "Returns a vector of [column column-value]. pred takes a column"
  [m pred]
  {:pre [(have? matrix? m)]}
  (reduce-kv #(if (pred %3) (conj % [%2 %3]) %) [] (columns m)))

(defn filter-symmetrically
  "Returns a matrix.  pred takes a row or column"
  [m pred & {:keys [byrow?] :or {byrow? true}}]
  {:pre [(have? matrix? m)]}
  (let [ma (if byrow? (rows m) (columns m)),
        keep-set (reduce-kv #(if (pred %3) (conj % %2) %) #{} ma)]
    (get-slices-as-matrix m :rows keep-set, :columns keep-set)))

(comment "MATRIX IMMUTABLE CHANGES")
(defn set-column
  [m ^long i column]
  {:pre [(have? matrix? m) (have? vec? column)]}
  (transpose (set-row (transpose m) i column)))

(defn insert-row
  [m ^long i row]
  {:pre [(have? matrix? m) (have? vec? row)]}
  (sequence-to-matrix m (co/insertv (rows m) (to-vector row) i)
                      (inc (row-count m)) true))

(defn insert-column
  [m ^long i column]
  {:pre [(have? matrix? m) (have? vec? column)]}
  (sequence-to-matrix m (co/insertv (columns m) (to-nested-vectors column) i)
                      (row-count m) false))

(defn insert-symmetrically
  [m ^long i v]
  {:pre [(have? matrix? m) (have? vec? v)]}
  (set-row (insert-column (insert-row m i (repeat (column-count m) 0.0)) i v)
           i v))

(comment "MATRIX NUMERICAL STABILITY")
(defn roughly? [m1 m2 accu]
  (cond (and (matrix? m1)
             (matrix? m2)) (every? identity
                                   (map #(roughly? %1 %2 accu)
                                        (to-nested-vectors m1)
                                        (to-nested-vectors m2)))
        (and (vector? m1)
             (vector? m2)) (every? identity
                                   (map #(m/roughly? %1 %2 accu) m1 m2))
        (and (number? m1) (number? m2)) (m/roughly? m1 m2 accu)
        :else false))

(defn roughly-distinct
  "Returns a matrix with later duplicate rows removed, 
   or a vector with later duplicate elements removed"
  [m ^double accu]
  (loop [[h & t] m, seen []]
    (cond (not h) seen
          (some #(roughly? h % accu) seen) (recur t seen)
          :else (recur t (conj seen h)))))

(defn- roughly-zero-row-fn [^double accu]
  #(every? (fn [e] (m/roughly? e 0.0 accu)) %))

(defn round-roughly-zero-rows
  "Returns a matrix after rounding any roughly-zero rows"
  [m accu]
  {:pre [(have? matrix? m)]}
  (matrix m (map #(if ((roughly-zero-row-fn accu) %)
                   (repeat (column-count m) 0.0) %)
                 (rows m))))

(defn round-roughly-zero-columns
  "Returns a matrix after rounding any roughly-zero columns"
  [m accu]
  {:pre [(have? matrix? m)]}
  (matrix m (co/flip-dbl-layered
              (map #(if ((roughly-zero-row-fn accu) %)
                     (repeat (row-count m) 0.0) %)
                   (columns m)))))

(defn round-roughly-zero-rows-and-columns
  "Returns a matrix after rounding any roughly-zero rows and columns"
  [m accu] (round-roughly-zero-columns (round-roughly-zero-rows m accu) accu))

;(defn force-symmetric-matrix-to-be-non-negative
;  "Attempts to return a non-negative matrix by reducing the absolute values 
;      of the off-diagaonal elements as necessary"
;  [m]
;  (symmetric-matrix 
;    m (fn [r c] 
;        (let [e (mget m r c)] 
;          (if (= r c) e 
;            (* (m/sgn e) (min (m/sqrt (* (mget m r r) (mget m c c))) 
;                              (m/abs e)))))) 
;    (row-count m) true))

(comment "MATRIX DECOMPOSITION")
(defn inverse
  "Computes the inverse of a number, vector, or matrix. 
   For Clatrix: 
      this is done via Gaussian elmination. 
      It can be numerically very unstable if the matrix is nearly singular.  
      Positivity and symmetry hints are used to cause `solve` to use optimized 
         LAPACK routines."
  [m]
  {:pre [(have? [:or number? square? vec?] m)]}
  (cond (number? m) (/ m)
        (vec? m) (create-vector m (emap / m))
        (apache-commons? m) (mxc/inverse m)
        :else (coerce m (clx/i (clx/maybe-positive
                                 (clx/maybe-symmetric (clatrix m)))))))

(defn cholesky-decomposition
  "Computes the Cholesky decomposition of a matrix. 
   Returns a vector containing two matrices [L U].
   Intended usage: (let [[L U] (cholesky-decomosition M)] ....).
   This is the Cholesky square root of a matrix, L such that (mmul L U) = m  
   Note that m must be positive (semi) definite for this to exist, 
      but `cholesky-decomposition` requires strict positivity."
  [m]
  {:pre [(have? square? m)]}
  (cond (apache-commons? m) (let [r (CholeskyDecomposition. m)]
                              [(.getL r) (.getLT r)])
        :else (let [u (clx/cholesky (clatrix m))]
                [(coerce m (clx/t u)) (coerce m u)])))

(defn lower-cholesky [m] (first (cholesky-decomposition m)))

(defn upper-cholesky [m] (second (cholesky-decomposition m)))

(defn cholesky-rectangular
  "Calculates the rectangular Cholesky decomposition of a matrix.
The rectangular Cholesky decomposition of a real symmetric positive 
   semidefinite matrix m consists of a rectangular matrix B with the same 
   number of rows such that: 
      m is almost equal to BB*, depending on a user-defined tolerance. 
In a sense, this is the square root of m.
The difference with respect to the regular CholeskyDecomposition is that 
   rows/columns may be permuted (hence the rectangular shape instead of the 
   traditional triangular shape) and there is a threshold to ignore small
   diagonal elements. 
This is used for example to generate correlated random n-dimensions vectors 
   in a p-dimension subspace (p < n). 
In other words, it allows generating random vectors from a covariance matrix 
   that is only positive semidefinite, and not positive definite.
accu - Diagonal elements threshold under which columns are considered to be 
       dependent on previous ones and are discarded.
Returns a map containing:
      :B -- rectangular root matrix
      :rank -- rank is the number of independent rows of original matrix, 
               and the number of columns of root matrix B"
  [m ^double accu]
  {:pre [(have? square? m)]}
  (let [r (RectangularCholeskyDecomposition. (apache-commons m) accu)]
    {:B (coerce m (.getRootMatrix r)), :rank (.getRank r)}))

(defn cholesky-decomposition-semi-definite
  "Returns a vector containing two matrices [L L*], 
      where 'm' may have zero (or close to zero) rows"
  [m ^double accu]
  (if (positive? m) (cholesky-decomposition m)
                    (let [c (row-count m), {b :B, r :rank} (cholesky-rectangular m accu),
                          s (- c r), nm (if (zero? s) b (conj-columns b (zero-matrix c s)))]
                      [nm (transpose nm)])))

(defn sv-decomposition-with-rank
  "Calculates the compact Singular Value Decomposition of a matrix.
The Singular Value Decomposition of matrix A is a set of three 
   matrices: U, S and V such that A = U × S × VT. 
Let A be a m × n matrix, then U is a m × p orthogonal matrix, 
   S is a p × p diagonal matrix with positive or null elements, 
V is a p × n orthogonal matrix (hence VT is also orthogonal) where p=min(m,n).
Returns a map containing:
      :S -- diagonal matrix S
      :V -- matrix V
      :U -- matrix U
      :VT -- transpose of matrix V 
      :UT -- transpose of matrix U
      :rank -- rank"
  [m]
  {:pre [(have? matrix? m)]}
  (cond (clatrix? m) (let [r (clx/svd m)]
                       {:S    (diagonal-matrix m (:values r)),
                        :V    (transpose (:right r)), :U (:left r),
                        :VT   (:right r), :UT (transpose (:left r)),
                        :rank (:rank r)})
        :else (let [d (SingularValueDecomposition. (apache-commons m))]
                {:S  (coerce m (.getS d)), :V (coerce m (.getV d)),
                 :U  (coerce m (.getU d)), :VT (coerce m (.getVT d)),
                 :UT (coerce m (.getUT d)), :rank (.getRank d)})))

(defn sv-decomposition
  "Computes the Singular Value decomposition of a matrix. 
   Intended usage: (let [[U S V*] (sv-decomosition M)] ....)
   Returns a vector containing three matrices [U S V*] such 
      that U (diag S) V = m with: 
      U -- the left singular vectors U
      S -- the diagonal matrix of singular values S 
           (the diagonal in vector form)
      V -- the right singular vectors V
   If 'm' is n x p and the rank is k, then 'U' is n x k, 'S' is k x k, 
       and 'V*' is k x p"
  [m]
  {:pre [(have? matrix? m)]}
  (cond (apache-commons? m) (let [r (sv-decomposition-with-rank m)]
                              [(:U r), (:S r), (:VT r)])
        :else (let [r (clx/svd (clatrix m))]
                [(coerce m (:left r)), (diagonal-matrix m (:values r)),
                 (coerce m (:right r))])))

(defn rank
  (^long [m] (:rank (sv-decomposition-with-rank m)))
  (^long [m accu] (:rank (rrqr-decomposition m accu))))     ; (mxc/rank m))

(defn condition
  "Returns the norm2 condition number, which is max(s) / min(s), 
      where s is the diagonal matrix of singular values from an SVD 
      decomposition."
  ^double [s]
  (/ (emax s) (emin s)))

(defn lu-decomposition-with-permutation-matrix
  "Returns a map containing:
      :L -- the lower triangular factor
      :U -- the upper triangular factor
      :P -- the permutation matrix"
  [m]
  {:pre [(have? square? m)]}
  (cond (apache-commons? m) (let [r (LUDecomposition. m)]
                              {:L (.getL r), :U (.getU r), :P (.getP r)})
        :else (let [r (clx/lu (clatrix m))]
                {:L (coerce m (:l r)), :U (coerce m (:u r)),
                 :P (coerce m (:p r))})))

(defn lu-decomposition
  "Computes the LU decompotion of a matrix. 
Returns a vector containing two matrices [L U]
Intended usage: (let [[L U] (lu-decomosition M)] ....)"
  [m]
  {:pre [(have? matrix? m)]}
  (let [r (lu-decomposition-with-permutation-matrix m)] [(:L r), (:U r)]))

(defn qr-decomposition
  "Computes the QR decomposition of a matrix. 
Returns a vector containing two matrices [Q R]
Intended usage: (let [[Q R] (qr-decomposition M)] ....)
   Q -- orthogonal factors
   R -- the upper triangular factors"
  [m]
  {:pre [(have? matrix? m)]}
  (cond (apache-commons? m) (let [d (QRDecomposition. m)]
                              [(.getQ d), (.getR d)])
        :else (let [r (clx/qr (clatrix m))]
                [(coerce m (:q r)), (coerce m (:r r))])))

(defn rrqr-decomposition
  "Calculates the rank-revealing QR-decomposition of a matrix, 
   with column pivoting.
The rank-revealing QR-decomposition of a matrix A consists of three 
   matrices Q, R and P such that AP=QR. 
Q is orthogonal (QTQ = I), and R is upper triangular. 
If A is m×n, Q is m×m and R is m×n and P is n×n.
QR decomposition with column pivoting produces a rank-revealing 
   QR decomposition and the getRank(double) method may be used to return the 
   rank of the input matrix A.
This class compute the decomposition using Householder reflectors.
Returns a map containing:
      :Q -- orthogonal factors
      :QT -- transform of orthogonal factors
      :R -- the upper triangular factors
      :P -- P Matrix
      :rank -- the rank"
  [m ^double accu]
  {:pre [(have? matrix? m)]}
  (let [d (RRQRDecomposition. (apache-commons m) accu)]
    {:Q    (coerce m (.getQ d)), :R (coerce m (.getR d)),
     :QT   (coerce m (.getQT d)), :P (coerce m (.getP d)),
     :rank (.getRank d accu)}))

(defn eigenvalues
  "Returns vector of real parts of eigenvalues"
  [m]
  {:pre [(have? square? m)]}
  (cond (diagonal? m) (sort (diagonal m))
        (clatrix? m) (let [r (clx/eigen (clx/maybe-symmetric (clatrix m)))]
                       (if (or (:ivalues r) (:ivectors r)) nil (:values r)))
        :else (let [r (EigenDecomposition. (apache-commons m))]
                (when-not (.hasComplexEigenvalues r)
                  (vec (.getRealEigenvalues r))))))

(defn eigen-decomposition
  "Computes the Eigendecomposition of a diagonalisable matrix.
   Returns a vector containing three matrices [Q A Qinv]
   A is a diagonal matrix whose diagonal elements are the eigenvalues.
   Intended usage: (let [[Q A Qinv] (eigen-decomosition M)] ....)"
  [m]
  {:pre [(have? square? m)]}
  (let [r (EigenDecomposition. (apache-commons m))]
    [(coerce m (.getV r)) (coerce m (.getD r)) (coerce m (.getVT r))]))

(comment "MATRIX SOLVE")
(defn linear-least-squares
  "Returns a vector"
  [m1 m2]
  {:pre [(have? matrix? m1)]}
  (let [^DecompositionSolver s (.getSolver (QRDecomposition.
                                             (apache-commons m1))),
        m (apache-commons m2)]
    (vec (.toArray (if (vec? m) ^RealVector (.solve s ^RealVector m)
                                ^RealVector (.solve s ^RealMatrix m))))))

(defn linear-least-squares-with-error-matrix
  "Returns a map containing:
      :S -- solution
      :E -- error matrix"
  [m1 m2]
  (let [d (QRDecomposition. (apache-commons m1))
        r (.getR d)
        r (square-matrix r)]
    (when-not (= (column-count r) (column-count m1))        ; TODO - use truss or assert
      (throw (ex-info "Icky matrices" {:fn (var linear-least-squares-with-error-matrix)})))
    (let [ri (inverse r), e (mmul ri (transpose ri)),
          ^DecompositionSolver s (.getSolver d), m (apache-commons m2)]
      {:S (vec (.toArray (if (vec? m)
                           ^RealVector (.solve s ^RealVector m)
                           ^RealVector (.solve s ^RealMatrix m)))),
       :E (coerce m1 e)})))

(defn matrix-solve-iterative
  "This seems to solve only when the matrix 'm' is psd.  
Not sure of any advantages over linear least squares.  
This could be improved by running both sovers on parallel threads.
m * x = v.
Returns the vector 'x'
'solver' types:
:cg Conjugate Gradient
:symm SymmLQ (default)
  A default stopping criterion is implemented. 
The iterations stop when || r || ≤ δ || v ||, where v is the right-hand side 
   vector, r the current estimate of the residual, and δ a user-specified 
   tolerance. 
It should be noted that r is the so-called updated residual, which might 
   differ from the true residual due to rounding-off 
   errors (see e.g. Strakos and Tichy, 2002).

Implementation of the SYMMLQ iterative linear solver proposed by Paige and 
   Saunders (1975). 
This implementation is largely based on the FORTRAN code 
   by Pr. Michael A. Saunders.
SYMMLQ is designed to solve the system of linear equations A · x = b where A 
   is an n × n self-adjoint linear operator (defined as a RealLinearOperator), 
   and b is a given vector. 
The operator A is not required to be positive definite. 
If A is known to be definite, the method of conjugate gradients might 
be preferred, since it will require about the same number of iterations 
as SYMMLQ but slightly less work per iteration.
SYMMLQ is designed to solve the system (A - shift · I) · x = b, where 
   shift is a specified scalar value. 
If shift and b are suitably chosen, the computed vector x may approximate 
   an (unnormalized) eigenvector of A, as in the methods of inverse 
   iteration and/or Rayleigh-quotient iteration. 
Again, the linear operator (A - shift · I) need not be positive definite 
   (but must be self-adjoint). 
The work per iteration is very slightly less if shift = 0."
  [m v
   & {:keys [solver guess max-iter delta check?]
      :or   {solver :symm, max-iter m/*max-iter*, delta m/*dbl-close*,
             check? true}}]
  {:pre [(have? matrix m)]}
  (let [^RealMatrix a (apache-commons m), ^RealVector b (apache-commons v),
        ^RealVector g (if guess (apache-commons guess)),
        ^PreconditionedIterativeLinearSolver s
        (condp = solver
          :cg (ConjugateGradient. ^long max-iter ^double delta
                                  ^boolean check?)
          :symm (SymmLQ. ^long max-iter ^double delta ^boolean check?)
          (throw (ex-info (format "Invalid solver type specified %s" solver)
                          {:fn 'matrix-solve-iterative})))]
    (vec (.toArray (if guess ^RealVector (.solve s ^RealLinearOperator a b g)
                             ^RealVector (.solve s a b))))))

(comment "RANDOM")
(defn rnd-vec
  "Returns [v rnd-lazy], where v has random elements"
  ([^long size rnd-lazy] (rnd-vec nil size rnd-lazy))
  ([implementation ^long size rnd-lazy]
   {:pre [(have? pos? size)]}
   [(into [] (take size rnd-lazy)) (drop size rnd-lazy)]))

(defn rnd-matrix
  "Returns [m rnd-lazy], where m has random elements"
  ([^long rows ^long columns rnd-lazy] (rnd-matrix nil rows columns rnd-lazy))
  ([implementation ^long rows ^long columns rnd-lazy]
   (let [[v s] (rnd-vec (* rows columns) rnd-lazy)]
     [(coerce implementation (partition columns v)) s])))

(defn rnd-reflection-matrix
  "Returns [m rnd-lazy], where m is a random Householder reflection."
  ([size rnd-lazy] (rnd-reflection-matrix nil size rnd-lazy))
  ([implementation size rnd-lazy]
   (let [v (column-matrix implementation (normalise (take size rnd-lazy)))]
     [(coerce implementation (sub (identity-matrix implementation size)
                                  (mmul (mmul v (transpose v)) 2.0))),
      (drop size rnd-lazy)])))

(defn rnd-spectral-matrix
  "Returns [m rnd-lazy], where m is a random matrix with a particular 
      spectrum vector.  
The orthogonal matrices are generated by using 2 * spectrum-length composed 
   Householder reflections."
  ([spectrum rnd-lazy] (rnd-spectral-matrix nil spectrum rnd-lazy))
  ([implementation spectrum rnd-lazy]
   (let [size (count spectrum),
         [v-mat r] (nth (iterate (fn [[prod-mat laz]]
                                   (let [[r-mat s] (rnd-reflection-matrix
                                                     implementation size laz)]
                                     [(mmul prod-mat r-mat) s]))
                                 [(identity-matrix implementation size) rnd-lazy])
                        (* 2 size))
         l-mat (diagonal-matrix implementation spectrum)]
     [(coerce implementation (-> v-mat (mmul l-mat) (mmul (transpose v-mat)))),
      r])))

(defn rnd-positive-matrix
  "Returns [m rnd-lazy], where m is a positive definite matrix with a random 
      spectrum. 
The orthogonal matrices are generated by using 2 * size composed Householder 
   reflections.
Alternative #1: Sample from the Inverse-Wishart Distribution.
Alternative #2: (let [[m s] (rnd-matrix size size rnd-lazy)] 
                   [(mmul (transpose m) m), s])"
  ([^long size rnd-lazy] (rnd-positive-matrix nil size rnd-lazy))
  ([implementation ^long size rnd-lazy]
   (rnd-spectral-matrix implementation (take size rnd-lazy)
                        (drop size rnd-lazy))))