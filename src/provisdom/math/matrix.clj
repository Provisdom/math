(ns provisdom.math.matrix
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]
            [provisdom.utility-belt.core :as co]
            [provisdom.math.core :as m]
            [provisdom.math.arrays :as ar]
            [clatrix.core :as clx]
            [clojure.core.matrix :as mxc]
            [clojure.core.matrix.protocols :as mp]
            [taoensso.truss :refer [have have?]])
  (:import [org.apache.commons.math3.linear RealVector RealMatrix
                                            QRDecomposition LUDecomposition CholeskyDecomposition
                                            RectangularCholeskyDecomposition Array2DRowRealMatrix
                                            EigenDecomposition SingularValueDecomposition RRQRDecomposition
                                            DecompositionSolver ConjugateGradient SymmLQ
                                            PreconditionedIterativeLinearSolver RealLinearOperator]))

(set! *warn-on-reflection* true)

;;;DECLARATIONS
(declare eigenvalues column-matrix transpose rrqr-decomposition diagonal
         get-slices-as-matrix esome esum matrix? row-matrix? column-matrix?
         row-count column-count dimensionality size-symmetric size-symmetric-with-unit-diagonal)

(s/def ::by-row? boolean?)
(s/def ::size ::m/int-non-)
(s/def ::row ::m/int-non-)
(s/def ::column ::m/int-non-)
(s/def ::rows ::m/int-non-)
(s/def ::columns ::m/int-non-)
(s/def ::row-matrix (s/with-gen row-matrix? #(gen/vector (s/gen ::vector) 1)))
(s/def ::column-matrix (s/with-gen column-matrix? #(gen/fmap (fn [v] (column-matrix v)) (s/gen ::vector))))
(s/def ::numbers (s/with-gen (s/coll-of ::m/number)
                             #(s/gen (s/or :v (s/coll-of ::m/number :min-count 0 :max-count 6 :kind vector?)
                                           :l (s/coll-of ::m/number :min-count 0 :max-count 6 :kind list?)))))
(s/def ::vector (s/with-gen (s/coll-of ::m/number :kind vector?) #(gen/vector (s/gen ::m/number) 0 6)))
(s/def ::vector-num (s/with-gen (s/coll-of ::m/num :kind vector?) #(gen/vector (s/gen ::m/num) 0 6)))
(s/def ::vector-finite (s/with-gen (s/coll-of ::m/finite :kind vector?) #(gen/vector (s/gen ::m/finite) 0 6)))
(s/def ::matrix (s/with-gen matrix? #(gen/vector (s/gen ::vector) 1 6)))
(s/def ::matrix-num (s/with-gen (s/coll-of (s/coll-of ::m/num :kind vector?) :kind vector?)
                                #(gen/vector (s/gen ::vector-num) 1 6)))
(s/def ::matrix-finite (s/with-gen (s/coll-of (s/coll-of ::m/finite :kind vector?) :kind vector?)
                                   #(gen/vector (s/gen ::vector-finite) 1 6)))
(s/def ::matrix3 (s/with-gen (s/coll-of ::matrix) #(gen/vector (s/gen ::matrix) 1 6)))
(s/def ::matrix4 (s/with-gen (s/coll-of ::matrix3) #(gen/vector (s/gen ::matrix3) 1 6)))
(s/def ::matrix5+ (s/with-gen (s/coll-of (s/coll-of (s/coll-of (s/coll-of (s/coll-of some?)))))
                              #(gen/vector (s/gen ::matrix4) 1 6)))
(s/def ::matrix4+ (s/or :four-m ::matrix4 :5+m ::matrix5+))
(s/def ::matrix3+ (s/or :three-m ::matrix3 :4+m ::matrix4+))
(s/def ::matrix2+ (s/or :m ::matrix :3+m ::matrix3+))
(s/def ::matrix1+ (s/or :v ::vector :2+m ::matrix2+))
(s/def ::tensor (s/or :value ::m/number :1+m ::matrix1+))
(s/def ::nan-or-matrix (s/or :nan ::m/nan :m ::matrix))
(s/def ::nan-or-matrix (s/or :nan ::m/nan :m ::matrix-num))
(s/def ::nan-or-matrix-finite (s/or :nan ::m/nan :m ::matrix-finite))
(s/def ::matrix-or-vector (s/or :m ::matrix :v ::vector))
(s/def ::matrix-or-less (s/or :m ::matrix :v ::vector :value ::m/number))
(s/def ::top-left ::matrix-or-vector)
(s/def ::bottom-left ::matrix-or-vector)
(s/def ::top-right ::matrix-or-vector)
(s/def ::bottom-right ::matrix-or-vector)
(s/def ::sparse-vector
  (s/with-gen (s/coll-of (s/tuple ::m/int-non- ::m/number) :kind vector?)
              #(gen/bind (gen/large-integer* {:min 0 :max 6})
                         (fn [i] (gen/vector
                                   (gen/tuple (gen/large-integer* {:min 0 :max (dec i)})
                                              (s/gen ::m/number))
                                   i)))))
(s/def ::sparse-matrix
  (s/with-gen (s/coll-of (s/tuple ::m/int-non- ::m/int-non- ::m/number) :kind vector?)
              #(gen/bind (gen/tuple (gen/large-integer* {:min 0 :max 6}) (gen/large-integer* {:min 0 :max 6}))
                         (fn [[i j]] (gen/vector (gen/vector
                                                   (gen/tuple (gen/large-integer* {:min 0 :max (dec i)})
                                                              (gen/large-integer* {:min 0 :max (dec j)})
                                                              (s/gen ::m/number))
                                                   i)
                                                 j)))))

;;;FLOATING-POINT FAST SUM
(defn kahan-sum
  "Kahan Summation algorithm -- for greater floating-point summation accuracy,
  as fast alternative to bigDecimal"
  [v]
  (loop [[h & t] v, sum 0.0, carry 0.0]
    (if-not h
      sum
      (if (m/inf? h)
        (esum v)
        (let [y (- h carry)
              new-sum (+ y sum)]
          (recur t new-sum (- new-sum sum y)))))))

(s/fdef kahan-sum
        :args (s/cat :v ::numbers)
        :ret ::m/number)

;;;NEEDED THIS...
(defn replace-nan
  "Takes a coll and returns a list with any NaN replaced with `replacement`.
  Necessary because clojure's replace doesn't work with NaN"
  [replacement coll] (reduce (fn [tot e] (conj tot (if (m/nan? e) replacement e))) '() coll))

;;;APACHE
(extend-protocol mp/PComputeMatrix
  RealMatrix
  (compute-matrix [_ shape f]
    (Array2DRowRealMatrix.
      ^"[[D" (ar/jagged-2D-array :d (co/create-dbl-layered
                                      (first shape) (second shape) f)))))

(defn apache-commons
  "Returns a matrix using the apache-commons matrix implementation."
  [data] (mxc/matrix :apache-commons data))

;; NOTE: This function was previously written to see if the type of the matrix was = to Array2DRowRealMatrix.
;; That comparison is faster than the instance? check, however it is less general. Same applies for apache-commons-vec?.
(defn apache-commons?
  "Returns true if `m` is an apache commons matrix."
  [m] (instance? RealMatrix m))

(defn apache-commons-vec?
  "Returns true if `m` is an apache commons vector."
  [m] (instance? RealVector m))

(defn diagonal-matrix-apache
  "Returns a diagonal matrix (a matrix with all elements not on the diagonal being 0.0), with the values on the diagonal
  given by the vector `diagonal-values`.
  `size` is the size of the matrix given by a single number. `f-or-val` is
  either a function or value. If given a function, the function will be called with `i` and should return the element
  at `i`, where `i` is the index of the diagonal element."
  ([diagonal-values]
   (if (apache-commons-vec? diagonal-values)
     (mxc/diagonal-matrix diagonal-values)
     (mxc/diagonal-matrix :apache-commons diagonal-values)))
  ([size value] (diagonal-matrix-apache (repeat size value))))

;;;TYPES
(defn numbers?
  "Returns true if the parameter is a collection of numbers (i.e., dimensionality is 1 and contains numbers only)."
  [x] (and (m/one? (dimensionality x)) (every? number? x)))

(s/fdef numbers?
        :args (s/cat :x any?)
        :ret boolean?)

(defn matrix?
  "Returns true if a matrix (i.e., dimensionality is 2, contains numbers only, and rows have equal lengths)"
  [x] (and (= 2 (dimensionality x))
           (every? #(= (count %) (count (first x))) x)
           (every? number? (flatten x))))

(s/fdef matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn row-matrix?
  "Returns true if a row-matrix (i.e., matrix with exactly one row)"
  [x] (and (= 2 (dimensionality x)) (m/one? (row-count x)) (every? number? (flatten x))))

(s/fdef row-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn column-matrix?
  "Returns true if a column-matrix (i.e., matrix with exactly one column)"
  [x] (and (= 2 (dimensionality x)) (every? #(m/one? (count %)) x) (every? number? (flatten x))))

(s/fdef column-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn row-or-column-matrix?
  "Returns true if a row or a column matrix."
  [x] (and (= 2 (dimensionality x))
           (every? number? (flatten x))
           (or (every? #(m/one? (count %)) x) (m/one? (row-count x)))))

(s/fdef row-or-column-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn zero-matrix?
  "Returns true if all the elements of the matrix are zeros."
  [x] (and (matrix? x) (every? zero? (flatten x))))

(s/fdef zero-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn empty-matrix?
  "Returns true if the matrix is an empty matrix."
  [x] (and (matrix? x) (empty? (get x 0))))

(s/fdef empty-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn square?
  "Returns true if matrix is square (i.e., same number of rows and columns)"
  [m] (= (row-count m) (column-count m)))

(s/fdef square?
        :args (s/cat :m ::matrix)
        :ret boolean?)

(defn diagonal?
  "Returns true if matrix is diagonal (the entries outside the main diagonal are all zero)."
  [m] (nil? (esome (fn [i j e] (not (or (= i j) (zero? e)))) m)))

(s/fdef diagonal?
        :args (s/cat :m ::matrix)
        :ret boolean?)

(defn unit-diagonal?
  "Returns true if matrix has a unit diagonal."
  [m] (every? m/one? (diagonal m)))

(s/fdef unit-diagonal?
        :args (s/cat :m ::matrix)
        :ret boolean?)

(defn symmetric?
  "Returns true if a symmetric tensor."
  [tensor] (= (transpose tensor) tensor))

(s/fdef symmetric?
        :args (s/cat :t ::tensor)
        :ret boolean?)

(defn symmetric-with-unit-diagonal?
  "Returns true if a symmetric matrix with a unit diagonal."
  [m] (and (unit-diagonal? m) (symmetric? m)))

(s/fdef symmetric-with-unit-diagonal?
        :args (s/cat :m ::matrix)
        :ret boolean?)

(defn positive?
  "Returns true if `m` is a positive definite matrix."
  ([m] (positive? m m/*dbl-close*))
  ([m accu] (and (symmetric? m) (every? #(> % accu) (eigenvalues m)))))

(s/fdef positive?
        :args (s/cat :m ::matrix :accu (s/? ::m/non-))
        :ret boolean?)

(defn positive-with-unit-diagonal?
  "Returns true if `m` has a unit diagonal and is a positive definite matrix."
  ([m] (and (unit-diagonal? m) (positive? m)))
  ([m accu] (and (unit-diagonal? m) (positive? m accu))))

(s/fdef positive-with-unit-diagonal?
        :args (s/cat :m ::matrix :accu (s/? ::m/non-))
        :ret boolean?)

(defn non-negative?
  "Returns true if `m` is a non-negative matrix."
  ([m] (non-negative? m m/*dbl-close*))
  ([m accu] (and (symmetric? m) (every? #(m/roughly-non-? % accu) (eigenvalues m)))))

(s/fdef non-negative?
        :args (s/cat :m ::matrix :accu (s/? ::m/non-))
        :ret boolean?)

;;;BASICS
(defn row-count
  "Returns the number of rows."
  [m] (count m))

(s/fdef row-count
        :args (s/cat :m ::matrix)
        :ret ::m/int-non-)

(defn column-count
  "Returns the number of columns in a matrix."
  [m] (count (first m)))

(s/fdef column-count
        :args (s/cat :m ::matrix)
        :ret ::m/int-non-)

(defn get-row
  "Gets a row of a matrix, as a vector."
  [m r] (vec (get m r)))

(s/fdef get-row
        :args (s/cat :m ::matrix :r ::row)
        :ret ::vector)

(defn get-column
  "Gets a column of a matrix, as a vector."
  [m c] (vec (map #(get % c) m)))

(s/fdef get-column
        :args (s/cat :m ::matrix :c ::column)
        :ret ::vector)

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
  "Performs element-wise multiplication with scalars and numerical arrays.
  Behaves like clojure.core/* for scalar values."
  ([] (mxc/mul))
  ([a] (mxc/mul a))
  ([a b] (mxc/mul a b))
  ([a b & more] (apply mxc/mul a b more)))

(defn abs
  "Computes the abs function on all elements of an array, using double precision values. Returns a new array."
  [m] (mp/abs m))

(defn set-row
  "Sets a row in a matrix using a specified vector."
  [m i row] (mxc/set-row m i row))

(defn emin
  "Gets the minimum element value."
  [m] (apply min (flatten m)))

(defn emax
  "Gets the maximum element value."
  [m] (apply max (flatten m)))

(defn ecount
  "Returns the total count of elements."
  [m] (if (number? m) 1 (count (flatten m))))

(defn square
  "Squares every element of a numerical array."
  [m] (mxc/square m))

(defn dimensionality
  "Returns the dimensionality of an tensor.
  The dimensionality is equal to the number of dimensions in the tensor's shape."
  [tensor]
  (if (number? tensor)
    0
    (inc (dimensionality (first tensor)))))

(s/fdef dimensionality
        :args (s/cat :tensor ::tensor)
        :ret ::m/int-non-)

(defn trace
  "Calculates the trace of a matrix (sum of elements on main diagonal).
  The matrix need not be square."
  [m] (mxc/trace m))

(defn columns
  "Gets the columns of a matrix, as a sequence of 1D vectors.
  If the array has more than 2 dimensions, will return the columns from all slices in order."
  [m] (mxc/columns m))

;;;TENSOR CONSTRUCTOR
(defn to-tensor
  "Tries to convert to tensor, otherwise returns nil."
  [x]
  (let [ret (cond (sequential? x) (let [t (map to-tensor x)] (when (every? some? t) (vec t)))
                  (number? x) x
                  :else nil)]
    (cond (nil? ret) ret
          (= (dimensionality ret) 2) (when (every? #(= (count %) (count (first ret))) ret) ret)
          :else ret)))

(s/fdef to-tensor
        :args (s/cat :x any?)
        :ret (s/nilable ::tensor))

;;;VECTOR CONSTRUCTORS
(defn to-vector
  "Creates a vector representing the flattened elements of tensor `t`."
  [tensor] (if (number? tensor) [tensor] (vec (flatten tensor))))

(s/fdef to-vector
        :args (s/cat :tensor ::tensor)
        :ret ::vector)

(defn maybe-to-vector
  "Returns `x` as a single flattened vector unless `x` is nil"
  [x] (when x (to-vector x)))

(s/fdef maybe-to-vector
        :args (s/cat :x (s/nilable ::tensor))
        :ret (s/nilable ::vector))

(defn constant-vector
  "Constructs a new vector of `value`'s (or zeros (doubles)) with the given `size`."
  ([size] (constant-vector size 0.0))
  ([size value] (vec (repeat size value))))

(s/fdef constant-vector
        :args (s/cat :size ::size :value (s/? ::m/number?))
        :ret ::vector)

(defn compute-vector
  "`f` takes an index and returns a number."
  [size f] (vec (map f (range 0 size))))

(s/fdef compute-vector
        :args (s/cat :size ::size
                     :f (s/fspec :args (s/cat :i ::m/int-non-)
                                 :ret ::m/number))
        :ret ::vector)

(defn sparse->vector
  "Builds a vector using sparse and an existing vector (often a zero-vector).
  `sparse` is a vector of tuples of `[index value]`.
  Later values will override prior overlapping values."
  [sparse v]
  (let [c (count v)]
    (vec (reduce (fn [new-v [i x]]
                   (if (or (>= i c) (neg? i))
                     new-v
                     (assoc new-v i x)))
                 v
                 sparse))))

(s/fdef sparse->vector
        :args (s/cat :sparse ::sparse-vector :v ::vector)
        :ret ::vector)

;;;MATRIX CONSTRUCTORS
(defn to-matrix
  "Builds a matrix representing the flattened elements of `tensor` (on a matrix of zeros (doubles) if necessary).
  `rows` is the number of rows of the returned matrix.
  The elements are placed `by-row?`.
  To set the number of columns instead, transpose returned matrix."
  [tensor rows by-row?]
  (let [coll (if (number? tensor) [tensor] (vec (flatten tensor)))
        c (count coll)
        [columns r] (m/quot-and-mod c rows)
        [columns r] (if (zero? r) [columns 0] [(inc columns) (- rows r)])
        coll (concat coll (repeat r 0.0))]
    (if by-row?
      (vec (map vec (partition columns coll)))
      (transpose (vec (map vec (partition rows coll)))))))

(s/fdef to-matrix
        :args (s/cat :tensor ::tensor :rows ::rows :by-row? ::by-row?)
        :ret ::matrix)

(defn constant-matrix
  "Constructs a new matrix of `value`'s (or zeros (doubles)) with the given `rows` and `columns`."
  ([rows columns] (constant-matrix rows columns 0.0))
  ([rows columns value] (vec (repeat rows (constant-vector columns value)))))

(s/fdef constant-matrix
        :args (s/cat :rows ::rows :columns ::columns :value (s/? ::m/number?))
        :ret ::matrix)

(defn compute-matrix
  "`f` takes a row and column and returns a number."
  [rows columns f] (vec (map (fn [r] (vec (map (fn [c] (f r c)) (range 0 columns)))) (range 0 rows))))

(s/fdef compute-matrix
        :args (s/cat :rows ::rows :columns ::columns
                     :f (s/fspec :args (s/cat :r ::row :c ::column)
                                 :ret ::m/number))
        :ret ::matrix)

(defn identity-matrix
  "Constructs an identity matrix with the given `size`."
  [size] (compute-matrix size size (fn [r c] (if (= r c) 1.0 0.0))))

(s/fdef identity-matrix
        :args (s/cat :size ::size)
        :ret ::matrix)

(defn square-matrix
  "Returns a square matrix my truncating values from the given matrix."
  [m]
  (let [size (min (column-count m) (row-count m))]
    (get-slices-as-matrix m :rows (range size) :columns (range size))))

(s/fdef square-matrix
        :args (s/cat :m ::matrix)
        :ret ::matrix)

(defn column-matrix
  "Returns a column matrix created from `numbers` or from `size` and `f`.
  `size` is the size of the returned matrix.
  `f` is a function that takes row `r` and returns a number."
  ([numbers] (vec (map vec (partition 1 numbers))))
  ([size f] (vec (map vec (partition 1 (compute-vector size f))))))

(s/fdef column-matrix
        :args (s/cat :numbers ::numbers)
        :ret ::column-matrix)

(defn row-matrix
  "Returns a row matrix created from `numbers` or from `size` and `f`.
  `size` is the size of the returned matrix.
  `f` is a function that takes column 'c' and returns a number."
  ([numbers] [(vec numbers)])
  ([size f] [(compute-vector size f)]))

(s/fdef row-matrix
        :args (s/cat :numbers ::numbers)
        :ret ::row-matrix)

(defn diagonal-matrix
  "Returns a diagonal matrix (a matrix with all elements not on the diagonal being 0.0), with the values on the diagonal
  given by the vector `diagonal-values`.
  `size` is the size of the matrix given by a single number. `f-or-val` is
  either a function or value. If given a function, the function will be called with `i` and should return the element
  at `i`, where `i` is the index of the diagonal element."
  ([diagonal-values]
   (if (apache-commons-vec? diagonal-values)
     (diagonal-matrix :apache-commons diagonal-values)
     (mxc/diagonal-matrix diagonal-values)))
  ([implementation diagonal-values]
   (cond (nil? implementation) (mxc/diagonal-matrix diagonal-values)
         (apache-commons-vec? diagonal-values) (mxc/diagonal-matrix diagonal-values)
         :else (mxc/diagonal-matrix implementation diagonal-values)))
  ([implementation ^long size f-or-val]
   {:pre [(have? [:or fn? number?] f-or-val)]}
   (let [impl (or implementation :persistent-vector)]
     (cond (fn? f-or-val) (diagonal-matrix impl (co/create-seq size f-or-val))
           (number? f-or-val) (diagonal-matrix impl (repeat size f-or-val))))))

(defn triangular-matrix
  "Returns a triangular matrix created from `coll`.
  `coll` is a 1D vector where each element will be used to create the triangular matrix.
  `upper?` is set to true to create an upper triangular matrix, false for a lower triangular matrix.
  `diagonal` is a 1D vector of elements on the diagonal.
  `off-diagonal` is a 1D vector of upper or lower matrix elements."
  ([coll upper?] (triangular-matrix nil coll upper?))
  ([implementation coll upper?]
   (let [size (size-symmetric (count coll))
         val-fn (fn [r c] (get coll (+ c (* r size) (* -0.5 (+ r (m/sq r))))))
         f (fn [r c] (if (> r c) 0.0 (val-fn r c)))
         f (if upper? f (fn [r c] (f c r)))]
     (compute-matrix implementation [size size] f)))
  ([implementation diagonal off-diagonal upper?]
   (let [size (size-symmetric-with-unit-diagonal (count off-diagonal))
         val-fn (fn [r c] (get off-diagonal (+ c (* r size) (* -0.5 (+ (inc r) (m/sq (inc r)))))))
         f (fn [r c] (cond (= r c) (nth diagonal r)
                           (> r c) 0.0
                           :else (val-fn r c)))
         f (if upper? f (fn [r c] (f c r)))]
     (compute-matrix implementation [size size] f))))

(defn symmetric-matrix
  "Returns a symmetric matrix created from `coll`.
  `coll` has the elements on the diagonal and the elements from the upper or lower half of the matrix.
  `byrow?` is true if `coll` is row major order, false if `coll` is col major order.
  You can pass `f` and `size` which will create a symmetric matrix of `size` by calling `f` with r,c which should
  return the element at r,c.
  `f` is only called for each element on the diagonal and either the upper or lower half of the matrix,
  depending on `by-row?`."
  ([coll] (symmetric-matrix nil coll))
  ([implementation coll]
   (let [size (size-symmetric (count coll))
         val-fn (fn [r c] (get coll (+ c (* r size) (* -0.5 (+ r (m/sq r))))))
         f (fn [r c] (if (<= r c) (val-fn r c) (val-fn c r)))]
     (compute-matrix implementation [size size] f)))
  ([f ^long size by-row?] (symmetric-matrix nil f size by-row?))
  ([implementation f ^long size by-row?]
   (compute-matrix
     implementation [size size]
     (fn [r c] (if (or (and (not by-row?) (>= r c)) (and by-row? (<= r c)))
                 (f r c)
                 (f c r))))))

(defn symmetric-with-unit-diagonal-matrix
  "Returns a symmetric matrix created from `coll`. `coll` has the elements from the upper or lower half of the matrix.
  `byrow?` is true if `coll` is row major order, false if `coll` is col major order. You can pass `f` and `size` which
  will create a symmetric matrix of size `size` by calling `f` with r,c which should return the element at r,c. `f` is
  only called for each element on either the upper or lower half of the matrix, depending on `byrow?`."
  ([coll] (symmetric-with-unit-diagonal-matrix nil coll))
  ([implementation coll]
   (let [size (size-symmetric-with-unit-diagonal (count coll))
         val-fn (fn [r c] (get coll (+ c (* r size) (* -0.5 (+ (inc r) (m/sq (inc r)))))))
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
            (get-in m [r c])
            (* 0.5 (+ (get-in m [r c]) (get-in m [c r]))))))))

(defn toeplitz-matrix
  "Returns a toeplitz matrix (also called a diagonal-constant matrix) computed by `first-row` and `first-column` where
  `first-row` is the first row in the matrix and `first-column` is the first column in the matrix."
  ([first-row first-column] (toeplitz-matrix nil first-row first-column))
  ([implementation first-row first-column]
   {:pre [(have? (fn [[first-row first-column]]
                   (and (= (first first-row) (first first-column))
                        (= (count first-row) (count first-column)))) [first-row first-column])]}
   (let [size (count first-row)]
     (compute-matrix size size
                     (fn [r c] (if (<= r c)
                                 (get first-row (- c r))
                                 (get first-column (- r c))))))))

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
                                 (constant-matrix (first m-or-shape) (second m-or-shape))])]
       (reduce (fn [m [r c value]]
                 (let [r (have [:and #(< % rows) m/non-?] r :data "Sparse row idx out of bounds")
                       c (have [:and #(< % columns) m/non-?] c :data "Sparse col idx out of bounds")]
                   (assoc-in m [r c] value)))
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
                    [m-or-size (constant-matrix m-or-size m-or-size)])]
     (reduce (fn [m [r c value]]
               (let [r (have [:and #(< % size) m/non-?] r :data "Sparse row idx out of bounds")
                     c (have [:and #(< % size) m/non-?] c :data "Sparse col idx out of bounds")]
                 (assoc-in (assoc-in m [r c] value) [c r] value)))
             m sparse))))

;===========================================
; MATRIX SPECIAL TYPE HELP
;===========================================
(defn size-symmetric
  "Returns the size of the matrix given `ecount`.
  `ecount` is the number of independent symmetric matrix elements (the number of elements on the diagonal plus
  the number either above or below the diagonal)."
  [ecount]
  (let [s (-> ecount (* 8) inc m/sqrt dec (* 0.5))]
    (when (m/roughly-round? s 1e-6)
      (long s))))

(s/fdef size-symmetric
        :args (s/cat :ecount ::m/int-non-)
        :ret (s/nilable ::m/int-non-))

(defn size-symmetric-with-unit-diagonal
  "Returns the size of the matrix given `ecount`.
  `ecount` is the number of elements above or below the unit diagonal."
  [ecount] (inc (size-symmetric ecount)))

(s/fdef size-symmetric-with-unit-diagonal
        :args (s/cat :ecount ::m/int-non-)
        :ret (s/nilable ::m/int-non-))

(defn ecount-symmetric
  "Returns the element count (`ecount`) for a symmetric matrix.
  This is the number of elements on the diagonal plus the number of elements above or below the diagonal."
  [size] (/ (+ (m/sq size) size) 2))

(s/fdef ecount-symmetric
        :args (s/cat :size ::m/int-non-)
        :ret ::m/int-non-)

(defn ecount-symmetric-with-unit-diagonal
  "Returns the element count (`ecount`) for a symmetric matrix with a unit diagonal.
  This is the number of elements above or below the diagonal."
  [size] (/ (- (m/sq size) size) 2))

(s/fdef ecount-symmetric-with-unit-diagonal
        :args (s/cat :size ::m/int-non-)
        :ret ::m/int-non-)

(defn to-vector-from-symmetric
  "Returns a vector that contains the upper (defualt) or lower half of the matrix.
  `m` doesn't have to be symmetric.
  Options: `::by-row?` (default: true). Set to false to get lower triangular values instead of upper."
  ([m] (to-vector-from-symmetric m {::by-row? true}))
  ([m {::keys [by-row?] :or {by-row? true}}]
   (let [nr (row-count m)
         nc (column-count m)]
     (vec (if by-row?
            (for [r (range nr), c (range r nc)] (get-in m [r c]))
            (for [c (range nc), r (range c nr)] (get-in m [r c])))))))

(s/fdef to-vector-from-symmetric
        :args (s/cat :m ::matrix :args (s/? (s/keys :req [::by-row?])))
        :ret ::vector)

(defn to-vector-from-symmetric-with-unit-diagonal
  "Returns a vector that contains the upper (defualt) or lower half of the matrix without the diagonal.
  `m` doesn't have to be symmetric or have a unit diagonal.
   Options: `::by-row?` (default: true). Set to false to get lower triangular values instead of upper."
  ([m] (to-vector-from-symmetric-with-unit-diagonal m {::by-row? true}))
  ([m {::keys [by-row?] :or {by-row? true}}]
   (let [nr (row-count m)
         nc (column-count m)]
     (vec (if by-row?
            (for [r (range nr), c (range (inc r) nc)] (get-in m [r c]))
            (for [c (range nc), r (range (inc c) nr)] (get-in m [r c])))))))

(s/fdef to-vector-from-symmetric
        :args (s/cat :m ::matrix :args (s/? (s/keys :req [::by-row?])))
        :ret ::vector)

;===========================================
; MATRIX IMPLEMENTATIONS
;===========================================
(defn clatrix
  "Returns a matrix using the Clatrix matrix implementation."
  [data]
  (cond (= data []) (clx/matrix [])
        (nil? data) nil
        :else (mxc/matrix :clatrix data)))

(defn clatrix?
  "Returns true if `m` is a Clatrix matrix."
  [m] (clx/matrix? m))

(defn clatrix-impl?
  "Returns true if `impl` is a Clatrix implementation. This can be either :clatrix or a matrix that is an instance of a
  Clatrix matrix."
  [impl] (or (= impl :clatrix) (clatrix? impl)))

(defn clatrix-vec?
  "Returns true if `m` is a Clatrix vector."
  [m] (clx/vec? m))

;; Not entirely sure why this exists. Clatrix may have problems handling row, column, and zero element matrices
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

;===========================================
; MATRIX GET
;===========================================
(defn get-row-as-matrix
  "Returns row `r` in matrix `m` as a row matrix."
  [m r] (row-matrix m (get m r)))

(s/fdef get-row-as-matrix
        :args (s/cat :m ::matrix :r ::row)
        :ret ::row-matrix)

(defn get-column-as-matrix
  "Returns column `c` in matrix `m` as a column matrix."
  [m c] (column-matrix m (get-column m c)))

(s/fdef get-column-as-matrix
        :args (s/cat :m ::matrix :c ::column)
        :ret ::column-matrix)

(defn diagonal
  "Returns the specified diagonal of a matrix as a vector.
   If `k`>0, returns a diagonal above the main diagonal.
   If `k`<0, returns a diagonal below the main diagonal.
   Works on both square and rectangular matrices.
   Returns `nil` if value of `k` is out of range (outside matrix)"
  ([m] (try (to-tensor (mxc/main-diagonal m)) (catch Exception e nil)))
  ([m k]
   (let [r (if (neg? k) (- k) 0)
         c (if (pos? k) k 0)
         nc (- (column-count m) c)
         nr (- (row-count m) r)
         n (min nc nr)]
     (when (pos? n)
       (vec (for [i (range n)] (get-in m [(+ i r) (+ i c)])))))))

(s/fdef diagonal
        :args (s/cat :m ::matrix :k (s/? ::m/int))
        :ret (s/nilable ::vector))

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
          (and (coll? columns) (empty? columns))) [[]]
      (and (number? rows) (number? columns)) [[(get-in m [rows columns])]]
      (and (number? rows) (coll? columns)) (row-matrix
                                             (let [r (get m rows)]
                                               (map #(get r %) columns)))
      (and (number? rows) (true? columns)) (get-row-as-matrix m rows)
      (and (coll? rows) (number? columns)) (column-matrix
                                             m (let [c (get-column m columns)]
                                                 (map #(nth c %) rows)))
      (and (coll? rows) (coll? columns)) (map
                                           (fn [e]
                                             (nth (co/flip-dbl-layered
                                                    (map #(get-column m %) columns))
                                                  e))
                                           rows)
      (and (coll? rows) (true? columns)) (map #(get m %) rows)
      (and (true? rows) (number? columns)) (get-column-as-matrix m columns)
      (and (true? rows) (coll? columns)) (co/flip-dbl-layered
                                           (map #(get-column m %)
                                                columns))
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
  "Element-wise map over all elements of one or more arrays.
  Can also map onto a number, multiple numbers, or a single fn.
  `f` is called with every element in the matrix."
  ([f m]
   (if (clatrix? m)
     (coerce :clatrix (mxc/emap f (to-tensor m)))
     (mxc/emap f m)))
  ([f m a]
   {:pre [(have? (fn [[m a]] (= (row-count m) (row-count a))) [m a])
          (have? (fn [[m a]] (or (and (numbers? m) (numbers? a))
                                 (= (column-count m) (column-count a)))) [m a])]}
   (mxc/emap f m a))
  ([f m a & more]
   {:pre [(have? (fn [[m a more]]
                   (let [v (apply vector m a more)]
                     (and (every? #(= (row-count m) (row-count %)) v)
                          (or (every? numbers? v) (every? #(= (column-count m)
                                                              (column-count %)) v)))))
                 [m a more])]}
   (if (clatrix? m)
     (coerce :clatrix (apply mxc/emap f (map to-tensor
                                             (apply vector m a more))))
     (apply mxc/emap f m a more))))

(defn transpose
  "Transposes a tensor, returning a new tensor.
  For matrices, rows and columns are swapped.
  More generally, the dimension indices are reversed.
  Note that vectors and scalars will be returned unchanged."
  [t] (try (mxc/transpose t) (catch Exception e nil)))

(s/fdef transpose
        :args (s/cat :t ::tensor)
        :ret (s/nilable ::tensor))

(defn conj-rows
  "Appends rows from all the matrices or vectors after the first to the first.
  Each row must be the same size or will return nil."
  [m & ms]
  (let [m (if (vector? m) (row-matrix m) m)
        ms (map #(if (vector? %) (row-matrix %) %) ms)
        c (column-count m)
        cs (map column-count ms)]
    (when (every? #(= c %) cs)
      (reduce (fn [tot e] (apply conj tot e)) m ms))))

(s/fdef conj-rows
        :args (s/cat :m ::matrix-or-vector :ms (s/* ::matrix-or-vector))
        :ret (s/nilable ::matrix))

(defn conj-columns
  "Appends columns from all the matrices or vectors after the first to the first.
  Each column must be the same size or will return nil."
  [m & ms]
  (let [mt (transpose m)
        mts (map transpose ms)]
    (transpose (apply conj-rows mt mts))))

(s/fdef conj-columns
        :args (s/cat :m ::matrix-or-vector :ms (s/* ::matrix-or-vector))
        :ret (s/nilable ::matrix))

(defn merge-matrices
  "Returns a Matrix created by binding four matrices together."
  [{:keys [top-left bottom-left top-right bottom-right]}]
  (conj-rows (conj-columns top-left top-right)
             (conj-columns bottom-left bottom-right)))

(s/fdef merge-matrices
        :args (s/cat :args (s/keys :req [::top-left ::bottom-left ::top-right ::bottom-right]))
        :ret (s/nilable ::matrix))

(defn conj-symmetrically [m1 m2]
  {:pre [(have? (fn [[m1 m2]]
                  (let [nr1 (row-count m1), nr2 (row-count m2),
                        nc2 (if (vector? m2) 1 (column-count m2))]
                    (and (square? m1) (or (= nr2 (+ nc2 nr1)) (= nc2 (+ nr2 nr1))))))
                [m1 m2])]}
  (let [nr1 (row-count m1)
        m2 (if (vector? m2) (row-matrix m2) m2)
        c? (> (row-count m2) (column-count m2))
        k (if c? :rows :columns)
        k2 (if c? :except-rows :except-columns)
        m (get-slices-as-matrix m2 k (range nr1))
        mt (transpose m)
        br (get-slices-as-matrix m2 k2 (range nr1))
        bl (if c? mt m)
        tr (if c? m mt)]
    (merge-matrices {::top-left m1 ::bottom-left bl ::top-right tr ::bottom-right br})))

(defn conj-diagonally
  "'x' can be a number, vector, or matrix"
  [m x]
  (cond (number? x) (let [v (conj (vec (repeat (row-count m) 0.0)) x)]
                      (conj-symmetrically m v))
        (matrix? x) (merge-matrices {::top-left     m
                                     ::bottom-left  (constant-matrix (row-count x) (column-count m))
                                     ::top-right    (constant-matrix (row-count m) (column-count x))
                                     ::bottom-right x})
        :else (let [d (diagonal-matrix x)
                    z (constant-matrix (row-count m) (ecount x))
                    m2 (conj-columns z d)]
                (conj-symmetrically m m2))))

(s/fdef conj-diagonally
        :args (s/cat :m ::matrix :x ::matrix-or-less)
        :ret ::matrix)

(defn replace-submatrix
  "Returns a Matrix after substituting a 'sub' matrix at top-left location 'row' and 'column'.
   Sub must be a matrix, not a vector.
   'row' and 'column' can be negative.
   Unassigned elements will be 0.0"
  [m sub ^long row ^long column]
  {:pre [(have? matrix? m sub)]}
  (let [sr (row-count sub), sc (column-count sub), tr (+ sr row),
        tc (+ sc column), nr (row-count m), nc (column-count m)]
    (for [r (range (min row 0) (max tr nr))]
      (for [c (range (min column 0) (max tc nc))]
        (cond (and (>= r row) (< r tr) (>= c column)
                   (< c tc)) (get-in sub [(- r row) (- c column)])
              (and (m/non-? r) (< r nr) (m/non-? c)
                   (< c nr)) (get-in m [r c])
              :else 0.0)))))

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
                         (vec a)
                         a)
                       b)))
  ([a b & more] (apply mmul (mmul a b) more)))

(defn esum
  "Returns the sum of the elements."
  ([m] (mxc/esum m))
  ([f m]
   (if (number? m)
     (f m)
     (reduce #(+ % (f %2)) 0 (flatten (to-tensor m))))))

(defn eaverage
  "Returns the average of the elements"
  [m] (m/div (esum m) (ecount m)))

(defn esum-squares
  [m]
  (if (number? m)
    (m/sq m)
    (reduce #(+ % (m/sq %2)) 0 (flatten (to-tensor m)))))

(defn eproduct
  ([m] (reduce * 1 (flatten (to-tensor m))))
  ([f m]
   (if (number? m)
     (f m)
     (reduce #(* % (f %2)) 1 (flatten (to-tensor m))))))

(defn norm
  "This is the standard norm2"
  ^double [m]
  (if (or (clatrix? m) (clatrix-vec? m))
    (clx/norm m)
    (m/sqrt (esum-squares (flatten (to-tensor m))))))

(defn norm1 ^double [m] (esum (map m/abs (flatten (to-tensor m)))))

(defn normp ^double [m ^double p]
  {:pre [(have? #(>= % 1.0) p)]}
  (m/pow (esum (map #(m/pow (m/abs %) p) (flatten (to-tensor m))))
         (/ p)))

(defn normalise
  "Returns as length one in norm2."
  ;;mxc/normalise only works for matrices,
  ;;w/ Clatrix it works like normalise! instead
  [m] (coerce m (let [s (norm m)] (emap #(m/div % s) m))))

(defn normalise1
  "Returns as length one in norm1."
  [m]
  (coerce m (let [s (norm1 m),
                  ser (emap #(m/div % s) m),
                  diff (m/one- (esum ser))]
              (if (zero? diff)
                ser
                (assoc (vec ser) 0 (+ diff (first ser)))))))

(defn normalisep
  "Returns as length one in normp."
  [m ^double p] (coerce m (let [s (normp m p)] (emap #(m/div % s) m))))

(defn inner-product
  "Computes the inner product of numerical arrays.
For matrix/matrix and matrix/vector arguments, this is equivalent to matrix multiplication.
The inner product of two arrays with indexed dimensions {..i j} and {j k..} has dimensions {..i k..}.
The inner-product of two vectors will be scalar."
  ([a] (mxc/inner-product a))
  ([a b]
   (let [i (mxc/inner-product (to-tensor a) (to-tensor b))]
     (if (number? i) i (coerce a i))))
  ([a b & more]
   (let [i (apply mxc/inner-product (map to-tensor
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
                                                    (mul (get-in a [i j]) b)))))))
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
   {:pre [(have? numbers? v)]}
   (let [s (ecount v)]
     (for [r (range s)]
       (for [c (range s)] (* (get v r) (get v c))))))
  ([implementation f v]
   {:pre [(have? numbers? v)]}
   (let [s (ecount v)]
     (for [r (range s)]
       (for [c (range s)] (f (* (get v r) (get v c)))))))
  ([implementation f v & ms]
   {:pre [(have? numbers? v)]}
   (let [s (ecount v)]
     (for [r (range s)]
       (for [c (range s)]
         (apply f (* (get v r) (get v c))
                (map #(get-in % [r c]) ms)))))))

(defn cross-product [v1 v2]
  {:pre [(have? numbers? v1 v2)]}
  (let [f1 (get v1 0), f2 (get v2 0), s1 (get v1 1), s2 (get v2 1),
        t (- (* f1 s2) (* f2 s1))]
    (cond
      (= (ecount v1) (ecount v2) 3) (let [t1 (get v1 2), t2 (get v2 2)]
                                      [(- (* s1 t2) (* s2 t1))
                                       (- (* t1 f2) (* t2 f1)) t])
      (= (ecount v1) (ecount v2) 2) t
      :else (throw (ex-info "Vectors must be of equal length of 2 or 3" ; TODO - use truss or assert
                            (var cross-product))))))

(defn projection
  "Returns vector of v1 projected onto v2."
  [v1 v2]
  {:pre [(have? numbers? v1 v2)]}
  (coerce v1 (let [s (m/div (inner-product v1 v2) (esum-squares v2))]
               (emap #(* s %) v2))))

(defn cumulative-sum
  "Returns vector with cumulative sum."
  [v]
  {:pre [(have? numbers? v)]}
  (let [vm (coerce [] v)] (coerce v (rest (reductions + 0 vm)))))

(defn differences
  "Returns vector with differences."
  ([v init]
   {:pre [(have? numbers? v)]}
   (let [vm (coerce [] v)] (coerce v (sub vm (cons init (pop vm))))))
  ([v init last]
   {:pre [(have? numbers? v)]}
   (let [vm (coerce [] v)] (coerce v (sub (conj vm last) (cons init vm))))))

(comment "REDUCE MATRIX")
(defn ereduce-kv
  "Function f takes a result, two indexes, and element(s)."
  ([f init m byrow?]
   {:pre [(have? matrix? m)]}
   (let [mt (if byrow? m (transpose m)), nr (row-count mt)]
     (loop [c 0, val init, s mt]
       (let [g (if byrow? #(f % c %2 %3) #(f % %2 c %3))]
         (if (>= c nr)
           val
           (recur (inc c) (reduce-kv g val (first s)) (rest s)))))))
  ([f init m1 m2 byrow?]
   {:pre [(have? matrix? m1 m2)]}
   (let [mt1 (if byrow? m1 (transpose m1)),
         mt2 (if byrow? m2 (transpose m2)),
         l (min (row-count mt1) (row-count mt2))]
     (loop [c 0, val init, s1 mt1, s2 mt2]
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
     (loop [c 0, val init, s1 mt1, s2 mt2, s3 mt3]
       (let [g (if byrow? #(f % c %2 %3 %4 %5) #(f % %2 c %3 %4 %5))]
         (if (>= c l)
           val
           (recur (inc c) (co/reduce-kv-ext
                            g val (first s1) (first s2) (first s3)) (rest s1)
                  (rest s2) (rest s3))))))))

(defn every-kv?
  "Returns true if (pred index e) is logical true for every element in coll, else false."
  [pred coll]
  (loop [c 0, s coll]
    (cond (nil? (seq s)) true
          (pred c (first s)) (recur (inc c) (rest s))
          :else false)))

(defn eevery?
  "Returns true if (pred row col e) is logical true for every element in m, else false."
  [pred m]
  {:pre [(have? matrix? m)]}
  (let [nr (row-count m)]
    (loop [c 0, s m]
      (cond (>= c nr) true
            (every-kv? #(pred c % %2) (first s)) (recur (inc c) (rest s))
            :else false))))

(defn some-kv
  "Returns the first logical true value of (pred index x) for any x in coll, else nil."
  [pred coll]
  (loop [idx 0, s coll]
    (when (seq s)
      (or (pred idx (first s)) (recur (inc idx) (next s))))))

(defn esome
  "Returns the first logical true value of (pred row col e) for any e in matrix, else nil.
  Options: `::by-row?` (default: true)."
  ([pred m] (esome pred m {::by-row? true}))
  ([pred m {::keys [by-row?] :or {by-row? true}}]
   (let [mt (if by-row? m (transpose m))
         num-rows (row-count mt)]
     (loop [c 0, s mt]
       (when (< c num-rows)
         (or (some-kv #(pred c % %2) (first s)) (recur (inc c) (next s))))))))

(s/fdef esome
        :args (s/cat :pred (s/fspec :args (s/cat :r ::row :c ::column :e ::m/number)
                                    :ret boolean?)
                     :m ::matrix
                     :args (s/? (s/keys :req [::by-row?])))
        :ret (s/nilable ::m/number))

(comment "FILTER MATRICES")
(defn filter-kv
  "Returns a vector of the items in coll for which (pred item) returns true. pred must be free of side-effects."
  [pred coll] (persistent! (reduce-kv #(if (pred %2 %3) (conj! % %3) %) (transient []) (vec coll))))

(defn efilter
  "Returns a sequence of filtered values.  pred takes an element"
  [m pred & {:keys [byrow?] :or {byrow? true}}]
  {:pre [(have? matrix? m)]}
  (let [mt (if byrow? m (transpose m))] (filter pred (flatten mt))))

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
  "Returns a matrix.
  'pred' takes a row"
  [m pred]
  {:pre [(have? matrix? m)]}
  (filter pred m))

(defn sparse-filter-by-row
  "Returns a vector of [row row-value].
  'pred' takes a row"
  [m pred]
  {:pre [(have? matrix? m)]}
  (reduce-kv #(if (pred %3) (conj % [%2 %3]) %) [] m))

(defn filter-by-column
  "Returns a matrix.
  'pred' takes a column"
  [m pred]
  {:pre [(have? matrix? m)]}
  (transpose (filter pred (transpose m))))

(defn sparse-filter-by-column
  "Returns a vector of [column column-value].
  'pred' takes a column"
  [m pred]
  {:pre [(have? matrix? m)]}
  (reduce-kv #(if (pred %3) (conj % [%2 %3]) %) [] (transpose m)))

(defn filter-symmetrically
  "Returns a matrix.
  'pred' takes a row or column"
  [m pred & {:keys [by-row?] :or {by-row? true}}]
  {:pre [(have? matrix? m)]}
  (let [ma (if by-row? m (transpose m)),
        keep-set (reduce-kv #(if (pred %3) (conj % %2) %) #{} ma)]
    (get-slices-as-matrix m :rows keep-set, :columns keep-set)))

(comment "MATRIX IMMUTABLE CHANGES")
(defn set-column
  [m ^long i column]
  {:pre [(have? matrix? m) (have? numbers? column)]}
  (transpose (set-row (transpose m) i column)))

(defn insert-row
  [m ^long i row]
  {:pre [(have? matrix? m) (have? numbers? row)]}
  (to-matrix m (co/insertv m (to-vector row) i)
             (inc (row-count m)) true))

(defn insert-column
  [m ^long i column]
  {:pre [(have? matrix? m) (have? numbers? column)]}
  (to-matrix m (co/insertv (transpose m) (to-tensor column) i)
             (row-count m) false))

(defn insert-symmetrically
  [m ^long i v]
  {:pre [(have? matrix? m) (have? numbers? v)]}
  (set-row (insert-column (insert-row m i (repeat (column-count m) 0.0)) i v)
           i
           v))

(comment "MATRIX NUMERICAL STABILITY")
(defn roughly? [m1 m2 accu]
  (cond (and (matrix? m1)
             (matrix? m2)) (every? identity
                                   (map #(roughly? %1 %2 accu)
                                        (to-tensor m1)
                                        (to-tensor m2)))
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
  (map #(if ((roughly-zero-row-fn accu) %)
          (repeat (column-count m) 0.0)
          %)
       m))

(defn round-roughly-zero-columns
  "Returns a matrix after rounding any roughly-zero columns"
  [m accu]
  {:pre [(have? matrix? m)]}
  (co/flip-dbl-layered
    (map #(if ((roughly-zero-row-fn accu) %)
            (repeat (row-count m) 0.0) %)
         (transpose m))))

(defn round-roughly-zero-rows-and-columns
  "Returns a matrix after rounding any roughly-zero rows and columns"
  [m accu] (round-roughly-zero-columns (round-roughly-zero-rows m accu) accu))

;(defn force-symmetric-matrix-to-be-non-negative
;  "Attempts to return a non-negative matrix by reducing the absolute values
;      of the off-diagaonal elements as necessary"
;  [m]
;  (symmetric-matrix
;    m (fn [r c]
;        (let [e (get-in m [r c])]
;          (if (= r c) e
;            (* (m/sgn e) (min (m/sqrt (* (get-in m [r r]) (get-in m [c c])))
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
  {:pre [(have? [:or number? square? numbers?] m)]}
  (cond (number? m) (m/div m)
        (numbers? m) (compute-vector m (emap m/div m))
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
                          s (- c r), nm (if (zero? s) b (conj-columns b (constant-matrix c s)))]
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
  ^double [s] (/ (emax s) (emin s)))

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
  (let [^DecompositionSolver s (.getSolver (QRDecomposition. (apache-commons m1))),
        m (apache-commons m2)]
    (vec (.toArray (if (numbers? m) ^RealVector (.solve s ^RealVector m)
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
      {:S (vec (.toArray (if (numbers? m)
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
   (rnd-spectral-matrix implementation (take size rnd-lazy) (drop size rnd-lazy))))