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
         get-slices-as-matrix esome esum matrix? row-matrix? column-matrix? square-matrix?
         row-count column-count dimensionality size-symmetric size-symmetric-with-unit-diagonal
         compute-vector coerce maybe-convert-clatrix-row-or-column to-vector ecount to-matrix
         inner-product emap constant-matrix)

(s/def ::by-row? boolean?)
(s/def ::upper? boolean?)
(s/def ::number ::m/number)
(s/def ::accu ::m/non-)
(s/def ::size ::m/int-non-)
(s/def ::shape (s/with-gen (s/coll-of ::m/int+) #(gen/vector (gen/large-integer* {:min 1 :max 6}) 0 6)))
(s/def ::index ::m/int-non-)
(s/def ::indices (s/with-gen (s/coll-of ::index) #(gen/vector (s/gen ::index) 0 6)))
(s/def ::row-indices (s/or :index ::index :indices ::indices))
(s/def ::column-indices (s/or :index ::index :indices ::indices))
(s/def ::exception-row-indices (s/or :index ::index :indices ::indices))
(s/def ::exception-column-indices (s/or :index ::index :indices ::indices))
(s/def ::row ::m/int-non-)
(s/def ::column ::m/int-non-)
(s/def ::rows ::m/int-non-)
(s/def ::columns ::m/int-non-)
(s/def ::row-matrix (s/with-gen row-matrix? #(gen/vector (s/gen ::vector) 1)))
(s/def ::column-matrix (s/with-gen column-matrix? #(gen/fmap (fn [v] (column-matrix v)) (s/gen ::vector))))
(s/def ::numbers (s/with-gen (s/coll-of ::number)
                             #(s/gen (s/or :v (s/coll-of ::number :min-count 0 :max-count 6 :kind vector? :into [])
                                           :l (s/coll-of ::number :min-count 0 :max-count 6 :kind list? :into '())))))
(s/def ::vector (s/with-gen (s/coll-of ::number :kind vector? :into []) #(gen/vector (s/gen ::number) 0 6)))
(s/def ::vector-2D (s/with-gen (s/coll-of ::number :kind vector? :into [] :min-count 2 :max-count 2)
                               #(gen/vector (s/gen ::number) 2)))
(s/def ::vector-3D (s/with-gen (s/coll-of ::number :kind vector? :into [] :min-count 3 :max-count 3)
                               #(gen/vector (s/gen ::number) 3)))
(s/def ::vector-num (s/with-gen (s/coll-of ::m/num :kind vector? :into []) #(gen/vector (s/gen ::m/num) 0 6)))
(s/def ::vector-finite (s/with-gen (s/coll-of ::m/finite :kind vector? :into []) #(gen/vector (s/gen ::m/finite) 0 6)))
(s/def ::matrix (s/with-gen matrix? #(gen/vector (s/gen ::vector) 1 6)))
(s/def ::matrix-num (s/with-gen (s/coll-of (s/coll-of ::m/num :kind vector? :into []) :kind vector? :into [])
                                #(gen/vector (s/gen ::vector-num) 1 6)))
(s/def ::matrix-finite (s/with-gen (s/coll-of (s/coll-of ::m/finite :kind vector? :into []) :kind vector? :into [])
                                   #(gen/vector (s/gen ::vector-finite) 1 6)))
(s/def ::square-matrix
  (s/with-gen square-matrix?
              #(gen/bind (gen/large-integer* {:min 0 :max 6})
                         (fn [i] (gen/vector (gen/vector i) i)))))
(s/def ::matrix3 (s/with-gen (s/coll-of ::matrix) #(gen/vector (s/gen ::matrix) 1 6)))
(s/def ::matrix4 (s/with-gen (s/coll-of ::matrix3) #(gen/vector (s/gen ::matrix3) 1 6)))
(s/def ::matrix5+ (s/with-gen (s/coll-of (s/coll-of (s/coll-of (s/coll-of (s/coll-of some?)))))
                              #(gen/vector (s/gen ::matrix4) 1 6)))
(s/def ::matrix4+ (s/or :four-m ::matrix4 :5+m ::matrix5+))
(s/def ::matrix3+ (s/or :three-m ::matrix3 :4+m ::matrix4+))
(s/def ::matrix2+ (s/or :m ::matrix :3+m ::matrix3+))
(s/def ::matrix1+ (s/or :v ::vector :2+m ::matrix2+))
(s/def ::tensor (s/or :value ::number :1+m ::matrix1+))
(s/def ::nan-or-matrix (s/or :nan ::m/nan :m ::matrix))
(s/def ::nan-or-matrix (s/or :nan ::m/nan :m ::matrix-num))
(s/def ::nan-or-matrix-finite (s/or :nan ::m/nan :m ::matrix-finite))
(s/def ::matrix-or-vector (s/or :m ::matrix :v ::vector))
(s/def ::matrix-or-less (s/or :m ::matrix :v ::vector :value ::number))
(s/def ::top-left ::matrix-or-vector)
(s/def ::bottom-left ::matrix-or-vector)
(s/def ::top-right ::matrix-or-vector)
(s/def ::bottom-right ::matrix-or-vector)
(s/def ::top-left-matrix ::matrix)
(s/def ::bottom-left-matrix ::matrix)
(s/def ::top-right-matrix ::matrix)
(s/def ::bottom-right-matrix ::matrix)
(s/def ::sparse-vector
  (s/with-gen (s/coll-of (s/tuple ::m/int-non- ::number) :kind vector? :into [])
              #(gen/bind (gen/large-integer* {:min 0 :max 6})
                         (fn [i] (gen/vector
                                   (gen/tuple (gen/large-integer* {:min 0 :max (dec i)})
                                              (s/gen ::number))
                                   i)))))
(s/def ::sparse-matrix
  (s/with-gen (s/coll-of (s/tuple ::m/int-non- ::m/int-non- ::number) :kind vector? :into [])
              #(gen/bind (gen/tuple (gen/large-integer* {:min 0 :max 6}) (gen/large-integer* {:min 0 :max 6}))
                         (fn [[i j]] (gen/vector (gen/vector
                                                   (gen/tuple (gen/large-integer* {:min 0 :max (dec i)})
                                                              (gen/large-integer* {:min 0 :max (dec j)})
                                                              (s/gen ::number))
                                                   i)
                                                 j)))))

;;;FLOATING-POINT FAST SUM
(defn kahan-sum
  "Kahan Summation algorithm -- for greater floating-point summation accuracy,
  as fast alternative to bigDecimal"
  [numbers]
  (loop [[h & t] numbers, sum 0.0, carry 0.0]
    (if-not h
      sum
      (if (m/inf? h)
        (esum numbers)
        (let [y (- h carry)
              new-sum (+ y sum)]
          (recur t new-sum (- new-sum sum y)))))))

(s/fdef kahan-sum
        :args (s/cat :numbers ::numbers)
        :ret ::number)

;;;this should go into a different ns
(defn replace-nan
  "Takes a coll and returns a list with any NaN replaced with `replacement`.
  Necessary because clojure's replace doesn't work with NaN"
  [replacement coll] (reduce (fn [tot e] (conj tot (if (m/nan? e) replacement e))) '() coll))

;;;TEMPORARY
(defn coerce
  "Coerces `param` into the provided implementation or matrix type"
  [matrix-or-implementation param]
  (mxc/coerce matrix-or-implementation
              (maybe-convert-clatrix-row-or-column param)))

;;;CLATRIX
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
  "Returns true if `impl` is a Clatrix implementation.
  This can be either :clatrix or a matrix that is an instance of a Clatrix matrix."
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

(defn compute-vector-apache
  "`f` takes an index and returns a number."
  [size f] (coerce :apache-commons (compute-vector size f)))

;;;TENSOR TYPES
(defn symmetric?
  "Returns true if a symmetric tensor."
  [tensor] (= (transpose tensor) tensor))

(s/fdef symmetric?
        :args (s/cat :t ::tensor)
        :ret boolean?)

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

(defn- recursive-compute-tensor
  "Recursively computes tensor for [[compute-tensor]]."
  [shape f sh]
  (let [c (count shape)
        dim (count sh)]
    (if (= dim c)
      (f sh)
      (mapv #(recursive-compute-tensor shape f (conj sh %)) (range (get shape dim))))))

(defn compute-tensor
  "`f` takes a vector of `indices` and returns a number."
  [shape f] (recursive-compute-tensor shape f []))

(s/fdef compute-tensor
        :args (s/cat :shape ::shape :f (s/fspec :args (s/cat :indices ::indices) :ret ::number))
        :ret ::tensor)

(defn constant-tensor
  "Constructs a new tensor of `number`'s (or zeros (doubles)) with the given `shape`."
  ([shape] (constant-tensor shape 0.0))
  ([shape number]
   (let [c (dec (count shape))
         n (vec (repeat (get shape c) number))]
     (loop [dim (dec c), t n]
       (if (neg? dim)
         t
         (recur (dec dim) (vec (repeat (get shape dim) t))))))))

(s/fdef constant-tensor
        :args (s/cat :shape ::shape :number (s/? ::number))
        :ret ::tensor)

;;;TENSOR INFO
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

(defn shape
  "Returns the shape of the tensor."
  [tensor]
  (loop [dim 0, sh [], remain tensor]
    (if (sequential? remain)
      (let [r (first remain)]
        (recur (inc dim) (conj sh (count remain)) r))
      sh)))

(s/fdef shape
        :args (s/cat :tensor ::tensor)
        :ret ::shape)

(defn every-kv?
  "Returns true if (pred indices number) is logical true for every element in tensor, else false."
  [pred tensor] (every? true? (flatten (compute-tensor shape #(pred % (get-in tensor %))))))

(s/fdef every-kv?
        :args (s/cat :pred (s/fspec :args (s/cat :indices ::indices :number ::number) :ret boolean?))
        :ret boolean?)

(defn some-kv
  "Returns the first logical true value of (pred index x) for any x in coll, else nil."
  [pred coll]
  (loop [idx 0, s coll]
    (when (seq s)
      (or (pred idx (first s)) (recur (inc idx) (next s))))))

;;;TENSOR MANIPULATION
(defn transpose
  "Transposes a tensor, returning a new tensor.
  For matrices, rows and columns are swapped.
  More generally, the dimension indices are reversed.
  Note that vectors and scalars will be returned unchanged."
  [t]
  (let [dim (dimensionality t)]
    (cond (<= dim 1) t
          (= dim 2) (apply mapv vector t)
          :else (mapv transpose t))))                       ;;this is wrong for dim > 2 -- come back to this

(s/fdef transpose
        :args (s/cat :t ::tensor)
        :ret (s/nilable ::tensor))

(defn emap
  "Element-wise mapping over all elements of one or more tensors."
  ([f tensor] (mxc/emap f tensor))
  ([f tensor & more] (apply mxc/emap f tensor more)))       ;;use 'walking' library? -- return nil if can't emap

(s/fdef emap
        :args (s/cat :f (s/fspec :args (s/cat))             ;finish this line
                     :tensor ::tensor
                     :more (s/? (s/keys* :tensors ::tensor)))
        :ret (s/nilable ::tensor))

;;;TENSOR MATH
(defn add
  "Performs element-wise addition for one or more tensors."
  ([] 0.0)
  ([tensor] (emap + tensor))
  ([tensor & more] (apply emap + tensor more)))

(s/fdef add
        :args (s/or :zero (s/cat)
                    :one+ (s/cat :tensor ::tensor :more (s/? (s/keys* :tensors ::tensor))))
        :ret (s/nilable ::tensor))

(defn subtract
  "Performs element-wise subtraction for one or more tensors."
  ([tensor] (emap - tensor))
  ([tensor & more] (apply emap - tensor more)))

(s/fdef subtract
        :args (s/cat :tensor ::tensor :more (s/? (s/keys* :tensors ::tensor)))
        :ret (s/nilable ::tensor))

(defn multiply
  "Performs element-wise multiplication for one or more tensors."
  ([] 1.0)
  ([tensor] (emap * tensor))
  ([tensor & more] (apply emap * tensor more)))

(s/fdef multiply
        :args (s/or :zero (s/cat)
                    :one+ (s/cat :tensor ::tensor :more (s/? (s/keys* :tensors ::tensor))))
        :ret (s/nilable ::tensor))

(defn divide
  "Performs element-wise division for one or more tensors."
  ([tensor] (emap / tensor))
  ([tensor & more] (apply emap / tensor more)))

(s/fdef divide
        :args (s/cat :tensor ::tensor :more (s/? (s/keys* :tensors ::tensor)))
        :ret (s/nilable ::tensor))

(defn ecount
  "Returns the total count of elements."
  [tensor] (if (number? tensor) 1 (count (flatten tensor))))

(s/fdef ecount
        :args (s/cat :tensor ::tensor)
        :ret ::size)

(defn norm1
  "The sum of the absolute values of the elements."
  [tensor] (reduce + (map m/abs (flatten tensor))))

(s/fdef norm1
        :args (s/cat :tensor ::tensor)
        :ret ::number)

(defn norm2
  "The square-root of the sum of the squared values of the elements."
  [tensor] (m/sqrt (reduce + (map m/sq (flatten tensor)))))

(s/fdef norm2
        :args (s/cat :tensor ::tensor)
        :ret ::number)

(def ^{:doc "See [[norm2]]"} norm norm2)

(defn normp
  "The 1/`p' power of the sum of the element values to the power `p`."
  [tensor p] (m/pow (esum (map #(m/pow (m/abs %) p) (flatten tensor))) (/ p)))

(s/fdef normp
        :args (s/cat :tensor ::tensor :p (s/and ::m/finite+ #(>= % 1.0)))
        :ret ::number)

(defn normalise
  "Returns as length one in norm2."
  [tensor] (let [n (norm tensor)] (emap #(m/div % n) tensor)))

(s/fdef normalise
        :args (s/cat :tensor ::tensor)
        :ret ::tensor)

(defn normalise1
  "Returns as length one in norm1."
  [tensor]
  (let [n1 (norm1 tensor)
        ser (vec (emap #(m/div % n1) tensor))
        diff (m/one- (esum ser))]
    ;;check for slight rounding errors...
    (if (zero? diff)
      ser
      (assoc ser 0 (+ diff (first ser))))))

(s/fdef normalise
        :args (s/cat :tensor ::tensor)
        :ret ::tensor)

(defn normalisep
  "Returns as length one in normp."
  [tensor p] (let [s (normp tensor p)] (emap #(m/div % s) tensor)))

(s/fdef normp
        :args (s/cat :tensor ::tensor :p (s/and ::m/finite+ #(>= % 1.0)))
        :ret ::tensor)

;;;TENSOR NUMERICAL STABILITY
(defn roughly?
  "Returns true if every element compared across two tensors are within `accu` of each other."
  [tensor1 tensor2 accu]
  (not-any? (emap #(not (roughly? %1 %2 accu)) tensor1 tensor2)))

(s/fdef roughly?
        :args (s/cat :tensor1 ::tensor :tensor2 ::tensor :accu ::accu)
        :ret boolean?)

(defn roughly-distinct
  "Returns a tensor with later duplicate top-level rows (or elements) removed"
  [tensor accu]
  (if (number? tensor)
    tensor
    (loop [[h & t] tensor, seen []]
      (cond (not h) seen
            (some #(roughly? h % accu) seen) (recur t seen)
            :else (recur t (conj seen h))))))

(s/fdef roughly-distinct
        :args (s/cat :tensor ::tensor :accu ::accu)
        :ret ::tensor)

;;;VECTOR TYPES
(defn numbers?
  "Returns true if the parameter is a collection of numbers (i.e., dimensionality is 1 and contains numbers only)."
  [x] (and (m/one? (dimensionality x)) (every? number? x)))

(s/fdef numbers?
        :args (s/cat :x any?)
        :ret boolean?)

;;;VECTOR CONSTRUCTORS
(defn to-vector
  "Creates a vector representing the flattened elements of `tensor`."
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
  "Constructs a new vector of `number`'s (or zeros (doubles)) with the given `size`."
  ([size] (constant-vector size 0.0))
  ([size number] (vec (repeat size number))))

(s/fdef constant-vector
        :args (s/cat :size ::size :number (s/? ::number?))
        :ret ::vector)

(defn compute-vector
  "`f` takes an `index` and returns a number."
  [size f] (mapv f (range 0 size)))

(s/fdef compute-vector
        :args (s/cat :size ::size
                     :f (s/fspec :args (s/cat :index ::index)
                                 :ret ::number))
        :ret ::vector)

(defn sparse->vector
  "Builds a vector using a sparse representation and an existing vector (often a zero-vector).
  `sparse` is a vector of tuples of `[index value]`.
  Later values will override prior overlapping values."
  [sparse v]
  (let [s (count v)]
    (vec (reduce (fn [new-v [i x]]
                   (if (or (>= i s) (neg? i))
                     new-v
                     (assoc new-v i x)))
                 v
                 sparse))))

(s/fdef sparse->vector
        :args (s/cat :sparse ::sparse-vector :v ::vector)
        :ret ::vector)

(defn flatten-matrix-by-column
  "Returns a vector that contains the elements of the matrix flattened by column."
  [m]
  (let [nr (row-count m)
        nc (column-count m)]
    (vec (for [c (range nc), r (range nr)] (get-in m [r c])))))

(s/fdef flatten-matrix-by-column
        :args (s/cat :m ::matrix)
        :ret ::vector)

(defn symmetric-matrix->vector
  "Returns a vector that contains the upper (default) or lower half of the matrix.
  `m` doesn't have to be symmetric.
  Options: `::by-row?` (default: true).
  Set to false to get lower triangular values instead of upper."
  ([m] (symmetric-matrix->vector m {::by-row? true}))
  ([m {::keys [by-row?] :or {by-row? true}}]
   (let [nr (row-count m)
         nc (column-count m)]
     (vec (if by-row?
            (for [r (range nr), c (range r nc)] (get-in m [r c]))
            (for [c (range nc), r (range c nr)] (get-in m [r c])))))))

(s/fdef symmetric-matrix->vector
        :args (s/cat :m ::matrix :args (s/? (s/keys :opt [::by-row?])))
        :ret ::vector)

(defn symmetric-matrix-with-unit-diagonal->vector
  "Returns a vector that contains the upper (defualt) or lower half of the matrix without the diagonal.
  `m` doesn't have to be symmetric or have a unit diagonal.
   Options: `::by-row?` (default: true). Set to false to get lower triangular values instead of upper."
  ([m] (symmetric-matrix-with-unit-diagonal->vector m {::by-row? true}))
  ([m {::keys [by-row?] :or {by-row? true}}]
   (let [nr (row-count m)
         nc (column-count m)]
     (vec (if by-row?
            (for [r (range nr), c (range (inc r) nc)] (get-in m [r c]))
            (for [c (range nc), r (range (inc c) nr)] (get-in m [r c])))))))

(s/fdef symmetric-matrix-with-unit-diagonal->vector
        :args (s/cat :m ::matrix :args (s/? (s/keys :opt [::by-row?])))
        :ret ::vector)

;;;VECTOR INFO
(defn filter-kv
  "Returns a vector of the items in coll for which (pred item) returns true.
  pred must be free of side-effects."
  [pred v] (persistent! (reduce-kv #(if (pred %2 %3) (conj! % %3) %) (transient []) v)))

;;;VECTOR MANIPULATION
(defn insertv
  "Returns a vector with the new value inserted into index."
  [v index number]
  (when (<= index (count v))
    (let [f (subvec v 0 index)
          l (subvec v index)]
      (vec (concat f [number] l)))))

(s/fdef insertv
        :args (s/cat :v ::vector :index ::index :number ::number)
        :ret (s/nilable ::vector))

(defn removev
  "Returns a vector with the value in the index removed."
  [v index]
  (if (<= (inc index) (count v))
    (let [f (subvec v 0 index)
          l (subvec v (inc index))]
      (vec (concat f l)))
    v))

(s/fdef removev
        :args (s/cat :v ::vector :index ::index)
        :ret ::vector)

;;;VECTOR MATH
(defn dot-product
  "The dot product is the sum of the products of the corresponding entries of the two sequences of numbers.
  Geometrically, the dot product is the product of the Euclidean magnitudes of the two vectors and the cosine of
  the angle between them.
  See [[inner-product]] for generalization of [[dot-product]] for any number of tensors."
  [v1 v2] (reduce + (map * v1 v2)))

(s/fdef dot-product
        :args (and (s/cat :v1 ::vector :v2 ::vector)
                   #(= (count (:v1 %)) (count (:v2 %))))
        :ret ::number)

(defn outer-product
  "An outer product is the tensor product of two coordinate vectors,
  a special case of the Kronecker product of matrices.
  The outer product of two coordinate vectors is a matrix such that the coordinates satisfy w_ij = u_i * u_j."
  [v]
  (let [s (count v)]
    (vec (for [r (range s)]
           (vec (for [c (range s)]
                  (* (get v r) (get v c))))))))

(s/fdef outer-product
        :args (s/cat :v ::vector)
        :ret ::matrix)

(defn cross-product
  "Given two linearly independent vectors v1 and v2, the cross product, v1 × v2,
  is a vector that is perpendicular to both v1 and v2.
  Only defined for 2D and 3D vectors."
  [v1 v2]
  (let [v10 (get v1 0)
        v20 (get v2 0)
        v11 (get v1 1)
        v21 (get v2 1)
        t (- (* v10 v21) (* v20 v11))]
    (cond
      (= (count v1) (count v2) 3) (let [v12 (get v1 2), v22 (get v2 2)]
                                    [(- (* v11 v22) (* v21 v12))
                                     (- (* v12 v20) (* v22 v10))
                                     t])
      (= (count v1) (count v2) 2) t
      :else nil)))

(s/fdef cross-product
        :args (s/and (s/cat :v1 (s/or ::vector-2D ::vector-3D) :v2 (s/or ::vector-2D ::vector-3D))
                     #(= (count (:v1 %)) (count (:v2 %))))
        :ret ::vector)

(defn projection
  "Returns vector of v1 projected onto v2."
  [v1 v2]
  (let [s (m/div (dot-product v1 v2) (reduce + (map m/sq v2)))]
    (emap #(* s %) v2)))

(s/fdef projection
        :args (and (s/cat :v1 ::vector :v2 ::vector)
                   #(= (count (:v1 %)) (count (:v2 %))))
        :ret ::vector)

;;;MATRIX TYPES
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

(defn square-matrix?
  "Returns true if matrix is square (i.e., same number of rows and columns)"
  [m] (= (row-count m) (column-count m)))

(s/fdef square-matrix?
        :args (s/cat :m ::matrix)
        :ret boolean?)

(defn diagonal-matrix?
  "Returns true if matrix is diagonal (the entries outside the main diagonal are all zero)."
  [m] (nil? (esome (fn [i j e] (not (or (= i j) (zero? e)))) m)))

(s/fdef diagonal-matrix?
        :args (s/cat :m ::matrix)
        :ret boolean?)

(defn matrix-with-unit-diagonal?
  "Returns true if matrix has a unit diagonal."
  [m] (every? m/one? (diagonal m)))

(s/fdef matrix-with-unit-diagonal?
        :args (s/cat :m ::matrix)
        :ret boolean?)

(defn symmetric-matrix?
  "Returns true if a symmetric matrix."
  [m] (symmetric? m))

(s/fdef symmetric-matrix?
        :args (s/cat :m ::matrix)
        :ret boolean?)

(defn symmetric-matrix-with-unit-diagonal?
  "Returns true if a symmetric matrix with a unit diagonal."
  [m] (and (matrix-with-unit-diagonal? m) (symmetric? m)))

(s/fdef symmetric-matrix-with-unit-diagonal?
        :args (s/cat :m ::matrix)
        :ret boolean?)

(defn positive-matrix?
  "Returns true if `m` is a positive definite matrix."
  ([m] (positive-matrix? m m/*dbl-close*))
  ([m accu] (and (symmetric? m) (every? #(> % accu) (eigenvalues m)))))

(s/fdef positive-matrix?
        :args (s/cat :m ::matrix :accu (s/? ::accu))
        :ret boolean?)

(defn positive-matrix-with-unit-diagonal?
  "Returns true if `m` has a unit diagonal and is a positive definite matrix."
  ([m] (and (matrix-with-unit-diagonal? m) (positive-matrix? m)))
  ([m accu] (and (matrix-with-unit-diagonal? m) (positive-matrix? m accu))))

(s/fdef positive-matrix-with-unit-diagonal?
        :args (s/cat :m ::matrix :accu (s/? ::accu))
        :ret boolean?)

(defn non-negative-matrix?
  "Returns true if `m` is a non-negative matrix."
  ([m] (non-negative-matrix? m m/*dbl-close*))
  ([m accu] (and (symmetric? m) (every? #(m/roughly-non-? % accu) (eigenvalues m)))))

(s/fdef non-negative-matrix?
        :args (s/cat :m ::matrix :accu (s/? ::accu))
        :ret boolean?)

;;;MATRIX CONSTRUCTORS
(defn to-matrix
  "Builds a matrix representing the flattened elements of `tensor` (on a matrix of zeros (doubles) if necessary).
  `rows` is the number of rows of the returned matrix.
  The elements are placed `by-row?`.
  To set the number of columns instead, transpose returned matrix."
  ([tensor rows] (to-matrix tensor rows {::by-row? true}))
  ([tensor rows {::keys [by-row?] :or {by-row? true}}]
   (let [coll (if (number? tensor) [tensor] (vec (flatten tensor)))
         c (count coll)
         [columns r] (m/quot-and-mod c rows)
         [columns r] (if (zero? r) [columns 0] [(inc columns) (- rows r)])
         coll (concat coll (repeat r 0.0))]
     (if by-row?
       (mapv vec (partition columns coll))
       (transpose (mapv vec (partition rows coll)))))))

(s/fdef to-matrix
        :args (s/cat :tensor ::tensor :rows ::rows :args (s/? (s/keys :opt [::by-row?])))
        :ret ::matrix)

(defn constant-matrix
  "Constructs a new matrix of `value`'s (or zeros (doubles)) with the given `rows` and `columns`."
  ([rows columns] (constant-matrix rows columns 0.0))
  ([rows columns number] (vec (repeat rows (constant-vector columns number)))))

(s/fdef constant-matrix
        :args (s/cat :rows ::rows :columns ::columns :number (s/? ::number))
        :ret ::matrix)

(defn compute-matrix
  "`f` takes a `row` and `column` and returns a number."
  [rows columns f] (mapv (fn [r] (mapv (fn [c] (f r c)) (range columns))) (range rows)))

(s/fdef compute-matrix
        :args (s/cat :rows ::rows :columns ::columns
                     :f (s/fspec :args (s/cat :row ::row :column ::column)
                                 :ret ::number))
        :ret ::matrix)

(defn identity-matrix
  "Constructs an identity matrix with the given `size`."
  [size] (compute-matrix size size (fn [r c] (if (= r c) 1.0 0.0))))

(s/fdef identity-matrix
        :args (s/cat :size ::size)
        :ret ::matrix)

(defn square-matrix
  "Returns a square matrix my truncating values from the given matrix `m`."
  [m]
  (let [r (row-count m)
        c (column-count m)
        k (if (> r c) ::exception-column-indices ::exception-row-indices)]
    (get-slices-as-matrix m {k (range (dec (min c r)) (max c r))})))

(s/fdef square-matrix
        :args (s/cat :m ::matrix)
        :ret ::matrix)

(defn column-matrix
  "Returns a column matrix created from `numbers` or from `size` and `f`.
  `size` is the size of the returned matrix.
  `f` is a function that takes `row` and returns a number."
  ([numbers] (mapv vec (partition 1 numbers)))
  ([size f] (mapv vec (partition 1 (compute-vector size f)))))

(s/fdef column-matrix
        :args (s/or :one (s/cat :numbers ::numbers)
                    :two (s/cat :size ::size :f (s/fspec :args (s/cat :row ::row)
                                                         :ret ::number)))
        :ret ::column-matrix)

(defn row-matrix
  "Returns a row matrix created from `numbers` or from `size` and `f`.
  `size` is the size of the returned matrix.
  `f` is a function that takes `column` and returns a number."
  ([numbers] [(vec numbers)])
  ([size f] [(compute-vector size f)]))

(s/fdef row-matrix
        :args (s/or :one (s/cat :numbers ::numbers)
                    :two (s/cat :size ::size :f (s/fspec :args (s/cat :column ::column)
                                                         :ret ::number)))
        :ret ::row-matrix)

(defn diagonal-matrix
  "Returns a diagonal matrix (a matrix with all elements not on the diagonal being 0.0).
  The values on the diagonal can be given by the vector `diagonal-numbers`.
  `size` is the size of the matrix given by a single number.
  `f` is a function that takes `index` and returns a number.
  Can also return a rectangular diagonal matrix using `rows` and `columns`."
  ([diagonal-numbers]
   (let [c (count diagonal-numbers)]
     (compute-matrix c c (fn [r c] (if (= r c) (get diagonal-numbers r 0.0) 0.0)))))
  ([size f] (compute-matrix size size (fn [r c] (if (= r c) (f r) 0.0))))
  ([rows columns f] (compute-matrix rows columns (fn [r c] (if (= r c) (f r) 0.0)))))

(s/fdef diagonal-matrix
        :args (s/or :one (s/cat :diagonal-numbers ::numbers)
                    :two (s/cat :size ::size :f (s/fspec :args (s/cat :index ::index)
                                                         :ret ::number)))
        :ret ::matrix)

(defn triangular-matrix
  "Returns a (square) triangular matrix (a matrix with all elements above or below the diagonal being 0.0).
  `numbers` are the elements that will be used to create the triangular matrix.
  `off-diagonal-numbers` can be used to create the off-diagonal elements,
  and then any existing `diagonal-numbers` will fill the diagonal elements.
  Set `upper?` to false for a lower triangular matrix.
  The elements are placed `by-row?`."
  ([numbers] (triangular-matrix numbers {::by-row? true, ::upper? true}))
  ([numbers {::keys [by-row? upper?] :or {by-row? true, upper? true}}]
   (let [size (size-symmetric (count numbers))
         val-fn (fn [r c] (get numbers (+ c (* r size) (* -0.5 (+ r (m/sq r))))))
         f (fn [r c] (if (or (and upper? (> r c)) (not (or upper? (> r c))))
                       0.0
                       (if by-row? (val-fn r c) (val-fn c r))))]
     (when size (compute-matrix size size f))))
  ([diagonal-numbers off-diagonal-numbers {::keys [by-row? upper?] :or {by-row? true, upper? true}}]
   (let [size (size-symmetric-with-unit-diagonal (count off-diagonal-numbers))
         val-fn (fn [r c] (get off-diagonal-numbers (+ c (* r size) (* -0.5 (+ (inc r) (m/sq (inc r)))))))
         f (fn [r c] (cond (= r c) (get diagonal-numbers r 0.0)
                           (or (and upper? (> r c)) (not (or upper? (> r c)))) 0.0
                           :else (if by-row? (val-fn r c) (val-fn c r))))]
     (when size (compute-matrix size size f)))))

(s/fdef triangular-matrix
        :args (s/cat :args (s/or :one-or-two (s/cat :numbers ::numbers :args (s/? (s/keys :opt [::by-row? ::upper?])))
                                 :three (s/cat :diagonal-numbers ::numbers
                                               :off-diagonal-numbers ::numbers
                                               :args (s/keys :opt [::by-row? ::upper?]))))
        :ret (s/nilable ::matrix))

(defn symmetric-matrix
  "Returns a symmetric matrix (a matrix with elements at r,c equal to elements at c,r).
  `numbers` are the same as the elements used to create a triangular matrix.
  Alternatively, you can pass `f` and `size` which will create a symmetric matrix of `size`
  by calling `f` with `row` and `column` and return a number.
  `f` is only called for each element on the diagonal and either the upper or lower half of the matrix,
  depending on `by-row?`."
  ([numbers]
   (let [size (size-symmetric (count numbers))
         val-fn (fn [r c] (get numbers (+ c (* r size) (* -0.5 (+ r (m/sq r))))))
         f (fn [r c] (if (<= r c) (val-fn r c) (val-fn c r)))]
     (when size (compute-matrix size size f))))
  ([size f] (symmetric-matrix size f {::by-row? true}))
  ([size f {::keys [by-row?] :or {by-row? true}}]
   (let [new-f (fn [r c] (if (or (and (not by-row?) (>= r c)) (and by-row? (<= r c))) (f r c) (f c r)))]
     (compute-matrix size size new-f))))

(s/fdef symmetric-matrix
        :args (s/cat :args (s/or :one (s/cat :numbers ::numbers)
                                 :two-or-three (s/cat :f (s/fspec :args (s/cat :row ::row :column ::column)
                                                                  :ret ::number)
                                                      :size ::size
                                                      :args (s/? (s/keys :opt [::by-row?])))))
        :ret (s/nilable ::matrix))

(defn symmetric-matrix-with-unit-diagonal
  "Returns a symmetric matrix with a unit diagonal (a matrix with a diagonal of all ones and elements
  at r,c equal to elements at c,r).
  `numbers` are the same as the elements used to create a triangular matrix but without the diagonal.
  Alternatively, you can pass `f` and `size` which will create a symmetric matrix of `size`
  by calling `f` with `row` and `column` and return a number.
  `f` is only called for each element on either the upper or lower half of the matrix, depending on `by-row?`."
  ([numbers]
   (let [size (size-symmetric-with-unit-diagonal (count numbers))
         val-fn (fn [r c] (get numbers (+ c (* r size) (* -0.5 (+ (inc r) (m/sq (inc r)))))))
         f (fn [r c] (cond (= r c) 1.0
                           (< r c) (val-fn r c)
                           :else (val-fn c r)))]
     (when size (compute-matrix size size f))))
  ([size f] (symmetric-matrix-with-unit-diagonal size f {::by-row? true}))
  ([size f {::keys [by-row?] :or {by-row? true}}]
   (let [new-f (fn [r c] (cond (= r c) 1.0
                               (or (and (not by-row?) (> r c)) (and by-row? (< r c))) (f r c)
                               :else (f c r)))]
     (compute-matrix size size new-f))))

(s/fdef symmetric-matrix-with-unit-diagonal
        :args (s/cat :args (s/or :one (s/cat :numbers ::numbers)
                                 :two-or-three (s/cat :f (s/fspec :args (s/cat :row ::row :column ::column)
                                                                  :ret ::number)
                                                      :size ::size
                                                      :args (s/? (s/keys :opt [::by-row?])))))
        :ret (s/nilable ::matrix))

(defn symmetric-matrix-by-averaging
  "Returns a symmetric matrix where each element above or below the diagonal is equal to the average of the matrix `m`
  at r,c and c,r.
  This is useful to help with rounding errors."
  [square-m]
  (let [size (row-count square-m)]
    (symmetric-matrix
      (for [r (range size)
            c (range r size)]
        (if (== c r)
          (get-in square-m [r c])
          (* 0.5 (+ (get-in square-m [r c]) (get-in square-m [c r]))))))))

(s/fdef symmetric-matrix-by-averaging
        :args (s/cat :square-m ::square-matrix)
        :ret ::matrix)

(defn toeplitz-matrix
  "Returns a toeplitz matrix (a matrix whose elements on any diagonal are the same).
  A Toeplitz matrix is also called a diagonal-constant matrix.
  `first-row` is the first row in the matrix and `first-column` is the first column in the matrix."
  [first-row first-column]
  (let [size (count first-row)]
    (compute-matrix size size (fn [r c] (if (<= r c)
                                          (get first-row (- c r))
                                          (get first-column (- r c)))))))

(s/fdef toeplitz-matrix
        :args (s/and (s/cat :first-row ::vector :first-column ::vector)
                     #(let [r (:first-row %), c (:first-column %)]
                        (and (= (first r) (first c))
                             (= (count r) (count c)))))
        :ret ::matrix)

(def ^{:doc "See [[toeplitz-matrix]]"} diagonal-constant toeplitz-matrix)

(defn sparse->matrix
  "Builds a matrix using a sparse representation and an existing matrix (often a zero-matrix).
  `sparse` is a vector of triples of `[row column value]`.
  Later values will override prior overlapping values."
  [sparse m]
  (let [[rows columns] [(row-count m) (column-count m)]]
    (vec (reduce (fn [new-m [r c x]]
                   (if (or (>= r rows) (neg? r) (>= c columns) (neg? c))
                     new-m
                     (assoc-in new-m [r c] x)))
                 m
                 sparse))))

(s/fdef sparse->matrix
        :args (s/cat :sparse ::sparse-matrix :m ::matrix)
        :ret ::matrix)

(defn sparse->symmetric-matrix
  "Builds a matrix using a sparse representation and an existing matrix (often a zero-matrix).
  `sparse` is a vector of triples of `[row column value]`.
  Later values will override prior overlapping values.
  Each off-diagonal inner sparse form is applied twice, with the row and column switched.
  If original matrix is not symmetric, then returned matrix may not be symmetric."
  [sparse m]
  (let [[rows columns] [(row-count m) (column-count m)]]
    (vec (reduce (fn [new-m [r c x]]
                   (if (or (>= r rows) (neg? r) (>= c columns) (neg? c))
                     new-m
                     (assoc-in (assoc-in new-m [r c] x) [c r] x)))
                 m
                 sparse))))

(s/fdef sparse->symmetric-matrix
        :args (s/cat :sparse ::sparse-matrix :m ::matrix)
        :ret ::matrix)

;;;MATRIX INFO
(defn row-count
  "Returns the number of rows."
  [m] (count m))

(s/fdef row-count
        :args (s/cat :m ::matrix)
        :ret ::rows)

(defn column-count
  "Returns the number of columns in a matrix."
  [m] (count (first m)))

(s/fdef column-count
        :args (s/cat :m ::matrix)
        :ret ::columns)

(defn get-row
  "Gets a `row` of a matrix, as a vector."
  [m row] (vec (get m row)))

(s/fdef get-row
        :args (s/cat :m ::matrix :row ::row)
        :ret ::vector)

(defn get-column
  "Gets a `column` of a matrix, as a vector."
  [m column] (mapv #(get % column) m))

(s/fdef get-column
        :args (s/cat :m ::matrix :column ::column)
        :ret ::vector)

(defn get-row-as-matrix
  "Returns `row` in matrix `m` as a row matrix."
  [m row] (row-matrix m (get m row)))

(s/fdef get-row-as-matrix
        :args (s/cat :m ::matrix :row ::row)
        :ret ::row-matrix)

(defn get-column-as-matrix
  "Returns `column` in matrix `m` as a column matrix."
  [m column] (column-matrix m (get-column m column)))

(s/fdef get-column-as-matrix
        :args (s/cat :m ::matrix :column ::column)
        :ret ::column-matrix)

(defn diagonal
  "Returns the specified diagonal of a matrix as a vector.
   If `k`>0, returns a diagonal above the main diagonal.
   If `k`<0, returns a diagonal below the main diagonal.
   Works on both square and rectangular matrices.
   Returns `nil` if value of `k` is out of range (outside matrix)"
  ([m] (reduce (fn [tot e] (conj tot (get-in m [e e]))) [] (range (count m))))
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

(defn trace
  "Calculates the trace of a matrix (sum of elements on main diagonal).
  The matrix need not be square."
  [m] (esum (diagonal m)))

(s/fdef trace
        :args (s/cat :m ::matrix)
        :ret ::number)

(defn get-slices-as-matrix
  "Performs a slice on the matrix given by the options.
  Options:
    `:rows` returns all rows by default, can pass a row index or sequence of row indices
    `:columns` returns all columns by default, can pass a column index or sequence of column indices
    `:except-rows` can pass a row index or sequence of row indices to exclude
    `:except-columns` can pass a column index or sequence of column indices to exclude.
    Exceptions override inclusions."
  [m {::keys [row-indices column-indices exception-row-indices exception-column-indices]}]
  (let [calc-fn (fn [i except-i n]
                  (cond (and (not i) (not except-i)) true
                        (not except-i) (if (< i n) i true)
                        (number? i) (if (number? except-i)
                                      (if (= except-i i) [] (if (< i n) i true))
                                      (if (contains? (set except-i) i) [] (if (< i n) i true)))
                        :else (let [i (or i (range n))]     ;i is a seq of indices
                                (if (number? except-i)
                                  (remove #(or (= except-i %) (>= % n)) i)
                                  (reduce
                                    (fn [tot e] (if (or (>= e n) (some #(= % e) except-i)) tot (conj tot e)))
                                    []
                                    i)))))
        rs (calc-fn row-indices exception-row-indices (row-count m))
        cs (calc-fn column-indices exception-column-indices (column-count m))]
    (cond
      (or (and (coll? rs) (empty? rs)) (and (coll? cs) (empty? cs))) [[]]
      (and (number? rs) (number? cs)) [[(get-in m [rs cs])]]
      (and (number? rs) (coll? cs)) (row-matrix (let [r (get m rs)] (map #(get r %) cs)))
      (and (number? rs) (true? cs)) (get-row-as-matrix m rs)
      (and (coll? rs) (number? cs)) (column-matrix (let [c (get-column m cs)] (map #(nth c %) rs)))
      (and (coll? rs) (coll? cs)) (map (fn [row] (reduce (fn [tot c] (conj tot (get row c))) [] cs))
                                       (map #(get m %) rs))
      (and (coll? rs) (true? cs)) (map #(get m %) rs)
      (and (true? rs) (number? cs)) (get-column-as-matrix m cs)
      (and (true? rs) (coll? cs)) (map (fn [row] (reduce (fn [tot c] (conj tot (get row c))) [] cs)) m)
      (and (true? rs) (true? cs)) m)))

(s/fdef get-slices-as-matrix
        :args (s/cat :m ::matrix
                     :args (s/keys :opt [::row-indices
                                         ::column-indices
                                         ::exception-row-indices
                                         ::exception-column-indices]))
        :ret ::matrix)

(defn matrix-partition
  "Returns a map containing the four sub-matrices labeled `::top-left`, `::bottom-left`, `::top-right`, and
  `::bottom-right`.
  `first-bottom-row` is the bottom of where the slice will occur.
  `first-right-column` is the right edge of where the slice will occur."
  [m first-bottom-row first-right-column]
  {::top-left-matrix     (get-slices-as-matrix m {::row-indices    (range first-bottom-row)
                                                  ::column-indices (range first-right-column)})
   ::bottom-left-matrix  (get-slices-as-matrix m {::exception-row-indices (range first-bottom-row)
                                                  ::column-indices        (range first-right-column)})
   ::top-right-matrix    (get-slices-as-matrix m {::row-indices              (range first-bottom-row)
                                                  ::exception-column-indices (range first-right-column)})
   ::bottom-right-matrix (get-slices-as-matrix m {::exception-row-indices    (range first-bottom-row)
                                                  ::exception-column-indices (range first-right-column)})})

(s/fdef matrix-partition
        :args (s/cat :m ::matrix :first-bottom-row ::row :first-right-column ::column)
        :ret (s/keys :req [::top-left-matrix ::bottom-left-matrix ::top-right-matrix ::bottom-right-matrix]))

;;;MATRIX MANIPULATION
(defn assoc-row
  "Sets a row in a matrix using the specified numbers."
  [m row numbers]
  (when (and (= (count numbers) (column-count m)) (<= row (row-count m)))
    (assoc m row (vec numbers))))

(s/fdef assoc-row
        :args (s/cat :m ::matrix :row ::row :numbers ::numbers)
        :ret (s/nilable ::matrix))

(defn assoc-column
  "Sets a column in a matrix using the specified numbers."
  [m column numbers]
  (vec (map-indexed (fn [row row-vector] (assoc row-vector column (get numbers row 0.0))) m)))

(s/fdef assoc-column
        :args (s/cat :m ::matrix :column ::column :numbers ::numbers)
        :ret (s/nilable ::matrix))

(defn insert-row
  "Inserts a row of `numbers` in a matrix at the specified `row`."
  [m row numbers]
  (when (<= row (count m))
    (let [f (subvec m 0 row)
          l (subvec m row)]
      (vec (concat f [numbers] l)))))

(s/fdef insert-row
        :args (s/cat :m ::matrix :row ::row :numbers ::numbers)
        :ret (s/nilable ::matrix))

(defn insert-column
  "Inserts a column of `numbers` in a matrix at the specified `column`."
  [m column numbers]
  (when (<= column (column-count m))
    (vec (map-indexed (fn [row row-vector] (insertv row-vector column (get numbers row 0.0))) m))))

(s/fdef insert-column
        :args (s/cat :m ::matrix :column ::column :numbers ::numbers)
        :ret (s/nilable ::matrix))

(defn remove-row
  "Removes a row in a matrix"
  [m row]
  (if (<= (inc row) (count m))
    (let [f (subvec m 0 row)
          l (subvec m (inc row))]
      (vec (concat f l)))
    m))

(s/fdef remove-row
        :args (s/cat :m ::matrix :row ::row)
        :ret ::matrix)

(defn remove-column
  "Removes a column in a matrix"
  [m column]
  (if (<= (inc column) (column-count m))
    (mapv #(removev % column) m)
    m))

(s/fdef remove-column
        :args (s/cat :m ::matrix :column ::column)
        :ret ::matrix)

(defn update-row
  "Updates a `row` of matrix `m`, using `f`, which is a function of the `column` and `number` and returns a number."
  ([m row f] (update m row f))
  ([m row f & args] (apply update m row f args)))

(s/fdef update-row
        :args (s/cat :m ::matrix
                     :row ::row
                     :f (s/fspec :args (s/cat :column ::column)
                                 :ret ::number)
                     :args (s/? (s/keys* :number ::number)))
        :ret ::matrix)

(defn update-column
  "Updates a `column` of matrix `m`, using `f`, which is a function of the `row` and `number` and returns a number."
  ([m column f] (vec (map-indexed (fn [row row-vector] (update row-vector column f row)) m)))
  ([m column f & args] (vec (map-indexed (fn [row row-vector] (apply update row-vector column f row args)) m))))

(s/fdef update-column
        :args (s/cat :m ::matrix
                     :column ::column
                     :f (s/fspec :args (s/cat :row ::row :number ::number)
                                 :ret ::number)
                     :args (s/? (s/keys* :number ::number)))
        :ret ::matrix)

(defn conj-rows
  "Appends rows from all the matrices or vectors after the first to the first.
  Each row must be the same size or will return nil."
  [mv & mvs]
  (let [m (if (vector? mv) (row-matrix mv) mv)
        ms (map #(if (vector? %) (row-matrix %) %) mvs)
        c (column-count m)
        cs (map column-count ms)]
    (when (every? #(= c %) cs)
      (reduce (fn [tot e] (apply conj tot e)) m ms))))

(s/fdef conj-rows
        :args (s/cat :mv ::matrix-or-vector :mvs (s/* ::matrix-or-vector))
        :ret (s/nilable ::matrix))

(defn conj-columns
  "Appends columns from all the matrices or vectors after the first to the first.
  Each column must be the same size or will return nil."
  [mv & mvs]
  (let [mt (transpose mv)
        mts (map transpose mvs)]
    (transpose (apply conj-rows mt mts))))

(s/fdef conj-columns
        :args (s/cat :mv ::matrix-or-vector :mvs (s/* ::matrix-or-vector))
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
                    (and (square-matrix? m1) (or (= nr2 (+ nc2 nr1)) (= nc2 (+ nr2 nr1))))))
                [m1 m2])]}
  (let [nr1 (row-count m1)
        m2 (if (vector? m2) (row-matrix m2) m2)
        c? (> (row-count m2) (column-count m2))
        k (if c? ::row-indices ::column-indices)
        k2 (if c? ::exception-row-indices ::exception-column-indices)
        m (get-slices-as-matrix m2 {k (range nr1)})
        mt (transpose m)
        br (get-slices-as-matrix m2 {k2 (range nr1)})
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
   'row-start' and 'column-start' can be negative.
   Unassigned elements will be 0.0"
  [m submatrix row-start column-start]
  (let [sr (row-count submatrix), sc (column-count submatrix), tr (+ sr row-start),
        tc (+ sc column-start), nr (row-count m), nc (column-count m)]
    (for [r (range (min row-start 0) (max tr nr))]
      (for [c (range (min column-start 0) (max tc nc))]
        (cond (and (>= r row-start) (< r tr) (>= c column-start)
                   (< c tc)) (get-in submatrix [(- r row-start) (- c column-start)])
              (and (m/non-? r) (< r nr) (m/non-? c)
                   (< c nr)) (get-in m [r c])
              :else 0.0)))))

(s/fdef replace-submatrix
        :args (s/cat :m ::matrix :submatrix ::matrix :row-start ::m/int :column-start ::m/int)
        :ret ::matrix)

(defn permute-matrix
  "Returns a Matrix with the rows and the columns of a matrix permuted.
    Options:
     :rows is the rowspec providing a seq listing the indices of the permutation.
     :cols is the colspec providing a seq listing the indices of the permutation."
  [m & {:keys [rows columns]}]
  {:pre [(have? matrix? m)]}
  (let [clx (clatrix m), p (clx/permute clx :r rows :c columns)]
    (coerce m p)))

;;;MATRIX MATH
(defn matrix-multiply
  ([] 1.0)
  ([m] m)
  ([m1 m2] (mapv (fn [a] (mapv (fn [b] (reduce + (map * a b))) (transpose m2))) m1))
  ([m1 m2 & ms] (apply matrix-multiply (matrix-multiply m1 m2) ms)))

(s/fdef matrix-multiply
        :args (s/or :zero-or-one (s/cat :m (s/? ::matrix))
                    :two+ (s/cat :m1 ::matrix :m2 ::matrix :ms (s/? (s/keys* :mats ::matrix)))))

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

(defn kronecker-product [m & ms]
  {:pre [(have? matrix? m) (have? (partial every? matrix?) ms)]}
  (coerce m (reduce (fn [a b]
                      (let [arows (row-count a), acols (column-count a)]
                        (apply conj-rows (for [i (range arows)]
                                           (apply conj-columns
                                                  (for [j (range acols)]
                                                    (multiply (get-in a [i j]) b)))))))
                    m ms)))

;;;SPECIAL-MATRIX INFO
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
        :args (s/cat :size ::size)
        :ret ::m/int-non-)

(defn ecount-symmetric-with-unit-diagonal
  "Returns the element count (`ecount`) for a symmetric matrix with a unit diagonal.
  This is the number of elements above or below the diagonal."
  [size] (/ (- (m/sq size) size) 2))

(s/fdef ecount-symmetric-with-unit-diagonal
        :args (s/cat :size ::size)
        :ret ::m/int-non-)

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
                     :args (s/? (s/keys :opt [::by-row?])))
        :ret (s/nilable ::m/number))

;;;SPARSE
(defn sparse-efilter     ;;these two sparse filters are how to create 'sparse' from matrix, default pred should be not= 0
  "Returns a vector of [row column value].
  pred takes an element"
  [m pred & {:keys [byrow?] :or {byrow? true}}]
  {:pre [(have? matrix? m)]}
  (ereduce-kv #(if (pred %4) (conj % [%2 %3 %4]) %) [] m byrow?))

(defn sparse-symmetric-efilter
  "Returns a vector of [row column value].
pred takes an element and will be evaluated only for upper-right or lower-left triangle of m."
  [m pred & {:keys [byrow?] :or {byrow? true}}]
  {:pre [(have? matrix? m)]}
  (let [f (if byrow? <= >=)]
    (ereduce-kv #(if (and (f %2 %3) (pred %4)) (conj % [%2 %3 %4]) %)
                [] m byrow?)))

(defn sparse-filter-by-row
  "Returns a vector of [row row-value].
  'pred' takes a row."
  [m pred]
  {:pre [(have? matrix? m)]}
  (reduce-kv #(if (pred %3) (conj % [%2 %3]) %) [] m))

(defn sparse-filter-by-column
  "Returns a vector of [column column-value].
  'pred' takes a column"
  [m pred]
  {:pre [(have? matrix? m)]}
  (reduce-kv #(if (pred %3) (conj % [%2 %3]) %) [] (transpose m)))

;;;MATRIX FILTERS
(defn filter-by-row
  "Returns a matrix.
  'pred' takes a row."
  [m pred] (filter pred m))

(s/fdef filter-by-row
        :args (s/cat :m ::matrix :pred (s/fspec :args (s/cat :row ::row) :ret boolean?))
        :ret ::matrix)

(defn filter-by-column
  "Returns a matrix.
  'pred' takes a column"
  [m pred]
  {:pre [(have? matrix? m)]}
  (transpose (filter pred (transpose m))))

(defn filter-symmetrically
  "Returns a matrix.
  'pred' takes a row or column"
  [m pred & {:keys [by-row?] :or {by-row? true}}]
  {:pre [(have? matrix? m)]}
  (let [ma (if by-row? m (transpose m)),
        keep-set (reduce-kv #(if (pred %3) (conj % %2) %) #{} ma)]
    (get-slices-as-matrix m {::row-indices keep-set, ::column-indices keep-set})))

;;;MATRIX NUMERICAL STABILITY
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
;      of the off-diagonal elements as necessary.
; Useful for rounding errors."
;  [m]
;  (symmetric-matrix
;    m (fn [r c]
;        (let [e (get-in m [r c])]
;          (if (= r c) e
;            (* (m/sgn e) (min (m/sqrt (* (get-in m [r r]) (get-in m [c c])))
;                              (m/abs e))))))
;    (row-count m) true))

;;;MATRIX DECOMPOSITION
(defn inverse
  "Computes the inverse of a number, vector, or matrix.
   For Clatrix:
      this is done via Gaussian elmination.
      It can be numerically very unstable if the matrix is nearly singular.
      Positivity and symmetry hints are used to cause `solve` to use optimized
         LAPACK routines."
  [m]
  {:pre [(have? [:or number? square-matrix? numbers?] m)]}
  (cond (number? m) (m/div m)
        (numbers? m) (compute-vector m (emap m/div m))
        (apache-commons? m) (mxc/inverse m)
        :else (coerce m (clx/i (clx/maybe-positive          ;;looks like Clatrix may have been faster than Apache
                                 (clx/maybe-symmetric (clatrix m)))))))

(defn cholesky-decomposition
  "Computes the Cholesky decomposition of a matrix.
   Returns a vector containing two matrices [L U].
   Intended usage: (let [[L U] (cholesky-decomosition M)] ....).
   This is the Cholesky square root of a matrix, L such that (mmul L U) = m
   Note that m must be positive (semi) definite for this to exist,
      but `cholesky-decomposition` requires strict positivity."
  [m]
  {:pre [(have? square-matrix? m)]}
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
  {:pre [(have? square-matrix? m)]}
  (let [r (RectangularCholeskyDecomposition. (apache-commons m) accu)]
    {:B (coerce m (.getRootMatrix r)), :rank (.getRank r)}))

(defn cholesky-decomposition-semi-definite
  "Returns a vector containing two matrices [L L*],
      where 'm' may have zero (or close to zero) rows"
  [m ^double accu]
  (if (positive-matrix? m) (cholesky-decomposition m)
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
  "The `singular-values-matrix` is the diagonal matrix of singular values from an SVD decomposition.
  Returns the norm2 condition number,
  which is the maximum element value from the singular-values-matrix divided by the minimum element value."
  [singular-values-matrix]
  (let [vs (flatten singular-values-matrix)]
    (m/div (apply max vs) (apply min vs) nil)))

(s/fdef condition
        :args (s/cat :singular-values-matrix ::matrix)
        :ret (s/nilable ::number))

(defn lu-decomposition-with-permutation-matrix
  "Returns a map containing:
      :L -- the lower triangular factor
      :U -- the upper triangular factor
      :P -- the permutation matrix"
  [m]
  {:pre [(have? square-matrix? m)]}
  (cond (apache-commons? m) (let [r (LUDecomposition. m)]
                              {:L (.getL r), :U (.getU r), :P (.getP r)})
        :else (let [r (clx/lu (clatrix m))]
                {:L (coerce m (:l r)), :U (coerce m (:u r)),
                 :P (coerce m (:p r))})))

;;could instead be calculated from [[lu-decomposition-with-permutation-matrix]]
;;probably better to use apache (or another library)
(defn determinant
  "Calculates the determinant of a square matrix."
  [square-m] (mxc/det (apache-commons square-m)))

(s/fdef determinant
        :args (s/cat :square-m ::square-matrix)
        :ret ::number)

(defn lu-decomposition
  "Computes the LU decomposition of a matrix.
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
  {:pre [(have? square-matrix? m)]}
  (cond (diagonal-matrix? m) (sort (diagonal m))
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
  {:pre [(have? square-matrix? m)]}
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
    (let [ri (inverse r), e (matrix-multiply ri (transpose ri)),
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

;;;RANDOM -- move these to vector/matrix sections?  Or to rnd library?
(defn rnd-vector
  "Returns [v rnd-lazy], where v has random elements"
  [size rnd-lazy] [(into [] (take size rnd-lazy)) (drop size rnd-lazy)])

(s/fdef rnd-vector
        :args (s/cat :size ::size)                          ;we don't use rnd-lazy any more
        :ret ::vector)

(defn rnd-matrix
  "Returns [m rnd-lazy], where m has random elements"
  [rows ^long columns rnd-lazy]
  (let [[v s] (rnd-vector (* rows columns) rnd-lazy)]
    [(partition columns v) s]))

(defn rnd-reflection-matrix
  "Returns [m rnd-lazy], where m is a random Householder reflection."
  [size rnd-lazy]
  (let [v (column-matrix (normalise (take size rnd-lazy)))]
    [(subtract (identity-matrix size) (matrix-multiply (matrix-multiply v (transpose v)) 2.0))
     (drop size rnd-lazy)]))

(defn rnd-spectral-matrix
  "Returns [m rnd-lazy], where m is a random matrix with a particular
      spectrum vector.
The orthogonal matrices are generated by using 2 * spectrum-length composed
   Householder reflections."
  [spectrum rnd-lazy]
  (let [size (count spectrum),
        [v-mat r] (nth (iterate (fn [[prod-mat laz]]
                                  (let [[r-mat s] (rnd-reflection-matrix
                                                    size laz)]
                                    [(matrix-multiply prod-mat r-mat) s]))
                                [(identity-matrix size) rnd-lazy])
                       (* 2 size))
        l-mat (diagonal-matrix spectrum)]
    [(-> v-mat (matrix-multiply l-mat) (matrix-multiply (transpose v-mat)))
     r]))

(defn rnd-positive-matrix
  "Returns [m rnd-lazy], where m is a positive definite matrix with a random
      spectrum.
The orthogonal matrices are generated by using 2 * size composed Householder
   reflections.
Alternative #1: Sample from the Inverse-Wishart Distribution.
Alternative #2: (let [[m s] (rnd-matrix size size rnd-lazy)]
                   [(mmul (transpose m) m), s])"
  [size rnd-lazy] (rnd-spectral-matrix (take size rnd-lazy) (drop size rnd-lazy)))