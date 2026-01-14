(ns provisdom.math.matrix
  "Matrix operations and creation utilities.

  Provides comprehensive matrix functionality including:
  - Matrix creation (identity, diagonal, Toeplitz, random)
  - Specialized types (symmetric, triangular, sparse)
  - Matrix multiplication, transpose, Kronecker product
  - Slicing, filtering, and partitioning
  - Serialization/deserialization of triangular matrices
  - Row/column manipulation (insert, remove, update)
  - Element-wise matrix math and rounding

  Matrices are represented as vectors of vectors with consistent row lengths.
  Supports both dense and sparse representations for memory efficiency.

  For linear algebra operations (decompositions, solve, inverse, etc.),
  see provisdom.math.linear-algebra."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.math.core :as m]
    [provisdom.math.random :as random]
    [provisdom.math.tensor :as tensor]
    [provisdom.math.vector :as vector]
    [provisdom.utility-belt.anomalies :as anom]
    [provisdom.utility-belt.extensions :as extensions]))

;;;DECLARATIONS
(declare assoc-diagonal column-matrix column-matrix? columns constant-matrix
  deserialize-lower-triangular-matrix deserialize-symmetric-matrix
  deserialize-upper-triangular-matrix diagonal diagonal-matrix diagonal-matrix?
  ecount-of-symmetric-or-triangular-matrix ecount-of-symmetric-or-triangular-matrix-without-diag
  get-slices-as-matrix lower-triangular-matrix? matrix-finite-non-? matrix-finite? matrix-prob?
  matrix? mx* row-matrix row-matrix? rows size-of-symmetric-or-triangular-matrix
  size-of-symmetric-or-triangular-matrix-without-diag some-kv square-matrix?
  symmetric-matrix-by-averaging symmetric-matrix? to-matrix transpose upper-triangular-matrix?)

(def mdl 6)                                                 ;max-dim-length for generators

(s/def ::by-row? boolean?)

(s/def ::row-indices
  (s/or :index ::tensor/index
    :indices ::tensor/indices))

(s/def ::column-indices
  (s/or :index ::tensor/index
    :indices ::tensor/indices))

(s/def ::exception-row-indices
  (s/or :index ::tensor/index
    :indices ::tensor/indices))

(s/def ::exception-column-indices
  (s/or :index ::tensor/index
    :indices ::tensor/indices))

(s/def ::row
  (s/with-gen ::m/int-non- #(gen/large-integer* {:min 0 :max mdl})))

(s/def ::column
  (s/with-gen ::m/int-non- #(gen/large-integer* {:min 0 :max mdl})))

(s/def ::rows
  (s/with-gen ::m/int-non- #(gen/large-integer* {:min 0 :max mdl})))

(s/def ::columns
  (s/with-gen ::m/int-non- #(gen/large-integer* {:min 0 :max mdl})))

(s/def ::row-start
  (s/with-gen ::m/int #(gen/large-integer* {:min (- mdl) :max mdl})))

(s/def ::column-start
  (s/with-gen ::m/int #(gen/large-integer* {:min (- mdl) :max mdl})))

(s/def ::sparse-matrix
  (s/with-gen
    (s/coll-of (s/tuple ::m/int-non- ::m/int-non- ::m/number)
      :kind vector?
      :into [])
    #(gen/bind
       (gen/tuple (gen/large-integer* {:min 0 :max mdl})
         (gen/large-integer* {:min 0 :max mdl})
         (gen/large-integer* {:min 0 :max mdl})
         (s/gen boolean?))
       (fn [[i j k tf?]]
         (let [[a b c] (sort [i j k])
               [b c] (if tf?
                       [b c]
                       [c b])]
           (gen/vector
             (gen/tuple (gen/large-integer* {:min 0 :max (max 0 (dec c))})
               (gen/large-integer* {:min 0 :max (max 0 (dec b))})
               (s/gen ::m/number))
             a))))))

(s/def ::column->number
  (s/fspec :args (s/cat :column ::column)
    :ret ::m/number))

(s/def ::row->number
  (s/fspec :args (s/cat :row ::row)
    :ret ::m/number))

(s/def ::number->bool
  (s/fspec :args (s/cat :number ::m/number)
    :ret boolean?))

(s/def ::matrix
  (s/with-gen
    #(matrix? %)
    #(gen/bind (gen/large-integer* {:min 0 :max mdl})
       (fn [i]
         (gen/vector
           (gen/vector (s/gen ::m/number) i)
           1
           mdl)))))

(s/def ::matrix-finite
  (s/with-gen
    #(matrix-finite? %)
    #(gen/bind (gen/large-integer* {:min 0 :max mdl})
       (fn [i]
         (gen/vector
           (gen/vector (s/gen ::m/finite) i)
           1
           mdl)))))

(s/def ::matrix-finite-non-
  (s/with-gen
    #(matrix-finite-non-? %)
    #(gen/bind (gen/large-integer* {:min 0 :max mdl})
       (fn [i]
         (gen/vector
           (gen/vector (s/gen ::m/finite-non-) i)
           1
           mdl)))))

(s/def ::matrix-prob
  (s/with-gen
    #(matrix-prob? %)
    #(gen/bind (gen/large-integer* {:min 0 :max mdl})
       (fn [i]
         (gen/vector
           (gen/vector (s/gen ::m/prob) i)
           1
           mdl)))))

(s/def ::empty-matrix #(= [[]] %))

(s/def ::row-matrix
  (s/with-gen
    #(row-matrix? %)
    #(gen/fmap row-matrix (s/gen ::vector/vector))))

(s/def ::column-matrix
  (s/with-gen
    #(column-matrix? %)
    #(gen/fmap column-matrix (s/gen ::vector/vector))))

(s/def ::zero-matrix
  (s/with-gen
    (s/coll-of (s/coll-of zero? :kind vector? :into [])
      :kind vector?
      :into [])
    #(gen/vector (gen/vector (s/gen zero?) 0 mdl) 1 mdl)))

(s/def ::square-matrix
  (s/with-gen
    #(square-matrix? %)
    #(gen/bind (gen/large-integer* {:min 0 :max mdl})
       (fn [i]
         (gen/vector
           (gen/vector (s/gen ::m/number) i)
           (max 1 i))))))

(s/def ::square-matrix-finite
  (s/with-gen
    (s/and (s/coll-of
             (s/coll-of ::m/finite :kind vector? :into [])
             :min-count 1
             :kind vector?
             :into [])
      (fn [m] (= (rows m) (columns m))))
    #(gen/bind (gen/large-integer* {:min 0 :max mdl})
       (fn [i]
         (gen/vector
           (gen/vector (s/gen ::m/finite) i)
           (max 1 i))))))

(s/def ::diagonal-matrix
  (s/with-gen
    #(diagonal-matrix? %)
    #(gen/fmap diagonal-matrix (s/gen ::vector/vector))))

(s/def ::upper-triangular-matrix
  (s/with-gen
    #(upper-triangular-matrix? %)
    #(gen/bind (gen/large-integer* {:min 0 :max mdl})
       (fn [i]
         (gen/fmap deserialize-upper-triangular-matrix
           (gen/vector
             (s/gen ::m/number)
             (ecount-of-symmetric-or-triangular-matrix i)))))))

(s/def ::lower-triangular-matrix
  (s/with-gen
    #(lower-triangular-matrix? %)
    #(gen/bind (gen/large-integer* {:min 0 :max mdl})
       (fn [i]
         (gen/fmap deserialize-lower-triangular-matrix
           (gen/vector
             (s/gen ::m/number)
             (ecount-of-symmetric-or-triangular-matrix i)))))))

(s/def ::symmetric-matrix
  (s/with-gen
    #(symmetric-matrix? %)
    #(gen/bind (gen/large-integer* {:min 0 :max mdl})
       (fn [i]
         (gen/fmap deserialize-symmetric-matrix
           (gen/vector
             (s/gen ::m/number)
             (ecount-of-symmetric-or-triangular-matrix i)))))))

(s/def ::top-left ::matrix)
(s/def ::bottom-left ::matrix)
(s/def ::top-right ::matrix)
(s/def ::bottom-right ::matrix)

;;;MATRIX TYPES
(defn matrix?
  "Returns `true` if `x` is a valid matrix.

  A valid matrix is a vector of vectors where:
  - All rows have equal length (including zero-length rows)
  - All elements are numbers
  - Empty matrix `[[]]` is valid
  - Single empty row with multiple rows `[[] []]` is invalid

  Examples:
    (matrix? [[1 2] [3 4]]) ;=> true
    (matrix? [[]]) ;=> true
    (matrix? [[] []]) ;=> false"
  [x]
  (and (vector? x)
    (vector? (first x))
    (not (and (empty? (first x)) (> (count x) 1)))
    (every? #(and (vector? %)
               (= (count %) (count (first x)))
               (every? number? %))
      x)))

(s/fdef matrix?
  :args (s/cat :x any?)
  :ret boolean?)

(defn matrix-num?
  "Returns `true` if `x` is a valid matrix containing only non-NaN numbers.

  Similar to [[matrix?]] but excludes NaN values. Accepts infinite values.

  Examples:
    (matrix-num? [[1 2] [3 4]]) ;=> true
    (matrix-num? [[1 ##NaN]]) ;=> false
    (matrix-num? [[1 ##Inf]]) ;=> true"
  [x]
  (and (vector? x)
    (vector? (first x))
    (not (and (empty? (first x)) (> (count x) 1)))
    (every? #(and (vector? %)
               (= (count %) (count (first x)))
               (every? m/num? %))
      x)))

(s/fdef matrix-num?
  :args (s/cat :x any?)
  :ret boolean?)

(s/def ::matrix-num
  (s/with-gen
    matrix-num?
    #(gen/bind (gen/large-integer* {:min 0 :max mdl})
       (fn [i]
         (gen/vector
           (gen/vector (s/gen ::m/num) i)
           1
           mdl)))))

(defn matrix-finite?
  "Returns `true` if `x` is a valid matrix containing only finite numbers.

  Excludes both NaN and infinite values.

  Examples:
    (matrix-finite? [[1 2] [3 4]]) ;=> true
    (matrix-finite? [[1 ##Inf]]) ;=> false
    (matrix-finite? [[1 ##NaN]]) ;=> false"
  [x]
  (and (vector? x)
    (vector? (first x))
    (not (and (empty? (first x)) (> (count x) 1)))
    (every? #(and (vector? %)
               (= (count %) (count (first x)))
               (every? m/finite? %))
      x)))

(s/fdef matrix-finite?
  :args (s/cat :x any?)
  :ret boolean?)

(defn all-matrix-values?
  "Returns `true` if all matrix elements are within the specified bounds.

  Checks that every element e satisfies `m1` <= e <= `m2`.

  Examples:
    (all-matrix-values? [[1 2] [3 4]] 0 5) ;=> true
    (all-matrix-values? [[1 2] [3 4]] 2 4) ;=> false"
  [x m1 m2]
  (every?
    #(every? (fn [e] (and (>= e m1) (<= e m2))) %)
    x))

(defn matrix-size?
  "Returns `true` if the matrix has exactly the specified dimensions.

  Examples:
    (matrix-size? [[1 2] [3 4]] 2 2) ;=> true
    (matrix-size? [[1 2 3]] 1 3) ;=> true
    (matrix-size? [[1 2]] 2 2) ;=> false"
  [x row-count column-count]
  (and (= (rows x) row-count)
    (= (columns x) column-count)))

(defn matrix-min-size?
  "Returns `true` if the matrix has at least the specified dimensions.

  Examples:
    (matrix-min-size? [[1 2] [3 4]] 2 2) ;=> true
    (matrix-min-size? [[1 2] [3 4]] 1 1) ;=> true
    (matrix-min-size? [[1 2]] 2 2) ;=> false"
  [x min-rows min-columns]
  (and (>= (rows x) min-rows)
    (>= (columns x) min-columns)))

(defmacro matrix-finite-spec
  [{m1          :min
    m2          :max
    min-columns :min-columns
    min-rows    :min-rows
    :or         {m1          m/min-dbl
                 m2          m/max-dbl
                 min-columns 1
                 min-rows    0}}]
  (let [columns-sym (gensym "columns")
        rows-sym (gensym "rows")]
    `(s/with-gen
       #(and (matrix-finite? %)
          (all-matrix-values? % ~m1 ~m2)
          (matrix-min-size? % ~min-rows ~min-columns))
       #(gen/bind (gen/large-integer* {:min ~min-columns :max (+ ~min-columns mdl)})
          (fn [~columns-sym]
            (gen/bind (gen/large-integer* {:min ~min-rows :max (+ ~min-rows mdl)})
              (fn [~rows-sym]
                (gen/vector
                  (gen/vector (s/gen (m/finite-spec {:min ~m1 :max ~m2})) ~columns-sym)
                  ~rows-sym))))))))

(defmacro sized-matrix-finite-spec
  [{m1           :min
    m2           :max
    column-count :column-count
    row-count    :row-count
    :or          {m1           m/min-dbl
                  m2           m/max-dbl
                  column-count 1
                  row-count    1}}]
  `(s/with-gen
     #(and (matrix-finite? %)
        (all-matrix-values? % ~m1 ~m2)
        (matrix-size? % ~row-count ~column-count))
     #(gen/vector
        (gen/vector (s/gen (m/finite-spec {:min ~m1 :max ~m2})) ~column-count)
        ~row-count)))

(defn matrix-finite-non-?
  "Returns `true` if `x` is a valid matrix of finite non-negative numbers.

  All elements must be finite and >= 0.

  Examples:
    (matrix-finite-non-? [[0 1] [2 3]]) ;=> true
    (matrix-finite-non-? [[1 -1]]) ;=> false
    (matrix-finite-non-? [[1 ##Inf]]) ;=> false"
  [x]
  (and (vector? x)
    (vector? (first x))
    (not (and (empty? (first x)) (> (count x) 1)))
    (every? #(and (vector? %)
               (= (count %) (count (first x)))
               (every? m/finite-non-? %))
      x)))

(s/fdef matrix-finite-non-?
  :args (s/cat :x any?)
  :ret boolean?)

(defn matrix-prob?
  "Returns `true` if `x` is a valid matrix of probability values.

  All elements must be finite numbers between 0 and 1 inclusive.

  Examples:
    (matrix-prob? [[0 0.5] [0.8 1]]) ;=> true
    (matrix-prob? [[0.5 1.2]]) ;=> false
    (matrix-prob? [[-0.1 0.5]]) ;=> false"
  [x]
  (and (vector? x)
    (vector? (first x))
    (not (and (empty? (first x)) (> (count x) 1)))
    (every? #(and (vector? %)
               (= (count %) (count (first x)))
               (every? m/prob? %))
      x)))

(s/fdef matrix-prob?
  :args (s/cat :x any?)
  :ret boolean?)

(defn empty-matrix?
  "Returns `true` if `x` is an empty matrix.

  An empty matrix is represented as `[[]]` - a vector containing one empty vector.

  Examples:
    (empty-matrix? [[]]) ;=> true
    (empty-matrix? []) ;=> false
    (empty-matrix? [[1]]) ;=> false"
  [x]
  (and (matrix? x) (empty? (get x 0))))

(s/fdef empty-matrix?
  :args (s/cat :x any?)
  :ret boolean?)

(defn row-matrix?
  "Returns `true` if `x` is a single-row matrix.

  A row matrix has exactly one row containing only numbers, or is an empty matrix.

  Examples:
    (row-matrix? [[1 2 3]]) ;=> true
    (row-matrix? [[1] [2]]) ;=> false
    (row-matrix? [[]]) ;=> true"
  [x]
  (or (empty-matrix? x)
    (and (vector? x)
      (m/one? (count x))
      (vector? (first x))
      (every? number? (first x)))))

(s/fdef row-matrix?
  :args (s/cat :x any?)
  :ret boolean?)

(defn column-matrix?
  "Returns `true` if `x` is a single-column matrix.

  A column matrix has exactly one column (all rows have length 1), or is an empty matrix.

  Examples:
    (column-matrix? [[1] [2] [3]]) ;=> true
    (column-matrix? [[1 2]]) ;=> false
    (column-matrix? [[]]) ;=> true"
  [x]
  (or (empty-matrix? x)
    (and (vector? x)
      (every? #(and (vector? %)
                 (= (count %) 1)
                 (every? number? %))
        x))))

(s/fdef column-matrix?
  :args (s/cat :x any?)
  :ret boolean?)

(defn zero-matrix?
  "Returns `true` if `x` is a valid matrix with all elements equal to zero.

  Examples:
    (zero-matrix? [[0 0] [0 0]]) ;=> true
    (zero-matrix? [[0.0 0] [0 0.0]]) ;=> true
    (zero-matrix? [[0 1]]) ;=> false"
  [x]
  (and (matrix? x) (every? zero? (flatten x))))

(s/fdef zero-matrix?
  :args (s/cat :x any?)
  :ret boolean?)

(defn square-matrix?
  "Returns `true` if `x` is a square matrix.

  A square matrix has the same number of rows and columns.

  Examples:
    (square-matrix? [[1 2] [3 4]]) ;=> true
    (square-matrix? [[1]]) ;=> true
    (square-matrix? [[1 2 3]]) ;=> false
    (square-matrix? [[]]) ;=> true"
  [x]
  (and (matrix? x) (= (rows x) (columns x))))

(s/fdef square-matrix?
  :args (s/cat :x any?)
  :ret boolean?)

(defn diagonal-matrix?
  "Returns `true` if `x` is a diagonal matrix.

  A diagonal matrix has zeros for all off-diagonal elements. Elements on the main diagonal can be
  any value.

  Examples:
    (diagonal-matrix? [[1 0] [0 2]]) ;=> true
    (diagonal-matrix? [[1 0] [0 0]]) ;=> true
    (diagonal-matrix? [[1 2] [0 3]]) ;=> false"
  [x]
  (and (matrix? x)
    (nil? (some-kv (fn [i j e]
                     (not (or (= i j) (zero? e))))
            x))))

(s/fdef diagonal-matrix?
  :args (s/cat :x any?)
  :ret boolean?)

(defn upper-triangular-matrix?
  "Returns `true` if `x` is an upper triangular matrix.

  An upper triangular matrix is square with all entries below the main diagonal equal to zero.
  Elements on or above the diagonal can be any value.

  Examples:
    (upper-triangular-matrix? [[1 2] [0 3]]) ;=> true
    (upper-triangular-matrix? [[1 0] [0 2]]) ;=> true
    (upper-triangular-matrix? [[1 2] [3 4]]) ;=> false"
  [x]
  (and (square-matrix? x)
    (nil? (some-kv (fn [i j e]
                     (not (or (<= i j) (m/roughly? e 0.0 m/sgl-close))))
            x))))

(s/fdef upper-triangular-matrix?
  :args (s/cat :x any?)
  :ret boolean?)

(defn lower-triangular-matrix?
  "Returns `true` if `x` is a lower triangular matrix.

  A lower triangular matrix is square with all entries above the main diagonal equal to zero.
  Elements on or below the diagonal can be any value.

  Examples:
    (lower-triangular-matrix? [[1 0] [2 3]]) ;=> true
    (lower-triangular-matrix? [[1 0] [0 2]]) ;=> true
    (lower-triangular-matrix? [[1 2] [3 4]]) ;=> false"
  [x]
  (and (square-matrix? x)
    (nil? (some-kv (fn [i j e]
                     (not (or (>= i j) (m/roughly? e 0.0 m/sgl-close))))
            x))))

(s/fdef lower-triangular-matrix?
  :args (s/cat :x any?)
  :ret boolean?)

(defn symmetric-matrix?
  "Returns `true` if `x` is a symmetric matrix.

  A symmetric matrix is square and equal to its [[transpose]]. Element at `[i,j]` equals element
  at `[j,i]` for all positions.

  Examples:
    (symmetric-matrix? [[1 2] [2 3]]) ;=> true
    (symmetric-matrix? [[1 2] [3 4]]) ;=> false
    (symmetric-matrix? [[5]]) ;=> true"
  [x]
  (and (square-matrix? x) (tensor/=== (transpose x) x)))

(s/fdef symmetric-matrix?
  :args (s/cat :x any?)
  :ret boolean?)

;;;MATRIX CONSTRUCTORS
(defn to-matrix
  "Converts a tensor into a matrix with the specified number of rows.

  Flattens the tensor and reshapes it into a matrix. If tensor has fewer elements than needed,
  pads with zeros. Elements are filled by row (default) or by column based on `::by-row?` option.

  Examples:
    (to-matrix [1 2 3 4] 2) ;=> [[1 2] [3 4]]
    (to-matrix [1 2 3] 2) ;=> [[1 2] [3 0.0]]
    (to-matrix [1 2 3 4] 2 {::by-row? false}) ;=> [[1 3] [2 4]]"
  ([tensor rows] (to-matrix tensor rows {::by-row? true}))
  ([tensor rows {::keys [by-row?] :or {by-row? true}}]
   (if (zero? rows)
     [[]]
     (let [coll (if (number? tensor)
                  [tensor]
                  (vec (flatten tensor)))
           c (count coll)
           [columns r] (m/quot-and-mod' c rows)
           [columns r] (if (zero? r)
                         [columns 0]
                         [(inc columns) (- rows r)])
           coll (concat coll (repeat r 0.0))]
       (if (zero? columns)
         [[]]
         (if by-row?
           (mapv vec (partition columns coll))
           (transpose (mapv vec (partition rows coll)))))))))

(s/fdef to-matrix
  :args (s/cat :tensor ::tensor/tensor
          :rows ::rows
          :opts (s/? (s/keys :opt [::by-row?])))
  :ret ::matrix)

(defn constant-matrix
  "Creates a matrix filled with a constant value.

  All elements are set to the specified value (default `0.0`).

  Examples:
    (constant-matrix 2 3) ;=> [[0.0 0.0 0.0] [0.0 0.0 0.0]]
    (constant-matrix 2 2 5) ;=> [[5 5] [5 5]]"
  ([rows columns] (constant-matrix rows columns 0.0))
  ([rows columns number]
   (if (or (zero? columns) (zero? rows))
     [[]]
     (vec (repeat rows (vec (repeat columns number)))))))

(s/fdef constant-matrix
  :args (s/cat :rows ::rows
          :columns ::columns
          :number (s/? ::m/number))
  :ret ::matrix)

(defn compute-matrix
  "Creates a matrix by computing each element using a function.

  The function `row+column->number` is called with row and column indices (0-based) and should
  return the value for that position.

  Examples:
    (compute-matrix 2 3 +) ;=> [[0 1 2] [1 2 3]]
    (compute-matrix 3 3 #(if (= %1 %2) 1 0)) ;=> identity matrix"
  [rows columns row+column->number]
  (if (zero? (* rows columns))
    [[]]
    (mapv (fn [row]
            (mapv (fn [column]
                    (row+column->number row column))
              (range columns)))
      (range rows))))

(s/fdef compute-matrix
  :args (s/cat :rows ::rows
          :columns ::columns
          :row+column->number (s/fspec :args (s/cat :row ::row
                                               :column ::column)
                                :ret ::m/number))
  :ret ::matrix)

(defn identity-matrix
  "Creates an identity matrix of the specified size.

  An identity matrix has `1.0` on the main diagonal and `0.0` elsewhere.

  Examples:
    (identity-matrix 2) ;=> [[1.0 0.0] [0.0 1.0]]
    (identity-matrix 0) ;=> [[]]"
  [size]
  (compute-matrix size size (fn [row column] (if (= row column) 1.0 0.0))))

(s/fdef identity-matrix
  :args (s/cat :size ::vector/size)
  :ret ::diagonal-matrix)

(defn row-matrix
  "Creates a single-row matrix from a sequence of numbers or a generator function.

  Two arities:
  - `(row-matrix numbers)` - converts sequence to single row
  - `(row-matrix size fn)` - generates row using function of column index

  Examples:
    (row-matrix [1 2 3]) ;=> [[1 2 3]]
    (row-matrix 3 #(* % %)) ;=> [[0 1 4]]"
  ([numbers] [(vec numbers)])
  ([size column->number] [(vector/compute-vector size column->number)]))

(s/fdef row-matrix
  :args (s/or :one (s/cat :numbers ::m/numbers)
          :two (s/cat :size ::vector/size :column->number ::column->number))
  :ret ::row-matrix)

(defn column-matrix
  "Creates a single-column matrix from a sequence of numbers or a generator function.

  Two arities:
  - `(column-matrix numbers)` - converts sequence to single column
  - `(column-matrix size fn)` - generates column using function of row index

  Examples:
    (column-matrix [1 2 3]) ;=> [[1] [2] [3]]
    (column-matrix 3 #(* % %)) ;=> [[0] [1] [4]]"
  ([numbers]
   (if (empty? numbers)
     [[]]
     (mapv vec (partition 1 numbers))))
  ([size row->number]
   (if (zero? size)
     [[]]
     (mapv vec (partition 1 (vector/compute-vector size row->number))))))

(s/fdef column-matrix
  :args (s/or :one (s/cat :numbers ::m/numbers)
          :two (s/cat :size ::vector/size :row->number ::row->number))
  :ret ::column-matrix)

(defn diagonal-matrix
  "Creates a diagonal matrix with specified diagonal values.

  Multiple arities:
  - `(diagonal-matrix values)` - creates square matrix with given diagonal
  - `(diagonal-matrix size fn)` - generates diagonal using function of index
  - `(diagonal-matrix rows cols fn)` - creates rectangular diagonal matrix

  All off-diagonal elements are `0.0`.

  Examples:
    (diagonal-matrix [1 2 3]) ;=> [[1.0 0.0 0.0] [0.0 2.0 0.0] [0.0 0.0 3.0]]
    (diagonal-matrix 3 inc) ;=> [[1.0 0.0 0.0] [0.0 2.0 0.0] [0.0 0.0 3.0]]"
  ([diagonal-numbers]
   (let [d (vec diagonal-numbers)
         c (count d)]
     (compute-matrix c c (fn [r c]
                           (if (= r c)
                             (get d r 0.0)
                             0.0)))))
  ([size index->number]
   (compute-matrix size size (fn [r c]
                               (if (= r c)
                                 (index->number r)
                                 0.0))))
  ([rows columns index->number]
   (compute-matrix rows columns (fn [r c]
                                  (if (= r c)
                                    (index->number r)
                                    0.0)))))

(s/fdef diagonal-matrix
  :args (s/or :one (s/cat :diagonal-numbers ::m/numbers)
          :two (s/cat :size ::vector/size
                 :index->number ::vector/index->number)
          :three (s/cat :rows ::rows
                   :columns ::columns
                   :index->number ::vector/index->number))
  :ret ::diagonal-matrix)

(defn- symmetric-row-fill
  [r c size numbers]
  (get numbers
    (long (+ c
             (* r size)
             (* -0.5 (+ r (m/sq' r)))))
    m/nan))

(defn- symmetric-column-fill
  [r c numbers]
  (get numbers
    (long (+ r (* 0.5 c (inc c))))
    m/nan))

(defn- symmetric-without-diagonal-row-fill
  [r c size numbers]
  (get numbers
    (long (+ c
             (* r size)
             (* -0.5 (+ (inc r) (m/sq' (inc r))))))
    m/nan))

(defn- symmetric-without-diagonal-column-fill
  [r c numbers]
  (get numbers
    (long (+ r (* 0.5 c (dec c))))
    m/nan))

(defn deserialize-upper-triangular-matrix
  "Reconstructs an upper triangular matrix from a flattened representation.

  Takes a vector of numbers representing the upper triangle (including diagonal) and reconstructs
  the full square matrix with zeros below the diagonal.

  Options:
  - `::by-row?` (default `true`) - whether numbers are ordered by row or column

  Can also take separate diagonal and off-diagonal vectors.

  Examples:
    (deserialize-upper-triangular-matrix [1 2 3]) ;=> [[1.0 2.0] [0.0 3.0]]"
  ([numbers] (deserialize-upper-triangular-matrix numbers {::by-row? true}))
  ([numbers {::keys [by-row?] :or {by-row? true}}]
   (let [size (size-of-symmetric-or-triangular-matrix (count numbers))
         f (fn [r c]
             (if (> r c)
               0.0
               (if by-row?
                 (symmetric-row-fill r c size numbers)
                 (symmetric-column-fill r c numbers))))]
     (when-not (m/nan? size) (compute-matrix size size f))))
  ([diagonal-numbers off-diagonal-numbers {::keys [by-row?] :or {by-row? true}}]
   (let [size (size-of-symmetric-or-triangular-matrix-without-diag
                (count off-diagonal-numbers))
         f (fn [r c]
             (cond (> r c) 0.0
               (= r c) (get diagonal-numbers r 0.0)

               :else
               (if by-row?
                 (symmetric-without-diagonal-row-fill
                   r c size off-diagonal-numbers)
                 (symmetric-without-diagonal-column-fill
                   r c off-diagonal-numbers))))]
     (when-not (m/nan? size) (compute-matrix size size f)))))

(s/fdef deserialize-upper-triangular-matrix
  :args (s/or :one-two (s/cat :numbers ::m/numbers
                         :opts (s/? (s/keys :opt [::by-row?])))
          :three (s/cat :diagonal-numbers ::m/numbers
                   :off-diagonal-numbers ::m/numbers
                   :opts (s/keys :opt [::by-row?])))
  :ret (s/nilable ::upper-triangular-matrix))

(defn deserialize-lower-triangular-matrix
  "Reconstructs a lower triangular matrix from a flattened representation.

  Takes a vector of numbers representing the lower triangle (including diagonal) and reconstructs
  the full square matrix with zeros above the diagonal.

  Options:
  - `::by-row?` (default `true`) - whether numbers are ordered by row or column

  Can also take separate diagonal and off-diagonal vectors.

  Examples:
    (deserialize-lower-triangular-matrix [1 2 3]) ;=> [[1.0 0.0] [2.0 3.0]]"
  ([numbers] (deserialize-lower-triangular-matrix numbers {::by-row? true}))
  ([numbers {::keys [by-row?] :or {by-row? true}}]
   (let [size (size-of-symmetric-or-triangular-matrix (count numbers))
         f (fn [r c]
             (if (< r c)
               0.0
               (if by-row?
                 (symmetric-column-fill c r numbers)
                 (symmetric-row-fill c r size numbers))))]
     (when-not (m/nan? size) (compute-matrix size size f))))
  ([diagonal-numbers off-diagonal-numbers {::keys [by-row?] :or {by-row? true}}]
   (let [size (size-of-symmetric-or-triangular-matrix-without-diag
                (count off-diagonal-numbers))
         f (fn [r c]
             (cond (< r c) 0.0
               (= r c) (get diagonal-numbers r 0.0)

               :else
               (if by-row?
                 (symmetric-without-diagonal-column-fill
                   c r off-diagonal-numbers)
                 (symmetric-without-diagonal-row-fill
                   c r size off-diagonal-numbers))))]
     (when-not (m/nan? size) (compute-matrix size size f)))))

(s/fdef deserialize-lower-triangular-matrix
  :args (s/or :one-two (s/cat :numbers ::m/numbers
                         :opts (s/? (s/keys :opt [::by-row?])))
          :three (s/cat :diagonal-numbers ::m/numbers
                   :off-diagonal-numbers ::m/numbers
                   :opts (s/keys :opt [::by-row?])))
  :ret (s/nilable ::lower-triangular-matrix))

(defn deserialize-symmetric-matrix
  "Reconstructs a symmetric matrix from its upper triangle representation.

  Takes a vector of numbers representing the upper triangle (including diagonal) and creates a full
  symmetric matrix by mirroring across the diagonal.

  Examples:
    (deserialize-symmetric-matrix [1 2 3]) ;=> [[1.0 2.0] [2.0 3.0]]"
  [numbers]
  (let [size (size-of-symmetric-or-triangular-matrix (count numbers))
        f (fn [r c]
            (if (<= r c)
              (symmetric-row-fill r c size numbers)
              (symmetric-row-fill c r size numbers)))]
    (when-not (m/nan? size) (compute-matrix size size f))))

(s/fdef deserialize-symmetric-matrix
  :args (s/cat :numbers ::m/numbers)
  :ret (s/nilable ::symmetric-matrix))

(defn toeplitz-matrix
  "Creates a Toeplitz (diagonal-constant) matrix.

  A Toeplitz matrix has constant values along each diagonal. The `first-row` and `first-column`
  define all matrix elements. The first elements of both vectors must be equal.

  Examples:
    (toeplitz-matrix [1 2 3] [1 4 5]) ;=> [[1 2 3] [4 1 2] [5 4 1]]"
  [first-row first-column]
  (let [columns (count first-row)
        rows (count first-column)]
    (compute-matrix rows columns (fn [r c]
                                   (if (<= r c)
                                     (get first-row (- c r) m/nan)
                                     (get first-column (- r c) m/nan))))))

(s/fdef toeplitz-matrix
  :args (s/with-gen (s/and (s/cat :first-row ::vector/vector
                             :first-column ::vector/vector)
                      (fn [{:keys [first-row first-column]}]
                        (= (first first-row) (first first-column))))
          #(gen/fmap
             (fn [[fr fc]]
               (let [ffr (first fr)
                     fc (if ffr
                          (assoc fc 0 ffr)
                          fc)]
                 [fr fc]))
             (gen/tuple (s/gen ::vector/vector)
               (s/gen ::vector/vector))))
  :ret ::matrix)

(def ^{:doc "See [[toeplitz-matrix]]"} diagonal-constant-matrix toeplitz-matrix)

(defn outer-product
  "Computes the outer product of a vector with itself.

  Creates a square matrix where element `[i,j]` equals `v[i] * v[j]`. This is the tensor product
  of the vector with itself.

  Examples:
    (outer-product [1 2]) ;=> [[1.0 2.0] [2.0 4.0]]
    (outer-product [2 3]) ;=> [[4.0 6.0] [6.0 9.0]]"
  [v]
  (let [s (count v)]
    (if (zero? s)
      [[]]
      (vec (for [r (range s)]
             (vec (for [c (range s)]
                    (* (double (get v r)) (get v c)))))))))

(s/fdef outer-product
  :args (s/cat :v ::vector/vector)
  :ret ::matrix)

(defn rnd-matrix!
  "Creates a matrix with random elements between 0 and 1.

  Uses the current random number generator state. Elements are uniformly distributed doubles.

  Examples:
    (rnd-matrix! 2 2) ;=> [[0.123 0.456] [0.789 0.012]] (example values)"
  [rows columns]
  (let [t (* rows columns)]
    (if (zero? t)
      [[]]
      (mapv vec (partition columns (take t (random/rnd-lazy!)))))))

(s/fdef rnd-matrix!
  :args (s/cat :rows ::rows :columns ::columns)
  :ret ::matrix)

(defn rnd-reflection-matrix!
  "Creates a random Householder reflection matrix.

  A Householder reflection matrix is symmetric and orthogonal, representing a reflection through a
  hyperplane. Generated using a random unit vector.

  Examples:
    (rnd-reflection-matrix! 2) ;=> [[-0.6 0.8] [0.8 0.6]] (example values)"
  [size]
  (let [v (column-matrix
            (tensor/normalize (vec (take size (random/rnd-lazy!)))))]
    (tensor/subtract (identity-matrix size)
      (tensor/multiply (mx* v (transpose v))
        2.0))))

(s/fdef rnd-reflection-matrix!
  :args (s/cat :size ::vector/size)
  :ret ::symmetric-matrix)

(defn rnd-spectral-matrix!
  "Creates a random symmetric matrix with specified eigenvalues.

  Uses the given `spectrum-vector` as eigenvalues and generates random eigenvectors through
  composed Householder reflections. The result is symmetric with the exact specified eigenvalues.

  Examples:
    (rnd-spectral-matrix! [1 3]) ;=> [[2.1 -0.9] [-0.9 1.9]] (example values)"
  [spectrum-vector]
  (let [size (count spectrum-vector)
        v-mat (nth (iterate (fn [prod-mat]
                              (mx* prod-mat (rnd-reflection-matrix! size)))
                     (identity-matrix size))
                (* 2 size))
        l-mat (diagonal-matrix spectrum-vector)]
    (symmetric-matrix-by-averaging (mx* (mx* v-mat l-mat) (transpose v-mat)))))

(s/fdef rnd-spectral-matrix!
  :args (s/cat :spectrum-vector ::vector/vector)
  :ret ::symmetric-matrix)

(defn sparse->matrix
  "Constructs a matrix from sparse representation.

  Takes a sparse representation (vector of `[row col value]` triples) and an existing matrix
  template. Sets specified positions to given values. Later values override earlier ones for the
  same position.

  Examples:
    (sparse->matrix [[0 1 5] [1 0 3]] ([[constant-matrix]] 2 2 0))
    ;=> [[0.0 5] [3 0.0]]"
  [sparse m]
  (let [[rows columns] [(rows m) (columns m)]]
    (vec (reduce (fn [new-m [r c x]]
                   (if (or (>= r rows)
                         (neg? r)
                         (>= c columns)
                         (neg? c))
                     new-m
                     (assoc-in new-m [r c] x)))
           m
           sparse))))

(s/fdef sparse->matrix
  :args (s/cat :sparse ::sparse-matrix
          :m ::matrix)
  :ret ::matrix)

(defn sparse->symmetric-matrix
  "Constructs a symmetric matrix from sparse representation.

  Takes sparse representation and applies each `[row col value]` entry symmetrically. Off-diagonal
  entries are mirrored (set at both `[r,c]` and `[c,r]`). Later values override earlier ones.

  Examples:
    (sparse->symmetric-matrix [[0 1 5]] ([[constant-matrix]] 2 2 0))
    ;=> [[0.0 5] [5 0.0]]"
  [sparse symmetric-m]
  (let [rc (rows symmetric-m)]
    (vec (reduce (fn [new-m [r c x]]
                   (if (or (>= r rc) (neg? r) (>= c rc) (neg? c))
                     new-m
                     (assoc-in (assoc-in new-m [r c] x) [c r] x)))
           symmetric-m
           sparse))))

(s/fdef sparse->symmetric-matrix
  :args (s/cat :sparse ::sparse-matrix
          :m ::symmetric-matrix)
  :ret ::symmetric-matrix)

;;;MATRIX INFO
(defn rows
  "Returns the number of rows in the matrix.

  For an empty matrix `[[]]`, returns `0`.

  Examples:
    (rows [[1 2] [3 4]]) ;=> 2
    (rows [[]]) ;=> 0"
  [m]
  (if (empty-matrix? m)
    0
    (count m)))

(s/fdef rows
  :args (s/cat :m ::matrix)
  :ret ::rows)

(defn columns
  "Returns the number of columns in the matrix.

  Based on the length of the first row.

  Examples:
    (columns [[1 2] [3 4]]) ;=> 2
    (columns [[]]) ;=> 0"
  [m]
  (count (first m)))

(s/fdef columns
  :args (s/cat :m ::matrix)
  :ret ::columns)

(defn get-row
  "Extracts the specified row from the matrix as a vector.

  Row indices are 0-based.

  Examples:
    (get-row [[1 2] [3 4]] 0) ;=> [1 2]
    (get-row [[1 2] [3 4]] 1) ;=> [3 4]"
  [m row]
  (vec (get m row)))

(s/fdef get-row
  :args (s/and (s/cat :m ::matrix
                 :row ::row)
          (fn [{:keys [m row]}]
            (< row (rows m))))
  :ret ::vector/vector)

(defn get-column
  "Extracts the specified column from the matrix as a vector.

  Column indices are 0-based. Returns empty vector if column is out of bounds.

  Examples:
    (get-column [[1 2] [3 4]] 0) ;=> [1 3]
    (get-column [[1 2] [3 4]] 1) ;=> [2 4]"
  [m column]
  (let [col (map #(get % column) m)]
    (if (some nil? col)
      []
      (vec col))))

(s/fdef get-column
  :args (s/and (s/cat :m ::matrix
                 :column ::column)
          (fn [{:keys [m column]}]
            (< column (columns m))))
  :ret ::vector/vector)

(defn diagonal
  "Extracts a diagonal from a matrix as a vector.

  With no offset (`k`), returns the main diagonal. Positive `k` returns diagonals above the main
  diagonal, negative `k` returns diagonals below it.

  Examples:
    (diagonal [[1 2 3] [4 5 6] [7 8 9]]) ;=> [1 5 9]
    (diagonal [[1 2 3] [4 5 6]] 1) ;=> [2 6]
    (diagonal [[1 2 3] [4 5 6]] -1) ;=> [4]"
  ([m]
   (if (empty-matrix? m)
     []
     (reduce (fn [tot e] (conj tot (get-in m [e e])))
       []
       (range (min (rows m) (columns m))))))
  ([m k]
   (if (empty-matrix? m)
     []
     (let [r (if (neg? k) (- k) 0)
           c (if (pos? k) k 0)
           nc (- (columns m) c)
           nr (- (rows m) r)
           start (- (min r c))
           end (min nc nr)]
       (if (pos? end)
         (vec (for [i (range start end)]
                (get-in m [(+ i r) (+ i c)])))
         [])))))

(s/fdef diagonal
  :args (s/cat :m ::matrix
          :k (s/? ::m/int))
  :ret ::vector/vector)

(defn serialize-symmetric-or-triangular-matrix
  "Flattens the upper or lower triangle of a matrix into a vector.

  By default, extracts the upper triangle (including diagonal) in row-major order. The matrix
  doesn't need to be symmetric.

  Options:
  - `::by-row?` (default `true`) - extract upper triangle; `false` for lower triangle

  Examples:
    (serialize-symmetric-or-triangular-matrix [[1 2] [3 4]]) ;=> [1 2 4]"
  ([m] (serialize-symmetric-or-triangular-matrix m {::by-row? true}))
  ([m {::keys [by-row?]
       :or    {by-row? true}}]
   (let [nr (rows m)
         nc (columns m)]
     (vec (if by-row?
            (for [r (range nr)
                  c (range r nc)]
              (get-in m [r c]))
            (for [c (range nc)
                  r (range c nr)]
              (get-in m [r c])))))))

(s/fdef serialize-symmetric-or-triangular-matrix
  :args (s/cat :m ::matrix
          :opts (s/? (s/keys :opt [::by-row?])))
  :ret ::vector/vector)

(defn size-of-symmetric-or-triangular-matrix
  "Calculates the matrix size from the number of triangular elements.

  Given the count of elements in a triangular matrix (including diagonal), determines the original
  square matrix size. Returns `NaN` if count is invalid.

  Examples:
    (size-of-symmetric-or-triangular-matrix 3) ;=> 2 (for 2x2 matrix)
    (size-of-symmetric-or-triangular-matrix 6) ;=> 3 (for 3x3 matrix)"
  [ecount]
  (let [s (-> ecount (* 8.0) inc m/sqrt dec (* 0.5))]
    (if (m/roughly-round? s 1e-6)
      (m/maybe-long-able s)
      m/nan)))

(s/fdef size-of-symmetric-or-triangular-matrix
  :args (s/cat :ecount ::m/int-non-)
  :ret (s/or :nan ::m/nan :size ::vector/size))

(defn size-of-symmetric-or-triangular-matrix-without-diag
  "Calculates matrix size from off-diagonal element count.

  Given the count of elements above or below the diagonal (excluding diagonal), determines the
  original square matrix size.

  Examples:
    (size-of-symmetric-or-triangular-matrix-without-diag 1) ;=> 2
    (size-of-symmetric-or-triangular-matrix-without-diag 3) ;=> 3"
  [ecount]
  (let [size (size-of-symmetric-or-triangular-matrix ecount)]
    (if (m/nan? size)
      size
      (m/maybe-long-able (inc (double size))))))

(s/fdef size-of-symmetric-or-triangular-matrix-without-diag
  :args (s/cat :ecount ::m/int-non-)
  :ret (s/or :nan ::m/nan :size ::vector/size))

(defn ecount-of-symmetric-or-triangular-matrix
  "Calculates the number of elements in a triangular matrix.

  For a square matrix of given size, returns the count of elements on or above (or below) the
  diagonal: `size*(size+1)/2`.

  Examples:
    (ecount-of-symmetric-or-triangular-matrix 2) ;=> 3
    (ecount-of-symmetric-or-triangular-matrix 3) ;=> 6"
  [size]
  (m/div (+ (m/sq' size) size) 2))

(s/fdef ecount-of-symmetric-or-triangular-matrix
  :args (s/cat :size ::vector/size)
  :ret ::m/int-non-)

(defn ecount-of-symmetric-or-triangular-matrix-without-diag
  "Calculates the number of off-diagonal elements in a triangular matrix.

  For a square matrix of given size, returns the count of elements above (or below) the diagonal:
  `size*(size-1)/2`.

  Examples:
    (ecount-of-symmetric-or-triangular-matrix-without-diag 2) ;=> 1
    (ecount-of-symmetric-or-triangular-matrix-without-diag 3) ;=> 3"
  [size]
  (m/div (- (m/sq' size) size) 2))

(s/fdef ecount-of-symmetric-or-triangular-matrix-without-diag
  :args (s/cat :size ::vector/size)
  :ret ::m/int-non-)

(defn trace
  "Calculates the trace of a square matrix.

  Returns the sum of all elements on the main diagonal.

  Examples:
    (trace [[1 2] [3 4]]) ;=> 5.0
    (trace [[]]) ;=> 0.0"
  [square-m]
  (if (empty-matrix? square-m)
    0.0
    (apply + (map double (diagonal square-m)))))

(s/fdef trace
  :args (s/cat :square-m ::square-matrix)
  :ret ::m/number)

(defn get-slices-as-matrix
  "Extracts a submatrix by selecting specific rows and columns.

  Options control which rows/columns to include or exclude:
  - `::row-indices` - specific rows to include (default: all)
  - `::column-indices` - specific columns to include (default: all)
  - `::exception-row-indices` - rows to exclude
  - `::exception-column-indices` - columns to exclude

  Exclusions take precedence over inclusions. Index sequences allow reordering.

  Examples:
    (get-slices-as-matrix m {::row-indices [1 0]}) ; swaps first two rows
    (get-slices-as-matrix m {::exception-row-indices 0}) ; removes first row"
  [m
   {::keys [row-indices
            column-indices
            exception-row-indices
            exception-column-indices]}]
  (let [calc-fn (fn [i except-i n]
                  (cond (and (not i) (not except-i)) true
                    (not except-i) (if (number? i)
                                     (if (< i n) i [])
                                     (remove #(>= % n) i))
                    (number? i) (if (number? except-i)
                                  (if (= except-i i)
                                    []
                                    (if (< i n) i []))
                                  (if (contains? (set except-i) i)
                                    []
                                    (if (< i n) i [])))
                    :else (let [indices (or i (range n))]
                            (if (number? except-i)
                              (remove (fn [index]
                                        (or (= except-i index)
                                          (>= index n)))
                                indices)
                              (reduce
                                (fn [tot e]
                                  (if (or (>= e n) (some #(= % e) except-i))
                                    tot
                                    (conj tot e)))
                                []
                                indices)))))
        rs (calc-fn row-indices exception-row-indices (rows m))
        cs (calc-fn column-indices exception-column-indices (columns m))]
    (cond
      (or (and (coll? rs) (empty? rs)) (and (coll? cs) (empty? cs))) [[]]
      (and (number? rs) (number? cs)) [[(get-in m [rs cs])]]

      (and (number? rs) (coll? cs))
      (row-matrix (let [row-vector (get m rs)]
                    (map #(get row-vector %) cs)))

      (and (number? rs) (true? cs)) (row-matrix (get-row m rs))

      (and (coll? rs) (number? cs))
      (column-matrix (let [column-vector (get-column m cs)]
                       (map #(nth column-vector %) rs)))

      (and (coll? rs) (coll? cs))
      (mapv (fn [row-vector]
              (reduce (fn [tot column]
                        (conj tot (get row-vector column)))
                []
                cs))
        (map #(get m %) rs))

      (and (coll? rs) (true? cs)) (mapv #(get m %) rs)
      (and (true? rs) (number? cs)) (column-matrix (get-column m cs))

      (and (true? rs) (coll? cs))
      (mapv (fn [row-vector]
              (reduce (fn [tot column]
                        (conj tot (get row-vector column)))
                []
                cs))
        m)

      (and (true? rs) (true? cs)) m)))

(s/fdef get-slices-as-matrix
  :args (s/cat :m ::matrix
          :opts (s/keys :opt [::row-indices
                              ::column-indices
                              ::exception-row-indices
                              ::exception-column-indices]))
  :ret ::matrix)

(defn filter-by-row
  "Filters matrix rows based on a predicate function.

  The predicate receives each row vector and should return `true` to keep the row. Returns empty
  matrix `[[]]` if no rows match.

  Examples:
    (filter-by-row #(> (apply + %) 5) [[1 2] [3 4]]) ;=> [[3 4]]"
  [row-v->bool m]
  (let [new-m (filter row-v->bool m)]
    (if (empty? new-m)
      [[]]
      (vec new-m))))

(s/fdef filter-by-row
  :args (s/cat :row-v->bool (s/fspec :args (s/cat :row-vector ::vector/vector)
                              :ret boolean?)
          :m ::matrix)
  :ret ::matrix)

(defn filter-by-column
  "Filters matrix columns based on a predicate function.

  The predicate receives each column vector and should return `true` to keep the column. Returns
  empty matrix `[[]]` if no columns match.

  Examples:
    (filter-by-column #(> (apply + %) 3) [[1 2] [3 4]]) ;=> [[2] [4]]"
  [column-v->bool m]
  (let [new-m (filter column-v->bool (transpose m))]
    (if (empty? new-m)
      [[]]
      (transpose (vec new-m)))))

(s/fdef filter-by-column
  :args (s/cat
          :column-v->bool (s/fspec :args (s/cat :column-vector ::vector/vector)
                            :ret boolean?)
          :m ::matrix)
  :ret ::matrix)

(defn filter-symmetric-matrix
  "Filters a symmetric matrix by keeping rows/columns that satisfy a predicate.

  The predicate receives each row vector. Keeps both row and corresponding column if the predicate
  returns `true`, maintaining symmetry.

  Examples:
    (filter-symmetric-matrix #(> (first %) 2) [[1 2] [2 3]]) ;=> [[3]]"
  [v->bool symmetric-m]
  (let [keep-set (reduce-kv (fn [tot index row-vector]
                              (if (v->bool row-vector)
                                (conj tot index)
                                tot))
                   #{}
                   symmetric-m)]
    (get-slices-as-matrix symmetric-m {::row-indices    keep-set
                                       ::column-indices keep-set})))

(s/fdef filter-symmetric-matrix
  :args (s/cat :v->bool (s/fspec :args (s/cat :row-vector ::vector/vector)
                          :ret boolean?)
          :symmetric-m ::symmetric-matrix)
  :ret ::symmetric-matrix)

(defn matrix-partition
  "Splits a matrix into four quadrants.

  Returns a map with keys `::top-left`, `::top-right`, `::bottom-left`, `::bottom-right`. The split
  occurs at the specified row and column boundaries.

  Parameters:
  - `first-bottom-row`: row index where bottom partition starts
  - `first-right-column`: column index where right partition starts

  Examples:
    (matrix-partition [[1 2] [3 4]] 1 1)
    ;=> {::top-left [[1]], ::top-right [[2]], ::bottom-left [[3]], ::bottom-right [[4]]}"
  [m first-bottom-row first-right-column]
  {::top-left     (get-slices-as-matrix
                    m {::row-indices    (range first-bottom-row)
                       ::column-indices (range first-right-column)})
   ::bottom-left  (get-slices-as-matrix
                    m {::exception-row-indices (range first-bottom-row)
                       ::column-indices        (range first-right-column)})
   ::top-right    (get-slices-as-matrix
                    m {::row-indices              (range first-bottom-row)
                       ::exception-column-indices (range first-right-column)})
   ::bottom-right (get-slices-as-matrix
                    m {::exception-row-indices    (range first-bottom-row)
                       ::exception-column-indices (range first-right-column)})})

(s/fdef matrix-partition
  :args (s/cat :m ::matrix :first-bottom-row ::row :first-right-column ::column)
  :ret (s/keys :req [::top-left ::bottom-left ::top-right ::bottom-right]))

(defn some-kv
  "Finds the first matrix element satisfying a predicate.

  The predicate function receives `(row column element)` and should return a truthy value. Returns
  the first element for which the predicate is truthy, or `nil` if none found.

  Options:
  - `::by-row?` (default `true`) - traverse by rows first, then columns

  Examples:
    (some-kv #(> %3 5) [[1 2] [6 3]]) ;=> 6"
  ([pred m] (some-kv pred m {::by-row? true}))
  ([pred m {::keys [by-row?] :or {by-row? true}}]
   (let [mt (if by-row?
              m
              (transpose m))
         rows (rows mt)]
     (loop [row 0]
       (when (< row rows)
         (or (vector/some-kv (fn [column number]
                               (pred row column number))
               (get-row mt row))
           (recur (inc row))))))))

(s/fdef some-kv
  :args (s/cat :pred (s/fspec :args (s/cat :r ::row
                                      :c ::column
                                      :e ::m/number)
                       :ret boolean?)
          :m ::matrix
          :opts (s/? (s/keys :opt [::by-row?])))
  :ret (s/nilable ::m/number))

(defn ereduce-kv
  "Reduces over matrix elements with row/column indices.

  The reduction function `f` receives:
  - 1 matrix: `(f result row col element)`
  - 2 matrices: `(f result row col elem1 elem2)`
  - 3 matrices: `(f result row col elem1 elem2 elem3)`

  For multiple matrices, the first matrix's column count must be smallest.

  Examples:
    (ereduce-kv #(+ %1 %2 %3 %4) 0 [[1 2] [3 4]]) ;=> 10 (sum + indices)"
  ([f init m]
   (let [nr (rows m)]
     (loop [r 0
            val init
            s m]
       (let [g (fn [result column number]
                 (when (and (m/int-non-? column) (number? number))
                   (f result r column number)))]
         (if (>= r nr)
           val
           (recur (inc r)
             (reduce-kv g val (first s))
             (rest s)))))))
  ([f init m1 m2]
   (let [nr (min (rows m1) (rows m2))]
     (loop [r 0
            val init
            s1 m1
            s2 m2]
       (let [g (fn [result column number1 number2]
                 (when (and (m/int-non-? column)
                         (number? number1)
                         (number? number2))
                   (f result r column number1 number2)))]
         (if (>= r nr)
           val
           (recur (inc r)
             (extensions/reduce-kv-ext g val (first s1) (first s2))
             (rest s1)
             (rest s2)))))))
  ([f init m1 m2 m3]
   (let [nr (min (rows m1) (rows m2) (rows m3))]
     (loop [r 0
            val init
            s1 m1
            s2 m2
            s3 m3]
       (let [g (fn [result column number1 number2 number3]
                 (when (and (m/int-non-? column)
                         (number? number1)
                         (number? number2)
                         (number? number3))
                   (f result r column number1 number2 number3)))]
         (if (>= r nr)
           val
           (recur (inc r)
             (extensions/reduce-kv-ext
               g val (first s1) (first s2) (first s3))
             (rest s1)
             (rest s2)
             (rest s3))))))))

(s/fdef ereduce-kv
  :args (s/or :three (s/cat :f (s/fspec :args (s/cat :res any?
                                                :row ::row
                                                :column ::column
                                                :number ::m/number)
                                 :ret any?)
                       :init any?
                       :m ::matrix)
          :four (s/and
                  (s/cat :f (s/fspec :args (s/cat :res any?
                                             :row ::row
                                             :column ::column
                                             :number1 ::m/number
                                             :number2 ::m/number)
                              :ret any?)
                    :init any?
                    :m1 ::matrix
                    :m2 ::matrix)
                  (fn [{:keys [m1 m2]}]
                    (<= (columns m1) (columns m2))))
          :five (s/and
                  (s/cat :f (s/fspec :args (s/cat :res any?
                                             :row ::row
                                             :column ::column
                                             :number1 ::m/number
                                             :number2 ::m/number
                                             :number3 ::m/number)
                              :ret any?)
                    :init any?
                    :m1 ::matrix
                    :m2 ::matrix
                    :m3 ::matrix)
                  (fn [{:keys [m1 m2 m3]}]
                    (and (<= (columns m1) (columns m2))
                      (<= (columns m1) (columns m3))))))
  :ret any?)

(defn matrix->sparse
  "Converts a matrix to sparse representation.

  Returns a vector of `[row col value]` triples for elements that satisfy the predicate function.
  By default includes all non-zero elements.

  Examples:
    (matrix->sparse [[1 0] [0 2]]) ;=> [[0 0 1] [1 1 2]]
    (matrix->sparse [[1 2] [3 4]] #(> % 2)) ;=> [[1 0 3] [1 1 4]]"
  ([m] (matrix->sparse m (complement zero?)))
  ([m number->bool]
   (ereduce-kv (fn [result row column number]
                 (if (number->bool number)
                   (when (vector? result)
                     (conj result [row column number]))
                   result))
     []
     m)))

(s/fdef matrix->sparse
  :args (s/cat :m ::matrix
          :number->bool (s/? ::number->bool))
  :ret ::sparse-matrix)

(defn symmetric-matrix->sparse
  "Converts a symmetric matrix to sparse representation of upper triangle.

  Only processes the upper triangle (including diagonal) to avoid duplication. The predicate
  determines which elements to include in the sparse representation.

  Examples:
    (symmetric-matrix->sparse [[1 0] [0 2]]) ;=> [[0 0 1] [1 1 2]]"
  ([symmetric-m] (symmetric-matrix->sparse symmetric-m (complement zero?)))
  ([symmetric-m number->bool]
   (ereduce-kv (fn [result row column number]
                 (if (and (<= row column) (number->bool number))
                   (when (vector? result)
                     (conj result [row column number]))
                   result))
     []
     symmetric-m)))

(s/fdef symmetric-matrix->sparse
  :args (s/cat :symmetric-m ::symmetric-matrix
          :number->bool (s/? ::number->bool))
  :ret ::sparse-matrix)

;;;MATRIX MANIPULATION
(defn transpose
  "Transposes a matrix by swapping rows and columns.

  Converts an m x n matrix to an n x m matrix where element at `[i,j]` becomes element at `[j,i]`
  in the result.

  Examples:
    (transpose [[1 2 3] [4 5 6]]) ;=> [[1 4] [2 5] [3 6]]
    (transpose [[]]) ;=> [[]]"
  [m]
  (if (empty-matrix? m)
    [[]]
    (apply mapv vector m)))

(s/fdef transpose
  :args (s/cat :m ::matrix)
  :ret ::matrix)

(defn assoc-row
  "Replaces a row in the matrix with new values.

  Returns anomaly if the new row length doesn't match existing columns or if row index is out of
  bounds.

  Examples:
    (assoc-row [[1 2] [3 4]] 0 [5 6]) ;=> [[5 6] [3 4]]
    (assoc-row [[]] 0 [1 2]) ;=> [[1 2]] (empty matrix case)"
  [m row numbers]
  (cond (and (zero? row) (empty-matrix? m)) (row-matrix numbers)

    (and (= (count numbers) (columns m)) (<= row (rows m)))
    (assoc m row (vec numbers))

    (not= (count numbers) (columns m))
    {::anom/category ::anom/incorrect
     ::anom/message  (str "Row length " (count numbers) " does not match matrix columns "
                       (columns m))}

    :else
    {::anom/category ::anom/incorrect
     ::anom/message  (str "Row index " row " out of bounds for matrix with " (rows m) " rows")}))

(s/fdef assoc-row
  :args (s/cat :m ::matrix :row ::row :numbers ::m/numbers)
  :ret (s/or :matrix ::matrix :anomaly ::anom/anomaly))

(defn assoc-column
  "Replaces a column in the matrix with new values.

  Returns anomaly if the new column length doesn't match existing rows or if column index is out of
  bounds.

  Examples:
    (assoc-column [[1 2] [3 4]] 0 [5 6]) ;=> [[5 2] [6 4]]
    (assoc-column [[]] 0 [1 2]) ;=> [[1] [2]] (empty matrix case)"
  [m column numbers]
  (cond (and (zero? column) (empty-matrix? m)) (column-matrix numbers)

    (and (= (count numbers) (rows m)) (<= column (columns m)))
    (vec (map-indexed (fn [row row-vector]
                        (assoc row-vector column (get numbers row 0.0)))
           m))

    (not= (count numbers) (rows m))
    {::anom/category ::anom/incorrect
     ::anom/message  (str "Column length " (count numbers) " does not match matrix rows " (rows m))}

    :else
    {::anom/category ::anom/incorrect
     ::anom/message  (str "Column index " column " out of bounds for matrix with "
                       (columns m) " columns")}))

(s/fdef assoc-column
  :args (s/cat :m ::matrix :column ::column :numbers ::m/numbers)
  :ret (s/or :matrix ::matrix :anomaly ::anom/anomaly))

(defn assoc-diagonal
  "Replaces the main diagonal elements with new values.

  Returns anomaly if the number of values doesn't match the diagonal length.
  For an empty matrix, creates a new diagonal matrix.

  Examples:
    (assoc-diagonal [[1 2] [3 4]] [5 6]) ;=> [[5 2] [3 6]]
    (assoc-diagonal [[]] [1 2]) ;=> [[1.0 0.0] [0.0 2.0]]"
  [m numbers]
  (let [diag-len (count (diagonal m))]
    (cond (empty-matrix? m) (diagonal-matrix numbers)

      (= (count numbers) diag-len)
      (let [v (vec numbers)]
        (vec (for [row (range (count numbers))]
               (assoc (get-row m row) row (get v row 0.0)))))

      :else
      {::anom/category ::anom/incorrect
       ::anom/message  (str "Diagonal length "
                         (count numbers) " does not match matrix diagonal length " diag-len)})))

(s/fdef assoc-diagonal
  :args (s/cat :m ::matrix :numbers ::m/numbers)
  :ret (s/or :matrix ::matrix :anomaly ::anom/anomaly))

(defn insert-row
  "Inserts a new row at the specified position.

  Returns anomaly if the new row length doesn't match existing columns or if row index is out of
  bounds.

  Examples:
    (insert-row [[1 2] [3 4]] 1 [5 6]) ;=> [[1 2] [5 6] [3 4]]
    (insert-row [[]] 0 [1 2]) ;=> [[1 2]] (empty matrix case)"
  [m row numbers]
  (cond (and (zero? row) (empty-matrix? m)) (row-matrix numbers)

    (and (= (count numbers) (columns m)) (<= row (rows m)))
    (vec (concat (subvec m 0 row) [(vec numbers)] (subvec m row)))

    (not= (count numbers) (columns m))
    {::anom/category ::anom/incorrect
     ::anom/message  (str "Row length " (count numbers) " does not match matrix columns "
                       (columns m))}

    :else
    {::anom/category ::anom/incorrect
     ::anom/message  (str "Row index " row " out of bounds for matrix with " (rows m) " rows")}))

(s/fdef insert-row
  :args (s/cat :m ::matrix :row ::row :numbers ::m/numbers)
  :ret (s/or :matrix ::matrix :anomaly ::anom/anomaly))

(defn insert-column
  "Inserts a new column at the specified position.

  Returns anomaly if the new column length doesn't match existing rows or if column index is out of
  bounds.

  Examples:
    (insert-column [[1 2] [3 4]] 1 [5 6]) ;=> [[1 5 2] [3 6 4]]
    (insert-column [[]] 0 [1 2]) ;=> [[1] [2]] (empty matrix case)"
  [m column numbers]
  (cond (and (zero? column) (empty-matrix? m)) (column-matrix numbers)

    (and (= (count numbers) (rows m)) (<= column (columns m)))
    (vec (map-indexed
           (fn [row row-vector]
             (vector/insertv row-vector column (get numbers row 0.0)))
           m))

    (not= (count numbers) (rows m))
    {::anom/category ::anom/incorrect
     ::anom/message  (str "Column length " (count numbers) " does not match matrix rows " (rows m))}

    :else
    {::anom/category ::anom/incorrect
     ::anom/message  (str "Column index " column " out of bounds for matrix with "
                       (columns m) " columns")}))

(s/fdef insert-column
  :args (s/cat :m ::matrix
          :column ::column
          :numbers ::m/numbers)
  :ret (s/or :matrix ::matrix :anomaly ::anom/anomaly))

(defn remove-row
  "Removes the specified row from the matrix.
  
  Returns the original matrix if row index is out of bounds.
  Returns empty matrix [[]] if removing the last row.
  
  Examples:
    (remove-row [[1 2] [3 4]] 0) ;=> [[3 4]]
    (remove-row [[1 2]] 0) ;=> [[]]"
  [m row]
  (if (<= (inc row) (count m))
    (let [m2 (vec (concat (subvec m 0 row) (subvec m (inc row))))]
      (if (empty? m2) [[]] m2))
    m))

(s/fdef remove-row
  :args (s/cat :m ::matrix
          :row ::row)
  :ret ::matrix)

(defn remove-column
  "Removes the specified column from the matrix.

  Returns the original matrix if column index is out of bounds.
  Returns empty matrix [[]] if removing the last column.

  Examples:
    (remove-column [[1 2] [3 4]] 0) ;=> [[2] [4]]
    (remove-column [[1] [2]] 0) ;=> [[]]"
  [m column]
  (if (<= (inc column) (columns m))
    (let [m2 (mapv (fn [r]
                     (vector/removev r column))
               m)]
      (if (or (empty? m2) (empty? (first m2)))
        [[]]
        m2))
    m))

(s/fdef remove-column
  :args (s/cat :m ::matrix
          :column ::column)
  :ret ::matrix)

(defn update-row
  "Updates all elements in the specified row using a function.
  
  The function receives (column-index element) and returns the new element value.
  Returns nil if row index is out of bounds.
  
  Examples:
    (update-row [[1 2] [3 4]] 0 (fn [col val] (+ col val))) ;=> [[1 3] [3 4]]"
  [m row column+number->number]
  (when (< row (rows m))
    (update m row (fn [row-vector]
                    (vec (map-indexed column+number->number row-vector))))))

(s/fdef update-row
  :args (s/and (s/cat :m ::matrix
                 :row ::row
                 :column+number->number (s/fspec :args (s/cat :column ::column
                                                         :number ::m/number)
                                          :ret ::m/number)))
  :ret (s/nilable ::matrix))

(defn update-column
  "Updates all elements in the specified column using a function.
  
  The function receives (row-index element) and returns the new element value.
  Returns nil if column index is out of bounds.
  
  Examples:
    (update-column [[1 2] [3 4]] 0 (fn [row val] (+ row val))) ;=> [[1 2] [4 4]]"
  [m column row+number->number]
  (when (< column (columns m))
    (vec (map-indexed (fn [row row-vector]
                        (update row-vector column #(row+number->number row %)))
           m))))

(s/fdef update-column
  :args (s/and (s/cat :m ::matrix
                 :column ::column
                 :row+number->number (s/fspec :args (s/cat :row ::row
                                                      :number ::m/number)
                                       :ret ::m/number)))
  :ret (s/nilable ::matrix))

(defn update-diagonal
  "Updates all diagonal elements using a function.
  
  The function receives (row-index diagonal-element) and returns the new value.
  Only affects positions where row index equals column index.
  
  Examples:
    (update-diagonal [[1 2] [3 4]] (fn [i val] (+ i val))) ;=> [[1 2] [3 5]]"
  [m row+number->number]
  (vec (map-indexed (fn [row row-vector]
                      (if (< row (columns m))
                        (update row-vector row #(row+number->number row %))
                        row-vector))
         m)))

(s/fdef update-diagonal
  :args (s/and (s/cat :m ::matrix
                 :row+number->number (s/fspec :args (s/cat :row ::row
                                                      :number ::m/number)
                                       :ret ::m/number)))
  :ret ::matrix)

(defn concat-rows
  "Vertically concatenates matrices by stacking rows.
  
  All matrices must have the same number of columns, otherwise returns nil.
  
  Examples:
    (concat-rows [[1 2]] [[3 4]]) ;=> [[1 2] [3 4]]
    (concat-rows [[1 2]] [[3 4 5]]) ;=> nil (column mismatch)"
  ([] [[]])
  ([m & ms]
   (let [c (columns m)
         cs (map columns ms)]
     (when (every? #(= c %) cs)
       (if (zero? c)
         [[]]
         (vec (apply concat m ms)))))))

(s/fdef concat-rows
  :args (s/or :zero (s/cat)
          :one+ (s/cat :m ::matrix
                  :ms (s/* ::matrix)))
  :ret (s/nilable ::matrix))

(defn concat-columns
  "Horizontally concatenates matrices by joining columns.
  
  All matrices must have the same number of rows, otherwise returns nil.
  
  Examples:
    (concat-columns [[1] [2]] [[3] [4]]) ;=> [[1 3] [2 4]]
    (concat-columns [[1] [2]] [[3 4]]) ;=> nil (row mismatch)"
  ([] [[]])
  ([m & ms]
   (let [r (rows m)
         rs (map rows ms)]
     (when (every? #(= r %) rs)
       (apply mapv (comp vec concat) m ms)))))

(s/fdef concat-columns
  :args (s/or :zero (s/cat)
          :one+ (s/cat :m ::matrix
                  :ms (s/* ::matrix)))
  :ret (s/nilable ::matrix))

(defn merge-matrices
  "Combines four matrices into a single matrix.
  
  Takes a map with keys ::top-left, ::top-right, ::bottom-left, ::bottom-right.
  This is the inverse operation of matrix-partition. Returns nil if matrices
  have incompatible dimensions.
  
  Examples:
    (merge-matrices {::top-left [[1]] ::top-right [[2]] 
                     ::bottom-left [[3]] ::bottom-right [[4]]})
    ;=> [[1 2] [3 4]]"
  [{::keys [top-left bottom-left top-right bottom-right]}]
  (let [top (concat-columns top-left top-right)
        bottom (concat-columns bottom-left bottom-right)]
    (when (and top bottom) (concat-rows top bottom))))

(s/fdef merge-matrices
  :args (s/cat :args (s/keys :req [::top-left
                                   ::bottom-left
                                   ::top-right
                                   ::bottom-right]))
  :ret (s/nilable ::matrix))

(defn replace-submatrix
  "Replaces a rectangular region of a matrix with a submatrix.
  
  Places the submatrix at the specified top-left position. Negative positions are allowed and will
  expand the result matrix. Unfilled positions default to 0.0.
  
  Examples:
    (replace-submatrix [[1 2] [3 4]] [[9]] 0 0) ;=> [[9 2] [3 4]]
    (replace-submatrix [[1 2]] [[9]] -1 1) ;=> [[0.0 9] [1 2]]"
  [m submatrix row-start column-start]
  (let [tr (+ (rows submatrix) row-start)
        tc (+ (columns submatrix) column-start)
        nr (rows m)
        nc (columns m)
        ret (vec (for [r (range (min row-start 0) (max tr nr))]
                   (vec (for [c (range (min column-start 0) (max tc nc))]
                          (cond (and (>= r row-start)
                                  (< r tr)
                                  (>= c column-start)
                                  (< c tc))
                            (get-in submatrix
                              [(- r row-start) (- c column-start)])

                            (and (m/non-? r)
                              (< r nr)
                              (m/non-? c)
                              (< c nc))
                            (get-in m [r c])

                            :else 0.0)))))]
    (if (matrix? ret)
      ret
      [[]])))

(s/fdef replace-submatrix
  :args (s/cat :m ::matrix
          :submatrix ::matrix
          :row-start ::row-start
          :column-start ::column-start)
  :ret ::matrix)

(defn symmetric-matrix-by-averaging
  "Creates a symmetric matrix by averaging corresponding off-diagonal elements.
  
  For each pair of elements at positions [i,j] and [j,i], replaces both with their average. Diagonal
  elements remain unchanged. Useful for correcting matrices that should be symmetric but have small
  numerical errors.
  
  Examples:
    (symmetric-matrix-by-averaging [[1 2] [3 4]]) ;=> [[1.0 2.5] [2.5 4.0]]"
  [square-m]
  (let [size (rows square-m)]
    (compute-matrix size size
      (fn [r c]
        (if (== c r)
          (get-in square-m [r c] 0.0)
          (* 0.5 (+ (get-in square-m [r c] 0.0)
                   (double (get-in square-m [c r] 0.0)))))))))

(s/fdef symmetric-matrix-by-averaging
  :args (s/cat :square-m ::square-matrix)
  :ret ::symmetric-matrix)

;;;MATRIX MATH
(defn mx*
  "Performs matrix multiplication on one or more matrices.
  
  For two matrices A and B, computes the dot product where A has dimensions m×n and B has dimensions
  n×p, resulting in an m×p matrix. The number of columns in the first matrix must equal the number
  of rows in the second.
  
  Returns nil if dimensions are incompatible.
  
  Examples:
    (mx* [[1 2] [3 4]] [[5 6] [7 8]]) ;=> [[19 22] [43 50]]
    (mx* [[1 2 3]]) ;=> [[1 2 3]] (identity)"
  ([m] m)
  ([m1 m2]
   (when (= (columns m1) (rows m2))
     (if (empty-matrix? m1)
       [[]]
       (mapv (fn [a]
               (mapv (fn [b]
                       (apply + (map (fn [i j]
                                       (* (double i) j))
                                  a
                                  b)))
                 (transpose m2)))
         m1))))
  ([m1 m2 & ms]
   (when-let [m3 (mx* m1 m2)]
     (apply mx* m3 ms))))

(s/fdef mx*
  :args (s/or :one (s/cat :m ::matrix)
          :two+ (s/cat :m1 ::matrix
                  :m2 ::matrix
                  :ms (s/* ::matrix)))
  :ret (s/nilable ::matrix))

(defn kronecker-product
  "Computes the Kronecker product of matrices.
  
  For matrices A (m×n) and B (p×q), produces an (mp)×(nq) matrix where each
  element A[i,j] is replaced by A[i,j] * B.
  
  Examples:
    (kronecker-product [[1 2]] [[3 4]]) ;=> [[3 4 6 8]]
    (kronecker-product [[1 2] [3 4]] [[0 1] [1 0]])
    ;=> [[0 1 0 2] [1 0 2 0] [0 3 0 4] [3 0 4 0]]"
  ([] [[]])
  ([m] m)
  ([m1 m2]
   (if (or (empty-matrix? m1) (empty-matrix? m2))
     [[]]
     (vec (apply concat (mapv (fn [row]
                                (apply concat-columns
                                  (mapv (fn [e]
                                          (tensor/multiply e m2))
                                    row)))
                          m1)))))
  ([m1 m2 & ms] (apply kronecker-product (kronecker-product m1 m2) ms)))

(s/fdef kronecker-product
  :args (s/or :zero-or-one (s/cat :m (s/? ::matrix))
          :two (s/cat :m1 ::matrix
                 :m2 ::matrix)
          :three+ (s/cat :m1 ::matrix
                    :m2 ::matrix
                    :ms (s/with-gen
                          (s/+ ::matrix)
                          #(gen/vector (s/gen ::matrix) 1 2))))
  :ret ::matrix)

;;;MATRIX ROUNDING
(defn- roughly-zero-fn
  [accu]
  (fn [v]
    (every? (fn [number]
              (m/roughly? number 0.0 (double accu)))
      v)))

(defn round-roughly-zero-rows
  "Rounds matrix rows that are approximately zero to exact zeros.
  
  Any row where all elements are within the accuracy tolerance of zero
  is replaced with a row of exact zeros.
  
  Examples:
    (round-roughly-zero-rows [[1e-10 2e-10] [1 2]] 1e-6) ;=> [[0.0 0.0] [1 2]]"
  [m accu]
  (mapv (fn [row]
          (if ((roughly-zero-fn accu) row)
            (vec (repeat (columns m) 0.0))
            row))
    m))

(s/fdef round-roughly-zero-rows
  :args (s/cat :m ::matrix :accu ::m/accu)
  :ret ::matrix)

(defn round-roughly-zero-columns
  "Rounds matrix columns that are approximately zero to exact zeros.
  
  Any column where all elements are within the accuracy tolerance of zero
  is replaced with a column of exact zeros.
  
  Examples:
    (round-roughly-zero-columns [[1e-10 1] [2e-10 2]] 1e-6) ;=> [[0.0 1] [0.0 2]]"
  [m accu]
  (transpose (round-roughly-zero-rows (transpose m) accu)))

(s/fdef round-roughly-zero-columns
  :args (s/cat :m ::matrix :accu ::m/accu)
  :ret ::matrix)

