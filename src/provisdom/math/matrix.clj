(ns provisdom.math.matrix
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]
            [provisdom.utility-belt.core :as co]
            [provisdom.math.core :as m]
            [provisdom.math.tensor :as tensor]
            [provisdom.math.vector :as vector]
            [clatrix.core :as clx]
            [clojure.core.matrix :as mxc]
            [taoensso.truss :refer [have have?]]
            [provisdom.math.random2 :as random]))

(set! *warn-on-reflection* true)

;;;DECLARATIONS
(declare eigenvalues column-matrix transpose rrqr-decomposition diagonal
         get-slices-as-matrix esome esum matrix? row-matrix? column-matrix? square-matrix?
         row-count column-count dimensionality size-symmetric size-symmetric-with-unit-diagonal
         compute-vector coerce maybe-convert-clatrix-row-or-column to-vector ecount to-matrix
         inner-product emap constant-matrix matrix-multiply)

(def mdl 6)                                                 ;max-dim-length for generators

(s/def ::by-row? boolean?)
(s/def ::upper? boolean?)
(s/def ::row-indices (s/or :index ::index :indices ::indices))
(s/def ::column-indices (s/or :index ::index :indices ::indices))
(s/def ::exception-row-indices (s/or :index ::index :indices ::indices))
(s/def ::exception-column-indices (s/or :index ::index :indices ::indices))
(s/def ::row ::m/int-non-)
(s/def ::column ::m/int-non-)
(s/def ::rows ::m/int-non-)
(s/def ::columns ::m/int-non-)
(s/def ::vector ::vector/vector)
(s/def ::row-matrix (s/with-gen row-matrix? #(gen/vector (s/gen ::vector) 1)))
(s/def ::column-matrix (s/with-gen column-matrix? #(gen/fmap (fn [v] (column-matrix v)) (s/gen ::vector))))
(s/def ::matrix-num (s/with-gen (s/coll-of (s/coll-of ::m/num :kind vector? :into []) :kind vector? :into [])
                                #(gen/vector (s/gen ::vector-num) 1 mdl)))
(s/def ::matrix-finite (s/with-gen (s/coll-of (s/coll-of ::m/finite :kind vector? :into []) :kind vector? :into [])
                                   #(gen/vector (s/gen ::vector-finite) 1 mdl)))
(s/def ::square-matrix
  (s/with-gen square-matrix?
              #(gen/bind (gen/large-integer* {:min 0 :max mdl})
                         (fn [i] (gen/vector (gen/vector i) i)))))
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
(s/def ::sparse-matrix
  (s/with-gen (s/coll-of (s/tuple ::m/int-non- ::m/int-non- ::number) :kind vector? :into [])
              #(gen/bind (gen/tuple (gen/large-integer* {:min 0 :max mdl})
                                    (gen/large-integer* {:min 0 :max mdl})
                                    (gen/large-integer* {:min 0 :max mdl}))
                         (fn [[i j k]] (gen/vector (gen/vector
                                                     (gen/tuple (gen/large-integer* {:min 0 :max (dec i)})
                                                                (gen/large-integer* {:min 0 :max (dec j)})
                                                                (s/gen ::number))
                                                     i)
                                                   j)))))   ;this needs updating, see vector version

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
  [m] (= (transpose m) m))

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
  ([m accu] (and (symmetric-matrix? m) (every? #(> % accu) (eigenvalues m)))))

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
  ([rows columns number] (vec (repeat rows (vec (repeat columns number))))))

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

;(defn non-negative-matrix-by-decreasing-off-diagonal
;  "Attempts to return a non-negative matrix by decreasing the absolute values
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

(defn rnd-matrix
  "Returns matrix with random elements"
  [rows columns]
  (let [[v s] (take (* rows columns) (random/rand-double-lazy))]
    [(partition columns v) s]))

(s/fdef rnd-matrix
        :args (s/cat :rows ::rows :columns ::columns)
        :ret ::matrix)

(defn rnd-reflection-matrix
  "Returns [m rnd-lazy], where m is a random Householder reflection."
  [size rnd-lazy]
  (let [v (column-matrix (tensor/normalize (take size rnd-lazy)))]
    [(tensor/subtract (identity-matrix size) (matrix-multiply (matrix-multiply v (transpose v)) 2.0))
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
                        :else (let [indices (or i (range n))]
                                (if (number? except-i)
                                  (remove #(or (= except-i %) (>= % n)) indices)
                                  (reduce
                                    (fn [tot e] (if (or (>= e n) (some #(= % e) except-i)) tot (conj tot e)))
                                    []
                                    indices)))))
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

(defn some-kv
  "Returns the first logical true value of (pred row column number) for any number in `m`, else nil."
  [pred m]
  (loop [i 0, s m]
    (when (seq s)
      (or (pred i (first s)) (recur (inc i) (next s))))))

(s/fdef some-kv
        :args (s/cat :pred (s/fspec :args (s/cat :row ::row :column ::column :number ::number)
                                    :ret boolean?)
                     :m ::matrix)
        :ret ::number)

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

;;;MATRIX MANIPULATION
(defn transpose
  "Transposes a matrix by swapping rows and columns, returning a new matrix."
  [m] (apply mapv vector m))

(s/fdef transpose
        :args (s/cat :m ::matrix)
        :ret ::matrix)

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
    (mapv #(vector/removev % column) m)
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
     ::row-indices provides the row index or indices of the permutation.
     ::column-indices provides the column index or indices of the permutation."
  [m & {::keys [row-indices column-indices]}]
  (let [after-rows (if row-indices (mapv (partial get-row m) (vector/to-vector row-indices)) m)]
    (if column-indices
      (transpose (mapv (partial get-row (transpose after-rows)) (vector/to-vector column-indices)))
      after-rows)))

(s/fdef permute-matrix
        :args (s/cat :m ::matrix :args (s/? (s/keys :opt [::row-indices ::column-indices])))
        :ret ::matrix)

;;;MATRIX MATH
(defn matrix-multiply
  ([] 1.0)
  ([m] m)
  ([m1 m2] (mapv (fn [a] (mapv (fn [b] (reduce + (map * a b))) (transpose m2))) m1))
  ([m1 m2 & ms] (apply matrix-multiply (matrix-multiply m1 m2) ms)))

(s/fdef matrix-multiply
        :args (s/or :zero-or-one (s/cat :m (s/? ::matrix))
                    :two+ (s/cat :m1 ::matrix :m2 ::matrix :ms (s/? (s/keys* :mats ::matrix)))))

(defn inner-product                                         ;is this defined for tensors?
  "Computes the inner product of numerical arrays.
For matrix/matrix and matrix/vector arguments, this is equivalent to matrix multiplication.
The inner product of two arrays with indexed dimensions {..i j} and {j k..} has dimensions {..i k..}.
The inner-product of two vectors will be scalar."
  ([a] (mxc/inner-product a))
  ([a b] (mxc/inner-product a b))
  ([a b & more] (apply mxc/inner-product a b more)))

(defn kronecker-product [m & ms]
  {:pre [(have? matrix? m) (have? (partial every? matrix?) ms)]}
  (reduce (fn [a b]
            (let [a-rows (row-count a)
                  a-cols (column-count a)]
              (apply conj-rows (for [i (range a-rows)]
                                 (apply conj-columns
                                        (for [j (range a-cols)]
                                          (tensor/multiply (get-in a [i j]) b)))))))
          m ms))

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
(defn sparse-efilter                                        ;;these two sparse filters are how to create 'sparse' from matrix, default pred should be not= 0
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
(defn filter-kv
  "Returns a vector of the items in coll for which (pred item) returns true.
  pred must be free of side-effects."
  [pred m] (persistent! (reduce-kv #(if (pred %2 %3) (conj! % %3) %) (transient []) m)))

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