(ns provisdom.math.apache-matrix
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]
            [provisdom.math.core :as m]
            [provisdom.math.matrix :as mx]
            [provisdom.math.vector :as vector]
            [provisdom.math.tensor :as tensor]
            [provisdom.math.arrays :as ar]
            [provisdom.math.random2 :as random])
  (:import [org.apache.commons.math3.linear Array2DRowRealMatrix RealMatrix
                                            QRDecomposition LUDecomposition CholeskyDecomposition
                                            RectangularCholeskyDecomposition Array2DRowRealMatrix
                                            EigenDecomposition SingularValueDecomposition RRQRDecomposition
                                            DecompositionSolver MatrixUtils]))

(set! *warn-on-reflection* true)

(declare apache-matrix? apache-square-matrix? apache-matrix eigen-decomposition
         rrqr-decomposition rows columns
         transpose positive-definite-apache-matrix-finite-by-squaring
         positive-semidefinite-apache-matrix-finite-by-squaring diagonal
         mx* add covariance-apache-matrix->correlation-apache-matrix
         correlation-apache-matrix-finite-by-squaring some-kv
         get-entry assoc-entry! symmetric-apache-matrix-by-averaging! assoc-diagonal!)

(s/def ::exception (partial instance? Exception))
(s/def ::accu ::tensor/accu)
(s/def ::size ::mx/size)
(s/def ::number ::m/number)
(s/def ::numbers ::vector/numbers)
(s/def ::vector ::vector/vector)
(s/def ::vector-finite+ ::vector/vector-finite+)
(s/def ::matrix ::mx/matrix)
(s/def ::rank ::m/int-non-)
(s/def ::row ::mx/row)
(s/def ::column ::mx/column)
(s/def ::rows ::mx/rows)
(s/def ::columns ::mx/columns)
(s/def ::by-row? ::mx/by-row?)
(s/def ::row-indices ::mx/row-indices)
(s/def ::column-indices ::mx/column-indices)
(s/def ::exception-row-indices ::mx/exception-row-indices)
(s/def ::exception-column-indices ::mx/exception-column-indices)

;;;MATRIX TYPES
(defn apache-matrix?
  "Returns true if an Apache Commons matrix."
  [x] (instance? Array2DRowRealMatrix x))

(s/fdef apache-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::apache-matrix (s/with-gen apache-matrix? #(gen/fmap apache-matrix (s/gen ::matrix))))

(defn empty-apache-matrix?
  "Returns true if an empty Apache Commons matrix."
  [x] (and (apache-matrix? x) (zero? (rows x))))

(s/fdef empty-apache-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn apache-matrix-finite?
  "Returns true if an Apache Commons matrix without infinite numbers."
  [x] (and (apache-matrix? x) (nil? (some-kv (fn [_ _ number] (m/inf? number)) x))))

(s/fdef apache-matrix-finite?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::apache-matrix-finite (s/with-gen apache-matrix-finite? #(gen/fmap apache-matrix (s/gen ::matrix-finite))))

(defn square-apache-matrix?
  "Returns true if a square Apache Commons matrix (i.e., same number of rows and columns)."
  [x] (and (apache-matrix? x) (.isSquare ^Array2DRowRealMatrix x)))

(s/fdef square-apache-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::square-apache-matrix (s/with-gen square-apache-matrix? #(gen/fmap apache-matrix (s/gen ::mx/square-matrix))))
(s/def ::square-apache-matrix-finite
  (s/with-gen (s/and square-apache-matrix? apache-matrix-finite?)
              #(gen/fmap apache-matrix (s/gen ::mx/square-matrix-finite))))

(defn diagonal-apache-matrix?
  "Returns true if a diagonal matrix (the entries outside the main diagonal are all zero)."
  [x] (and (apache-matrix? x) (nil? (some-kv (fn [i j e] (not (or (= i j) (zero? e)))) x))))

(s/fdef diagonal-apache-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::diagonal-apache-matrix
  (s/with-gen diagonal-apache-matrix? #(gen/fmap apache-matrix (s/gen ::mx/diagonal-matrix))))

(defn upper-triangular-apache-matrix?
  "Returns true if an upper triangular matrix (the entries below the main diagonal are all zero)."
  [x] (and (square-apache-matrix? x) (nil? (some-kv (fn [i j e] (not (or (<= i j) (zero? e)))) x))))

(s/fdef upper-triangular-apache-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::upper-triangular-apache-matrix
  (s/with-gen upper-triangular-apache-matrix? #(gen/fmap apache-matrix (s/gen ::mx/upper-triangular-matrix))))

(defn lower-triangular-apache-matrix?
  "Returns true if a lower triangular matrix (the entries above the main diagonal are all zero)."
  [x] (and (square-apache-matrix? x) (nil? (some-kv (fn [i j e] (not (or (>= i j) (zero? e)))) x))))

(s/fdef lower-triangular-apache-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::lower-triangular-apache-matrix
  (s/with-gen lower-triangular-apache-matrix? #(gen/fmap apache-matrix (s/gen ::mx/lower-triangular-matrix))))

(defn symmetric-apache-matrix?
  "Returns true is a symmetric Apache Commons matrix."
  [x] (and (apache-matrix? x) (MatrixUtils/isSymmetric x 0.0)))

(s/fdef symmetric-apache-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::symmetric-apache-matrix
  (s/with-gen symmetric-apache-matrix? #(gen/fmap apache-matrix (s/gen ::mx/symmetric-matrix))))

(defn positive-semidefinite-apache-matrix-finite?
  "Returns true if a positive-semidefinite Apache matrix.
  Larger `accu` creates more false positives and less false negatives."
  [x accu]
  (and (symmetric-apache-matrix? x)
       (apache-matrix-finite? x)
       (every? #(m/roughly-non-? % accu)
               (try (::eigenvalues (eigen-decomposition x))
                    (catch Exception _ nil)))))

(s/fdef positive-semidefinite-apache-matrix-finite?
        :args (s/cat :x any? :accu ::accu)
        :ret boolean?)

(s/def ::positive-semidefinite-apache-matrix-finite
  (s/with-gen #(positive-semidefinite-apache-matrix-finite? % m/*sgl-close*)
              #(gen/fmap (fn [m] (when-not (mx/empty-matrix? m)
                                   (positive-semidefinite-apache-matrix-finite-by-squaring (apache-matrix m))))
                         (s/gen ::mx/square-matrix-finite))))

(defn positive-definite-apache-matrix-finite?
  "Returns true if a positive definite Apache matrix.
  Larger `accu` creates more false negatives and less false positives."
  [x accu]
  (and (symmetric-apache-matrix? x)
       (apache-matrix-finite? x)
       (every? #(> % accu)
               (try (::eigenvalues (eigen-decomposition x))
                    (catch Exception _ nil)))))

(s/fdef positive-definite-apache-matrix-finite?
        :args (s/cat :x any? :accu ::accu)
        :ret boolean?)

(s/def ::positive-definite-apache-matrix-finite
  (s/with-gen #(positive-definite-apache-matrix-finite? % m/*sgl-close*)
              #(gen/fmap (fn [m] (when-not (mx/empty-matrix? m)
                                   (positive-definite-apache-matrix-finite-by-squaring (apache-matrix m))))
                         (s/gen ::mx/square-matrix-finite))))

(defn correlation-apache-matrix-finite?
  "Returns true if a positive definite Apache Commons matrix with a unit diagonal.
  Larger `accu` creates more false negatives and less false positives."
  [x accu] (and (positive-definite-apache-matrix-finite? x accu) (every? m/one? (diagonal x))))

(s/fdef correlation-apache-matrix-finite?
        :args (s/cat :x any? :accu ::accu)
        :ret boolean?)

(s/def ::correlation-apache-matrix-finite
  (s/with-gen #(correlation-apache-matrix-finite? % m/*sgl-close*)
              #(gen/fmap (fn [m] (when-not (mx/empty-matrix? m)
                                   (correlation-apache-matrix-finite-by-squaring (apache-matrix m))))
                         (s/gen ::mx/square-matrix-finite))))

;;;MATRIX CONSTRUCTORS
(defn- block-apache-matrix->apache-matrix
  "Converts BlockRealMatrix (or other RealMatrix) to Array2DRowRealMatrix."
  [block-apache-matrix]
  (if (zero? (.getRowDimension ^RealMatrix block-apache-matrix))
    (Array2DRowRealMatrix.)
    (Array2DRowRealMatrix. ^"[[D" (.getData ^RealMatrix block-apache-matrix))))

(defn apache-matrix
  "Returns a matrix using the Apache Commons matrix implementation."
  [m]
  (if (mx/empty-matrix? m)
    (Array2DRowRealMatrix.)
    (Array2DRowRealMatrix. ^"[[D" (ar/jagged-2D-array :d m))))

(s/fdef apache-matrix
        :args (s/cat :m ::matrix)
        :ret (s/nilable ::apache-matrix))

(defn apache-matrix->matrix
  "Converts an Apache Commons matrix into a matrix."
  [apache-m]
  (if (zero? (rows apache-m))
    [[]]
    (mapv vec (.getData ^Array2DRowRealMatrix apache-m))))

(s/fdef apache-matrix->matrix
        :args (s/cat :apache-m ::apache-matrix)
        :ret ::matrix)

(defn positive-semidefinite-apache-matrix-finite-by-squaring
  "Returns a positive semidefinite Apache Commons matrix by first squaring 'square-apache-m-finite'."
  [square-apache-m-finite]
  (let [size (rows square-apache-m-finite)
        new-m square-apache-m-finite]
    (if (zero? size)
      square-apache-m-finite
      (loop [i 0]
        (when (< i 310)
          (let [lower-m (mx* new-m (apache-matrix (mx/diagonal-matrix (repeat size (m/pow 10 (m/one- (m/pow 2 i)))))))
                lower-m (mx* lower-m (transpose lower-m))
                _ (symmetric-apache-matrix-by-averaging! lower-m)]
            (if (positive-semidefinite-apache-matrix-finite? lower-m m/*sgl-close*)
              lower-m
              (recur (inc i)))))))))

(s/fdef positive-semidefinite-apache-matrix-finite-by-squaring
        :args (s/cat :square-apache-m-finite ::square-apache-matrix-finite)
        :ret (s/nilable ::positive-semidefinite-apache-matrix-finite))

(defn positive-definite-apache-matrix-finite-by-squaring
  "Returns a positive definite Apache Commons matrix squaring it.
  Will tweak original matrix if necessary to ensure positive definite."
  [square-apache-m-finite]
  (let [size (rows square-apache-m-finite)
        new-m square-apache-m-finite]
    (if (zero? size)
      square-apache-m-finite
      (loop [i 0]
        (when (< i 10)
          (let [lower-m (mx* new-m (apache-matrix (mx/diagonal-matrix (repeat size (m/pow 10 (m/one- (m/pow 2 i)))))))
                lower-m (if (zero? i)
                          lower-m
                          (add lower-m
                               (apache-matrix
                                 (mx/diagonal-matrix
                                   size (fn [_] (* (if (odd? i) (- 1.0) 1.0) (m/pow 10 (- i m/*sgl-digits*))))))))
                lower-m (mx* lower-m (transpose lower-m))
                _ (symmetric-apache-matrix-by-averaging! lower-m)]
            (if (positive-definite-apache-matrix-finite? lower-m m/*sgl-close*)
              lower-m
              (recur (inc i)))))))))

(s/fdef positive-definite-apache-matrix-finite-by-squaring
        :args (s/cat :square-apache-m-finite ::square-apache-matrix-finite)
        :ret (s/nilable ::positive-definite-apache-matrix-finite))

(defn correlation-apache-matrix-finite-by-squaring
  "Returns a Correlation Apache Commons matrix by first squaring 'square-apache-m'."
  [square-apache-m-finite]
  (if (zero? (rows square-apache-m-finite))
    square-apache-m-finite
    (let [new-m (covariance-apache-matrix->correlation-apache-matrix
                  (positive-definite-apache-matrix-finite-by-squaring square-apache-m-finite))
          size (when new-m (rows new-m))]
      (when new-m
        (loop [i 0]
          (when (< i 100)
            (let [lower-m (mx* new-m (apache-matrix (mx/diagonal-matrix (repeat size (m/one- (* 0.01 i))))))
                  _ (symmetric-apache-matrix-by-averaging! lower-m)
                  _ (assoc-diagonal! lower-m (repeat size 1.0))]
              (if (correlation-apache-matrix-finite? lower-m m/*sgl-close*)
                lower-m
                (recur (inc i))))))))))

(s/fdef correlation-apache-matrix-finite-by-squaring
        :args (s/cat :square-apache-m-finite ::square-apache-matrix-finite)
        :ret (s/nilable ::correlation-apache-matrix-finite))

(defn rnd-positive-definite-apache-matrix-finite!
  "Returns a positive definite Apache Commons matrix with a random spectrum.
  The orthogonal matrices are generated by using 2 × `size` composed Householder reflections.
  Alternative #1: Sample from the Inverse-Wishart Distribution.
  Alternative #2: Use [[positive-definite-apache-matrix-by-squaring]] with a random square matrix."
  [size]
  (if (zero? size)
    (apache-matrix [[]])
    (loop [i 0]
      (if (< i 100)
        (let [m (apache-matrix (mx/rnd-spectral-matrix! (vec (take size (random/rand-double-lazy!)))))]
          (if (positive-definite-apache-matrix-finite? m m/*sgl-close*)
            m
            (recur (inc i))))))))

(s/fdef rnd-positive-definite-apache-matrix-finite!
        :args (s/cat :size ::size)
        :ret ::positive-definite-apache-matrix-finite)

(defn rnd-correlation-apache-matrix-finite!
  "Returns a correlation Apache Commons matrix from a covariance matrix with a random spectrum.
  The orthogonal matrices are generated by using 2 * `size` composed Householder reflections.
  Alternative #1: Sample Covariance from the Inverse-Wishart Distribution.
  Alternative #2: Use [[correlation-apache-matrix-by-squaring]] with a random square matrix."
  [size]
  (if (zero? size)
    (apache-matrix [[]])
    (covariance-apache-matrix->correlation-apache-matrix (rnd-positive-definite-apache-matrix-finite! size))))

(s/fdef rnd-correlation-apache-matrix-finite!
        :args (s/cat :size ::size)
        :ret ::correlation-apache-matrix-finite)

;;;MATRIX INFO
(defn rows
  "Returns the number of rows."
  [apache-m] (.getRowDimension ^Array2DRowRealMatrix apache-m))

(s/fdef rows
        :args (s/cat :apache-m ::apache-matrix)
        :ret ::rows)

(defn columns
  "Returns the number of columns."
  [apache-m] (.getColumnDimension ^Array2DRowRealMatrix apache-m))

(s/fdef columns
        :args (s/cat :apache-m ::apache-matrix)
        :ret ::columns)

(defn get-entry
  "Returns the specified Apache Commons matrix element."
  [apache-m row column] (.getEntry ^Array2DRowRealMatrix apache-m row column))

(s/fdef get-entry
        :args (s/and (s/cat :apache-m ::apache-matrix :row ::row :column ::column)
                     #(and (< (:row %) (rows (:apache-m %))) (< (:column %) (columns (:apache-m %)))))
        :ret ::number)

(defn get-row
  "Gets a `row` of an Apache Commons matrix, as a vector."
  [apache-m row] (vec (.getRow ^Array2DRowRealMatrix apache-m row)))

(s/fdef get-row
        :args (s/and (s/cat :apache-m ::apache-matrix :row ::row)
                     #(< (:row %) (rows (:apache-m %))))
        :ret ::vector)

(defn get-column
  "Gets a `column` of an Apache Commons matrix, as a vector."
  [apache-m column] (vec (.getColumn ^Array2DRowRealMatrix apache-m column)))

(s/fdef get-column
        :args (s/and (s/cat :apache-m ::apache-matrix :column ::column)
                     #(< (:column %) (columns (:apache-m %))))
        :ret ::vector)

(defn diagonal
  "Returns the specified diagonal of an Apache Commons matrix as a vector.
   If `k`>0, returns a diagonal above the main diagonal.
   If `k`<0, returns a diagonal below the main diagonal.
   Works on both square and rectangular matrices."
  ([apache-m]
   (if (zero? (rows apache-m))
     []
     (reduce (fn [tot e] (conj tot (get-entry apache-m e e))) [] (range (min (rows apache-m) (columns apache-m))))))
  ([apache-m k]
   (if (zero? (rows apache-m))
     []
     (let [r (if (neg? k) (- k) 0)
           c (if (pos? k) k 0)
           nc (- (columns apache-m) c)
           nr (- (rows apache-m) r)
           start (- (min r c))
           end (min nc nr)]
       (if (pos? end)
         (vec (for [i (range start end)] (get-entry apache-m (+ i r) (+ i c))))
         [])))))

(s/fdef diagonal
        :args (s/cat :apache-m ::apache-matrix :k (s/? ::m/int))
        :ret ::vector)

(defn trace
  "Calculates the trace of a square Apache Commons matrix (sum of elements on main diagonal)."
  [square-apache-m] (.getTrace ^Array2DRowRealMatrix square-apache-m))

(s/fdef trace
        :args (s/cat :square-apache-m ::square-apache-matrix)
        :ret ::number)

(defn get-slices-as-matrix
  "Performs a slice on the Apache Commons matrix given by the options.
  Options:
    `::row-indices` returns all rows by default, can pass a row index or sequence of row indices
    `::column-indices` returns all columns by default, can pass a column index or sequence of column indices
    `::exception-row-indices` can pass a row index or sequence of row indices to exclude
    `::exception-column-indices` can pass a column index or sequence of column indices to exclude.
    Exceptions override inclusions.
    Can be used to permute matrix through index sequence ordering."
  [apache-m {::keys [row-indices column-indices exception-row-indices exception-column-indices]}]
  (let [calc-fn (fn [i except-i n]
                  (cond (and (not i) (not except-i)) true
                        (not except-i) (if (number? i)
                                         (if (< i n) i [])
                                         (remove #(>= % n) i))
                        (number? i) (if (number? except-i)
                                      (if (= except-i i) [] (if (< i n) i []))
                                      (if (contains? (set except-i) i) [] (if (< i n) i [])))
                        :else (let [indices (or i (range n))]
                                (if (number? except-i)
                                  (remove #(or (= except-i %) (>= % n)) indices)
                                  (reduce
                                    (fn [tot e] (if (or (>= e n) (some #(= % e) except-i)) tot (conj tot e)))
                                    []
                                    indices)))))
        n-rows (rows apache-m)
        rs (calc-fn row-indices exception-row-indices n-rows)
        cs (calc-fn column-indices exception-column-indices (columns apache-m))
        new-m (cond
                (or (and (coll? rs) (empty? rs)) (and (coll? cs) (empty? cs))) nil
                (and (number? rs) (number? cs)) [[(get-entry apache-m rs cs)]]
                (and (number? rs) (coll? cs)) (mx/row-matrix (let [row-vector (get-row apache-m rs)]
                                                               (map #(get row-vector %) cs)))
                (and (number? rs) (true? cs)) (mx/row-matrix (get-row apache-m rs))
                (and (coll? rs) (number? cs)) (mx/column-matrix (let [column-vector (get-column apache-m cs)]
                                                                  (map #(nth column-vector %) rs)))
                (and (coll? rs) (coll? cs)) (mapv (fn [row-vector]
                                                    (reduce (fn [tot column] (conj tot (get row-vector column))) [] cs))
                                                  (map #(get-row apache-m %) rs))
                (and (coll? rs) (true? cs)) (mapv #(get-row apache-m %) rs)
                (and (true? rs) (number? cs)) (mx/column-matrix (get-column apache-m cs))
                (and (true? rs) (coll? cs)) (mapv (fn [row]
                                                    (reduce (fn [tot column] (conj tot (get-entry apache-m row column)))
                                                            []
                                                            cs))
                                                  (range n-rows))
                (and (true? rs) (true? cs)) apache-m)]
    (when new-m
      (if (apache-matrix? new-m) new-m (apache-matrix new-m)))))

(s/fdef get-slices-as-matrix
        :args (s/cat :apache-m ::apache-matrix
                     :opts (s/keys :opt [::row-indices
                                         ::column-indices
                                         ::exception-row-indices
                                         ::exception-column-indices]))
        :ret (s/nilable ::apache-matrix))

(defn some-kv
  "Returns the first logical true value of (pred row column number) for any number in Apache Commons matrix, else nil.
  Options: `::by-row?` (default: true)."
  ([pred apache-m] (some-kv pred apache-m {::by-row? true}))
  ([pred apache-m {::keys [by-row?] :or {by-row? true}}]
   (let [mt (if by-row? apache-m (transpose apache-m))
         rows (rows mt)]
     (loop [row 0]
       (when (< row rows)
         (or (vector/some-kv (fn [column number] (pred row column number)) (get-row mt row))
             (recur (inc row))))))))

(s/fdef some-kv
        :args (s/cat :pred (s/fspec :args (s/cat :row ::row :column ::column :number ::number)
                                    :ret boolean?)
                     :apache-m ::apache-matrix
                     :opts (s/? (s/keys :opt [::by-row?])))
        :ret (s/nilable ::number))

;;;MATRIX MANIPULATION
(defn transpose
  "Transposes an Apache Commons matrix by swapping rows and columns, returning a new Apache Commons matrix."
  [apache-m]
  (if (zero? (rows apache-m))
    apache-m
    (.transpose ^Array2DRowRealMatrix apache-m)))

(s/fdef transpose
        :args (s/cat :apache-m ::apache-matrix)
        :ret ::apache-matrix)

(defn assoc-entry!
  "Sets an entry in an Apache Commons matrix."
  [apache-m row column number]
  (when (and (< row (rows apache-m)) (< column (columns apache-m)))
    (.setEntry ^Array2DRowRealMatrix apache-m ^long row ^long column ^double (double number))))

(s/fdef assoc-entry!
        :args (s/cat :apache-m ::apache-matrix :row ::row :column ::column :number ::number)
        :ret nil)

(defn assoc-diagonal!
  "Sets a diagonal in an Apache Commons matrix using the specified numbers."
  [apache-m numbers]
  (let [v (vec numbers)
        c (count numbers)]
    (when (= c (count (diagonal apache-m)))
      (doseq [rc (range c)]
        (assoc-entry! apache-m rc rc (get v rc 0.0))))))

(s/fdef assoc-diagonal!
        :args (s/cat :apache-m ::apache-matrix :numbers ::numbers)
        :ret nil)

(defn symmetric-apache-matrix-by-averaging!
  "Updates symmetric Apache Commons matrix where each element above or below the diagonal is equal to the
  average of the corresponding numbers.
  This is useful to help with rounding errors."
  [square-apache-m]
  (doseq [row (range (rows square-apache-m))]
    (doseq [column (range (inc row) (columns square-apache-m))]
      (let [number (* 0.5 (+ (get-entry square-apache-m row column) (get-entry square-apache-m column row)))]
        (assoc-entry! square-apache-m row column number)
        (assoc-entry! square-apache-m column row number)))))

(s/fdef symmetric-apache-matrix-by-averaging!
        :args (s/cat :square-apache-m ::square-apache-matrix)
        :ret nil)

(defn concat-rows
  "Appends rows from all the Apache Common matrices after the first to the first.
  Each matrix's column count must be the same or will return nil."
  ([] (apache-matrix [[]]))
  ([apache-m] apache-m)
  ([apache-m & apache-ms]
   (when-let [new-m (apply mx/concat-rows (apache-matrix->matrix apache-m) (map apache-matrix->matrix apache-ms))]
     (apache-matrix new-m))))

(s/fdef concat-rows
        :args (s/or :zero (s/cat) :one+ (s/cat :apache-m ::apache-matrix :apache-ms (s/* ::apache-matrix)))
        :ret (s/nilable ::apache-matrix))

(defn concat-columns
  "Appends columns from all the Apache Common matrices after the first to the first.
  Each matrix's row count must be the same or will return nil."
  ([] (apache-matrix [[]]))
  ([apache-m] apache-m)
  ([apache-m & apache-ms]
   (when-let [new-m (apply mx/concat-columns (apache-matrix->matrix apache-m) (map apache-matrix->matrix apache-ms))]
     (apache-matrix new-m))))

(s/fdef concat-columns
        :args (s/or :zero (s/cat) :one+ (s/cat :apache-m ::apache-matrix :apache-ms (s/* ::apache-matrix)))
        :ret (s/nilable ::apache-matrix))

(defn correlation-apache-matrix->covariance-apache-matrix
  "Returns Covariance Apache Commons matrix from a Correlation Apache Commons matrix."
  [correlation-apache-matrix-finite variances]
  (when (= (count variances) (rows correlation-apache-matrix-finite))
    (if (zero? (count variances))
      correlation-apache-matrix-finite
      (let [sqrt-m (apache-matrix (mx/diagonal-matrix (mapv m/sqrt variances)))
            cov (mx* sqrt-m correlation-apache-matrix-finite sqrt-m)
            _ (symmetric-apache-matrix-by-averaging! cov)]
        (when (positive-definite-apache-matrix-finite? cov m/*sgl-close*) cov)))))

(s/fdef correlation-apache-matrix->covariance-apache-matrix
        :args (s/cat :correlation-apache-matrix-finite ::correlation-apache-matrix-finite :variances ::vector-finite+)
        :ret (s/nilable ::positive-definite-apache-matrix-finite))

(defn covariance-apache-matrix->correlation-apache-matrix
  "Returns Correlation Apache Commons matrix from a Covariance Apache Commons matrix."
  [covariance-apache-matrix]
  (if (zero? (rows covariance-apache-matrix))
    covariance-apache-matrix
    (let [inv-sqrt (apache-matrix (mx/diagonal-matrix (map #(m/pow % -0.5) (diagonal covariance-apache-matrix))))
          corr (mx* inv-sqrt covariance-apache-matrix inv-sqrt)
          _ (symmetric-apache-matrix-by-averaging! corr)
          _ (assoc-diagonal! corr (repeat (rows inv-sqrt) 1.0))]
      (when (positive-definite-apache-matrix-finite? corr m/*sgl-close*) corr))))

(s/fdef covariance-apache-matrix->correlation-apache-matrix
        :args (s/cat :covariance-apache-matrix ::positive-definite-apache-matrix-finite)
        :ret (s/nilable ::correlation-apache-matrix-finite))

;;;MATRIX MATH
(defn mx*
  "Apache Commons matrix multiplication.
  Number of columns of the first matrix must match the number of rows of the second matrix."
  ([apache-m] apache-m)
  ([apache-m1 apache-m2]
   (when (= (columns apache-m1) (rows apache-m2))
     (if (zero? (rows apache-m1))
       apache-m1
       (.multiply ^Array2DRowRealMatrix apache-m1 ^Array2DRowRealMatrix apache-m2))))
  ([apache-m1 apache-m2 & apache-ms] (when-let [apache-m3 (mx* apache-m1 apache-m2)] (apply mx* apache-m3 apache-ms))))

(s/fdef mx*
        :args (s/or :one (s/cat :apache-m ::apache-matrix)
                    :two+ (s/cat :apache-m1 ::apache-matrix
                                 :apache-m2 ::apache-matrix
                                 :apache-ms (s/* ::apache-matrix)))
        :ret (s/nilable ::apache-matrix))

(defn add
  "Apache Commons matrix addition."
  ([apache-m] apache-m)
  ([apache-m1 apache-m2]
   (if (and (zero? (rows apache-m1)) (zero? (rows apache-m2)))
     apache-m1
     (try (.add ^Array2DRowRealMatrix apache-m1 ^Array2DRowRealMatrix apache-m2)
          (catch Exception _ nil))))
  ([apache-m1 apache-m2 & apache-ms] (when-let [apache-m3 (add apache-m1 apache-m2)] (apply add apache-m3 apache-ms))))

(s/fdef add
        :args (s/or :one (s/cat :apache-m ::apache-matrix)
                    :two+ (s/cat :apache-m1 ::apache-matrix
                                 :apache-m2 ::apache-matrix
                                 :apache-ms (s/* ::apache-matrix)))
        :ret (s/nilable ::apache-matrix))

(defn subtract
  "Apache Commons matrix subtraction."
  ([apache-m] apache-m)
  ([apache-m1 apache-m2]
   (if (and (zero? (rows apache-m1)) (zero? (rows apache-m2)))
     apache-m1
     (try (.subtract ^Array2DRowRealMatrix apache-m1 ^Array2DRowRealMatrix apache-m2)
          (catch Exception _ nil))))
  ([apache-m1 apache-m2 & apache-ms]
   (when-let [apache-m3 (subtract apache-m1 apache-m2)] (apply subtract apache-m3 apache-ms))))

(s/fdef subtract
        :args (s/or :one (s/cat :apache-m ::apache-matrix)
                    :two+ (s/cat :apache-m1 ::apache-matrix
                                 :apache-m2 ::apache-matrix
                                 :apache-ms (s/* ::apache-matrix)))
        :ret (s/nilable ::apache-matrix))

;;;MATRIX DECOMPOSITION
(s/def ::inverse (s/nilable ::square-apache-matrix))
(defn inverse
  "Returns the inverse of a square Apache Commons matrix.
  Uses QR Decomposition by default but will use other methods depending on matrix structure."
  [square-apache-m]
  (if (zero? (rows square-apache-m))
    square-apache-m
    (try (block-apache-matrix->apache-matrix (MatrixUtils/inverse ^RealMatrix square-apache-m))
         (catch Exception _ nil))))

(s/fdef inverse
        :args (s/cat :square-apache-m ::square-apache-matrix)
        :ret ::inverse)

(s/def ::LU-permutation (s/nilable ::apache-matrix))
(s/def ::L (s/nilable ::lower-triangular-apache-matrix))
(s/def ::U (s/nilable ::upper-triangular-apache-matrix))
(s/def ::determinant ::number)
(defn lu-decomposition-with-determinant-and-inverse
  "Returns a map containing
      ::L -- the lower triangular factor Apache Commons matrix
      ::U -- the upper triangular factor Apache Commons matrix
      ::LU-permutation -- the permutation Apache Commons matrix
      ::determinant -- the determinant
      ::inverse -- the inverse Apache Commons matrix."
  [square-apache-m]
  (if (zero? (rows square-apache-m))
    {::L              square-apache-m
     ::U              square-apache-m
     ::LU-permutation square-apache-m
     ::determinant    m/nan
     ::inverse        square-apache-m}
    (let [lud (LUDecomposition. ^Array2DRowRealMatrix square-apache-m)
          s (.getSolver lud)
          inverse (try (.getInverse s) (catch Exception _ nil))
          det (.getDeterminant lud)]
      {::L (.getL lud), ::U (.getU lud), ::LU-permutation (.getP lud), ::determinant det, ::inverse inverse})))

(s/fdef lu-decomposition-with-determinant-and-inverse
        :args (s/cat :square-apache-m ::square-apache-matrix)
        :ret (s/keys :req [::L ::U ::LU-permutation ::determinant ::inverse]))

(defn lu-decomposition-with-determinant
  "Returns a map containing:
      ::L -- the lower triangular factor Apache Commons matrix
      ::U -- the upper triangular factor Apache Commons matrix
      ::LU-permutation -- the permutation Apache Commons matrix
      ::determinant -- the determinant."
  [square-apache-m]
  (if (zero? (rows square-apache-m))
    {::L square-apache-m, ::U square-apache-m, ::LU-permutation square-apache-m, ::determinant m/nan}
    (let [lud (LUDecomposition. ^Array2DRowRealMatrix square-apache-m)]
      {::L (.getL lud), ::U (.getU lud), ::LU-permutation (.getP lud) ::determinant (.getDeterminant lud)})))

(s/fdef lu-decomposition-with-determinant
        :args (s/cat :square-apache-m ::square-apache-matrix)
        :ret (s/keys :req [::L ::U ::LU-permutation ::determinant]))

(s/def ::eigenvectorsT ::square-apache-matrix)
(s/def ::eigenvalues-matrix ::apache-matrix)
(s/def ::eigenvectors ::square-apache-matrix)
(s/def ::eigenvalues (s/nilable ::vector/vector))

(defn eigen-decomposition
  "Computes the Eigendecomposition of a diagonalisable matrix.
   Returns a map containing:
   ::eigenvectorsT -- square Apache Commons matrix with each column containing the eigenvectors
   ::eigenvaluesD -- Apache Commons matrix whose diagonal elements are the eigenvalues, if they exist
   ::eigenvalues -- vector of real parts of eigenvalues (nil if imaginary parts exist)
   ::eigenvectors -- square Apache Commons matrix with each row containing the eigenvectors
   'square-apache-m-finite' = `eigenvectorsT` × `eigenvaluesD` × (inverse `eigenvectorsT`)."
  [square-apache-m-finite]
  (if (zero? (rows square-apache-m-finite))
    {::eigenvectorsT      square-apache-m-finite
     ::eigenvalues-matrix square-apache-m-finite
     ::eigenvalues        []
     ::eigenvectors       square-apache-m-finite}
    (try (let [r (EigenDecomposition. ^Array2DRowRealMatrix square-apache-m-finite)
               eigenvalues (when-not (.hasComplexEigenvalues r) (vec (.getRealEigenvalues r)))]
           {::eigenvectorsT      (.getV r)
            ::eigenvalues-matrix (.getD r)
            ::eigenvalues        eigenvalues
            ::eigenvectors       (.getVT r)})
         (catch Exception e (ex-info (.getMessage e) {:fn (var eigen-decomposition)})))))

(s/fdef eigen-decomposition
        :args (s/cat :square-apache-m-finite ::square-apache-matrix-finite)
        :ret (s/or :exception ::exception
                   :res (s/keys :req [::eigenvectorsT ::eigenvalues-matrix ::eigenvalues ::eigenvectors])))

(s/def ::cholesky-L ::lower-triangular-apache-matrix)
(s/def ::cholesky-LT ::upper-triangular-apache-matrix)
(defn cholesky-decomposition
  "Computes the Cholesky decomposition of a positive definite Apache Commons matrix.
   Returns a map of two Apache Commons matrices ::cholesky-L and ::choleskyLT.
   This is the Cholesky square root of a matrix, 'L' and 'LT' such that `positive-definite-apache-m` = L × LT.
   Note that `positive-definite-apache-m` must be positive semidefinite for this to exist,
      but [[cholesky-decomposition]] requires strict positivity."
  [positive-definite-apache-m]
  (if (zero? (rows positive-definite-apache-m))
    {::cholesky-L positive-definite-apache-m ::cholesky-LT positive-definite-apache-m}
    (try (let [r (CholeskyDecomposition. ^Array2DRowRealMatrix positive-definite-apache-m)]
           {::cholesky-L (.getL r) ::cholesky-LT (.getLT r)})
         (catch Exception e (ex-info (.getMessage e) {:fn (var cholesky-decomposition)})))))

(s/fdef cholesky-decomposition
        :args (s/cat :positive-definite-apache-m ::positive-definite-apache-matrix-finite)
        :ret (s/or :exception ::exception
                   :res (s/keys :req [::cholesky-L ::cholesky-LT])))

(s/def ::rectangular-root ::apache-matrix)
(defn rectangular-cholesky-decomposition
  "Calculates the rectangular Cholesky decomposition of a positive semidefinite Apache Commons matrix.
  The rectangular Cholesky decomposition of a real 'positive-semidefinite-apache-m' consists of a
  `rectangular-root` matrix with the same number of rows such that:
      'positive-semidefinite-apache-m' is almost equal to `rectangular-root` × (transpose `rectangular-root`),
      depending on a user-defined tolerance.
  In a sense, this is the square root of 'positive-semidefinite-apache-m'.
  The difference with respect to the regular CholeskyDecomposition is that rows/columns may be permuted
  (hence the rectangular shape instead of the traditional triangular shape) and there is a threshold to ignore small
   diagonal elements.
   This is used for example to generate correlated random n-dimensions vectors in a p-dimension subspace (p < n).
   In other words, it allows generating random vectors from a covariance matrix that is only positive semidefinite,
   and not positive definite.
   'accu' - Diagonal elements threshold under which columns are considered to be dependent on
      previous ones and are discarded.
      Returns a map containing:
         ::rectangular-root -- rectangular root Apache Commons matrix
         ::rank -- rank is the number of independent rows of original matrix,
            and the number of columns of `rectangular-root` matrix."
  [positive-semidefinite-apache-m accu]
  (if (zero? (rows positive-semidefinite-apache-m))
    {::rectangular-root positive-semidefinite-apache-m, ::rank 0}
    (try (let [r (RectangularCholeskyDecomposition. ^Array2DRowRealMatrix positive-semidefinite-apache-m (double accu))]
           {::rectangular-root (.getRootMatrix r) ::rank (.getRank r)})
         (catch Exception e (ex-info (.getMessage e) {:fn (var rectangular-cholesky-decomposition)})))))

(s/fdef rectangular-cholesky-decomposition
        :args (s/cat :positive-semidefinite-apache-m ::positive-semidefinite-apache-matrix-finite :accu ::accu)
        :ret (s/or :exception ::exception
                   :res (s/keys :req [::rectangular-root ::rank])))

(s/def ::svd-left ::apache-matrix)
(s/def ::singular-values ::diagonal-apache-matrix)
(s/def ::svd-right ::apache-matrix)
(defn sv-decomposition
  "Calculates the compact Singular Value Decomposition of an Apache Commons matrix.
  The Singular Value Decomposition of `apache-m` is a set of three
   matrices: `svd-left`, `singular-values`, and `svd-right` such that:
    `apache-m` = `svd-left` × `singular-values` × `svd-right`.
   Let `apache-m` be a m × n matrix, then `svd-left` is a m × p orthogonal matrix of the left singular vectors,
   `singular-values` is a p × p diagonal matrix of singular values with positive or nil elements,
   and are ordered from largest to smallest.
   `svd-right` is a p × n orthogonal matrix of the right singular vectors where p = min(m,n).
   Note that Identity Matrix = (transpose `svd-left`) × `svd-left` = `svd-right` × (transpose `svd-right`).
   Returns a map containing:
      ::svd-left -- Apache Commons matrix of left singular vectors
      ::singular-values -- diagonal Apache Commons matrix
      ::svd-right -- transpose of Apache Commons matrix of right singular vectors
      ::rank -- rank."
  [apache-m]
  (if (zero? (rows apache-m))
    {::svd-left apache-m, ::singular-values apache-m, ::svd-right apache-m, ::rank 0}
    (let [d (SingularValueDecomposition. ^Array2DRowRealMatrix apache-m)]
      {::svd-left (.getU d), ::singular-values (.getS d), ::svd-right (.getVT d), ::rank (.getRank d)})))

(s/fdef sv-decomposition
        :args (s/cat :apache-m ::apache-matrix)
        :ret (s/keys :req [::svd-left ::singular-values ::svd-right ::rank]))

(defn condition
  "The `singular-values-apache-matrix` is the diagonal matrix of ::singular-values from [[sv-decomposition]].
  Returns the norm2 condition number,
  which is the maximum element value from the `singular-values-apache-matrix` divided by the minimum element value."
  [singular-values-apache-matrix]
  (if (zero? (rows singular-values-apache-matrix))
    m/nan
    (let [vs (diagonal singular-values-apache-matrix)]
      (m/div (apply max vs) (apply min vs) m/nan))))

(s/fdef condition
        :args (s/cat :apache-singular-values-matrix ::singular-values)
        :ret ::number)

(s/def ::Q ::apache-matrix)
(s/def ::R ::apache-matrix)
(s/def ::LLS-solution (s/or :sol ::apache-matrix :exception ::exception))
(s/def ::error (s/nilable ::symmetric-apache-matrix))
(defn qr-decomposition-with-linear-least-squares-and-error-matrix
  "Returns a map containing:
      ::Q -- orthogonal factors of `apache-m1`
      ::R -- the upper triangular factors of `apache-m2` (not necessarily square)
      ::LLS-solution -- Apache Commons matrix with linear least squares solution
      ::error -- Apache Commons matrix of errors."
  [apache-m1 apache-m2]
  (if (zero? (rows apache-m1))
    {::Q apache-m1, ::R apache-m1, ::LLS-solution apache-m1, ::error apache-m1}
    (let [d (QRDecomposition. ^Array2DRowRealMatrix apache-m1)
          ^DecompositionSolver s (.getSolver d)
          ex-d {:fn (var qr-decomposition-with-linear-least-squares-and-error-matrix)}
          solution (try (block-apache-matrix->apache-matrix (.solve s ^Array2DRowRealMatrix apache-m2))
                        (catch Exception e (ex-info (.getMessage e) ex-d)))
          r (.getR d)
          r-rows (rows r)
          r-columns (columns r)
          error (when (and (apache-matrix? solution) (>= r-rows r-columns))
                  (let [trimmed-r (get-slices-as-matrix r {::exception-row-indices (range r-columns r-rows)})
                        ri (inverse trimmed-r)]
                    (mx* ri (transpose ri))))]
      {::Q (.getQ d), ::R r, ::LLS-solution solution, ::error error})))

(s/fdef qr-decomposition-with-linear-least-squares-and-error-matrix
        :args (s/cat :apache-m1 ::apache-matrix :apache-m2 ::apache-matrix)
        :ret (s/keys :req [::Q ::R ::LLS-solution ::error]))

(defn qr-decomposition-with-linear-least-squares
  "Returns a map containing:
    ::Q -- orthogonal factors of `apache-m1`
    ::R -- the upper triangular factors of `apache-m2`
    ::linear-least-squares -- Apache Commons matrix with linear least squares solution."
  [apache-m1 apache-m2]
  (if (zero? (rows apache-m1))
    {::Q apache-m1, ::R apache-m1, ::LLS-solution apache-m1}
    (let [d (QRDecomposition. ^Array2DRowRealMatrix apache-m1)
          ^DecompositionSolver s (.getSolver d)
          ex-d {:fn (var qr-decomposition-with-linear-least-squares)}
          solution (try (block-apache-matrix->apache-matrix (.solve s ^Array2DRowRealMatrix apache-m2))
                        (catch Exception e (ex-info (.getMessage e) ex-d)))]
      {::Q (.getQ d), ::R (.getR d), ::LLS-solution solution})))

(s/fdef qr-decomposition-with-linear-least-squares
        :args (s/cat :apache-m1 ::apache-matrix :apache-m2 ::apache-matrix)
        :ret (s/keys :req [::Q ::R ::LLS-solution]))

(defn qr-decomposition
  "Computes the QR decomposition of a matrix.
  Returns a map containing:
    ::Q -- orthogonal factors
    ::R -- the upper triangular factors."
  [apache-m]
  (if (zero? (rows apache-m))
    {::Q apache-m, ::R apache-m}
    (let [d (QRDecomposition. ^Array2DRowRealMatrix apache-m)]
      {::Q (.getQ d), ::R (.getR d)})))

(s/fdef qr-decomposition
        :args (s/cat :apache-m ::apache-matrix)
        :ret (s/keys :req [::Q ::R]))

(s/def ::RRQR-permutation ::apache-matrix)
(defn rank-revealing-qr-decomposition
  "Calculates the rank-revealing QR-decomposition of a matrix, with column pivoting.
  The rank-revealing QR-decomposition of `apache-m` consists of three matrices `Q`, `R`, and `permutation`
  such that `apache-m` × `permutation` = `Q` × `R`.
  `Q` is orthogonal (`QT` × `Q` = I), and `R` is upper triangular.
  If `apache-m` is m × n, `Q` is m × m, `R` is m × n, and `permutation` is n × n.
  QR decomposition with column pivoting produces a rank-revealing QR decomposition.
  This class computes the decomposition using Householder reflectors.
  Returns a map containing:
      ::Q -- orthogonal factors
      ::R -- the upper triangular factors
      ::RRQR-permutation -- Permutation Matrix
      ::rank -- the rank."
  [apache-m accu]
  (if (zero? (rows apache-m))
    {::Q                apache-m
     ::R                apache-m
     ::RRQR-permutation apache-m
     ::rank             0}
    (let [d (RRQRDecomposition. ^Array2DRowRealMatrix apache-m (double accu))]
      {::Q                (.getQ d)
       ::R                (.getR d)
       ::RRQR-permutation (.getP d)
       ::rank             (.getRank d accu)})))

(s/fdef rank-revealing-qr-decomposition
        :args (s/cat :apache-m ::apache-matrix :accu ::accu)
        :ret (s/keys :req [::Q ::R ::RRQR-permutation ::rank]))