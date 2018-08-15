(ns provisdom.math.matrix
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.utility-belt.extensions :as extensions]
    [provisdom.math.core :as m]
    [provisdom.math.tensor :as tensor]
    [provisdom.math.vector :as vector]
    [provisdom.math.random :as random]))

;;;DECLARATIONS
(declare column-matrix transpose diagonal deserialize-symmetric-matrix
         deserialize-upper-triangular-matrix deserialize-lower-triangular-matrix
         get-slices-as-matrix some-kv matrix? row-matrix? column-matrix?
         square-matrix? symmetric-matrix? diagonal-matrix? diagonal-matrix
         row-matrix rows columns size-of-symmetric-or-triangular-matrix
         size-of-symmetric-or-triangular-matrix-without-diagonal
         to-matrix symmetric-matrix-by-averaging constant-matrix mx* assoc-diagonal
         ecount-of-symmetric-or-triangular-matrix
         ecount-of-symmetric-or-triangular-matrix-without-diagonal
         lower-triangular-matrix? upper-triangular-matrix?)

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
    #(gen/bind (gen/tuple (gen/large-integer* {:min 0 :max mdl})
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

;;;MATRIX TYPES
(defn matrix?
  "Returns true if a matrix (i.e., dimensionality is 2, contains numbers only,
  rows have equal positive lengths unless the empty matrix.)"
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

(s/def ::matrix
  (s/with-gen
    matrix?
    #(gen/bind (gen/large-integer* {:min 0 :max mdl})
               (fn [i]
                 (gen/vector
                   (gen/vector (s/gen ::m/number) i)
                   1
                   mdl)))))

(defn matrix-num?
  "Returns true if a matrix of num (numbers without NaN)."
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
  "Returns true if a matrix of finite numbers."
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

(s/def ::matrix-finite
  (s/with-gen
    matrix-finite?
    #(gen/bind (gen/large-integer* {:min 0 :max mdl})
               (fn [i]
                 (gen/vector
                   (gen/vector (s/gen ::m/finite) i)
                   1
                   mdl)))))

(defn empty-matrix?
  "Returns true if the matrix is an empty matrix."
  [x]
  (and (matrix? x) (empty? (get x 0))))

(s/fdef empty-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::empty-matrix #(= [[]] %))

(defn row-matrix?
  "Returns true if a row-matrix (i.e., matrix with exactly one row)"
  [x]
  (and (vector? x)
       (m/one? (count x))
       (vector? (first x))
       (every? number? (first x))))

(s/fdef row-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::row-matrix
  (s/with-gen
    row-matrix?
    #(gen/fmap row-matrix (s/gen ::vector/vector))))

(defn column-matrix?
  "Returns true if a column-matrix (i.e., matrix with exactly one column)."
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

(s/def ::column-matrix
  (s/with-gen
    column-matrix?
    #(gen/fmap column-matrix (s/gen ::vector/vector))))

(defn zero-matrix?
  "Returns true if all the elements of the matrix are zeros."
  [x]
  (and (matrix? x) (every? zero? (flatten x))))

(s/fdef zero-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::zero-matrix
  (s/with-gen
    (s/coll-of (s/coll-of zero? :kind vector? :into [])
               :kind vector?
               :into [])
    #(gen/vector (gen/vector (s/gen zero?) 0 mdl) 0 mdl)))

(defn square-matrix?
  "Returns true if matrix is square (i.e., same number of rows and columns)"
  [x]
  (and (matrix? x) (= (rows x) (columns x))))

(s/fdef square-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::square-matrix
  (s/with-gen
    square-matrix?
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

(defn diagonal-matrix?
  "Returns true if a diagonal matrix (the entries outside the main diagonal are
  all zero)."
  [x]
  (and (matrix? x)
       (nil? (some-kv (fn [i j e]
                        (not (or (= i j) (zero? e))))
                      x))))

(s/fdef diagonal-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::diagonal-matrix
  (s/with-gen
    diagonal-matrix?
    #(gen/fmap diagonal-matrix (s/gen ::vector/vector))))

(defn upper-triangular-matrix?
  "Returns true if an upper triangular matrix (the entries below the main
  diagonal are all zero)."
  [x]
  (and (square-matrix? x)
       (nil? (some-kv (fn [i j e]
                        (not (or (<= i j) (zero? e))))
                      x))))

(s/fdef upper-triangular-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::upper-triangular-matrix
  (s/with-gen
    upper-triangular-matrix?
    #(gen/bind (gen/large-integer* {:min 0 :max mdl})
               (fn [i]
                 (gen/fmap deserialize-upper-triangular-matrix
                           (gen/vector (s/gen ::m/number)
                                       (ecount-of-symmetric-or-triangular-matrix i)))))))

(defn lower-triangular-matrix?
  "Returns true if a lower triangular matrix (the entries above the main
  diagonal are all zero)."
  [x]
  (and (square-matrix? x)
       (nil? (some-kv (fn [i j e]
                        (not (or (>= i j) (zero? e))))
                      x))))

(s/fdef lower-triangular-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::lower-triangular-matrix
  (s/with-gen
    lower-triangular-matrix?
    #(gen/bind (gen/large-integer* {:min 0 :max mdl})
               (fn [i]
                 (gen/fmap deserialize-lower-triangular-matrix
                           (gen/vector (s/gen ::m/number)
                                       (ecount-of-symmetric-or-triangular-matrix i)))))))

(defn symmetric-matrix?
  "Returns true if a symmetric matrix."
  [x]
  (and (square-matrix? x) (tensor/=== (transpose x) x)))

(s/fdef symmetric-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::symmetric-matrix
  (s/with-gen
    symmetric-matrix?
    #(gen/bind (gen/large-integer* {:min 0 :max mdl})
               (fn [i]
                 (gen/fmap deserialize-symmetric-matrix
                           (gen/vector (s/gen ::m/number)
                                       (ecount-of-symmetric-or-triangular-matrix i)))))))

;;;MATRIX CONSTRUCTORS
(defn to-matrix
  "Builds a matrix representing the flattened elements of `tensor` (onto a
  matrix of zeros (doubles) if necessary). `rows` is the number of rows of the
  returned matrix. The elements are placed `by-row?` (default is true). To set
  the number of columns instead, transpose returned matrix."
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
  "Constructs a new matrix of `value`'s (or zeros (doubles)) with the given
  `rows` and `columns`."
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
  "Function `row+column->number` takes a `row` and `column` and returns a
  number."
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
  "Constructs an identity matrix with the given `size`."
  [size]
  (compute-matrix size size (fn [row column] (if (= row column) 1.0 0.0))))

(s/fdef identity-matrix
        :args (s/cat :size ::vector/size)
        :ret ::diagonal-matrix)

(defn row-matrix
  "Returns a row matrix created from `numbers` or from `size` and
  `column->number`. `size` is the size of the returned matrix. Function
  `column->number` takes a `column` and returns a number."
  ([numbers] [(vec numbers)])
  ([size column->number] [(vector/compute-vector size column->number)]))

(s/fdef row-matrix
        :args (s/or :one (s/cat :numbers ::m/numbers)
                    :two (s/cat :size ::vector/size :column->number ::column->number))
        :ret ::row-matrix)

(defn column-matrix
  "Returns a column matrix created from `numbers` or from `size` and
  `row->number`. `size` is the size of the returned matrix. Function
  `row->number` takes a row and returns a number."
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
  "Returns a diagonal matrix (a matrix with all elements not on the diagonal
  being 0.0). The values on the diagonal can be given by the vector
  `diagonal-numbers`. `size` is the size of the matrix given by a single number.
  `f` is a function that takes `index` and returns a number. Can also return a
  rectangular diagonal matrix using `rows` and `columns`."
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
       (+ c
          (* r size)
          (* -1 m/half (+ r (m/sq' r))))
       m/nan))

(defn- symmetric-column-fill
  [r c numbers]
  (get numbers
       (+ r (* m/half c (inc c)))
       m/nan))

(defn- symmetric-without-diagonal-row-fill
  [r c size numbers]
  (get numbers
       (+ c
          (* r size)
          (* -1 m/half (+ (inc r) (m/sq' (inc r)))))
       m/nan))

(defn- symmetric-without-diagonal-column-fill
  [r c numbers]
  (get numbers
       (+ r (* m/half c (dec c)))
       m/nan))

(defn deserialize-upper-triangular-matrix
  "Returns a (square) upper triangular matrix (a matrix with all elements below
  the diagonal being 0.0). `numbers` are the elements that will be used to
  create the upper triangular matrix. `off-diagonal-numbers` can be used to
  create the off-diagonal elements, and then any existing `diagonal-numbers`
  will fill the diagonal elements. The elements are placed `by-row?` (default
  is true)."
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
   (let [size (size-of-symmetric-or-triangular-matrix-without-diagonal (count off-diagonal-numbers))
         f (fn [r c]
             (cond (> r c) 0.0
                   (= r c) (get diagonal-numbers r 0.0)
                   :else (if by-row?
                           (symmetric-without-diagonal-row-fill r c size off-diagonal-numbers)
                           (symmetric-without-diagonal-column-fill r c off-diagonal-numbers))))]
     (when-not (m/nan? size) (compute-matrix size size f)))))

(s/fdef deserialize-upper-triangular-matrix
        :args (s/or :one-two (s/cat :numbers ::m/numbers :opts (s/? (s/keys :opt [::by-row?])))
                    :three (s/cat :diagonal-numbers ::m/numbers
                                  :off-diagonal-numbers ::m/numbers
                                  :opts (s/keys :opt [::by-row?])))
        :ret (s/nilable ::upper-triangular-matrix))

(defn deserialize-lower-triangular-matrix
  "Returns a (square) lower triangular matrix (a matrix with all elements above
  the diagonal being 0.0). `numbers` are the elements that will be used to
  create the lower triangular matrix. `off-diagonal-numbers` can be used to
  create the off-diagonal elements, and then any existing `diagonal-numbers`
  will fill the diagonal elements. The elements are placed `by-row?` (default is
  true)."
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
   (let [size (size-of-symmetric-or-triangular-matrix-without-diagonal (count off-diagonal-numbers))
         f (fn [r c]
             (cond (< r c) 0.0
                   (= r c) (get diagonal-numbers r 0.0)
                   :else (if by-row?
                           (symmetric-without-diagonal-column-fill c r off-diagonal-numbers)
                           (symmetric-without-diagonal-row-fill c r size off-diagonal-numbers))))]
     (when-not (m/nan? size) (compute-matrix size size f)))))

(s/fdef deserialize-lower-triangular-matrix
        :args (s/or :one-two (s/cat :numbers ::m/numbers :opts (s/? (s/keys :opt [::by-row?])))
                    :three (s/cat :diagonal-numbers ::m/numbers
                                  :off-diagonal-numbers ::m/numbers
                                  :opts (s/keys :opt [::by-row?])))
        :ret (s/nilable ::lower-triangular-matrix))

(defn deserialize-symmetric-matrix
  "Returns a symmetric matrix (a matrix with elements at r,c equal to elements
  at c,r). `numbers` are the same as the elements used to create a triangular
  matrix."
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
  "Returns a toeplitz matrix (a matrix whose elements on any diagonal are the
  same). A Toeplitz matrix is also called a diagonal-constant matrix.
  `first-row` is the first row in the matrix and `first-column` is the first
  column in the matrix."
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
  "An outer product is the tensor product of two coordinate vectors, a special
  case of the Kronecker product of matrices. The outer product of two coordinate
  vectors is a matrix such that the coordinates satisfy w_ij = u_i × u_j."
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
  "Returns matrix with random elements."
  [rows columns]
  (let [t (* rows columns)]
    (if (zero? t)
      [[]]
      (mapv vec (partition columns (take t (random/rnd-lazy!)))))))

(s/fdef rnd-matrix!
        :args (s/cat :rows ::rows :columns ::columns)
        :ret ::matrix)

(defn rnd-reflection-matrix!
  "Returns a random Householder reflection matrix of `size`."
  [size]
  (let [v (column-matrix (tensor/normalize (vec (take size (random/rnd-lazy!)))))]
    (tensor/subtract (identity-matrix size)
                     (tensor/multiply (mx* v (transpose v))
                                      2.0))))

(s/fdef rnd-reflection-matrix!
        :args (s/cat :size ::vector/size)
        :ret ::symmetric-matrix)

(defn rnd-spectral-matrix!
  "Returns a random matrix with a particular `spectrum-vector`. The orthogonal
  matrices are generated by using 2 × size of `spectrum-vector` composed
  Householder reflections."
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
  "Builds a matrix using a sparse representation and an existing matrix (often
  a zero-matrix). `sparse` is a vector of triples of `[row column value]`. Later
  values will override prior overlapping values."
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
  "Builds a symmetric matrix using a sparse representation and an existing
  symmetric matrix (often a zero-matrix). `sparse` is a vector of triples of
  [row column value]. Later values will override prior overlapping values.
  Each off-diagonal inner sparse form is applied twice, with the row and column
  switched."
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
  "Returns the number of rows."
  [m]
  (if (empty-matrix? m)
    0
    (count m)))

(s/fdef rows
        :args (s/cat :m ::matrix)
        :ret ::rows)

(defn columns
  "Returns the number of columns."
  [m]
  (count (first m)))

(s/fdef columns
        :args (s/cat :m ::matrix)
        :ret ::columns)

(defn get-row
  "Gets a `row` of a matrix, as a vector."
  [m row]
  (vec (get m row)))

(s/fdef get-row
        :args (s/and (s/cat :m ::matrix
                            :row ::row)
                     (fn [{:keys [m row]}]
                       (< row (rows m))))
        :ret ::vector/vector)

(defn get-column
  "Gets a `column` of a matrix, as a vector."
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
  "Returns the specified diagonal of a matrix as a vector. If `k`>0, returns a
  diagonal above the main diagonal. If `k`<0, returns a diagonal below the main
  diagonal. Works on both square and rectangular matrices."
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
  "Returns a vector that contains the upper (default) or lower half of the
  matrix. `m` doesn't have to be symmetric. Options: `::by-row?`
  (default: true). Set to false to get lower triangular values instead of
  upper."
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
  "Returns the size of the matrix given `ecount`. `ecount` is the number of
  independent symmetric or triangular matrix elements (the number of elements on
  the diagonal plus the number either above or below the diagonal)."
  [ecount]
  (let [s (-> ecount (* 8.0) inc m/sqrt dec (* 0.5))]
    (if (m/roughly-round? s 1e-6)
      (m/maybe-long-able s)
      m/nan)))

(s/fdef size-of-symmetric-or-triangular-matrix
        :args (s/cat :ecount ::m/int-non-)
        :ret (s/or :nan ::m/nan :size ::vector/size))

(defn size-of-symmetric-or-triangular-matrix-without-diagonal
  "Returns the size of the matrix given `ecount`. `ecount` is the number of
  elements above or below the diagonal."
  [ecount]
  (let [size (size-of-symmetric-or-triangular-matrix ecount)]
    (if (m/nan? size)
      size
      (m/maybe-long-able (inc (double size))))))

(s/fdef size-of-symmetric-or-triangular-matrix-without-diagonal
        :args (s/cat :ecount ::m/int-non-)
        :ret (s/or :nan ::m/nan :size ::vector/size))

(defn ecount-of-symmetric-or-triangular-matrix
  "Returns the element count (`ecount`) for a symmetric or triangular matrix.
  This is the number of elements on the diagonal plus the number of elements
  above or below the diagonal."
  [size]
  (m/div (+ (m/sq' size) size) 2))

(s/fdef ecount-of-symmetric-or-triangular-matrix
        :args (s/cat :size ::vector/size)
        :ret ::m/int-non-)

(defn ecount-of-symmetric-or-triangular-matrix-without-diagonal
  "Returns the element count (`ecount`) for a symmetric or triangular matrix
  without the diagonal. This is the number of elements above or below the
  diagonal."
  [size]
  (m/div (- (m/sq' size) size) 2))

(s/fdef ecount-of-symmetric-or-triangular-matrix-without-diagonal
        :args (s/cat :size ::vector/size)
        :ret ::m/int-non-)

(defn trace
  "Calculates the trace of a square matrix (sum of elements on main diagonal)."
  [square-m]
  (if (empty-matrix? square-m)
    0.0
    (apply + (map double (diagonal square-m)))))

(s/fdef trace
        :args (s/cat :square-m ::square-matrix)
        :ret ::m/number)

(defn get-slices-as-matrix
  "Performs a slice on the matrix given by the options.
  Options:
    `::row-indices` returns all rows by default, can pass a row index or
      sequence of row indices
    `::column-indices` returns all columns by default, can pass a column index
      or sequence of column indices
    `::exception-row-indices` can pass a row index or sequence of row indices
      to exclude
    `::exception-column-indices` can pass a column index or sequence of column
      indices to exclude.
    Exceptions override inclusions. Can be used to permute matrix through index
    sequence ordering."
  [m {::keys [row-indices column-indices exception-row-indices exception-column-indices]}]
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
                                            (or (= except-i index) (>= index n)))
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
      (and (number? rs) (coll? cs)) (row-matrix (let [row-vector (get m rs)]
                                                  (map #(get row-vector %) cs)))
      (and (number? rs) (true? cs)) (row-matrix (get-row m rs))
      (and (coll? rs) (number? cs)) (column-matrix (let [column-vector (get-column m cs)]
                                                     (map #(nth column-vector %) rs)))
      (and (coll? rs) (coll? cs)) (mapv (fn [row-vector]
                                          (reduce (fn [tot column] (conj tot (get row-vector column)))
                                                  []
                                                  cs))
                                        (map #(get m %) rs))
      (and (coll? rs) (true? cs)) (mapv #(get m %) rs)
      (and (true? rs) (number? cs)) (column-matrix (get-column m cs))
      (and (true? rs) (coll? cs)) (mapv (fn [row-vector]
                                          (reduce (fn [tot column] (conj tot (get row-vector column)))
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
  "Returns a matrix. Function 'row-v->bool' takes a row-vector."
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
  "Returns a matrix. Function 'column-v->bool' takes a column-vector."
  [column-v->bool m]
  (let [new-m (filter column-v->bool (transpose m))]
    (if (empty? new-m)
      [[]]
      (transpose (vec new-m)))))

(s/fdef filter-by-column
        :args (s/cat :column-v->bool (s/fspec :args (s/cat :column-vector ::vector/vector)
                                              :ret boolean?)
                     :m ::matrix)
        :ret ::matrix)

(defn filter-symmetric-matrix
  "Takes and returns a symmetric matrix. 'v->bool' takes a row-vector or
  column-vector."
  [v->bool symmetric-m]
  (let [keep-set (reduce-kv (fn [tot index row-vector]
                              (if (v->bool row-vector)
                                (conj tot index)
                                tot))
                            #{}
                            symmetric-m)]
    (get-slices-as-matrix symmetric-m {::row-indices keep-set, ::column-indices keep-set})))

(s/fdef filter-symmetric-matrix
        :args (s/cat :v->bool (s/fspec :args (s/cat :row-vector ::vector/vector)
                                       :ret boolean?)
                     :symmetric-m ::symmetric-matrix)
        :ret ::symmetric-matrix)

(s/def ::top-left ::matrix)
(s/def ::bottom-left ::matrix)
(s/def ::top-right ::matrix)
(s/def ::bottom-right ::matrix)

(defn matrix-partition
  "Returns a map containing the four sub-matrices labeled `::top-left`,
  `::bottom-left`, `::top-right`, and `::bottom-right`. Matrices can be merged
  back together using [[merge-matrices]]. `first-bottom-row` is the bottom of
  where the slice will occur. `first-right-column` is the right edge of where
  the slice will occur."
  [m first-bottom-row first-right-column]
  {::top-left     (get-slices-as-matrix m {::row-indices    (range first-bottom-row)
                                           ::column-indices (range first-right-column)})
   ::bottom-left  (get-slices-as-matrix m {::exception-row-indices (range first-bottom-row)
                                           ::column-indices        (range first-right-column)})
   ::top-right    (get-slices-as-matrix m {::row-indices              (range first-bottom-row)
                                           ::exception-column-indices (range first-right-column)})
   ::bottom-right (get-slices-as-matrix m {::exception-row-indices    (range first-bottom-row)
                                           ::exception-column-indices (range first-right-column)})})

(s/fdef matrix-partition
        :args (s/cat :m ::matrix :first-bottom-row ::row :first-right-column ::column)
        :ret (s/keys :req [::top-left ::bottom-left ::top-right ::bottom-right]))

(defn some-kv
  "Returns the first logical true value of (pred row column number) for any
  number in matrix, else nil. Options: `::by-row?` (default: true)."
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
  "Function `f` takes a result, row, column, and number(s). Reduces over 1, 2,
  or 3 matrices. Number of columns in the first matrix must be the least."
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
                  (extensions/reduce-kv-ext g val (first s1) (first s2) (first s3))
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
  "Returns a sparse-matrix (i.e., a vector of tuples of [row column number]).
  Function `number->bool` takes a number."
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
  "Returns a sparse-matrix (i.e., a vector of tuples of [row column number]).
  `number->bool` takes a number and will be evaluated only for upper-right
  triangle of `symmetric-m`."
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
  "Transposes a matrix by swapping rows and columns, returning a new matrix."
  [m]
  (if (empty-matrix? m)
    [[]]
    (apply mapv vector m)))

(s/fdef transpose
        :args (s/cat :m ::matrix)
        :ret ::matrix)

(defn assoc-row
  "Sets a row in a matrix using the specified numbers."
  [m row numbers]
  (cond (and (zero? row) (empty-matrix? m)) (row-matrix numbers)
        (and (= (count numbers) (columns m)) (<= row (rows m))) (assoc m row (vec numbers))
        :else nil))

(s/fdef assoc-row
        :args (s/cat :m ::matrix :row ::row :numbers ::m/numbers)
        :ret (s/nilable ::matrix))

(defn assoc-column
  "Sets a column in a matrix using the specified numbers."
  [m column numbers]
  (cond (and (zero? column) (empty-matrix? m)) (column-matrix numbers)

        (and (= (count numbers) (rows m)) (<= column (columns m)))
        (vec (map-indexed (fn [row row-vector]
                            (assoc row-vector column (get numbers row 0.0)))
                          m))

        :else nil))

(s/fdef assoc-column
        :args (s/cat :m ::matrix :column ::column :numbers ::m/numbers)
        :ret (s/nilable ::matrix))

(defn assoc-diagonal
  "Sets a diagonal in a matrix using the specified numbers."
  [m numbers]
  (cond (empty-matrix? m) (diagonal-matrix numbers)

        (= (count numbers) (count (diagonal m)))
        (let [v (vec numbers)]
          (vec (for [row (range (count numbers))]
                 (assoc (get-row m row) row (get v row 0.0)))))

        :else nil))

(s/fdef assoc-diagonal
        :args (s/cat :m ::matrix :numbers ::m/numbers)
        :ret (s/nilable ::matrix))

(defn insert-row
  "Inserts a row of `numbers` in a matrix at the specified `row`."
  [m row numbers]
  (cond (and (zero? row) (empty-matrix? m)) (row-matrix numbers)

        (and (= (count numbers) (columns m)) (<= row (rows m)))
        (vec (concat (subvec m 0 row) [(vec numbers)] (subvec m row)))

        :else nil))

(s/fdef insert-row
        :args (s/cat :m ::matrix :row ::row :numbers ::m/numbers)
        :ret (s/nilable ::matrix))

(defn insert-column
  "Inserts a column of `numbers` in a matrix at the specified `column`."
  [m column numbers]
  (cond (and (zero? column) (empty-matrix? m)) (column-matrix numbers)

        (and (= (count numbers) (rows m)) (<= column (columns m)))
        (vec (map-indexed (fn [row row-vector]
                            (vector/insertv row-vector column (get numbers row 0.0)))
                          m))

        :else nil))

(s/fdef insert-column
        :args (s/cat :m ::matrix
                     :column ::column
                     :numbers ::m/numbers)
        :ret (s/nilable ::matrix))

(defn remove-row
  "Removes a row in a matrix"
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
  "Removes a column in a matrix"
  [m column]
  (if (<= (inc column) (columns m))
    (let [m2 (mapv (fn [r]
                     (vector/removev r column))
                   m)]
      (if (empty? m2) [[]] m2))
    m))

(s/fdef remove-column
        :args (s/cat :m ::matrix
                     :column ::column)
        :ret ::matrix)

(defn update-row
  "Updates a `row` of matrix `m`, using function `column+number->number`, which
  is a function of the `column` and `number` and returns a number."
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
  "Updates a `column` of matrix `m`, using function `row+number->number`, which
  is a function the `row` and `number` and returns a number."
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
  "Updates the diagonal of matrix `m`, using function `row+number->number`,
  which is a function of the `row` and `number` and returns a number."
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
  "Appends rows from all the matrices after the first to the first. Each
  matrix's column count must be the same or will return nil."
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
  "Appends columns from all the matrices after the first to the first. Each
  matrix's row count must be the same or will return nil."
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
  "Returns a Matrix created by binding four matrices together. Matrices can be
  partitioned into four matrices using [[matrix-partition]]. Takes a map
  containing keys:
    `::top-left`
    `::bottom-left`
    `::top-right`
    `::bottom-right`."
  [{::keys [top-left bottom-left top-right bottom-right]}]
  (let [top (concat-columns top-left top-right)
        bottom (concat-columns bottom-left bottom-right)]
    (when (and top bottom) (concat-rows top bottom))))

(s/fdef merge-matrices
        :args (s/cat :args (s/keys :req [::top-left ::bottom-left ::top-right ::bottom-right]))
        :ret (s/nilable ::matrix))

(defn replace-submatrix
  "Returns a Matrix after substituting a `submatrix` at top-left location
  `row-start` and `column-start`. `row-start` and `column-start` can be
  negative. Unassigned elements will be 0.0."
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
                                     (< c tc)) (get-in submatrix [(- r row-start) (- c column-start)])
                                (and (m/non-? r)
                                     (< r nr)
                                     (m/non-? c)
                                     (< c nc)) (get-in m [r c])
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
  "Returns a symmetric matrix where each element above or below the diagonal is
  equal to the average of the corresponding numbers. This is useful to help with
  rounding errors."
  [square-m]
  (let [size (rows square-m)]
    (compute-matrix size size (fn [r c]
                                (if (== c r)
                                  (get-in square-m [r c] 0.0)
                                  (* 0.5 (+ (get-in square-m [r c] 0.0)
                                            (double (get-in square-m [c r] 0.0)))))))))

(s/fdef symmetric-matrix-by-averaging
        :args (s/cat :square-m ::square-matrix)
        :ret ::symmetric-matrix)

;;;MATRIX MATH
(defn mx*
  "Matrix multiplication. Number of columns of the first matrix must match the
  number of rows of the second matrix. Use [tensor/multiply] for more general
  multiplication."
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
  "Generalization of the outer product."
  ([] [[]])
  ([m] m)
  ([m1 m2]
   (if (or (empty-matrix? m1) (empty-matrix? m2))
     [[]]
     (vec (apply concat (mapv (fn [row]
                                (apply concat-columns (mapv (fn [e]
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
  "Returns a matrix after rounding any roughly-zero rows."
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
  "Returns a matrix after rounding any roughly-zero columns."
  [m accu]
  (transpose (round-roughly-zero-rows (transpose m) accu)))

(s/fdef round-roughly-zero-columns
        :args (s/cat :m ::matrix :accu ::m/accu)
        :ret ::matrix)