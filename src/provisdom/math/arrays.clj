(ns provisdom.math.arrays
  "High-performance array operations for numerical computation.
  
  Provides efficient operations on Java arrays, particularly double arrays,
  with mathematical functions for linear algebra and statistics.
  
  Key features:
  - Array creation, copying, and manipulation
  - Element-wise mathematical operations  
  - Statistical functions (mean, variance, correlation)
  - Linear algebra primitives (dot product, norm, projection)
  - Memory-efficient array processing with areduce/amap"
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.math.core :as m]
    [provisdom.math.tensor :as tensor]
    [provisdom.math.vector :as vector])
  (:import
    [java.util Arrays]))

;http://clj-me.cgrand.net/2009/10/15/multidim-arrays/

(declare double-array2D? array2D array3D)

(def mdl 6)

(s/def ::array-type #{Long/TYPE Boolean/TYPE Character/TYPE Double/TYPE})
(s/def ::array-type-keyword #{:long :boolean :char :double})

(s/def ::array-element
  (s/or :long ::m/long
    :boolean boolean?
    :char char?
    :double ::m/double))

(s/def ::coll2D
  (s/with-gen
    (s/coll-of (s/coll-of ::array-element))
    #(gen/vector (s/gen ::array-element) 0 mdl)))

(s/def ::coll3D
  (s/with-gen
    (s/coll-of ::coll2D)
    #(gen/vector (s/gen ::coll2D) 0 mdl)))

;;;ARRAY TYPES
(def double-array-type (Class/forName "[D"))
(def long-array-type (Class/forName "[J"))
(def boolean-array-type (Class/forName "[Z"))
(def char-array-type (Class/forName "[C"))
(def double-array-2D-type (Class/forName "[[D"))
(def double-array-3D-type (Class/forName "[[[D"))

(defn array?
  "Tests if a value is a Java array.
  
  Returns true if `x` is any type of Java array (primitive or object arrays).
  Returns false for nil values.
  
  Examples:
    (array? (double-array [1 2 3]))  ;=> true
    (array? (into-array [1 2 3]))    ;=> true
    (array? [1 2 3])                 ;=> false (vector, not array)
    (array? nil)                     ;=> false"
  [x]
  (if (some? x)
    (-> x class .isArray)
    false))

(s/fdef array?
  :args (s/cat :x any?)
  :ret boolean?)

(s/def ::array
  (s/with-gen
    array?
    #(gen/fmap double-array
       (gen/vector (s/gen ::m/double) 0 mdl))))

(defn array2D?
  "Tests if a value is a 2D array (array of arrays).
  
  Returns true if `x` is an array where every element is also an array.
  
  Examples:
    (array2D? (array2D :double [[1 2] [3 4]]))  ;=> true
    (array2D? (double-array [1 2 3]))           ;=> false (1D array)
    (array2D? [[1 2] [3 4]])                    ;=> false (vector of vectors)"
  [x]
  (and (array? x) (every? array? x)))

(s/fdef array2D?
  :args (s/cat :x any?)
  :ret boolean?)

(s/def ::array2D
  (s/with-gen
    array2D?
    #(gen/fmap (fn [v]
                 (array2D :double v))
       (gen/vector (gen/vector (s/gen ::m/double) 0 mdl)
         1
         mdl))))

(defn array3D?
  "Returns true if `x` is an Array and each element is a 2D Array."
  [x]
  (and (array? x) (every? array2D? x)))

(s/fdef array3D?
  :args (s/cat :x any?)
  :ret boolean?)

(s/def ::array3D
  (s/with-gen
    array3D?
    #(gen/fmap (fn [v]
                 (array3D :double v))
       (gen/vector
         (gen/vector
           (gen/vector (s/gen ::m/double) 0 mdl)
           1
           mdl)
         1
         mdl))))

(defn double-array?
  "Tests if a value is a double array.
  
  Returns true if `x` is an array containing only double values.
  
  Examples:
    (double-array? (double-array [1.0 2.0]))  ;=> true
    (double-array? (into-array [1 2 3]))      ;=> false (integers)
    (double-array? [1.0 2.0])                 ;=> false (vector)"
  [x]
  (and (array? x) (every? double? x)))

(s/fdef double-array?
  :args (s/cat :x any?)
  :ret boolean?)

(defn- double-array-gen
  ([] (gen/fmap double-array (gen/vector (s/gen ::m/double))))
  ([count] (gen/fmap double-array (gen/vector (s/gen ::m/double) count)))
  ([min-count max-count]
   (gen/fmap double-array
     (gen/vector (s/gen ::m/double) min-count max-count))))

(s/def ::double-array
  (s/with-gen
    double-array?
    #(double-array-gen 0 mdl)))

(defn double-array2D?
  "Returns true if `x` is an Array and each element is a Double Array."
  [x]
  (and (array? x) (every? double-array? x)))

(s/fdef double-array2D?
  :args (s/cat :x any?)
  :ret boolean?)

(s/def ::double-array2D
  (s/with-gen
    double-array2D?
    #(gen/fmap (fn [v]
                 (array2D :double v))
       (gen/vector
         (gen/vector (s/gen ::m/double) 0 mdl)
         1
         mdl))))

(defn double-finite-array?
  "Returns true if 'x' is a Array and each element is a finite double."
  [x]
  (and (array? x) (every? m/double-finite? x)))

(s/fdef double-finite-array?
  :args (s/cat :x any?)
  :ret boolean?)

(defn- double-finite-array-gen
  ([] (gen/fmap double-array (gen/vector (s/gen ::m/double-finite))))
  ([count] (gen/fmap double-array (gen/vector (s/gen ::m/double-finite) count)))
  ([min-count max-count]
   (gen/fmap double-array
     (gen/vector (s/gen ::m/double-finite) min-count max-count))))

(s/def ::double-finite-array
  (s/with-gen double-finite-array? double-finite-array-gen))

;;;ARRAY CONSTRUCTORS
(defn- arrayND->vector-recursion
  [x]
  (if (array? x)
    (mapv arrayND->vector-recursion x)
    x))

(defn arrayND->vector
  "Converts a multidimensional array to nested vectors.
  
  Recursively converts an array and all nested arrays to Clojure vectors,
  preserving the nested structure.
  
  Examples:
    (arrayND->vector (double-array [1 2 3]))            ;=> [1.0 2.0 3.0]
    (arrayND->vector (array2D :double [[1 2] [3 4]]))   ;=> [[1.0 2.0] [3.0 4.0]]"
  [array]
  (arrayND->vector-recursion array))

(s/fdef arrayND->vector
  :args (s/cat :array ::array)
  :ret (s/coll-of any? :kind vector? :into []))

(defn keyword->array-type
  "Converts array type keywords to Java primitive types.
  
  Maps keyword symbols to their corresponding Java primitive class types
  for array creation.
  
  Supported types:
  - `:double` → Double/TYPE  
  - `:long` → Long/TYPE
  - `:boolean` → Boolean/TYPE
  - `:char` → Character/TYPE
  
  Examples:
    (keyword->array-type :double)   ;=> java.lang.Double/TYPE
    (keyword->array-type :long)     ;=> java.lang.Long/TYPE"
  [array-type-keyword]
  (condp = array-type-keyword
    :long Long/TYPE
    :boolean Boolean/TYPE
    :char Character/TYPE
    Double/TYPE))

(s/fdef keyword->array-type
  :args (s/cat :array-type-keyword ::array-type-keyword)
  :ret ::array-type)

(defn array2D
  "Creates a 2D array from nested collections.
  
  Constructs a Java 2D array from a collection of collections, where each
  inner collection becomes a row in the resulting array.
  
  Parameters:
  - `array-type-keyword`: Element type (:double, :long, :boolean, :char)
  - `coll2D`: Collection of collections (e.g., [[1 2] [3 4]])
  
  Examples:
    (array2D :double [[1 2] [3 4]])     ;=> 2x2 double array
    (array2D :long [[1 2 3] [4 5 6]])   ;=> 2x3 long array"
  [array-type-keyword coll2D]
  (let [array-type (keyword->array-type array-type-keyword)]
    (into-array (map (partial into-array array-type) coll2D))))

(s/fdef array2D
  :args (s/cat :array-type-keyword ::array-type-keyword
          :coll2D ::coll2D)
  :ret ::array2D)

(defn array3D
  "Create a 3D Array.
   `array-type-keyword` can be:
      `:double`
      `:long`
      `:boolean`
      `:char`."
  [array-type-keyword coll3D]
  (into-array (map (partial array2D array-type-keyword) coll3D)))

(s/fdef array3D
  :args (s/cat :array-type-keyword ::array-type-keyword
          :coll3D ::coll3D)
  :ret ::array3D)

;;;DOUBLE ARRAY INFO
(defn double-array-copy
  "Creates a copy of a double array.
  
  Returns a new double array with the same elements as the input.
  Uses Arrays/copyOf for efficient native copying.
  
  Examples:
    (def arr (double-array [1 2 3]))
    (def copy (double-array-copy arr))
    (identical? arr copy)  ;=> false (different objects)
    (= (seq arr) (seq copy))  ;=> true (same contents)"
  [dbl-array]
  (Arrays/copyOf (doubles dbl-array) (alength dbl-array)))

(s/fdef double-array-copy
  :args (s/cat :dbl-array ::double-array)
  :ret ::double-array)

(defn double-array2D-copy
  "This function will create a copy of `dbl-array-2D`."
  [dbl-array2D]
  (into-array (map double-array-copy dbl-array2D)))

(s/fdef double-array2D-copy
  :args (s/cat :dbl-array2D ::double-array2D)
  :ret ::double-array2D)

(defn double-array=
  "Tests if two double arrays are equal element-wise.
  
  Returns true if both arrays have the same length and all corresponding
  elements are equal. Uses efficient native comparison via Arrays/equals.
  Handles nil arrays correctly (both nil → true, one nil → false).
  
  Examples:
    (double-array= [1.0 2.0] [1.0 2.0])  ;=> true
    (double-array= [1.0 2.0] [1.0 3.0])  ;=> false  
    (double-array= nil nil)              ;=> true
    (double-array= [1.0] nil)            ;=> false"
  [dbl-array1 dbl-array2]
  (Arrays/equals (double-array dbl-array1) (double-array dbl-array2)))

(s/fdef double-array=
  :args (s/cat :dbl-array1 (s/nilable ::double-array)
          :dbl-array2 (s/nilable ::double-array))
  :ret boolean?)

(defn double-array2D=
  "Checks whether two 2D double arrays are equal or not. Two array references
  are considered deeply equal if both are nil, or if they refer to arrays that
  contain the same number of elements and all corresponding pairs of elements in
  the two arrays are deeply equal."
  [dbl-array2D-1 dbl-array2D-2]
  (Arrays/deepEquals ^"[[D" dbl-array2D-1 ^"[[D" dbl-array2D-2))

(s/fdef double-array2D=
  :args (s/cat :dbl-array2D-1 (s/nilable ::double-array2D)
          :dbl-array2D-2 (s/nilable ::double-array2D))
  :ret boolean?)

(defn double-array-reduce-kv
  "Reduces over double arrays with index and value(s).
  
  Like reduce-kv but for double arrays. The function `f` receives:
  - The accumulated result
  - The current index
  - The value(s) at that index from the array(s)
  
  For multiple arrays, all arrays are processed in parallel. The first array
  determines the iteration length, so it should be the shortest.
  
  Examples:
    (double-array-reduce-kv + 0.0 [1.0 2.0 3.0])  
    ;=> calls (+ 0.0 0 1.0), then (+ result 1 2.0), then (+ result 2 3.0)
    
    (double-array-reduce-kv 
      (fn [sum idx a b] (+ sum (* a b))) 
      0.0 [1.0 2.0] [3.0 4.0])  ;=> 11.0 (1*3 + 2*4)"
  ([f init-dbl dbl-array]
   (areduce (doubles dbl-array) i ret (double init-dbl)
     (double (f ret
               i
               (aget (doubles dbl-array) i)))))
  ([f init-dbl dbl-array1 dbl-array2]
   (areduce (doubles dbl-array1) i ret (double init-dbl)
     (double (f ret
               i
               (aget dbl-array1 i)
               (aget dbl-array2 i)))))
  ([f init-dbl dbl-array1 dbl-array2 dbl-array3]
   (areduce (doubles dbl-array1) i ret (double init-dbl)
     (double (f ret
               i
               (aget (doubles dbl-array1) i)
               (aget (doubles dbl-array2) i)
               (aget (doubles dbl-array3) i))))))

(s/fdef double-array-reduce-kv
  :args (s/or :one (s/cat :f (s/fspec :args (s/cat :ret ::m/double
                                              :index ::tensor/index
                                              :val ::m/double)
                               :ret ::m/double)
                     :init-dbl ::m/double
                     :dbl-array ::double-array)
          :two (s/and (s/cat :f (s/fspec :args (s/cat :ret ::m/double
                                                 :index ::tensor/index
                                                 :val1 ::m/double
                                                 :val2 ::m/double)
                                  :ret ::m/double)
                        :init-dbl ::m/double
                        :dbl-array1 ::double-array
                        :dbl-array2 ::double-array)
                 (fn [{:keys [dbl-array1 dbl-array2]}]
                   (<= (count dbl-array1) (count dbl-array2))))
          :three (s/and (s/cat :f (s/fspec :args (s/cat :ret ::m/double
                                                   :index ::tensor/index
                                                   :val1 ::m/double
                                                   :val2 ::m/double
                                                   :val3 ::m/double)
                                    :ret ::m/double)
                          :init-dbl ::m/double
                          :dbl-array1 ::double-array
                          :dbl-array2 ::double-array
                          :dbl-array3 ::double-array)
                   (fn [{:keys [dbl-array1 dbl-array2 dbl-array3]}]
                     (and (<= (count dbl-array1) (count dbl-array2))
                       (<= (count dbl-array1) (count dbl-array3))))))
  :ret ::m/double)

(defn double-array-find-all
  "Returns a vector with all the indices where `dbl` is found in `dbl-array`."
  [dbl-array dbl]
  (areduce (doubles dbl-array) i ret []
    (if (= (aget (doubles dbl-array) i) (double dbl))
      (conj ret i)
      ret)))

(s/fdef double-array-find-all
  :args (s/cat :dbl-array ::double-array :dbl ::m/double)
  :ret ::vector/vector)

(defn double-array-sorted-find
  "Searches the specified array of doubles for the specified value using the
  binary search algorithm, and returns the index. If the specified value does
  not exist, then will return negative index of where value would fit in,
  starting at -1 at ending at negative (count + 1). The array must be sorted (as
  by the [[double-array-sort!]] function prior to making this call. If it is not
  sorted, the results are undefined. If the array contains multiple elements
  with the specified value, there is no guarantee which one will be found. This
  method considers all NaN values to be equivalent and equal."
  [dbl-array dbl]
  (Arrays/binarySearch (doubles dbl-array) (double dbl)))

(s/fdef double-array-sorted-find
  :args (s/cat :dbl-array ::double-array :dbl ::m/double)
  :ret ::m/int)

;;;DOUBLE ARRAY CHANGES
(defn double-array-sort!
  "Sorts `dbl-array`."
  [dbl-array]
  (Arrays/sort (doubles dbl-array)))

(s/fdef double-array-sort!
  :args (s/cat :dbl-array ::double-array)
  :ret nil)

(defn double-array-set!
  "Sets `dbl` at `index` in `dbl-array`."
  [dbl-array index dbl]
  (when (and (m/non-? index)
          (< index (count dbl-array)))
    (aset (doubles dbl-array) index (double dbl))))

(s/fdef double-array-set!
  :args (s/cat :dbl-array ::double-array
          :index ::tensor/index
          :dbl ::m/double)
  :ret nil)

;;;DOUBLE ARRAY MANIPULATION
(defn double-array-map
  "Similar to [[map]] but for double arrays. First array must be the shortest."
  ([f dbl-array]
   (amap (doubles dbl-array) i ret
     (double (f (aget (doubles dbl-array) i)))))
  ([f dbl-array1 dbl-array2]
   (amap (doubles dbl-array1) i ret
     (double (f (aget (doubles dbl-array1) i)
               (aget (doubles dbl-array2) i)))))
  ([f dbl-array1 dbl-array2 dbl-array3]
   (amap (doubles dbl-array1) i ret
     (double (f (aget (doubles dbl-array1) i)
               (aget (doubles dbl-array2) i)
               (aget (doubles dbl-array3) i))))))

(s/fdef double-array-map
  :args (s/or :one (s/cat :f (s/fspec :args (s/cat :val ::m/double)
                               :ret ::m/double)
                     :dbl-array ::double-array)
          :two (s/and (s/cat :f (s/fspec :args (s/cat :val1 ::m/double
                                                 :val2 ::m/double)
                                  :ret ::m/double)
                        :dbl-array1 ::double-array
                        :dbl-array2 ::double-array)
                 (fn [{:keys [dbl-array1 dbl-array2]}]
                   (<= (count dbl-array1) (count dbl-array2))))
          :three (s/and (s/cat :f (s/fspec :args (s/cat :val1 ::m/double
                                                   :val2 ::m/double
                                                   :val3 ::m/double)
                                    :ret ::m/double)
                          :dbl-array1 ::double-array
                          :dbl-array2 ::double-array
                          :dbl-array3 ::double-array)
                   (fn [{:keys [dbl-array1 dbl-array2 dbl-array3]}]
                     (and (<= (count dbl-array1) (count dbl-array2))
                       (<= (count dbl-array1) (count dbl-array3))))))
  :ret ::double-array)

(defn double-array-map-indexed
  "Similar to [[map-indexed]] but for double arrays. First array must be the
  shortest."
  ([f dbl-array]
   (amap (doubles dbl-array) i ret
     (double (f i
               (aget (doubles dbl-array) i)))))
  ([f dbl-array1 dbl-array2]
   (amap (doubles dbl-array1) i ret
     (double (f i
               (aget (doubles dbl-array1) i)
               (aget (doubles dbl-array2) i)))))
  ([f dbl-array1 dbl-array2 dbl-array3]
   (amap (doubles dbl-array1) i ret
     (double (f i
               (aget (doubles dbl-array1) i)
               (aget (doubles dbl-array2) i)
               (aget (doubles dbl-array3) i))))))

(s/fdef double-array-map-indexed
  :args (s/or :one (s/cat :f (s/fspec :args (s/cat :index ::tensor/index
                                              :val ::m/double)
                               :ret ::m/double)
                     :dbl-array ::double-array)
          :two (s/and (s/cat :f (s/fspec :args (s/cat :index ::tensor/index
                                                 :val1 ::m/double
                                                 :val2 ::m/double)
                                  :ret ::m/double)
                        :dbl-array1 ::double-array
                        :dbl-array2 ::double-array)
                 (fn [{:keys [dbl-array1 dbl-array2]}]
                   (<= (count dbl-array1) (count dbl-array2))))
          :three (s/and (s/cat :f (s/fspec :args (s/cat :index ::tensor/index
                                                   :val1 ::m/double
                                                   :val2 ::m/double
                                                   :val3 ::m/double)
                                    :ret ::m/double)
                          :dbl-array1 ::double-array
                          :dbl-array2 ::double-array
                          :dbl-array3 ::double-array)
                   (fn [{:keys [dbl-array1 dbl-array2 dbl-array3]}]
                     (and (<= (count dbl-array1) (count dbl-array2))
                       (<= (count dbl-array1) (count dbl-array3))))))
  :ret ::double-array)

;;;DOUBLE ARRAY MATH
(defn- double-array-in-place
  "In-place Double Array math helper."
  ([f dbl-array1 dbl-array2]
   (double-array-map-indexed (fn [_ da1 da2]
                               (f da1 da2))
     (doubles dbl-array1)
     (doubles dbl-array2)))
  ([f dbl-array1 dbl-array2 dbl-array3]
   (double-array-map-indexed (fn [_ da1 da2 da3]
                               (f da1 da2 da3))
     (doubles dbl-array1)
     (doubles dbl-array2)
     (doubles dbl-array3)))
  ([f dbl-array1 dbl-array2 dbl-array3 & dbl-arrays]
   (loop [a (list* (doubles dbl-array2) (doubles dbl-array3) dbl-arrays)
          ret (doubles dbl-array1)]
     (if (empty? a)
       ret
       (recur (rest a)
         (double-array-map-indexed
           (fn [_ da1 da2] (f da1 da2))
           ret
           (first a)))))))

(defn double-array-add
  "Adding Double Arrays."
  ([dbl-array] dbl-array)
  ([dbl-array1 dbl-array2]
   (double-array-in-place + dbl-array1 dbl-array2))
  ([dbl-array1 dbl-array2 dbl-array3]
   (double-array-in-place + dbl-array1 dbl-array2 dbl-array3))
  ([dbl-array1 dbl-array2 dbl-array3 & dbl-arrays]
   (apply double-array-in-place + dbl-array1 dbl-array2 dbl-array3 dbl-arrays)))

(s/fdef double-array-add
  :args (s/or :one (s/cat :dbl-array ::double-array)
          :two (s/with-gen
                 (s/and (s/cat :dbl-array1 ::double-array
                          :dbl-array2 ::double-array)
                   (fn [{:keys [dbl-array1 dbl-array2]}]
                     (= (count dbl-array1) (count dbl-array2))))
                 #(gen/bind
                    (s/gen (s/int-in 0 6))
                    (fn [i]
                      (gen/tuple (double-array-gen i)
                        (double-array-gen i)))))
          :three+ (s/with-gen
                    (s/and (s/cat :dbl-array1 ::double-array
                             :dbl-array2 ::double-array
                             :dbl-array3 ::double-array
                             :dbl-arrays (s/* ::double-array))
                      (fn [{:keys [dbl-array1
                                   dbl-array2
                                   dbl-array3
                                   dbl-arrays]}]
                        (and (= (count dbl-array1)
                               (count dbl-array2)
                               (count dbl-array3))
                          (every? (fn [da]
                                    (= (count dbl-array1) (count da)))
                            dbl-arrays))))
                    #(gen/bind
                       (s/gen (s/int-in 0 6))
                       (fn [i]
                         (gen/vector (double-array-gen i) 3 6)))))
  :ret ::double-array)

(defn double-array-subtract
  "Subtracting Double Arrays."
  ([dbl-array] dbl-array)
  ([dbl-array1 dbl-array2]
   (double-array-in-place - dbl-array1 dbl-array2))
  ([dbl-array1 dbl-array2 dbl-array3]
   (double-array-in-place - dbl-array1 dbl-array2 dbl-array3))
  ([dbl-array1 dbl-array2 dbl-array3 & dbl-arrays]
   (apply double-array-in-place - dbl-array1 dbl-array2 dbl-array3 dbl-arrays)))

(s/fdef double-array-subtract
  :args (s/or :one (s/cat :dbl-array ::double-array)
          :two (s/with-gen
                 (s/and (s/cat :dbl-array1 ::double-array
                          :dbl-array2 ::double-array)
                   (fn [{:keys [dbl-array1 dbl-array2]}]
                     (= (count dbl-array1) (count dbl-array2))))
                 #(gen/bind
                    (s/gen (s/int-in 0 6))
                    (fn [i]
                      (gen/tuple (double-array-gen i)
                        (double-array-gen i)))))
          :three+ (s/with-gen
                    (s/and (s/cat :dbl-array1 ::double-array
                             :dbl-array2 ::double-array
                             :dbl-array3 ::double-array
                             :dbl-arrays (s/* ::double-array))
                      (fn [{:keys [dbl-array1
                                   dbl-array2
                                   dbl-array3
                                   dbl-arrays]}]
                        (and (= (count dbl-array1)
                               (count dbl-array2)
                               (count dbl-array3))
                          (every? (fn [da]
                                    (= (count dbl-array1) (count da)))
                            dbl-arrays))))
                    #(gen/bind
                       (s/gen (s/int-in 0 6))
                       (fn [i]
                         (gen/vector (double-array-gen i) 3 6)))))
  :ret ::double-array)

(defn double-array-sum
  "Computes the sum of all elements in a double array.
  
  Returns the arithmetic sum of all array elements using efficient
  array reduction.
  
  Examples:
    (double-array-sum (double-array [1.0 2.0 3.0]))  ;=> 6.0
    (double-array-sum (double-array []))             ;=> 0.0
    (double-array-sum (double-array [-1.0 1.0]))     ;=> 0.0"
  [dbl-array]
  (double-array-reduce-kv (fn [tot _ da]
                            (+ tot da))
    0.0
    dbl-array))

(s/fdef double-array-sum
  :args (s/cat :dbl-array ::double-array)
  :ret ::m/double)

(defn double-array-sum-of-squares
  "Sum of squares of `dbl-array` elements."
  [dbl-array]
  (double-array-reduce-kv (fn [tot _ da]
                            (+ tot (m/sq da)))
    0.0
    dbl-array))

(s/fdef double-array-sum-of-squares
  :args (s/cat :dbl-array ::double-array)
  :ret ::m/double)

(defn double-array-dot-product
  "Computes the dot product of two double arrays.
  
  The dot product is the sum of element-wise products. Geometrically, for vectors
  it represents |a| × |b| × cos(θ) where θ is the angle between vectors.
  Arrays must have the same length.
  
  Examples:
    (double-array-dot-product (double-array [1.0 2.0 3.0]) (double-array [4.0 5.0 6.0]))  ;=> 32.0
    (double-array-dot-product (double-array [1.0 0.0]) (double-array [0.0 1.0]))          ;=> 0.0"
  [dbl-array1 dbl-array2]
  (double-array-reduce-kv (fn [tot _ da1 da2]
                            (+ tot (* da1 da2)))
    0.0
    dbl-array1
    dbl-array2))

(s/fdef double-array-dot-product
  :args (s/and (s/cat :dbl-array1 ::double-array
                 :dbl-array2 ::double-array)
          (fn [{:keys [dbl-array1 dbl-array2]}]
            (= (count dbl-array1) (count dbl-array2))))
  :ret ::m/double)

(defn double-array-projection
  "Returns `::double-array`` of `dbl-array1` projected onto `dbl-array2`."
  [dbl-array1 dbl-array2]
  (let [s (m/div (double-array-dot-product dbl-array1 dbl-array2)
            (double-array-sum-of-squares dbl-array1))]
    (double-array-map #(* s %) dbl-array1)))

(s/fdef double-array-projection
  :args (s/and (s/cat :dbl-array1 ::double-array
                 :dbl-array2 ::double-array)
          (fn [{:keys [dbl-array1 dbl-array2]}]
            (= (count dbl-array1) (count dbl-array2))))
  :ret ::double-array)

(defn double-array-norm
  "The square-root of the sum of the squared values of the elements."
  [dbl-array]
  (m/sqrt (double-array-sum-of-squares dbl-array)))

(s/fdef double-array-norm
  :args (s/cat :dbl-array ::double-array)
  :ret ::m/double)

(def  double-array-norm2 "See [[double-array-norm]]." double-array-norm)

(defn double-array-norm1
  "The sum of the absolute values of the elements."
  [dbl-array]
  (let [abs-array (double-array-map m/abs dbl-array)]
    (double-array-sum abs-array)))

(s/fdef double-array-norm1
  :args (s/cat :dbl-array ::double-array)
  :ret ::m/double)

;;;DOUBLE ARRAY STATS
(defn double-array-mean
  "Computes the arithmetic mean of a double array.
  
  Returns the average value (sum / count) of all elements.
  Returns NaN for empty arrays due to division by zero.
  
  Examples:
    (double-array-mean (double-array [1.0 2.0 3.0]))  ;=> 2.0
    (double-array-mean (double-array [5.0]))          ;=> 5.0
    (double-array-mean (double-array []))             ;=> ##NaN"
  [dbl-array]
  (m/div (double-array-sum dbl-array) (alength dbl-array)))

(s/fdef double-array-mean
  :args (s/cat :dbl-array ::double-array)
  :ret ::m/double)

(defn double-array-second-moment
  "The second moment of the elements in `dbl-array`."
  [dbl-array]
  (m/div (double-array-sum-of-squares dbl-array) (alength dbl-array)))

(s/fdef double-array-second-moment
  :args (s/cat :dbl-array ::double-array)
  :ret ::m/double)

(defn double-array-variance
  "Computes the population variance of a double array.
  
  Returns the variance using the formula: Var(X) = E[X²] - (E[X])²
  This is the population variance (divides by N, not N-1).
  
  Examples:
    (double-array-variance (double-array [1.0 2.0 3.0]))  ;=> 0.6666666666666666
    (double-array-variance (double-array [5.0 5.0 5.0]))  ;=> 0.0 (no variation)
    (double-array-variance (double-array [1.0]))          ;=> 0.0 (single element)"
  [dbl-array]
  (- (double-array-second-moment dbl-array)
    (m/sq (double-array-mean dbl-array))))

(s/fdef double-array-variance
  :args (s/cat :dbl-array ::double-array)
  :ret ::m/double)

(defn double-array-std-dev
  "The standard deviation of the elements in `dbl-array`."
  [dbl-array]
  (m/sqrt (double-array-variance dbl-array)))

(s/fdef double-array-std-dev
  :args (s/cat :dbl-array ::double-array)
  :ret ::m/double)

(defn double-array-cross-moment
  "The cross moment between the elements of `dbl-array1` and `dbl-array2`."
  [dbl-array1 dbl-array2]
  (m/div (double-array-dot-product dbl-array1 dbl-array2)
    (alength dbl-array1)))

(s/fdef double-array-cross-moment
  :args (s/and (s/cat :dbl-array1 ::double-array
                 :dbl-array2 ::double-array)
          (fn [{:keys [dbl-array1 dbl-array2]}]
            (= (count dbl-array1) (count dbl-array2))))
  :ret ::m/double)

(defn double-array-covariance
  "The covariance between the elements of `dbl-array1` and `dbl-array2`."
  [dbl-array1 dbl-array2]
  (- (double-array-cross-moment dbl-array1 dbl-array2)
    (* (double-array-mean dbl-array1) (double-array-mean dbl-array2))))

(s/fdef double-array-covariance
  :args (s/and (s/cat :dbl-array1 ::double-array
                 :dbl-array2 ::double-array)
          (fn [{:keys [dbl-array1 dbl-array2]}]
            (= (count dbl-array1) (count dbl-array2))))
  :ret ::m/double)

(defn double-array-correlation
  "The correlation between the elements of `dbl-array1` and `dbl-array2`."
  [dbl-array1 dbl-array2]
  (m/div (double-array-covariance dbl-array1 dbl-array2)
    (* (double-array-std-dev dbl-array1) (double-array-std-dev dbl-array2))))

(s/fdef double-array-correlation
  :args (s/and (s/cat :dbl-array1 ::double-array
                 :dbl-array2 ::double-array)
          (fn [{:keys [dbl-array1 dbl-array2]}]
            (= (count dbl-array1) (count dbl-array2))))
  :ret ::m/double)
