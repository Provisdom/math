(ns provisdom.math.arrays
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [provisdom.math.core :as m])
  (:import [java.util Arrays]))

(set! *warn-on-reflection* true)

(declare avec)

(def double-array-type (Class/forName "[D"))
(def double-2D-array-type (Class/forName "[[D"))
(def double-3D-array-type (Class/forName "[[[D"))

;;;UTILITIES ; http://clj-me.cgrand.net/2009/10/15/multidim-arrays/
(defn array?
  "Returns true if `x` is an Array."
  [x] (if (some? x) (-> x class .isArray) false))

(defn num-array?
  "Returns true if 'x' is an Array and each element is a num"
  [x] (and (array? x) (every? m/num? x)))

(defn finite-array?
  "Returns true if 'x' is an Array and each element is finite"
  [x] (and (array? x) (every? m/finite? x)))

(defn number-array-gen
  ([] (gen/fmap avec (gen/vector (s/gen ::m/number))))
  ([count] (gen/fmap avec (gen/vector (s/gen ::m/number) count))))
(s/def ::array (s/with-gen array? number-array-gen))

(defn num-array-gen
  ([] (gen/fmap avec (gen/vector (s/gen ::m/num))))
  ([count] (gen/fmap avec (gen/vector (s/gen ::m/num) count))))
(s/def ::num-array (s/with-gen num-array? num-array-gen))

(defn finite-array-gen
  ([] (gen/fmap avec (gen/vector (s/gen ::m/finite))))
  ([count] (gen/fmap avec (gen/vector (s/gen ::m/finite) count))))
(s/def ::finite-array (s/with-gen finite-array? finite-array-gen))

(defn array->seq [x] (if (array? x) (map array->seq x) x))

;;;DEEP-ARRAYS
(def ^:private arr-type-lookup
  {:l :long :b :boolean :c :char :character :char})

(defn type->obj
  "Returns the Java type for given keyword type. Keyword types can be:
    :double double (default)
    :long long
    :boolean boolean
    :char char"
  [type]
  (condp = (or (arr-type-lookup type) type)
    :long Long/TYPE
    :boolean Boolean/TYPE
    :char Character/TYPE
    Double/TYPE))

(defn block-array
  "Create a typed block array. 
   type:
      :double double (default)
      :long long
      :boolean boolean
      :char char"
  [type dim & dims]
  (let [type (type->obj type)]
    (if-not dims (make-array type dim) (apply (partial make-array type dim) dims))))

(defn jagged-2D-array
  "Create a typed jagged 2D array.
   type-key:
      :double double (default)
      :long long
      :boolean boolean
      :char char"
  [type coll]
  (assert (sequential? (first coll)) "You must pass a 2D collection")
  (let [type (type->obj type)]
    (into-array (map (partial into-array type) coll))))

(defn jagged-3D-array
  "Create a typed jagged 3D array.
   type-key:
      :double double (default)
      :long long
      :boolean boolean
      :char char"
  [type-key coll]
  (assert (sequential? (ffirst coll)) "You must pass a 3D collection")
  (into-array (map (partial jagged-2D-array type-key) coll)))

;;;clojure.core.matrix.impl.double-array

;;;DOUBLE-ARRAYS -- useful for huge vectors or repetitive computation.
;;Conversion time is slow.
(comment
  ;; Perf comp between aget and aget-d
  (perf/bench (aget arr (rand-int 3)))
  ;Evaluation count : 10252620 in 60 samples of 170877 calls.
  ;Execution time mean : 5.799010 µs
  ;Execution time std-deviation : 107.785879 ns
  ;Execution time lower quantile : 5.620088 µs ( 2.5%)
  ;Execution time upper quantile : 5.990912 µs (97.5%)
  ;Overhead used : 1.453119 ns
  ;
  ;Found 1 outliers in 60 samples (1.6667 %)
  ;low-severe	 1 (1.6667 %)
  ;Variance from outliers : 7.8099 % Variance is slightly inflated by outliers
  (perf/bench (a/aget-d arr (rand-int 3)))
  ;Evaluation count : 1743407820 in 60 samples of 29056797 calls.
  ;Execution time mean : 32.368536 ns
  ;Execution time std-deviation : 0.239115 ns
  ;Execution time lower quantile : 32.111605 ns ( 2.5%)
  ;Execution time upper quantile : 32.846321 ns (97.5%)
  ;Overhead used : 1.453119 ns
  ;
  ;Found 5 outliers in 60 samples (8.3333 %)
  ;low-severe	 2 (3.3333 %)
  ;low-mild	 3 (5.0000 %)
  ;Variance from outliers : 1.6389 % Variance is slightly inflated by outliers
  (perf/bench (aget ^doubles arr (rand-int 3)))
  ;Evaluation count : 1785795300 in 60 samples of 29763255 calls.
  ;Execution time mean : 32.860436 ns
  ;Execution time std-deviation : 0.813892 ns
  ;Execution time lower quantile : 32.099554 ns ( 2.5%)
  ;Execution time upper quantile : 35.539697 ns (97.5%)
  ;Overhead used : 1.453119 ns
  ;
  ;Found 5 outliers in 60 samples (8.3333 %)
  ;low-severe	 2 (3.3333 %)
  ;low-mild	 3 (5.0000 %)
  ;Variance from outliers : 12.5787 % Variance is moderately inflated by outliers
  )

(defn aget-d
  "Returns the value in the collection at the given index"
  ^double [^doubles coll ^long idx]
  (aget coll idx))

;; TODO: This is the same thing as System.arraycopy. Is there any reason to keep this impl?
(defn aset!
  "Returns `a` after setting all the values in `new-a` to `a`. Optionally takes an `idx` and `value`
  which sets `value` at `idx` in `a`.
  Note: Type hinted for Double"
  (^doubles [^doubles a ^doubles new-a]
   (dotimes [idx (count new-a)] (aset a idx (aget new-a idx)))
   a)
  (^doubles [^doubles a ^long idx ^double value]
   (aset a idx value)))

(defn aset-l!
  "Returns `a` after setting all the values in `new-a` to `a`. Optionally takes an `idx` and `value`
  which sets `value` at `idx` in `a`.
  Note: Type hinted for Long"
  (^longs [^longs a ^longs new-a]
   (dotimes [idx (count new-a)] (aset a idx (aget new-a idx)))
   a)
  (^longs [^longs a ^long idx ^long value]
   (aset a idx value)))

(defn aset-2D!
  "Returns `a` after setting all the values in the 2D array `new-a` into the 2D array `a`."
  [a new-a]
  (doseq [i (range (count new-a))
          j (range (count (aget ^"[[D" new-a i)))]
    (aset a i j (aget ^"[[D" new-a i j)))
  a)

(defn avec
  "Creates a new double-array containing the contents of `coll`.
  Note: This is faster than avector."
  ^doubles [coll] (double-array coll))

(defn avector
  "Creates a new double-array containing the args.
  Note: This is slower than avec."
  ^doubles [& args] (double-array (apply vector args)))

(defn arepeat
  "Creates a new double-array of length 'size' and value 'v'"
  (^doubles [^long size] (double-array size))
  (^doubles [^long size ^double v] (double-array size v)))

(defn amap-ext
  "Similar to 'map' but for double arrays."
  (^doubles [f ^doubles a]
   (amap a i ret (double (f (aget a i)))))
  (^doubles [f ^doubles a1 ^doubles a2]
   (amap a1 i ret (double (f (aget a1 i) (aget a2 i)))))
  (^doubles [f ^doubles a1 ^doubles a2 ^doubles a3]
   (amap a1 i ret (double (f (aget a1 i) (aget a2 i) (aget a3 i))))))

(defn amap-indexed
  "Similar to 'map-indexed' but for double arrays.  
First array must be the shortest."
  (^doubles [f ^doubles a]
   (amap a i ret (double (f i (aget a i)))))
  (^doubles [f ^doubles a1 ^doubles a2]
   (amap a1 i ret (double (f i (aget a1 i) (aget a2 i)))))
  (^doubles [f ^doubles a1 ^doubles a2 ^doubles a3]
   (amap a1 i ret (double (f i (aget a1 i) (aget a2 i) (aget a3 i))))))

(defn areduce-kv
  "Similar to 'reduce-kv' but for double arrays. First array must be the shortest.
  Calls f with the return value, index, and the value at that index"
  (^double [f ^double init ^doubles a]
   (areduce a i ret init (double (f ret i (aget a i)))))
  (^double [f ^double init ^doubles a1 ^doubles a2]
   (areduce a1 i ret init (double (f ret i (aget a1 i) (aget a2 i)))))
  ([f init a1 a2 a3]
   (areduce ^doubles a1 i ret init
            (double (f ret i (aget ^doubles a1 i)
                       (aget ^doubles a2 i) (aget ^doubles a3 i))))))

;;write Macro to prevent '=' sign from returning false to value checks?
(defn a= [^doubles a1 ^doubles a2]
  "Returns true if the two specified arrays of booleans are equal to one another. Two arrays
  are considered equal if both arrays contain the same number of elements, and all corresponding
  pairs of elements in the two arrays are equal. In other words, two arrays are equal if they
  contain the same elements in the same order. Also, two array references are considered equal if
  both are null."
  (Arrays/equals a1 a2))

(defn ^double afirst
  "Returns the first element in `a`"
  [^doubles a] (aget a 0))

(defn ^double alast
  "Returns the last element in `a`"
  [^doubles a] (aget a (dec (alength a))))

(defn afind-all
  "Returns a vector with all the indices where `item` is found in `a`."
  [^doubles a ^double item] (areduce-kv #(if (= %3 item) (conj % %2) %) [] a))

(defn ^doubles asort
  "Returns a sorted array of `a`. This function will create a copy of `a` before sorting it."
  [^doubles a]
  (doto (Arrays/copyOf a (alength a)) (Arrays/sort)))

(defn ^long asorted-find
  "Searches the specified array of doubles for the specified value using the binary search algorithm.
  The array must be sorted (as by the sort(double[]) method) prior to making this call. If it is not
  sorted, the results are undefined. If the array contains multiple elements with the specified value,
  there is no guarantee which one will be found. This method considers all NaN values to be equivalent
  and equal."
  [^doubles a ^double item] (Arrays/binarySearch a item))

(defn ^doubles afill!
  "Assigns the specified double value to each element of the specified array of doubles.
  Given `from` and `to` `afill!` will assign the specified double value to each element of the specified
  range of the specified array of doubles. The range to be filled extends from index fromIndex, inclusive,
  to index toIndex, exclusive. (If fromIndex==toIndex, the range to be filled is empty.)"
  ([^doubles a ^double item] (Arrays/fill a item))
  ([^doubles a ^long from ^long to ^double item] (Arrays/fill a from to item)))

;;;DOUBLE-ARRAY MATH
(defn ain-place
  (^doubles [f ^doubles a1 ^doubles a2] (amap-indexed #(f %2 %3) a1 a2))
  (^doubles [f ^doubles a1 ^doubles a2 ^doubles a3]
   (amap-indexed #(f %2 %3 %4) a1 a2 a3))
  (^doubles [f ^doubles a1 ^doubles a2 ^doubles a3 ^doubles & as]
   (loop [a (list* a2 a3 as), ret a1]
     (if (empty? a)
       ret
       (recur (rest a) (amap-indexed #(f %2 %3) ret (first a)))))))

(defn aplus
  (^doubles [^doubles a1 ^doubles a2] (ain-place + a1 a2))
  (^doubles [^doubles a1 ^doubles a2 ^doubles a3] (ain-place + a1 a2 a3))
  (^doubles [^doubles a1 ^doubles a2 ^doubles a3 ^doubles & as]
   (apply ain-place + a1 a2 a3 as)))

(defn aminus
  (^doubles [^doubles a1 ^doubles a2] (ain-place - a1 a2))
  (^doubles [^doubles a1 ^doubles a2 ^doubles a3] (ain-place - a1 a2 a3))
  (^doubles [^doubles a1 ^doubles a2 ^doubles a3 ^doubles & as]
   (apply ain-place - a1 a2 a3 as)))

(defn asum ^double [^doubles a] (areduce-kv #(+ % %3) 0.0 a))

(defn asum-squares ^double [^doubles a] (areduce-kv #(+ % (m/sq %3)) 0.0 a))

(defn aprod ^double [^doubles a] (areduce-kv #(* % %3) 1.0 a))

(defn adot-product ^double [^doubles a1 ^doubles a2]
  (areduce-kv #(+ % (* %3 %4)) 0.0 a1 a2))

(defn aproj ^double [^doubles a1 ^doubles a2]
  (let [s (m/div (adot-product a1 a2) (asum-squares a1))] (amap-ext #(* s %) a1)))

(defn anorm ^doubles [^doubles a]
  (let [sum (asum a)] (amap-ext #(m/div % sum) a)))

(defn anorm2 ^double [^doubles a] (m/sqrt (asum-squares a)))

;;;STATS
(defn amean ^double [^doubles a] (m/div (asum a) (alength a)))

(defn asecond-moment ^double [^doubles a] (m/div (asum-squares a) (alength a)))

(defn avariance ^double [^doubles a] (- (asecond-moment a) (m/sq (amean a))))

(defn astd-dev ^double [^doubles a] (m/sqrt (avariance a)))

(defn across-moment ^double [^doubles a1 ^doubles a2]
  (m/div (adot-product a1 a2) (alength a1)))

(defn acovariance ^double [^doubles a1 ^doubles a2]
  (- (across-moment a1 a2) (* (amean a1) (amean a2))))

(defn acorrelation ^double [^doubles a1 ^doubles a2]
  (m/div (acovariance a1 a2) (* (astd-dev a1) (astd-dev a2))))