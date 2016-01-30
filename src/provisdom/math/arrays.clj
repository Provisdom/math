(ns provisdom.math.arrays
  (:require [provisdom.math.core :as m]))

(set! *warn-on-reflection* true)

;MACROS
(defmacro deep-aget
  "Gets a value from a multidimensional array as if via 'aget', 
but with automatic application of appropriate type hints to each step 
in the array traversal as guided
by the hint added to the source array.
e.g. (deep-aget ^doubles arr i j)
Note: taken from p447-8 of ClojureProgramming (circa 2012)"
([array idx]
  '(aget ~array ~idx))
([array idx & idxs]
  (let [a-sym (gensym "a")]
    '(let [~a-sym (aget ~(vary-meta array assoc :tag 'objects) ~idx)]
       (deep-aget ~(with-meta a-sym {:tag (-> array meta :tag)}) ~@idxs)))))

(defmacro deep-aset
  "Sets a value from a multidimensional array as if via 'aset', 
but with automatic application of appropriate type hints to each step 
in the array traversal as guided
by the hint added to the target array.
e.g. (deep-aset ^doubles arr i j 1.0)
Note: taken from p448 of ClojureProgramming (circa 2012)"
  [array & idxsv]
  (let [hints '{booleans boolean, bytes byte, chars char, longs long, ints int, 
                shorts short, doubles double, floats float}
        hint (-> array meta :tag)
        [v idx & sxdi] (reverse idxsv)
        idxs (reverse sxdi)
        v (if-let [h (hints hint)] (list h v) v)
        nested-array (if (seq idxs) 
                       '(deep-aget ~(vary-meta array assoc :tag 'objects) 
                                   ~@idxs) 
                       array)
        a-sym (gensym "a")]
    '(let [~a-sym ~nested-array]
       (aset ~(with-meta a-sym {:tag hint}) ~idx ~v))))
  
(def double-array-type (Class/forName "[D"))
(def double-2D-array-type (Class/forName "[[D"))
(def double-3D-array-type (Class/forName "[[[D"))

;;;UTILITIES ; http://clj-me.cgrand.net/2009/10/15/multidim-arrays/
(defn array? [x] (-> x class .isArray))

(defn aprint [x] (if (array? x) (map aprint x) x))

;;;DEEP-ARRAYS
(defn block-array
  "Create a typed block array. 
   type-key:
      :d double (default)
      :l long
      :b boolean
      :c char"
  [type-key dim & dims]
  (let [t (condp = type-key :l Long/TYPE, :b Boolean/TYPE, :c Character/TYPE, 
            Double/TYPE)]
    (if-not dims (make-array t dim) (apply (partial make-array t dim) dims))))

(defn jagged-2D-array
  "Create a typed jagged 2D array.
   type-key:
      :d double (default)
      :l long
      :b boolean
      :c char"
  [type-key coll]
  (let [t (condp = type-key :l Long/TYPE, :b Boolean/TYPE, :c Character/TYPE, 
            Double/TYPE)]
    (into-array (map (partial into-array t) coll))))

(defn jagged-3D-array
  "Create a typed jagged 3D array.
   type-key:
      :d double (default)
      :l long
      :b boolean
      :c char"
  [type-key coll]
  (into-array (map (partial jagged-2D-array type-key) coll)))

;;;clojure.core.matrix.impl.double-array

;;;DOUBLE-ARRAYS -- useful for huge vectors or repetitive computation.
;;Conversion time is slow.
(defn aget-d
  ^double [^doubles coll ^long idx]
  (aget coll idx))

(defn aset!
  (^doubles [^doubles a ^doubles new-a]
    (do (dotimes [idx (count new-a)] (aset a idx (aget new-a idx)))
      a))
  (^doubles [^doubles a ^long idx ^double value]
    (aset a idx value)))

(defn aset-l!
  (^longs [^longs a ^longs new-a]
    (do (dotimes [idx (count new-a)] (aset a idx (aget new-a idx)))
      a))
  (^longs [^longs a ^long idx ^long value]
    (aset a idx value)))

(defn aset-2D!
  [a new-a]
  (do (doseq [i (range (count new-a)), j (range (count (aget ^"[[D" new-a i)))] 
        (aset a i j (aget ^"[[D" new-a i j)))
    a))

(defn avec 
  "Creates a new double-array containing the contents of coll.  
This is faster than avector."
  ^doubles [coll]
  (double-array coll))

(defn avector 
  "Creates a new double-array containing the args.  This is slower than avec."
  ^doubles [& args] 
  (double-array (apply vector args)))

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
  "Similar to 'reduce-kv' but for double arrays.  
First array must be the shortest." 
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
  (java.util.Arrays/equals a1 a2))

(defn afirst ^double [^doubles a]
  (aget a 0))

(defn alast ^double [^doubles a]
  (aget a (dec (alength a))))

(defn afind-all [^doubles a ^double item]
  (areduce-kv #(if (= %3 item) (conj % %2) %) [] a))

(defn asort ^doubles [^doubles a]
  (doto (java.util.Arrays/copyOf a (alength a)) (java.util.Arrays/sort)))

(defn asorted-find ^long [^doubles a ^double item]
  (java.util.Arrays/binarySearch a item))

(defn afill!
  (^doubles [^doubles a ^double item] (java.util.Arrays/fill a item))
  (^doubles [^doubles a ^long from ^long to ^double item] 
    (java.util.Arrays/fill a from to item)))

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
  (let [s (/ (adot-product a1 a2) (asum-squares a1))] (amap-ext #(* s %) a1)))

(defn anorm ^doubles [^doubles a] 
  (let [sum (asum a)] (amap-ext #(/ % sum) a)))

(defn anorm2 ^double [^doubles a] (m/sqrt (asum-squares a)))

;;;STATS
(defn amean ^double [^doubles a] (/ (asum a) (alength a)))

(defn asecond-moment ^double [^doubles a] (/ (asum-squares a) (alength a)))

(defn avariance ^double [^doubles a] (- (asecond-moment a) (m/sq (amean a))))

(defn astd-dev ^double [^doubles a] (m/sqrt (avariance a)))

(defn across-moment ^double [^doubles a1 ^doubles a2] 
  (/ (adot-product a1 a2) (alength a1)))              

(defn acovariance ^double [^doubles a1 ^doubles a2] 
  (- (across-moment a1 a2) (* (amean a1) (amean a2))))

(defn acorrelation ^double [^doubles a1 ^doubles a2] 
  (/ (acovariance a1 a2) (* (astd-dev a1) (astd-dev a2))))