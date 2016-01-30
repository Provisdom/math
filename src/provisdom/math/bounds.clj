(ns provisdom.math.bounds
  (:require [provisdom.utility-belt.core :as co]
            [provisdom.math [core :as m]
             [matrix :as mx]]))

(set! *warn-on-reflection* true)

(defprotocol Range
  (out-of-range? [r v])
  (bracket [r])
  (lower-bound [r])
  (upper-bound [r]))

(defrecord Bounds [lower upper open-lower? open-upper?]
  Range
  (out-of-range? [r v]
    (or
      (if open-lower? (<= v lower) (< v lower))
      (if open-upper? (>= v upper) (> v upper))))
  (bracket [r] [lower upper])
  (lower-bound [r] [lower open-lower?])
  (upper-bound [r] [upper open-upper?]))

;;;BOUNDS CONSTRUCTORS
(defn bounds
  "Default bounds are inf- to inf+.  
Bounds are closed by default."
  ([] (bounds m/inf- m/inf+))
  ([lower upper] (bounds lower upper false false))
  ([lower upper open-lower? open-upper?] 
    (Bounds. lower upper open-lower? open-upper?)))

(def bounds-open (bounds m/inf- m/inf+ true true))
(def bounds+ (bounds 0 m/inf+ true false))
(def bounds-non- (bounds 0 m/inf+))
(def bounds-prob (bounds 0 1))
(def bounds-open-prob (bounds 0 1 true true))
(def bounds-long-non- (bounds 0 m/max-long))

(defn vector-bounds 
  "Returns a vector of bounds."
  ([^long size] (vector-bounds size (bounds)))
  ([^long size bounds] (into [] (repeat size bounds))))

(defn positive-matrix-bounds 
  "Returns a vector of bounds flattened for a symmetric positive matrix."
  [^long size]
  (mx/to-vector-from-symmetric 
    (mx/symmetric-matrix #(if (== % %2) bounds+ (bounds)) size true)))

;;;BOUNDS MANIPULATION
(defn- min-bound 
  [bound-coll] 
  (reduce (fn [[b1 ob1?] [b2 ob2?]] 
            (cond (< b1 b2) [b1 ob1?], 
                  (< b2 b1) [b2 ob2?], 
                  :else [b1 (and ob1? ob2?)])) [m/inf+ false] bound-coll))

(defn- max-bound 
  [bound-coll] 
  (reduce (fn [[b1 ob1?] [b2 ob2?]] 
            (cond (> b1 b2) [b1 ob1?] (> b2 b1) [b2 ob2?] 
                  :else [b1 (and ob1? ob2?)])) [m/inf- false] bound-coll))

(defn sort-bounds 
  "Returns a bounds collection sorted by lower bound first (by default)."
  [bounds-coll & {:keys [by-upper?]}]
  (let [f (if by-upper? 
            #(vector (- (.upper ^Bounds %)) (not (.open-upper? ^Bounds %))
                     (.lower ^Bounds %) (.open-lower? ^Bounds %)) 
            #(vector (.lower ^Bounds %) (.open-lower? ^Bounds %)
                     (- (.upper ^Bounds %)) (not (.open-upper? ^Bounds %))))] 
    (sort-by f bounds-coll)))

(defn intersection 
  "Returns the bounds intersection or nil."
  [bounds-coll] 
  (let [[l lo?] (max-bound (map #(lower-bound %) bounds-coll)), 
        [u uo?] (min-bound (map #(upper-bound %) bounds-coll))]
    (if (or (> l u) (and (= u l) lo? (not uo?))) nil
      (bounds l u lo? uo?))))

(defn union 
  "Returns a collection of non-overlapping bounds"
  [bounds-coll] 
  (loop [sep [], [a b & c] (sort-bounds bounds-coll)]
    (if-not b (conj sep a)
      (let [g (intersection [a b])]
        (if g (recur sep (cons g c)) (recur (conj sep a) (cons b c)))))))

(defn encompassing-bounds
  "Returns smallest bounds the encompass the bounds-coll"
  [bounds-coll] 
  (let [[l lo?] (min-bound (map #(lower-bound %) bounds-coll)), 
        [u uo?] (max-bound (map #(upper-bound %) bounds-coll))]
    (bounds l u lo? uo?)))
  

