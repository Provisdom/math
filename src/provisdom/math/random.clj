(ns provisdom.math.random
  (:require [provisdom.utility-belt.core :as co]
            [provisdom.math [core :as m]
             [special-functions :as mf]
             [apache :as ap]]
            [clojure.core.reducers :as ccr]
            [taoensso.truss :refer (have have! have?)]))

(set! *warn-on-reflection* true)

(defn quasi-random
  "Better coverage but more predictable lazy sequence of vectors.
Because of predictability, better for a single use."
  [^long dimensions] 
    (let [rand (ap/sobol-quasi-random-sequence dimensions)] 
      (repeatedly #(vec (ap/next-quasi-random-vector! rand)))))

(defn secure-random
  "Less predictable but slower rnd-lazy"
  ([] (secure-random 4))
  ([^long seed] 
    (let [rand (ap/isaac-random-cryptographic seed)] 
      (repeatedly #(ap/next-double! rand)))))

(defn random
  "Returns rnd-lazy using default or selected seed.  
Good for repeating a run."
  ([] (random 4))
  ([^long seed] 
    (let [rand (ap/mersenne-twister seed)] 
      (repeatedly #(ap/next-double! rand)))))

(defn rnd-long
  "low is inclusive and high is exclusive."
  (^long [^double rnd] (m/floor (* m/max-long (dec (* 2 rnd)))))
  (^long [^double rnd ^long low ^long high]
   {:pre [(have? (fn [[low high]] (< low high)) [low high])]}
    ;;range must be less than m/max-long
    (m/floor (+ low (* (- high low) rnd))))) 

(defn rnd-boolean 
  "Returns true or false"
  [^double rnd] (<= rnd 0.5))

(defn rnd-normal 
  "Returns a value drawn from a standard normal distribution"
  ^double [^double rnd] (mf/inv-cdf-standard-normal rnd))

(defn random$ 
  "Returns [rnd-lazy seed], where seed is a pseudo-random number.  
The seed is generated from a random without a seed, and can be stored for 
   repeatability."
  [] 
  (let [seed (rnd-long (ap/next-double! (ap/mersenne-twister$)))] 
    [(random seed) seed]))

(defn split-random-lazy
  "Returns tuple of a lazy-seq of rnd-lazy and a rnd-lazy.  
Useful for parallelization.  
There is an extremely tiny chance (2^-64 perhaps) of non-randomness per split."
  [rnd-lazy] 
  [(map #(random (rnd-long %)) (rest rnd-lazy)) 
   (random (rnd-long (first rnd-lazy)))])

(defn split-random
  "Returns tuple of rnd-lazy. 
Useful for parallelization.  
There is an extremely tiny chance (2^-64 perhaps) of non-randomness per split."
  [rnd-lazy] [(rest rnd-lazy) (random (rnd-long (first rnd-lazy)))])

(defn multi-sample
  "Returns tuple of [sampled-values rnd-lazy].
Use :r meta-tag on samplef for inputting :rnd or :rnd-lazy (default)."
  [samplef rnd-lazy ^long ntake]
  (if-not (= :rnd (:r (meta samplef)))
    (let [s (take ntake (iterate #(samplef (second %)) (samplef rnd-lazy)))]
    [(map first s) (second (last s))])
  [(map samplef (take ntake rnd-lazy)), (drop ntake rnd-lazy)]))

(defn multi-sample-indexed
  "Returns tuple of [sampled-values rnd-lazy].  
samplef should be function of index and either rnd or rnd-lazy.
Use :r meta-tag on samplef for inputting :rnd or :rnd-lazy (default)"
  [samplef rnd-lazy ^long ntake]
  (if-not (= :rnd (:r (meta samplef)))
    (loop [coll [], r rnd-lazy, i 0]
      (if (>= i ntake) [coll r]
        (let [[s laz] (samplef i r)] (recur (conj coll s) laz (inc i)))))
    [(map-indexed samplef (take ntake rnd-lazy)), (drop ntake rnd-lazy)]))

(defn fold-random
  "Returns tuple of value and rnd-lazy.
This fn is an extension of 'fold' in core.reducers for folding rnd-lazy or rnd.   
Reduces a collection using a (potentially parallel) reduce-combine
   strategy. 
The collection is partitioned into groups of approximately chunk-size 
   (default 512), each of which is reduced with reducef (with a seed value 
   obtained by calling (combinef) with no arguments).
For rnd-lazy, the reducef should take the result and first part of the tuple
   from the samplef.
The results of these reductions are then reduced with combinef 
   (default reducef). 
combinef must be associative, and, when called with no arguments, 
   (combinef) must produce its identity element.
These operations may be performed in parallel, but the results will preserve 
   order. 
Use :r meta-tag on samplef for inputting :rnd or :rnd-lazy (default)"
  ([^long min-runs reducef samplef rnd-lazy]
    (fold-random min-runs reducef reducef samplef rnd-lazy))
  ([min-runs combinef reducef samplef rnd-lazy]
    (let [chunk-size 512] 
      (fold-random chunk-size (m/ceil (/ min-runs chunk-size)) 
                   combinef reducef samplef rnd-lazy)))
  ([chunk-size chunks combinef reducef samplef rnd-lazy]
    (let [runs (* chunk-size chunks)] 
      (if (= :rnd (:r (meta samplef)))
        [(ccr/fold 
           chunk-size combinef #(reducef %1 (samplef %2)) 
           (take runs rnd-lazy)) 
         (drop runs rnd-lazy)]
        (let [[lazies laz] (split-random-lazy rnd-lazy)]
          [(ccr/fold 
             chunk-size combinef #(reducef %1 (first (samplef %2))) 
             (take runs lazies)) 
           laz])))))