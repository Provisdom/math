(ns provisdom.test.math.bounds
  (:require [midje.sweet :refer :all]
            [criterium.core :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.bounds :refer :all]
            [provisdom.math.core :as m]))

(facts "constructors"
       (fact "scalars"
             (bounds) 
             => {:lower m/inf-, :open-lower? false, 
                 :open-upper? false, :upper m/inf+}
             (bounds -1.0 1.0) 
             => {:lower -1.0, :open-lower? false, 
                 :open-upper? false, :upper 1.0}
             (bounds -1.0 1.0 true true) 
             => {:lower -1.0, :open-lower? true, 
                 :open-upper? true, :upper 1.0}
             bounds-open 
             => {:lower m/inf-, :open-lower? true, 
                 :open-upper? true, :upper m/inf+}
             bounds+ 
             => {:lower 0, :open-lower? true, 
                 :open-upper? false, :upper m/inf+}
             bounds-non- 
             => {:lower 0, :open-lower? false, 
                 :open-upper? false, :upper m/inf+}
             bounds-prob 
             => {:lower 0, :open-lower? false, :open-upper? false, :upper 1}
             bounds-open-prob 
             => {:lower 0, :open-lower? true, :open-upper? true, :upper 1}
             bounds-long-non- 
             => {:lower 0, :open-lower? false, 
                 :open-upper? false, :upper 9223372036854775807})
       (fact "vectors"
             (vector-bounds 3) 
             => (just [{:lower m/inf-, :open-lower? false, :open-upper? false, 
                        :upper m/inf+} 
                       {:lower m/inf-, :open-lower? false, :open-upper? false, 
                        :upper m/inf+} 
                       {:lower m/inf-, :open-lower? false, :open-upper? false, 
                        :upper m/inf+}])
             (vector-bounds 2 bounds+) 
             => (just [{:lower 0, :open-lower? true, :open-upper? false, 
                        :upper m/inf+} 
                       {:lower 0, :open-lower? true, :open-upper? false, 
                        :upper m/inf+}]))
       (fact "matrices"
             (positive-definite-matrix-bounds 2)
             => (just [{:lower 0, :open-lower? true, :open-upper? false, 
                        :upper m/inf+}
                       {:lower m/inf-, :open-lower? false, :open-upper? false, 
                        :upper m/inf+}
                       {:lower 0, :open-lower? true, :open-upper? false, 
                        :upper m/inf+}])))

(facts "properties"
       (fact "out of range?"
             (.out-of-range? (bounds) 3.0) => false
             (.out-of-range? bounds-prob 3.0) => true)
       (fact "bracket"
             (.bracket (bounds)) => [m/inf- m/inf+]
             (.bracket bounds-prob) => [0 1])
       (fact "lower bound"
             (.lower-bound (bounds)) => [m/inf- false]
             (.lower-bound bounds-prob) => [0 false])
       (fact "upper bound"
             (.upper-bound (bounds)) => [m/inf+ false]
             (.upper-bound bounds-open-prob) => [1 true]))

(facts "manipulation"
       (fact "sort"
             (sort-bounds [(bounds -1.0 1.0) (bounds -2.0 0.0 true true)])
             => (just [{:lower -2.0, :open-lower? true, :open-upper? true,
                        :upper 0.0}
                       {:lower -1.0, :open-lower? false, :open-upper? false,
                        :upper 1.0}])
             (sort-bounds [(bounds -1.0 1.0) (bounds -2.0 0.0 true true)]
                          :by-upper? true)
             => (just [{:lower -1.0, :open-lower? false, :open-upper? false,
                        :upper 1.0}
                       {:lower -2.0, :open-lower? true, :open-upper? true,
                        :upper 0.0}]))
       (fact "intersection"
             (intersection [(bounds -1.0 1.0) (bounds -2.0 0.0 true true)])
             => {:lower -1.0, :open-lower? false, :open-upper? true, 
                 :upper 0.0}
             (intersection [(bounds -3.0 -1.0 true true) (bounds 1.0 3.0)]) 
             => nil
             (intersection [(bounds -3.0 -1.0 true true)
                             (bounds -3.0 1.0 false false)])
             => {:lower -3.0, :open-lower? false, :open-upper? true,
                 :upper -1.0})
       (fact "union"
             (union [(bounds -1.0 1.0) (bounds -2.0 0.0 true true)])
             => (just [{:lower -1.0, :open-lower? false,
                        :open-upper? true, :upper 0.0}])
             (union [(bounds -3.0 -1.0 true true) (bounds 1.0 3.0)])
             => (just [{:lower -3.0, :open-lower? true, :open-upper? true,
                        :upper -1.0}
                       {:lower 1.0, :open-lower? false, :open-upper? false,
                        :upper 3.0}])
             (union [(bounds -3.0 -1.0 true true)
                     (bounds -3.0 1.0 false false)])
             => (just [{:lower -3.0, :open-lower? false, :open-upper? true,
                        :upper -1.0}]))
       (fact "encompassing-bounds"
             (encompassing-bounds [(bounds -1.0 1.0) 
                                   (bounds -2.0 0.0 true true)])
             => {:lower -2.0, :open-lower? true, :open-upper? false,
                 :upper 1.0}
             (encompassing-bounds [(bounds -3.0 -1.0 true true) 
                                   (bounds 1.0 3.0)])
             => {:lower -3.0, :open-lower? true, :open-upper? false,
                 :upper 3.0}
             (encompassing-bounds [(bounds -3.0 -1.0 true true)
                                   (bounds -3.0 1.0 false false)])
             => {:lower -3.0, :open-lower? false, :open-upper? false,
                 :upper 1.0}))

