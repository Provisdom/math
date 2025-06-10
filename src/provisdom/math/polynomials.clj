(ns provisdom.math.polynomials
  "Polynomial functions and orthogonal polynomial bases.
  
  Provides polynomial evaluation, interpolation, and specialized polynomial 
  families including Chebyshev, Legendre, and other orthogonal polynomials.
  Useful for function approximation, numerical integration, and spectral methods.
  
  Features polynomial evaluation using Horner's method for numerical stability."
  (:require
    [clojure.spec.alpha :as s]
    [provisdom.math.combinatorics :as combinatorics]
    [provisdom.math.core :as m]
    [provisdom.math.derivatives :as derivatives]
    [provisdom.math.vector :as vector]))

;;;DECLARATIONS
(declare polynomial-fn)

(s/def ::second-kind? boolean?)

(s/def ::degree
  (s/with-gen ::m/long-non-
    #(s/gen (s/int-in 0 3))))

(s/def ::start-degree ::degree)
(s/def ::end-degree ::degree)
(s/def ::basis-count ::degree)
(s/def ::chebyshev-kind (s/int-in 0 3))

(s/def ::number->number
  (s/fspec :args (s/cat :number ::m/number)
    :ret ::m/number))

(s/def ::number->v
  (s/fspec :args (s/cat :number ::m/number)
    :ret ::vector/vector))

(s/def ::number2->v
  (s/fspec :args (s/cat :number1 ::m/number
                   :number2 ::m/number)
    :ret ::vector/vector))

(s/def ::v->v
  (s/fspec :args (s/cat :v ::vector/vector)
    :ret ::vector/vector))

;;;CONSTANTS
(def ^:const ^:private chebyshev-polynomial-of-the-first-kind-fns
  [(fn [_] 1.0)
   #(* 1.0 %)
   #(dec (* 2.0 (m/sq %)))
   #(+ (* 4 (m/cube %)) (* -3 %))
   #(+ (* 8 (m/pow % 4)) (* -8 (m/sq %)) 1)
   #(+ (* 16 (m/pow % 5)) (* -20 (m/cube %)) (* 5 %))
   #(+ (* 32 (m/pow % 6)) (* -48 (m/pow % 4)) (* 18 (m/sq %)) -1)
   #(+ (* 64 (m/pow % 7)) (* -112 (m/pow % 5)) (* 56 (m/cube %)) (* -7 %))
   #(+ (* 128 (m/pow % 8))
      (* -256 (m/pow % 6))
      (* 160 (m/pow % 4))
      (* -32 (m/sq %)) 1)
   #(+ (* 256 (m/pow % 9))
      (* -576 (m/pow % 7))
      (* 432 (m/pow % 5))
      (* -120 (m/cube %)) (* 9 %))
   #(+ (* 512 (m/pow % 10))
      (* -1280 (m/pow % 8))
      (* 1120 (m/pow % 6))
      (* -400 (m/pow % 4))
      (* 50 (m/sq %)) -1)
   #(+ (* 1024 (m/pow % 11))
      (* -2816 (m/pow % 9))
      (* 2816 (m/pow % 7))
      (* -1232 (m/pow % 5))
      (* 220 (m/cube %))
      (* -11 %))])

(def ^:const ^:private chebyshev-polynomial-of-the-second-kind-fns
  [(fn [_] 1.0)
   #(* 2.0 %)
   #(dec (* 4.0 (m/sq %)))
   #(+ (* 8 (m/cube %)) (* -4 %))
   #(+ (* 16 (m/pow % 4)) (* -12 (m/sq %)) 1)
   #(+ (* 32 (m/pow % 5)) (* -32 (m/cube %)) (* 6 %))
   #(+ (* 64 (m/pow % 6)) (* -80 (m/pow % 4)) (* 24 (m/sq %)) -1)
   #(+ (* 128 (m/pow % 7)) (* -192 (m/pow % 5)) (* 80 (m/cube %)) (* -8 %))
   #(+ (* 256 (m/pow % 8))
      (* -448 (m/pow % 6))
      (* 240 (m/pow % 4))
      (* -40 (m/sq %))
      1)
   #(+ (* 512 (m/pow % 9))
      (* -1024 (m/pow % 7))
      (* 672 (m/pow % 5))
      (* -160 (m/cube %))
      (* 10 %))])

;;;CHEBYSHEV POLYNOMIALS
(defn chebyshev-polynomial-fn
  "Creates a Chebyshev polynomial function of the specified degree.
  
  Chebyshev polynomials are orthogonal polynomials useful for function
  approximation and numerical integration. First kind polynomials T_n(x)
  are bounded on [-1,1], while second kind polynomials U_n(x) are their
  derivatives scaled by (1-x²).
  
  Parameters:
    degree - Non-negative integer degree of the polynomial
  
  Options:
    ::second-kind? - If true, returns U_n(x) instead of T_n(x) (default false)
  
  Returns a function that evaluates the Chebyshev polynomial at a given point.
  
  Example:
    ((chebyshev-polynomial-fn 2) 0.5) ; => T₂(0.5) = -0.5
    ((chebyshev-polynomial-fn 2 {::second-kind? true}) 0.5) ; => U₂(0.5) = -1.0"
  ([degree] (chebyshev-polynomial-fn degree {}))
  ([degree {::keys [second-kind?] :or {second-kind? false}}]
   (let [degree (long degree)
         fns (if second-kind?
               chebyshev-polynomial-of-the-second-kind-fns
               chebyshev-polynomial-of-the-first-kind-fns)
         m (if second-kind? 9 11)]
     (if (<= degree m)
       (fns degree)
       #(second (last (take (inc (- degree m))
                        (iterate (fn [[old new]]
                                   [new
                                    (- (* 2 new %) old)])
                          [((fns (dec m)) %) ((fns m) %)]))))))))

(s/fdef chebyshev-polynomial-fn
  :args (s/cat :degree ::degree
          :opts (s/? (s/keys :opt [::second-kind?])))
  :ret ::number->number)

;;http://en.wikipedia.org/wiki/Chebyshev_polynomials 
;;-- also solved for the derivative of second-kind at x = +-1
(defn chebyshev-derivative-fn
  "Creates a function that computes derivatives of Chebyshev polynomials.
  
  Computes the nth derivative of Chebyshev polynomials using analytic
  formulas when available, falling back to numerical differentiation.
  
  Parameters:
    degree - Degree of the base Chebyshev polynomial
    derivative - Order of derivative (1 for first derivative, 2 for second, etc.)
  
  Options:
    ::second-kind? - If true, differentiates U_n(x) instead of T_n(x) (default false)
  
  Returns a function that evaluates the derivative at a given point.
  
  Example:
    ((chebyshev-derivative-fn 3 1) 0.5) ; => T₃'(0.5)"
  ([degree derivative] (chebyshev-derivative-fn degree derivative {}))
  ([degree derivative {::keys [second-kind?] :or {second-kind? false}}]
   (cond (zero? degree) (constantly 0.0)

         (m/one? derivative)
         (if second-kind?
           (let [y (inc degree)]
             #(if (m/one? (m/abs %))
                (* (/ 3)
                  (m/pow (m/sgn %) y)
                  (- (m/cube y) y))
                (/ (- (* y ((chebyshev-polynomial-fn y) %))
                     (* %
                       ((chebyshev-polynomial-fn degree {::second-kind? true})
                        %)))
                  (dec (m/sq %)))))
           #(* degree
              ((chebyshev-polynomial-fn (dec degree) {::second-kind? true}) %)))

         (and (= 2 derivative) (not second-kind?))
         #(if (m/one? (m/abs %))
            (* (/ 3)
              (m/pow (m/sgn %) degree)
              (- (m/pow degree 4) (m/sq degree)))
            (let [first-kind ((chebyshev-polynomial-fn degree) %)
                  second-kind ((chebyshev-polynomial-fn
                                 degree {::second-kind? true}) %)]
              (* degree
                (- (* (inc degree) first-kind) second-kind)
                (/ (dec (m/sq %))))))

         :else
         (derivatives/derivative-fn
           (chebyshev-polynomial-fn degree {::second-kind? second-kind?})
           {::derivatives/derivative derivative}))))

(s/fdef chebyshev-derivative-fn
  :args (s/cat :degree ::degree
          :derivative ::derivatives/derivative
          :opts (s/? (s/keys :opt [::second-kind?])))
  :ret ::number->number)

(defn chebyshev-poly-factors-to-regular-poly-factors
  "Converts Chebyshev polynomial coefficients to standard polynomial form.
  
  Transforms a representation as a linear combination of Chebyshev polynomials:
  b₀T₀(x) + b₁T₁(x) + b₂T₂(x) + ...
  
  Into standard polynomial form:
  a₀ + a₁x + a₂x² + ...
  
  This is useful for integration with systems that expect standard polynomials.
  
  Parameters:
    chebyshev-factors - Sequence of coefficients for Chebyshev expansion
  
  Options:
    ::second-kind? - If true, converts from U_n(x) basis instead of T_n(x)
  
  Returns coefficients for the equivalent standard polynomial.
  
  Example:
    (chebyshev-poly-factors-to-regular-poly-factors [1 2 3])
    ; => [0 2 6] representing 0 + 2x + 6x²"
  ([chebyshev-factors]
   (chebyshev-poly-factors-to-regular-poly-factors
     chebyshev-factors {}))
  ([chebyshev-factors {::keys [second-kind?] :or {second-kind? false}}]
   (let [n (count chebyshev-factors)]
     (map (fn [i]
            ((derivatives/derivative-fn
               #(vector/dot-product
                  (vec chebyshev-factors)
                  (vec ((polynomial-fn (dec n)
                          {::chebyshev-kind (if second-kind? 2 1)})
                        %)))
               {::derivatives/derivative i})
             0.0))
       (range n)))))

(s/fdef chebyshev-poly-factors-to-regular-poly-factors
  :args (s/cat :chebyshev-factors ::m/numbers
          :opts (s/? (s/keys :opt [::second-kind?])))
  :ret (s/coll-of ::m/number))

;;;POLYNOMIAL SERIES
(defn- polynomial-functions
  "Cheybshev-kind can be 0 (default), 1, or 2, where 0 means a regular
  polynomial."
  [chebyshev-kind]
  (condp = chebyshev-kind
    0 (fn [x]
        (fn [degree]
          (m/pow x degree)))
    1 (fn [x]
        (fn [degree]
          ((chebyshev-polynomial-fn degree) x)))
    2 (fn [x]
        (fn [degree]
          ((chebyshev-polynomial-fn degree {::second-kind? true}) x)))))

(s/fdef polynomial-functions
  :args (s/cat :chebyshev-kind ::chebyshev-kind)
  :ret (s/fspec :args (s/cat :number ::m/number)
         :ret (s/fspec :args (s/cat :degree ::degree)
                :ret ::m/number)))

(defn polynomial-fn
  "Creates a function that evaluates polynomial basis functions up to a degree.
  
  Generates a vector of polynomial values [P₀(x), P₁(x), ..., P_n(x)] where
  P_i(x) depends on the polynomial kind specified.
  
  Parameters:
    end-degree - Maximum degree of polynomials to include
  
  Options:
    ::start-degree - Minimum degree to include (default 0)
    ::chebyshev-kind - Polynomial type: 0=standard (x^n), 1=Chebyshev T_n(x), 2=Chebyshev U_n(x)
  
  Returns a function that takes a number x and returns a vector of polynomial evaluations.
  
  Example:
    ((polynomial-fn 3) 2.0) ; => [1.0 2.0 4.0 8.0] (standard: [1, x, x², x³])
    ((polynomial-fn 2 {::chebyshev-kind 1}) 0.5) ; => [1.0 0.5 -0.5] (Chebyshev T_n)"
  ([end-degree] (polynomial-fn end-degree {}))
  ([end-degree {::keys [start-degree chebyshev-kind]
                :or    {start-degree 0, chebyshev-kind 0}}]
   (fn [x] (mapv (fn [degree]
                   (((polynomial-functions chebyshev-kind) x) degree))
             (range start-degree (inc end-degree))))))

(s/fdef polynomial-fn
  :args (s/and (s/cat :end-degree ::end-degree
                 :opts (s/? (s/keys :opt [::start-degree ::chebyshev-kind])))
          (fn [{:keys [end-degree opts]}]
            (let [{::keys [start-degree]} opts]
              (or (not start-degree) (<= start-degree end-degree)))))
  :ret ::number->v)

(defn polynomial-fns
  "Creates a collection of individual polynomial basis functions.
  
  Similar to polynomial-fn but returns separate functions rather than
  a single function that returns a vector.
  
  Parameters:
    end-degree - Maximum degree of polynomials to include
  
  Options:
    ::start-degree - Minimum degree to include (default 0)
    ::chebyshev-kind - Polynomial type: 0=standard, 1=Chebyshev T_n, 2=Chebyshev U_n
  
  Returns a sequence of functions, each evaluating one polynomial basis function.
  
  Example:
    (map #(% 2.0) (polynomial-fns 2)) ; => (1.0 2.0 4.0)"
  ([end-degree] (polynomial-fns end-degree {}))
  ([end-degree {::keys [start-degree chebyshev-kind]
                :or    {start-degree 0, chebyshev-kind 0}}]
   (map (fn [degree]
          (fn [x]
            (((polynomial-functions chebyshev-kind) x) degree)))
     (range start-degree (inc end-degree)))))

(s/fdef polynomial-fns
  :args (s/and (s/cat :end-degree ::end-degree
                 :opts (s/? (s/keys :opt [::start-degree ::chebyshev-kind])))
          (fn [{:keys [end-degree opts]}]
            (let [{::keys [start-degree]} opts]
              (or (not start-degree) (<= start-degree end-degree)))))
  :ret (s/coll-of ::number->number))

(defn polynomial-2D-count
  "Calculates the number of 2D polynomial terms within degree bounds.
  
  For 2D polynomials up to degree n, counts terms like: 1, x, y, x², xy, y², ...
  The total count follows the formula: Σ(i+1) for i from start-degree to end-degree.
  
  Parameters:
    end-degree - Maximum total degree to include
  
  Options:
    ::start-degree - Minimum total degree to include (default 0)
  
  Returns the number of polynomial terms.
  
  Example:
    (polynomial-2D-count 2) ; => 6 (terms: 1, x, y, x², xy, y²)"
  ([end-degree] (polynomial-2D-count end-degree {}))
  ([end-degree {::keys [start-degree]
                :or    {start-degree 0}}]
   (let [d (inc end-degree)
         f (fn [degree]
             (* 0.5 (+ degree (m/sq degree))))]
     (long (- (f d) (f start-degree))))))

(s/fdef polynomial-2D-count
  :args (s/and (s/cat :end-degree ::end-degree
                 :opts (s/? (s/keys :opt [::start-degree])))
          (fn [{:keys [end-degree opts]}]
            (let [{::keys [start-degree]} opts]
              (or (not start-degree) (<= start-degree end-degree)))))
  :ret ::m/long-non-)

(defn- polynomial-2D-degrees
  ^double [count]
  (- (m/sqrt (+ 0.25 (* 2.0 count))) 1.5))

(defn polynomial-2D-fn-by-degree
  "Creates a 2D polynomial basis function organized by total degree.
  
  Generates all polynomial terms x^i × y^j where i+j ≤ end-degree.
  Terms are ordered by total degree, then by x-power (highest first):
  [1, x, y, x², xy, y², x³, x²y, xy², y³, ...]
  
  Parameters:
    end-degree - Maximum total degree (i+j) to include
  
  Options:
    ::start-degree - Minimum total degree to include (default 0)
    ::chebyshev-kind - Basis type: 0=standard, 1=Chebyshev T_n, 2=Chebyshev U_n
  
  Returns a function that takes (x, y) and returns a vector of polynomial evaluations.
  
  Example:
    ((polynomial-2D-fn-by-degree 2) 2.0 3.0) ; => [1.0 2.0 3.0 4.0 6.0 9.0]"
  ([end-degree] (polynomial-2D-fn-by-degree end-degree {}))
  ([end-degree {::keys [start-degree chebyshev-kind]
                :or    {start-degree 0, chebyshev-kind 0}}]
   (let [p (polynomial-functions chebyshev-kind)]
     (fn [x y]
       (let [fx (p x)
             fy (p y)]
         (loop [arr []
                i start-degree]
           (if (> i end-degree)
             arr
             (recur (reduce (fn [tot e]
                              (conj tot (* (fx e) (fy (- i e)))))
                      arr
                      (range (inc i)))
               (inc i)))))))))

(s/fdef polynomial-2D-fn-by-degree
  :args (s/and (s/cat :end-degree ::end-degree
                 :opts (s/? (s/keys :opt [::start-degree ::chebyshev-kind])))
          (fn [{:keys [end-degree opts]}]
            (let [{::keys [start-degree]} opts]
              (or (not start-degree) (<= start-degree end-degree)))))
  :ret ::number2->v)

(defn polynomial-2D-fn-by-basis-count
  "Creates a 2D polynomial basis function with a specified number of terms.
  
  Similar to polynomial-2D-fn-by-degree but determines the degree based on
  the desired number of basis functions rather than a maximum degree.
  
  Parameters:
    basis-count - Exact number of polynomial terms to include
  
  Options:
    ::start-degree - Minimum total degree to include (default 0)
    ::chebyshev-kind - Basis type: 0=standard, 1=Chebyshev T_n, 2=Chebyshev U_n
  
  Returns a function that takes (x, y) and returns a vector with exactly
  basis-count polynomial evaluations.
  
  Example:
    ((polynomial-2D-fn-by-basis-count 4) 2.0 3.0) ; => [1.0 2.0 3.0 4.0]"
  ([basis-count] (polynomial-2D-fn-by-basis-count basis-count {}))
  ([basis-count {::keys [start-degree chebyshev-kind]
                 :or    {start-degree 0, chebyshev-kind 0}}]
   (let [end-degree (m/ceil' (polynomial-2D-degrees basis-count))]
     (fn [x y]
       (if (> start-degree end-degree)
         []
         (vec (take
                basis-count
                ((polynomial-2D-fn-by-degree
                   end-degree
                   {::start-degree   start-degree
                    ::chebyshev-kind chebyshev-kind})
                 x
                 y))))))))

(s/fdef polynomial-2D-fn-by-basis-count
  :args (s/cat :basis-count ::basis-count
          :opts (s/? (s/keys :opt [::start-degree ::chebyshev-kind])))
  :ret ::number2->v)

(defn polynomial-ND-fn
  "Creates an N-dimensional polynomial basis function.
  
  Generates all polynomial terms up to the specified degree for an arbitrary
  number of variables. Terms are ordered by total degree, then lexicographically
  by dimension with a small perturbation to ensure consistent ordering.
  
  For variables [x, y, z, ...], generates terms like:
  [1, x, y, z, x², xy, xz, y², yz, z², x³, x²y, x²z, xy², xyz, xz², y³, ...]
  
  Parameters:
    end-degree - Maximum total degree for polynomial terms
  
  Options:
    ::chebyshev-kind - Basis type: 0=standard, 1=Chebyshev T_n, 2=Chebyshev U_n
  
  Returns a function that takes a vector [x y z ...] and returns a vector
  of polynomial evaluations.
  
  Example:
    ((polynomial-ND-fn 2) [2.0 3.0 4.0]) ; => all terms up to degree 2"
  ([end-degree] (polynomial-ND-fn end-degree {}))
  ([end-degree {::keys [chebyshev-kind] :or {chebyshev-kind 0}}]
   (let [p (polynomial-functions chebyshev-kind)]
     (fn [v]
       (let [fv (mapv p v)]
         (mapv (fn [degrees]
                 (reduce-kv (fn [tot index degree]
                              (* tot ((get fv index m/nan) degree)))
                   1.0
                   (vec degrees)))
           (sort-by
             (fn [degrees]
               (reduce-kv
                 (fn [[tot1 tot2] index degree]
                   [(+ tot1 degree)
                    (+ tot2 (* degree (inc (- (m/pow 0.01 (+ 2 index))))))])
                 [0.0 0.0]
                 (vec degrees)))
             (apply combinatorics/cartesian-product
               (repeat (count v) (range (inc end-degree)))))))))))

(s/fdef polynomial-ND-fn
  :args (s/cat :end-degree ::end-degree
          :opts (s/? (s/keys :opt [::chebyshev-kind])))
  :ret ::v->v)

(defn polynomial-ND-fn-without-cross-terms
  "Creates an N-dimensional polynomial basis with no interaction terms.
  
  Generates only pure power terms (no cross-products) for each variable:
  [1, x, y, z, x², y², z², x³, y³, z³, ...]
  
  This is useful for additive models where variables don't interact.
  
  Parameters:
    end-degree - Maximum degree for individual variable terms
  
  Options:
    ::chebyshev-kind - Basis type: 0=standard, 1=Chebyshev T_n, 2=Chebyshev U_n
  
  Returns a function that takes a vector [x y z ...] and returns a vector
  of polynomial evaluations without cross-terms.
  
  Example:
    ((polynomial-ND-fn-without-cross-terms 2) [2.0 3.0 4.0])
    ; => [1.0 2.0 3.0 4.0 4.0 9.0 16.0] (no xy, xz, yz terms)"
  ([end-degree] (polynomial-ND-fn-without-cross-terms end-degree {}))
  ([end-degree {::keys [chebyshev-kind] :or {chebyshev-kind 0}}]
   (fn [v]
     (if (zero? end-degree)
       [1.0]
       (vec (cons 1.0
              (apply interleave
                (map
                  (polynomial-fn end-degree {::start-degree   1
                                             ::chebyshev-kind chebyshev-kind})
                  v))))))))

(s/fdef polynomial-ND-fn-without-cross-terms
  :args (s/cat :end-degree ::end-degree
          :opts (s/? (s/keys :opt [::chebyshev-kind])))
  :ret ::v->v)
