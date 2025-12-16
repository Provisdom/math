(ns provisdom.math.polynomials
  "Polynomial evaluation, orthogonal polynomials, and interpolation.

  Features:
  - Efficient polynomial evaluation (Horner's method, Clenshaw algorithm)
  - Polynomial arithmetic (add, subtract, multiply, divide, scale)
  - Chebyshev polynomials (T_n and U_n) with derivatives
  - Bidirectional Chebyshev ↔ standard polynomial coefficient conversion
  - Chebyshev nodes and extrema for optimal interpolation
  - Legendre, Hermite, and Laguerre orthogonal polynomials
  - Lagrange and Newton polynomial interpolation
  - Multi-dimensional polynomial basis functions (1D, 2D, N-D)

  Useful for function approximation, spectral methods, numerical integration,
  Gaussian quadrature, and regression."
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

(s/def ::coefficients
  (s/with-gen (s/coll-of ::m/number :kind vector? :min-count 1)
    #(s/gen (s/coll-of (s/double-in :min -10.0 :max 10.0 :NaN? false :infinite? false)
              :kind vector? :min-count 1 :max-count 5))))

(s/def ::points
  (s/with-gen (s/coll-of (s/tuple ::m/finite ::m/finite) :min-count 1)
    #(s/gen (s/coll-of (s/tuple (s/double-in :min -10.0 :max 10.0 :NaN? false :infinite? false)
                         (s/double-in :min -10.0 :max 10.0 :NaN? false :infinite? false))
              :min-count 1 :max-count 5))))

(s/def ::node-count
  (s/with-gen (s/int-in 1 1000)
    #(s/gen (s/int-in 1 10))))

(s/def ::physicist? boolean?)

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

;;;POLYNOMIAL EVALUATION
(defn horner-eval
  "Evaluates a polynomial at x using Horner's method.

  Horner's method is numerically stable and efficient, requiring only n
  multiplications and n additions for a degree-n polynomial.

  Coefficients are ordered [a₀ a₁ a₂ ...] representing:
  a₀ + a₁x + a₂x² + ...

  Parameters:
    coefficients - Vector of polynomial coefficients [a₀ a₁ a₂ ...]
    x - Point at which to evaluate the polynomial

  Returns the polynomial value at x.

  Example:
    (horner-eval [1 2 3] 2.0) ; => 1 + 2*2 + 3*4 = 17.0"
  [coefficients x]
  (reduce (fn [acc coef]
            (+ coef (* acc x)))
    0.0
    (rseq (vec coefficients))))

(s/fdef horner-eval
  :args (s/cat :coefficients ::coefficients :x ::m/number)
  :ret ::m/number)

(defn- clenshaw-recurrence
  "Helper for Clenshaw algorithm recurrence."
  [coeffs x multiplier]
  (let [n (dec (count coeffs))]
    (loop [k n, b1 0.0, b2 0.0]
      (if (zero? k)
        [b1 b2]
        (recur (dec k) (+ (get coeffs k) (* multiplier x b1) (- b2)) b1)))))

(defn clenshaw-eval
  "Evaluates a Chebyshev series at x using the Clenshaw algorithm.

  Given coefficients [c₀ c₁ c₂ ...] for the expansion:
  c₀T₀(x) + c₁T₁(x) + c₂T₂(x) + ...

  Uses the recurrence relation for efficient and stable evaluation.

  Parameters:
    coefficients - Vector of Chebyshev coefficients [c₀ c₁ c₂ ...]
    x - Point at which to evaluate (typically in [-1, 1])

  Options:
    ::second-kind? - If true, evaluates sum of U_n(x) instead of T_n(x)

  Returns the value of the Chebyshev series at x.

  Example:
    (clenshaw-eval [1 2 3] 0.5) ; => T₀(0.5) + 2T₁(0.5) + 3T₂(0.5)"
  ([coefficients x] (clenshaw-eval coefficients x {}))
  ([coefficients x {::keys [second-kind?] :or {second-kind? false}}]
   (let [coeffs (vec coefficients)
         n (dec (count coeffs))]
     (if (neg? n)
       0.0
       (let [[b1 b2] (clenshaw-recurrence coeffs x 2.0)]
         (if second-kind?
           (+ (get coeffs 0) (* 2.0 x b1) (- b2))
           (+ (get coeffs 0) (* x b1) (- b2))))))))

(s/fdef clenshaw-eval
  :args (s/cat :coefficients ::coefficients
          :x ::m/number
          :opts (s/? (s/keys :opt [::second-kind?])))
  :ret ::m/number)

;;;POLYNOMIAL ARITHMETIC
(defn poly-add
  "Adds two polynomials represented as coefficient vectors.

  Coefficients are ordered [a₀ a₁ a₂ ...] representing a₀ + a₁x + a₂x² + ...

  Parameters:
    coeffs1 - First polynomial coefficients
    coeffs2 - Second polynomial coefficients

  Returns coefficient vector of the sum polynomial.

  Example:
    (poly-add [1 2 3] [4 5]) ; => [5.0 7.0 3.0]"
  [coeffs1 coeffs2]
  (let [c1 (vec coeffs1)
        c2 (vec coeffs2)
        n (max (count c1) (count c2))]
    (mapv (fn [i]
            (+ (double (get c1 i 0.0)) (double (get c2 i 0.0))))
      (range n))))

(s/fdef poly-add
  :args (s/cat :coeffs1 ::coefficients :coeffs2 ::coefficients)
  :ret ::coefficients)

(defn poly-subtract
  "Subtracts two polynomials represented as coefficient vectors.

  Parameters:
    coeffs1 - First polynomial coefficients (minuend)
    coeffs2 - Second polynomial coefficients (subtrahend)

  Returns coefficient vector of coeffs1 - coeffs2.

  Example:
    (poly-subtract [5 7 3] [4 5]) ; => [1.0 2.0 3.0]"
  [coeffs1 coeffs2]
  (let [c1 (vec coeffs1)
        c2 (vec coeffs2)
        n (max (count c1) (count c2))]
    (mapv (fn [i]
            (- (double (get c1 i 0.0)) (double (get c2 i 0.0))))
      (range n))))

(s/fdef poly-subtract
  :args (s/cat :coeffs1 ::coefficients :coeffs2 ::coefficients)
  :ret ::coefficients)

(defn poly-multiply
  "Multiplies two polynomials represented as coefficient vectors.

  Uses convolution of coefficient vectors.

  Parameters:
    coeffs1 - First polynomial coefficients
    coeffs2 - Second polynomial coefficients

  Returns coefficient vector of the product polynomial.

  Example:
    (poly-multiply [1 1] [1 1]) ; => [1.0 2.0 1.0] ((1+x)² = 1+2x+x²)"
  [coeffs1 coeffs2]
  (let [c1 (vec coeffs1)
        c2 (vec coeffs2)
        n1 (count c1)
        n2 (count c2)]
    (if (or (zero? n1) (zero? n2))
      [0.0]
      (let [result-len (dec (+ n1 n2))]
        (mapv (fn [k]
                (reduce (fn [sum i]
                          (let [j (- k i)]
                            (if (and (>= j 0) (< j n2))
                              (+ sum (* (double (get c1 i 0.0)) (double (get c2 j 0.0))))
                              sum)))
                  0.0
                  (range n1)))
          (range result-len))))))

(s/fdef poly-multiply
  :args (s/cat :coeffs1 ::coefficients :coeffs2 ::coefficients)
  :ret ::coefficients)

(defn poly-scale
  "Scales a polynomial by a constant.

  Parameters:
    coeffs - Polynomial coefficients
    scalar - Scalar multiplier

  Returns scaled coefficient vector.

  Example:
    (poly-scale [1 2 3] 2.0) ; => [2.0 4.0 6.0]"
  [coeffs scalar]
  (mapv #(* (double scalar) (double %)) coeffs))

(s/fdef poly-scale
  :args (s/cat :coeffs ::coefficients :scalar ::m/number)
  :ret ::coefficients)

(defn poly-divide
  "Divides two polynomials using polynomial long division.

  Returns a map with :quotient and :remainder coefficient vectors.

  Parameters:
    dividend - Dividend polynomial coefficients
    divisor - Divisor polynomial coefficients

  Returns {:quotient [...] :remainder [...]}

  Example:
    (poly-divide [1 0 1] [1 1])
    ; => {:quotient [-1.0 1.0] :remainder [2.0]} (x²+1)/(x+1) = x-1 + 2/(x+1)"
  [dividend divisor]
  (let [roughly-zero? (fn [x] (< (m/abs x) 1e-14))
        dividend (mapv double dividend)
        divisor (mapv double divisor)
        ;; Remove trailing zeros from divisor
        divisor (loop [d divisor]
                  (if (and (> (count d) 1) (roughly-zero? (peek d)))
                    (recur (pop d))
                    d))
        deg-dividend (dec (count dividend))
        deg-divisor (dec (count divisor))]
    (if (or (empty? divisor) (and (= 1 (count divisor)) (roughly-zero? (first divisor))))
      {:quotient [m/inf+] :remainder [0.0]}
      (if (< deg-dividend deg-divisor)
        {:quotient [0.0] :remainder dividend}
        (loop [remainder dividend
               quotient (vec (repeat (inc (- deg-dividend deg-divisor)) 0.0))]
          (let [deg-rem (loop [d (dec (count remainder))]
                          (if (and (>= d 0) (roughly-zero? (get remainder d)))
                            (recur (dec d))
                            d))]
            (if (or (neg? deg-rem) (< deg-rem deg-divisor))
              {:quotient quotient
               :remainder (if (neg? deg-rem)
                            [0.0]
                            (mapv double (take (inc deg-rem) remainder)))}
              (let [coef (/ (double (get remainder deg-rem))
                           (double (get divisor deg-divisor)))
                    pos (- deg-rem deg-divisor)
                    new-quotient (assoc quotient pos coef)
                    shift-divisor (vec (concat (repeat pos 0.0)
                                         (map #(* coef (double %)) divisor)))
                    new-remainder (mapv (fn [i]
                                          (- (double (get remainder i 0.0))
                                            (double (get shift-divisor i 0.0))))
                                    (range (count remainder)))]
                (recur new-remainder new-quotient)))))))))

(s/fdef poly-divide
  :args (s/cat :dividend ::coefficients :divisor ::coefficients)
  :ret (s/keys :req-un [::quotient ::remainder]))

;;;CHEBYSHEV UTILITIES
(defn chebyshev-nodes
  "Returns the Chebyshev nodes (zeros of T_n) for polynomial interpolation.

  These are the optimal interpolation points on [-1, 1] that minimize
  the Runge phenomenon.

  Parameters:
    n - Number of nodes to generate

  Returns a vector of n Chebyshev node locations in [-1, 1].

  Example:
    (chebyshev-nodes 3) ; => [0.866... 0.0 -0.866...]"
  [n]
  (mapv (fn [k]
          (m/cos (* m/PI (/ (- (* 2.0 (inc k)) 1.0)
                          (* 2.0 n)))))
    (range n)))

(s/fdef chebyshev-nodes
  :args (s/cat :n ::node-count)
  :ret ::vector/vector)

(defn chebyshev-extrema
  "Returns the Chebyshev extrema (including endpoints) for interpolation.

  These are the points where T_n reaches its extreme values ±1.

  Parameters:
    n - Number of extrema points (n+1 points from 0 to n)

  Returns a vector of n Chebyshev extrema locations in [-1, 1].

  Example:
    (chebyshev-extrema 3) ; => [1.0 0.5 -0.5 -1.0]"
  [n]
  (mapv (fn [k]
          (m/cos (/ (* m/PI k) (dec n))))
    (range n)))

(s/fdef chebyshev-extrema
  :args (s/cat :n (s/and ::node-count #(>= % 2)))
  :ret ::vector/vector)

(defn regular-poly-factors-to-chebyshev-poly-factors
  "Converts standard polynomial coefficients to Chebyshev form.

  Transforms a standard polynomial:
  a₀ + a₁x + a₂x² + ...

  Into Chebyshev form:
  c₀T₀(x) + c₁T₁(x) + c₂T₂(x) + ...

  Uses the relationship between powers of x and Chebyshev polynomials.

  Parameters:
    regular-factors - Standard polynomial coefficients [a₀ a₁ a₂ ...]

  Returns Chebyshev coefficients [c₀ c₁ c₂ ...].

  Example:
    (regular-poly-factors-to-chebyshev-poly-factors [1 0 1])
    ; => [1.5 0.0 0.5] since x² = (T₂(x) + T₀(x))/2"
  [regular-factors]
  (let [n (count regular-factors)
        ;; Coefficients for x^k in terms of Chebyshev polynomials
        ;; x^0 = T_0
        ;; x^1 = T_1
        ;; x^2 = (T_2 + T_0)/2
        ;; x^3 = (T_3 + 3T_1)/4
        ;; etc. Using recurrence: x*T_n = (T_{n+1} + T_{n-1})/2
        power-to-chebyshev (fn [k]
                             (if (zero? k)
                               [1.0]
                               (loop [coeffs [0.0 1.0]  ; Start with T_1 = x
                                      power 1]
                                 (if (= power k)
                                   coeffs
                                   ;; Multiply by x: x*sum(c_i*T_i) = sum(c_i*(T_{i+1}+T_{i-1})/2)
                                   (let [old-len (count coeffs)
                                         new-len (inc old-len)
                                         new-coeffs (vec (repeat new-len 0.0))]
                                     (recur
                                       (reduce (fn [acc i]
                                                 (let [c (get coeffs i 0.0)]
                                                   (-> acc
                                                     (update (inc i) + (* 0.5 c))
                                                     (update (long (m/abs (dec i))) + (* 0.5 c)))))
                                         new-coeffs
                                         (range old-len))
                                       (inc power)))))))]
    (reduce (fn [result k]
              (let [chebyshev-coeffs (power-to-chebyshev k)
                    scaled (mapv #(* (get regular-factors k) %) chebyshev-coeffs)]
                (poly-add result scaled)))
      [0.0]
      (range n))))

(s/fdef regular-poly-factors-to-chebyshev-poly-factors
  :args (s/cat :regular-factors ::m/numbers)
  :ret (s/coll-of ::m/number))

;;;ORTHOGONAL POLYNOMIALS
(defn legendre-polynomial-fn
  "Creates a Legendre polynomial function of the specified degree.

  Legendre polynomials P_n(x) are orthogonal on [-1, 1] with weight 1.
  They satisfy: ∫₋₁¹ P_m(x)P_n(x)dx = 2/(2n+1) δ_{mn}

  Used in:
  - Gaussian quadrature
  - Spherical harmonics
  - Solving Laplace's equation

  Parameters:
    degree - Non-negative integer degree of the polynomial

  Returns a function that evaluates P_n(x).

  Example:
    ((legendre-polynomial-fn 2) 0.5) ; => P₂(0.5) = -0.125"
  [degree]
  (let [degree (long degree)]
    (cond
      (zero? degree) (constantly 1.0)
      (= 1 degree) (fn [x] (* 1.0 x))
      :else
      (fn [x]
        (loop [p0 1.0
               p1 x
               n 1]
          (if (= n degree)
            p1
            (let [p2 (/ (- (* (inc (* 2 n)) x p1) (* n p0))
                       (inc n))]
              (recur p1 p2 (inc n)))))))))

(s/fdef legendre-polynomial-fn
  :args (s/cat :degree ::degree)
  :ret ::number->number)

(defn hermite-polynomial-fn
  "Creates a Hermite polynomial function of the specified degree.

  Two conventions exist:
  - Physicist's (default): H_n(x), orthogonal on ℝ with weight e^(-x²)
  - Probabilist's: He_n(x), orthogonal on ℝ with weight e^(-x²/2)

  Used in quantum mechanics (harmonic oscillator) and probability theory.

  Parameters:
    degree - Non-negative integer degree of the polynomial

  Options:
    ::physicist? - If true (default), uses physicist's convention H_n
                   If false, uses probabilist's convention He_n

  Returns a function that evaluates the Hermite polynomial.

  Example:
    ((hermite-polynomial-fn 2) 1.0) ; => H₂(1) = 2
    ((hermite-polynomial-fn 2 {::physicist? false}) 1.0) ; => He₂(1) = 0"
  ([degree] (hermite-polynomial-fn degree {}))
  ([degree {::keys [physicist?] :or {physicist? true}}]
   (let [degree (long degree)]
     (cond
       (zero? degree) (constantly 1.0)
       (= 1 degree) (if physicist?
                      (fn [x] (* 2.0 x))
                      (fn [x] (* 1.0 x)))
       :else
       (if physicist?
         ;; H_{n+1}(x) = 2xH_n(x) - 2nH_{n-1}(x)
         (fn [x]
           (loop [h0 1.0
                  h1 (* 2.0 x)
                  n 1]
             (if (= n degree)
               h1
               (let [h2 (- (* 2.0 x h1) (* 2.0 n h0))]
                 (recur h1 h2 (inc n))))))
         ;; He_{n+1}(x) = xHe_n(x) - nHe_{n-1}(x)
         (fn [x]
           (loop [h0 1.0
                  h1 x
                  n 1]
             (if (= n degree)
               h1
               (let [h2 (- (* x h1) (* n h0))]
                 (recur h1 h2 (inc n)))))))))))

(s/fdef hermite-polynomial-fn
  :args (s/cat :degree ::degree
          :opts (s/? (s/keys :opt [::physicist?])))
  :ret ::number->number)

(defn laguerre-polynomial-fn
  "Creates a Laguerre polynomial function of the specified degree.

  Laguerre polynomials L_n(x) are orthogonal on [0, ∞) with weight e^(-x).
  They satisfy: ∫₀^∞ L_m(x)L_n(x)e^(-x)dx = δ_{mn}

  Used in:
  - Quantum mechanics (hydrogen atom radial functions)
  - Solving differential equations with exponential decay

  Parameters:
    degree - Non-negative integer degree of the polynomial

  Returns a function that evaluates L_n(x).

  Example:
    ((laguerre-polynomial-fn 2) 1.0) ; => L₂(1) = 0.5"
  [degree]
  (let [degree (long degree)]
    (cond
      (zero? degree) (constantly 1.0)
      (= 1 degree) (fn [x] (- 1.0 x))
      :else
      ;; L_{n+1}(x) = ((2n+1-x)L_n(x) - nL_{n-1}(x)) / (n+1)
      (fn [x]
        (loop [l0 1.0
               l1 (- 1.0 x)
               n 1]
          (if (= n degree)
            l1
            (let [l2 (/ (- (* (- (inc (* 2 n)) x) l1) (* n l0))
                       (inc n))]
              (recur l1 l2 (inc n)))))))))

(s/fdef laguerre-polynomial-fn
  :args (s/cat :degree ::degree)
  :ret ::number->number)

;;;INTERPOLATION
(defn lagrange-interpolation-fn
  "Creates a Lagrange interpolating polynomial for given data points.

  Given n points (x₀,y₀), (x₁,y₁), ..., constructs the unique polynomial
  of degree at most n-1 passing through all points.

  Parameters:
    points - Sequence of [x y] coordinate pairs

  Returns a function that evaluates the interpolating polynomial at any x.

  Note: For many points or points outside the data range, consider using
  Chebyshev interpolation for better numerical stability.

  Example:
    (def f (lagrange-interpolation-fn [[0 1] [1 2] [2 5]]))
    (f 1.5) ; => 3.25"
  [points]
  (let [points (vec points)
        xs (mapv first points)
        ys (mapv second points)]
    (fn [x]
      (reduce-kv
        (fn [sum i yi]
          (let [xi (get xs i)
                li (reduce-kv
                     (fn [prod j xj]
                       (if (= i j)
                         prod
                         (* prod (/ (- x xj) (- xi xj)))))
                     1.0
                     xs)]
            (+ sum (* yi li))))
        0.0
        ys))))

(s/fdef lagrange-interpolation-fn
  :args (s/cat :points ::points)
  :ret ::number->number)

(defn lagrange-interpolation-coefficients
  "Returns the coefficients of the Lagrange interpolating polynomial.

  Given n points, computes the standard polynomial coefficients [a₀ a₁ ... a_{n-1}]
  such that p(x) = a₀ + a₁x + ... + a_{n-1}x^{n-1} passes through all points.

  Parameters:
    points - Sequence of [x y] coordinate pairs

  Returns coefficient vector [a₀ a₁ a₂ ...].

  Example:
    (lagrange-interpolation-coefficients [[0 1] [1 2] [2 5]])
    ; => [1.0 0.0 1.0] representing 1 + x²"
  [points]
  (let [points (vec points)
        n (count points)
        xs (mapv first points)
        ys (mapv second points)]
    (reduce-kv
      (fn [result i yi]
        (let [xi (get xs i)
              ;; Build the basis polynomial L_i(x) = prod_{j≠i} (x - xj)/(xi - xj)
              ;; Start with [1] and multiply by (x - xj)/(xi - xj) = [-xj/(xi-xj), 1/(xi-xj)]
              basis (reduce-kv
                      (fn [coeffs j xj]
                        (if (= i j)
                          coeffs
                          (let [denom (- xi xj)
                                factor [(/ (- xj) denom) (/ 1.0 denom)]]
                            (poly-multiply coeffs factor))))
                      [1.0]
                      xs)]
          (poly-add result (poly-scale basis yi))))
      (vec (repeat n 0.0))
      ys)))

(s/fdef lagrange-interpolation-coefficients
  :args (s/cat :points ::points)
  :ret ::coefficients)

(defn newton-interpolation-coefficients
  "Computes Newton's divided difference interpolation coefficients.

  Returns coefficients [c₀ c₁ c₂ ...] for Newton's form:
  p(x) = c₀ + c₁(x-x₀) + c₂(x-x₀)(x-x₁) + ...

  This form is more efficient for adding new points incrementally.

  Parameters:
    points - Sequence of [x y] coordinate pairs

  Returns a map with:
    :coefficients - Newton form coefficients
    :xs - The x-values for computing the polynomial

  Example:
    (newton-interpolation-coefficients [[0 1] [1 2] [2 5]])"
  [points]
  (let [points (vec points)
        n (count points)
        xs (mapv first points)
        ys (mapv second points)
        ;; Build divided difference table
        table (loop [table [ys]
                     k 1]
                (if (= k n)
                  table
                  (let [prev (peek table)
                        next-col (mapv (fn [i]
                                         (/ (- (get prev (inc i)) (get prev i))
                                           (- (get xs (+ i k)) (get xs i))))
                                   (range (- n k)))]
                    (recur (conj table next-col) (inc k)))))]
    {:coefficients (mapv first table)
     :xs xs}))

(s/fdef newton-interpolation-coefficients
  :args (s/cat :points ::points)
  :ret (s/keys :req-un [::coefficients ::xs]))

(defn newton-interpolation-fn
  "Creates a Newton interpolating polynomial for given data points.

  Uses Newton's divided differences for efficient evaluation.

  Parameters:
    points - Sequence of [x y] coordinate pairs

  Returns a function that evaluates the interpolating polynomial at any x.

  Example:
    (def f (newton-interpolation-fn [[0 1] [1 2] [2 5]]))
    (f 1.5) ; => 3.25"
  [points]
  (let [{:keys [coefficients xs]} (newton-interpolation-coefficients points)
        n (count coefficients)]
    (fn [x]
      ;; Horner-like evaluation: p(x) = c₀ + (x-x₀)(c₁ + (x-x₁)(c₂ + ...))
      (loop [k (dec n)
             result 0.0]
        (if (neg? k)
          result
          (recur (dec k)
            (+ (get coefficients k)
              (* result (- x (get xs k))))))))))

(s/fdef newton-interpolation-fn
  :args (s/cat :points ::points)
  :ret ::number->number)
