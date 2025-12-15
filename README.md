# provisdom.math

A comprehensive Clojure mathematics library providing numerical computing primitives with robust handling of edge cases, IEEE 754 compliance, and spec-driven development.

## Features

### Core (`provisdom.math.core`)
Foundational mathematical functions and constants:
- Mathematical constants (PI, E, sqrt-two, log-two-pi, etc.)
- Enhanced numeric type predicates that handle NaN/infinity correctly
- Robust arithmetic with division-by-zero handling
- High-precision rounding and comparison functions
- Angle conversion utilities (radians/degrees)
- Functions with apostrophe variants (e.g., `floor'`) that return longs when possible

### Random (`provisdom.math.random`)
Splittable random number generation for reproducible parallel computation:
- Immutable RNG with `rng`, `rnd`, `rnd-long`, `rnd-normal`, `rnd-uuid`
- Bound RNG via `bind-seed` macro for scoped randomness
- Lazy infinite sequences of random values
- Clock-seeded generators for non-reproducible randomness

### Tensor (`provisdom.math.tensor`)
N-dimensional array operations:
- Tensor creation, validation, and manipulation
- Element-wise operations with broadcasting (`emap`, `emap-kv`)
- Tensor arithmetic (add, subtract, multiply, divide)
- Norms (L1, L2, Lp) and normalization
- Inner products and numerical stability utilities

### Vector (`provisdom.math.vector`)
Specialized 1D tensor operations:
- Dot products, cross products, projections
- Kahan summation for improved numerical accuracy
- Probability vector validation and operations
- Vector filtering and manipulation

### Matrix (`provisdom.math.matrix`)
Comprehensive matrix operations:
- Matrix creation (identity, diagonal, Toeplitz, random)
- Specialized types (symmetric, triangular, sparse, positive-definite)
- Matrix multiplication, transpose, Kronecker product
- Slicing, filtering, and partitioning
- Row/column manipulation (insert, remove, update)
- Serialization/deserialization of triangular matrices
- Matrix trace, outer product, and element-wise operations
- Matrix rounding utilities

### Linear Algebra (`provisdom.math.linear-algebra`)
Matrix decompositions and linear system solving:
- LU decomposition with partial pivoting
- Cholesky decomposition for positive-definite matrices
- QR decomposition using Householder reflections
- Eigendecomposition for symmetric matrices
- Singular Value Decomposition (SVD)
- Linear system solving (exact and least squares)
- Matrix inverse and Moore-Penrose pseudoinverse
- Determinant, condition number, and matrix rank
- Minors, cofactors, and adjugate matrices
- Matrix power (including negative powers via inverse)
- Matrix exponential (e^M) using Padé approximation
- Induced matrix norms (1-norm, infinity-norm, spectral norm)
- Positive definite/semi-definite utilities
- Correlation/covariance matrix conversions

### Special Functions (`provisdom.math.special-functions`)
Advanced mathematical functions:
- Gamma functions (gamma, log-gamma, digamma, trigamma)
- Incomplete gamma functions (regularized P and Q)
- Beta functions (beta, log-beta, regularized incomplete beta)
- Error functions (erf, erfc, inverse erf)
- Sigmoid functions (logistic, logit, probit)
- Log-sum-exp for numerical stability

### Combinatorics (`provisdom.math.combinatorics`)
Combinatorial computations:
- Factorials and subfactorials (derangements)
- Combinations and permutations
- Stirling numbers and Bell numbers
- Log-space calculations to avoid overflow

### Series (`provisdom.math.series`)
Infinite series summation:
- Adaptive convergence testing
- Power series evaluation
- Continued fractions
- Kahan summation integration

### Polynomials (`provisdom.math.polynomials`)
Polynomial operations:
- Polynomial evaluation (Horner's method)
- Chebyshev polynomials (first and second kind)
- Legendre and other orthogonal polynomials

### Derivatives (`provisdom.math.derivatives`)
Numerical differentiation:
- Scalar derivatives (1st through 8th order)
- Gradients, Jacobians, and Hessians
- Central, forward, and backward difference schemes
- Configurable accuracy levels

### Integrals (`provisdom.math.integrals`)
Numerical integration:
- Adaptive Gauss-Kronrod quadrature
- Single and multi-variable integration
- Finite and infinite interval support
- Parallel processing support

### Intervals (`provisdom.math.intervals`)
Interval arithmetic and bounds manipulation:
- **Intervals**: Simple `[lower, upper]` vectors with inclusive endpoints
- **Bounds**: Maps with `::lower`, `::upper`, `::open-lower?`, `::open-upper?` keys for flexible endpoint handling
- Interval operations: `in-interval?`, `bound-by-interval`, `interval-width`, `interval-midpoint`
- Bounds operations: `intersection`, `union`, `encompassing-bounds`, `overlaps?`, `contains-bounds?`
- Bounds utilities: `bound-by-bounds`, `bounds-width`, `bounds-midpoint`
- Predefined bounds: `bounds-prob` [0,1], `bounds-open-prob` (0,1), `bounds+` (0,∞], `bounds-finite` (-∞,+∞)
- Specialized bounds for optimization constraints and positive-definite matrices

### Arrays (`provisdom.math.arrays`)
Java primitive array operations for performance-critical code.

### Format (`provisdom.math.format`)
Number formatting utilities.

## Usage

```clojure
(require '[provisdom.math.core :as m])
(require '[provisdom.math.random :as random])
(require '[provisdom.math.matrix :as mx])
(require '[provisdom.math.linear-algebra :as la])
(require '[provisdom.math.special-functions :as special-fns])

;; Core math
(m/sqrt 2)           ;=> 1.4142135623730951
(m/roughly? 1.0 1.0000001 1e-6)  ;=> true
(m/div 1 0)          ;=> ##Inf (no exception)

;; Random generation with reproducibility
(random/bind-seed 42
  (random/rnd!))     ;=> deterministic random double

;; Matrix operations
(mx/mx* [[1 2] [3 4]] [[5 6] [7 8]])  ;=> [[19 22] [43 50]]
(mx/transpose [[1 2] [3 4]])          ;=> [[1 3] [2 4]]

;; Linear algebra
(la/determinant [[1 2] [3 4]])        ;=> -2.0
(la/inverse [[4 7] [2 6]])            ;=> [[0.6 -0.7] [-0.2 0.4]]
(la/solve [[2 1] [1 3]] [4 5])        ;=> [1.4 1.2]
(la/matrix-power [[1 2] [3 4]] 3)     ;=> [[37 54] [81 118]]

;; Special functions
(special-fns/gamma 5)      ;=> 24.0 (4!)
(special-fns/erf 1.0)      ;=> 0.8427007929497149
(special-fns/logistic 0)   ;=> 0.5
```

## Design Philosophy

- **Doubles by default**: Functions return doubles unless marked with `'` (apostrophe), which attempts to return longs when possible
- **NaN/Infinity aware**: Type predicates and comparisons handle IEEE 754 special values correctly
- **Spec-driven**: All functions have `clojure.spec.alpha` specs with generators for property-based testing
- **Anomalies over exceptions**: Prefer returning anomaly values rather than throwing exceptions
- **Immutable RNG**: Random number generators are immutable and splittable for parallel computation

## License

Copyright 2018 Provisdom

Distributed under the GNU Lesser General Public License version 3.0.
