# provisdom.math

A comprehensive mathematics library for Clojure, providing extensive mathematical functionality with strong support for spec and generative testing.

## Overview

provisdom.math provides a wide range of mathematical functions and utilities for Clojure programs, including:

- Basic arithmetic and mathematical operations with consistent NaN handling
- Trigonometric functions
- Number type checking and conversion
- Vector and matrix operations
- Random number generation
- Statistical distributions
- Special mathematical functions
- Polynomial operations
- Series and interval calculations
- Tensor operations
- Integration and derivative functions

The library is designed with a focus on precision, correctness, and comprehensive spec coverage, making it suitable for scientific computing, data analysis, and other applications requiring robust mathematical capabilities.

## Installation

### deps.edn

```clojure
{:deps {provisdom/math {:git/url "https://github.com/Provisdom/math.git"
                        :sha "469dcb7"}}}
```

## Usage

### Core Mathematical Functions

```clojure
(require '[provisdom.math.core :as m])

;; Constants
m/PI          ; 3.141592653589793
m/E           ; 2.718281828459045
m/nan         ; Not-a-Number
m/inf+        ; Positive infinity
m/inf-        ; Negative infinity

;; Basic operations
(m/sqrt 16)   ; 4.0
(m/cbrt 27)   ; 3.0
(m/pow 2 8)   ; 256.0
(m/exp 1)     ; 2.718281828459045 (e^1)
(m/log 10)    ; 2.302585092994046 (ln 10)
(m/log10 100) ; 2.0

;; Trigonometric functions
(m/sin m/PI)  ; 1.2246467991473532E-16 (approximately 0)
(m/cos m/PI)  ; -1.0
(m/tan m/PI)  ; -1.2246467991473532E-16 (approximately 0)

;; Type checking
(m/finite? 42)        ; true
(m/nan? m/nan)        ; true
(m/inf? m/inf+)       ; true
(m/prob? 0.5)         ; true (between 0 and 1 inclusive)
(m/open-prob? 0.5)    ; true (between 0 and 1 exclusive)

;; Rounding
(m/round 3.7 :up)     ; 4
(m/floor 3.7)         ; 3.0
(m/ceil 3.2)          ; 4.0
(m/round-significant 123.456 2 :up) ; 120.0
```

### Vectors

```clojure
(require '[provisdom.math.vector :as vector])

;; Vector operations
(vector/magnitude [3 4])            ; 5.0 (Euclidean norm)
(vector/angle-between [1 0] [0 1])  ; 1.5707963267948966 (π/2 radians)
(vector/dot-product [1 2 3] [4 5 6]) ; 32
(vector/normalize [3 4])            ; [0.6 0.8]
(vector/cross-product [1 0 0] [0 1 0]) ; [0 0 1]
```

### Matrices

```clojure
(require '[provisdom.math.matrix :as matrix])

;; Create matrices
(def m1 [[1 2] [3 4]])
(def m2 [[5 6] [7 8]])

;; Matrix operations
(matrix/mx+ m1 m2)                  ; [[6 8] [10 12]]
(matrix/mx* m1 m2)                  ; [[19 22] [43 50]]
(matrix/transpose m1)               ; [[1 3] [2 4]]
(matrix/determinant m1)             ; -2.0
(matrix/invert m1)                  ; [[-2.0 1.0] [1.5 -0.5]]
```

### Random Numbers

```clojure
(require '[provisdom.math.random :as random])

;; Generate random numbers
(def rng (random/rng))             ; Create a random number generator
(random/rand-double rng)           ; Generate a random double between 0.0 and 1.0
(random/rand-int rng 10)           ; Generate a random integer between 0 and 9
(random/rand-normal rng)           ; Generate a random number from normal distribution
```

### Arrays and Tensors

```clojure
(require '[provisdom.math.arrays :as arrays])
(require '[provisdom.math.tensor :as tensor])

;; Work with arrays
(arrays/array-map inc [1 2 3 4])   ; [2 3 4 5]
(arrays/array-reduce + [1 2 3 4])  ; 10

;; Tensor operations
(tensor/tensor-map inc [[1 2] [3 4]]) ; [[2 3] [4 5]]
(tensor/assoc-deep [[1 2] [3 4]] [0 1] 9) ; [[1 9] [3 4]]
```

### Intervals and Series

```clojure
(require '[provisdom.math.intervals :as intervals])
(require '[provisdom.math.series :as series])

;; Work with intervals
(intervals/interval-length [1 5])  ; 4.0
(intervals/interval-contains? [0 1] 0.5) ; true

;; Series operations
(series/sum (fn [n] (/ 1 (m/sq n))) 1 10) ; 1.5497677311665408 (sum of 1/n² from n=1 to n=10)
```

## Namespaces

- `provisdom.math.core` - Basic math functions, constants, and predicates
- `provisdom.math.vector` - Vector operations
- `provisdom.math.matrix` - Matrix operations
- `provisdom.math.random` - Random number generation
- `provisdom.math.special-functions` - Special mathematical functions
- `provisdom.math.tensor` - Tensor operations
- `provisdom.math.arrays` - Array utilities
- `provisdom.math.combinatorics` - Combinatorial functions
- `provisdom.math.derivatives` - Numerical differentiation
- `provisdom.math.integrals` - Numerical integration
- `provisdom.math.intervals` - Interval operations
- `provisdom.math.polynomials` - Polynomial functions
- `provisdom.math.series` - Series and sequence operations
- `provisdom.math.format` - Formatting utilities for numbers

## Features

- **Type-aware operations**: Many functions return types consistent with inputs when possible
- **Comprehensive spec coverage**: Extensive use of clojure.spec for validation and generative testing
- **NaN handling**: Consistent handling of NaN (Not a Number) values across functions
- **Performance optimized**: Efficient implementations for numerical computing
- **Generative testing**: Built-in support for property-based testing

## Design Philosophy

The library follows these design principles:

1. **Pragmatic type handling**: Functions marked with a prime (e.g., `floor'`) will attempt to return a long if possible, otherwise a double
2. **Comprehensive error handling**: Functions dealing with mathematical edge cases (division by zero, etc.) return appropriate values (infinity, NaN)
3. **Strong spec support**: All functions include spec definitions for argument validation and generative testing
4. **Predicates for everything**: Comprehensive predicates for checking number properties

## Development

To run tests:

```
clojure -M:test:test-clj-runner
```

## License

Copyright © 2018-2025 Provisdom

Distributed under the GNU Lesser General Public License version 3.0.
