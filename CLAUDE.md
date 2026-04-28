# math

Library-specific notes for `provisdom.math`.

## See Also

- `../../CLAUDE.md` — repo-wide conventions
- `../../rules/testing.md` — testing rules (read the CRITICAL section first)
- `../../rules/file-review.md` — review workflows
- `../../rules/anti-patterns.md` — quick reference card

## Library-specific conventions

- **`code/math/src/provisdom/math/core.clj` is the lint exemption** for `bin/check-gen-double`. It defines `finite-gen`, `finite+-gen`, `finite-non--gen` and therefore must use `gen/double*` internally. Every other file in the repo uses the wrappers. The exemption is recorded in `bin/check-gen-double` (`excluded-files` set); when adding new generator primitives here, update that set.
- **Apostrophe-suffixed functions return `long` when the result fits.** `floor'`, `ceil'`, `sq'`, `mod'`, `lcm'` etc. return a long when the value is in the ±2^63 range, otherwise return a double. Avoids silent truncation and overflow surprises. Prefer `m/floor'` to `(long (m/floor x))` everywhere.
- **Predicate naming covers signed bounded variants of every numeric type.** `finite?`, `finite+?`, `finite-?`, `finite-non+?`, `finite-non-?` (and the same for `long?`, `int?`). All IEEE 754-aware: NaN and ±Infinity handled per IEEE rules, *not* by Clojure default coercion.
- **Numerical stability primitives are first-class.** `log-sum-exp` for log-of-sums, Kahan summation in the vector module, and saturating `exp` (returns 0 below ≈−700, `+Inf` above ≈+700). Reach for these before rolling a hand-stable formula.
- **NaN / Infinity semantics are explicit.** `div` returns `NaN` for 0/0 and `±Inf` for n/0. `num?` excludes `##NaN`. Specs needing finite-only generators use `{:NaN? false}` and `{:infinite? false}`. Bounds shortcuts: `bounds-prob` is [0,1], `bounds-finite` is the open finite range.
