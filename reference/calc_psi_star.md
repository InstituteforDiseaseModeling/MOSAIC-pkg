# Calibrate Daily Suitability on the Logit Scale (vector; optional causal EWMA + time offset + NA filling)

Transforms a **daily** suitability series \\\psi_t \in \[0,1\]\\ with a
**logit-scale affine calibration**, optional **causal EWMA smoothing**,
and an optional **time offset** \\k\\. Any missing values introduced by
the shift (or pre-existing) are filled using either **LOCF** (default)
or **linear interpolation**.

## Usage

``` r
calc_psi_star(
  psi,
  a = 1,
  b = 0,
  z = 1,
  k = 0,
  eps = 1e-06,
  fill_method = c("locf", "linear"),
  warn_k_rounding = TRUE
)
```

## Arguments

- psi:

  Numeric vector in \\\[0,1\]\\ (daily suitability \\\psi_t\\).

- a:

  Numeric (\>0). Shape/gain on the logit scale (\\a\>1\\ sharpens;
  \\a\<1\\ flattens).

- b:

  Numeric. Scale/offset on the logit scale (baseline up/down).

- z:

  Numeric in \\(0,1\]\\ controlling causal EWMA smoothing; `1` = no
  smoothing.

- k:

  Numeric or integer; time offset in **days**. Positive = forward/right;
  negative = backward/left. Non-integers are rounded to nearest integer.

- eps:

  Small positive number to clip `psi` away from {0,1} before `qlogis`.

- fill_method:

  Character; one of `c("locf","linear")` controlling how NA gaps are
  filled.

- warn_k_rounding:

  Logical; if `TRUE` (default), warns when `k` is rounded to nearest
  integer. Set to `FALSE` to suppress this warning.

## Value

Numeric vector \\\tilde\psi^{\star}\_t\\ (same length as `psi`):
calibrated, offset, NA-filled, and smoothed if `z<1`.

## Details

**Offset (index shift, in days):** \\\psi^{(k)}\_t = \psi\_{t-k}\\.
Positive \\k\\ shifts the series forward/right (delay); negative \\k\\
shifts backward/left. Non-integer \\k\\ is rounded to the nearest
integer.

**Calibration (odds-space power & scale):**
\$\$\psi_t^{\star}=\sigma\\\big(a\cdot
\mathrm{logit}(\tilde\psi_t)+b\big),\quad
\tilde\psi_t=\min\\\max(\psi^{(k)}\_t,\varepsilon),\\1-\varepsilon\\\$\$
with \\\text{odds}^\star_t = e^{b}\\(\text{odds}\_t)^{a}\\, \\
\\\text{odds}\_t=\tilde\psi_t/(1-\tilde\psi_t)\\.

**Optional causal smoothing (no look-ahead):** \$\$\tilde\psi^{\star}\_t
= z\\\psi^{\star}\_t + (1-z)\\\tilde\psi^{\star}\_{t-1},\quad
z\in(0,1\],\\ \tilde\psi^{\star}\_1=\psi^{\star}\_1.\$\$

**NA filling (after calibration, before smoothing):** Choose via
`fill_method`:

- `"locf"` — forward fill (LOCF) then backward pass for leading gaps.

- `"linear"` — linear interpolation for interior gaps with constant edge
  extension.

If all values are NA after shifting, the function falls back to a
constant baseline \\\sigma(b)\\.

## See also

[`qlogis`](https://rdrr.io/r/stats/Logistic.html),
[`plogis`](https://rdrr.io/r/stats/Logistic.html),
[`approx`](https://rdrr.io/r/stats/approxfun.html)

## Examples

``` r
# Basic usage with integer k (no warnings)
psi <- c(0.1, 0.3, 0.7, 0.9, 0.5)
result1 <- calc_psi_star(psi, a = 1.2, b = 0.5, k = 2)

# Non-integer k with warning (default behavior)
result2 <- calc_psi_star(psi, a = 1.2, b = 0.5, k = 2.744)
#> Warning: `k` (2.744) is not an integer; rounding to 3.

# Non-integer k without warning
result3 <- calc_psi_star(psi, a = 1.2, b = 0.5, k = 2.744, warn_k_rounding = FALSE)

# With EWMA smoothing
result4 <- calc_psi_star(psi, a = 1.2, b = 0.5, k = 2, z = 0.8)
```
