# Mean Squared Error Between Observed and Estimated Series

Computes the **mean squared error (MSE)** between an observed series and
a model-estimated series. Inputs may be numeric vectors or matrices
(e.g., \\L \times T\\ location-by-time arrays); values are flattened and
compared **pairwise** with optional weights.

## Usage

``` r
calc_model_mse(
  observed,
  estimated,
  na_rm = TRUE,
  finite_only = TRUE,
  weights = NULL,
  verbose = FALSE
)
```

## Arguments

- observed:

  A numeric vector or matrix of observed values.

- estimated:

  A numeric vector or matrix of model-estimated values of the same
  length (after flattening) as `observed`.

- na_rm:

  Logical; if `TRUE` (default), remove any pair with `NA`. If `FALSE`
  and any `NA`/`Inf` are found (or lengths mismatch), returns
  `NA_real_`.

- finite_only:

  Logical; if `TRUE` (default), also drop non-finite values (`Inf`,
  `-Inf`, `NaN`) during pairwise filtering.

- weights:

  Optional numeric vector of non-negative weights (recycled if length
  1). Must align with the number of **valid pairs** after filtering. If
  `NULL` (default), all pairs receive equal weight.

- verbose:

  Logical; if `TRUE`, emit brief diagnostic messages.

## Value

A single numeric value: the mean squared error. Returns `NA_real_` if
fewer than one valid pair remains or if total weight is zero.

## Details

Let \\\\(y_i, \hat{y}\_i)\\\_{i=1}^{n}\\ be the set of **valid pairs**
after removing any entries with `NA`/`Inf` (when `na_rm = TRUE` and
`finite_only = TRUE`). The weighted MSE is \$\$ \mathrm{MSE}\_w \\=\\
\frac{\sum\_{i=1}^{n} w_i \\ ( \hat{y}\_i - y_i )^2}{\sum\_{i=1}^{n}
w_i}, \$\$ which reduces to the standard unweighted MSE when \\w_i
\equiv 1\\.

## Input shapes

Vectors and matrices are supported. Matrices are flattened
(column-major) in both `observed` and `estimated` before pairwise
comparison.

## See also

[`calc_model_cor()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cor.md),
[`calc_model_likelihood()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md)

## Examples

``` r
# Vector example
set.seed(1)
y  <- rpois(20, 10)
yh <- y + rnorm(20, 0, 2)
calc_model_mse(y, yh)
#> [1] 2.708462

# Matrix example (locations x time)
Y  <- matrix(rpois(30, 5), nrow = 3)
Yh <- Y + matrix(rnorm(30, 0, 1), nrow = 3)
calc_model_mse(Y, Yh)
#> [1] 0.9081703

# With weights (e.g., emphasize recent time points)
w <- rep(seq(0.5, 1.5, length.out = ncol(Y)), each = nrow(Y))
calc_model_mse(Y, Yh, weights = w)
#> [1] 0.8112577
```
