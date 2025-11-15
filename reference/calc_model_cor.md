# Pearson (or Rank) Correlation Between Observed and Estimated Series

Computes the correlation between an observed series and a
model-estimated series. By default this is **Pearson's correlation**.
Inputs may be numeric vectors or matrices; values are flattened and
compared **pairwise**. An optional set of non-negative weights can be
supplied for a **weighted Pearson correlation**.

## Usage

``` r
calc_model_cor(
  observed,
  estimated,
  method = c("pearson", "spearman", "kendall"),
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

- method:

  Correlation type. One of `"pearson"` (default), `"spearman"`, or
  `"kendall"`.

- na_rm:

  Logical; if `TRUE` (default), remove any pair with `NA`. If `FALSE`
  and any `NA`/`Inf` are found (or lengths mismatch), returns
  `NA_real_`.

- finite_only:

  Logical; if `TRUE` (default), also drop non-finite values (`Inf`,
  `-Inf`, `NaN`) during pairwise filtering.

- weights:

  Optional numeric vector of non-negative weights (recycled if length
  1). Only used when `method = "pearson"`; ignored otherwise.

- verbose:

  Logical; if `TRUE`, emit brief diagnostic messages.

## Value

A single numeric value: the correlation coefficient for the chosen
`method`. Returns `NA_real_` if fewer than two valid pairs remain or if
a variance term is zero.

## Details

After removing invalid pairs (see `na_rm` and `finite_only`), the
**weighted Pearson correlation** is: \$\$ r_w \\=\\ \frac{\sum_i w_i
(x_i - \bar{x}\_w) (y_i - \bar{y}\_w)}{ \sqrt{\sum_i w_i (x_i -
\bar{x}\_w)^2}\\ \sqrt{\sum_i w_i (y_i - \bar{y}\_w)^2}}, \$\$ where
\\\bar{x}\_w = \frac{\sum_i w_i x_i}{\sum_i w_i}\\ and similarly for
\\\bar{y}\_w\\. If `weights = NULL`, the standard (unweighted) Pearson
correlation is returned. For `method = "spearman"` or `"kendall"`, any
provided `weights` are **ignored**.

## Input shapes

Vectors and matrices are supported. Matrices are flattened
(column-major) in both `observed` and `estimated` before pairwise
comparison.

## See also

[`calc_model_mse()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_mse.md),
[`calc_model_likelihood()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md)

## Examples

``` r
# Vector example
set.seed(2)
y  <- rpois(20, 10)
yh <- y + rnorm(20, 0, 2)
calc_model_cor(y, yh)                   # Pearson (default)
#> [1] 0.8853289
calc_model_cor(y, yh, method = "spearman")
#> [1] 0.8788916

# Matrix example
Y  <- matrix(rpois(30, 5), nrow = 3)
Yh <- Y + matrix(rnorm(30, 0, 1), nrow = 3)
calc_model_cor(Y, Yh)
#> [1] 0.9191996

# Weighted Pearson (e.g., emphasize recent weeks)
w <- rep(seq(0.5, 1.5, length.out = ncol(Y)), each = nrow(Y))
calc_model_cor(Y, Yh, weights = w)
#> [1] 0.9154576
```
