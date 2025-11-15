# Calculate the Composite Loss of a Model

Computes a simple composite **loss** for model fit defined as \$\$
\mathcal{L} \\=\\ \alpha \cdot \mathrm{MSE}(y, \hat{y}) \\-\\ \beta
\cdot \mathrm{Corr}(y, \hat{y}), \$\$ where \\\mathrm{MSE}\\ is the
(optionally weighted) mean squared error and \\\mathrm{Corr}\\ is the
(optionally weighted) **Pearson correlation**. Lower values indicate
better fit (since large positive correlation reduces the loss).

## Usage

``` r
calc_model_loss(
  observed,
  estimated,
  alpha = 1,
  beta = 1,
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

- alpha:

  Non-negative scalar weight for the MSE term. Default `1`.

- beta:

  Non-negative scalar weight for the correlation term. Default `1`.

- na_rm:

  Logical; if `TRUE` (default), drop any pair with `NA`.

- finite_only:

  Logical; if `TRUE` (default), also drop non-finite values (`Inf`,
  `-Inf`, `NaN`) pairwise.

- weights:

  Optional numeric vector of non-negative weights (recycled if length
  1). Used for both MSE and Pearson correlation to keep consistency. If
  `NULL` (default), all valid pairs receive equal weight.

- verbose:

  Logical; if `TRUE`, emit brief diagnostic messages.

## Value

A single numeric value: \\\alpha \cdot \mathrm{MSE} - \beta \cdot
\mathrm{Corr}\\. Returns `NA_real_` if either component cannot be
computed.

## Details

- Inputs may be vectors or matrices (e.g., locations \\\times\\ time).
  They are flattened and compared **pairwise** with optional removal of
  non-finite values.

- MSE and correlation are computed by delegating to
  [`calc_model_mse()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_mse.md)
  and
  [`calc_model_cor()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cor.md)
  (with `method = "pearson"`), using identical filtering options so
  valid pairs are consistent for both components.

- If either component is `NA` (e.g., no valid pairs or zero variance),
  the loss is returned as `NA_real_`.

## Interpretation

- **Lower is better**. High MSE increases the loss; high positive
  correlation decreases the loss. Negative correlation increases the
  loss.

- Units depend on the scale of `observed`/`estimated` (via MSE) and the
  dimensionless correlation term. Choose `alpha`, `beta` to balance
  scales.

## See also

[`calc_model_mse()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_mse.md),
[`calc_model_cor()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cor.md),
[`calc_model_likelihood()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md)

## Examples

``` r
set.seed(3)
y  <- rpois(50, 10)
yh <- y + rnorm(50, 0, 2)

# Default: alpha = 1, beta = 1
calc_model_loss(y, yh)
#> [1] 1.483312

# Emphasize correlation relative to MSE
calc_model_loss(y, yh, alpha = 0.5, beta = 2)
#> [1] -0.5973181

# Matrix example (locations x time) with recency weights
Y  <- matrix(rpois(60, 5), nrow = 3)     # 3 locations x 20 weeks
Yh <- Y + matrix(rnorm(60, 0, 1), nrow = 3)
w  <- rep(seq(0.5, 1.5, length.out = ncol(Y)), each = nrow(Y))
calc_model_loss(Y, Yh, weights = w)
#> [1] 0.4001887
```
