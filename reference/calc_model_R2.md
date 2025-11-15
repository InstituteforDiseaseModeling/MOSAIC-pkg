# Coefficient of Determination (R-squared) Between Observed and Estimated Series

Computes \\R^2\\ between an observed series and model estimates.
Supports two definitions:

- `method = "sse"` (default): \\R^2 = 1 - \mathrm{SSE}/\mathrm{SST}\\
  with optional weights. This can be *negative* when the model
  underperforms the intercept-only baseline.

- `method = "corr"`: \\R^2 = \mathrm{cor}(y,\hat y)^2\\ (Pearson),
  always in \\\[0,1\]\\.

## Usage

``` r
calc_model_R2(
  observed,
  estimated,
  method = c("sse", "corr"),
  bounded = FALSE,
  na_rm = TRUE,
  finite_only = TRUE,
  weights = NULL,
  verbose = FALSE
)
```

## Arguments

- observed:

  Numeric vector or matrix.

- estimated:

  Numeric vector or matrix (same length after flattening).

- method:

  One of `"sse"` (default) or `"corr"`.

- bounded:

  Logical; if `TRUE`, clamp the result to \\\[0,1\]\\. Default `FALSE`.

- na_rm:

  Logical; drop NA pairs. Default `TRUE`.

- finite_only:

  Logical; drop non-finite values. Default `TRUE`.

- weights:

  Optional non-negative weights (used for `method="sse"` and weighted
  Pearson in `"corr"`).

- verbose:

  Logical; if `TRUE`, emit brief diagnostics.

## Value

Scalar \\R^2\\. May be negative for `method="sse"` unless
`bounded=TRUE`.

## See also

[`calc_model_mse()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_mse.md),
[`calc_model_cor()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cor.md)
