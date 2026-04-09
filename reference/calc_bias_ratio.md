# Bias Ratio Between Estimated and Observed Series

Computes `mean(estimated) / mean(observed)` as a measure of systematic
over- or under-prediction. A value of 1.0 indicates no bias. Pairs with
`calc_model_R2(method = "corr")` to give both pattern agreement and
magnitude calibration.

## Usage

``` r
calc_bias_ratio(observed, estimated, na_rm = TRUE, finite_only = TRUE)
```

## Arguments

- observed:

  Numeric vector or matrix.

- estimated:

  Numeric vector or matrix (same length after flattening).

- na_rm:

  Logical; drop NA pairs. Default `TRUE`.

- finite_only:

  Logical; drop non-finite values. Default `TRUE`.

## Value

Scalar ratio. Values \> 1 indicate over-prediction, \< 1
under-prediction. Returns `NA_real_` if observed mean is zero or no
valid pairs exist.

## See also

[`calc_model_R2()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_R2.md)
