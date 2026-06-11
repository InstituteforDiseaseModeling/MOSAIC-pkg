# Fit Diagnostics: Bias, Shape, and Variance for a Single Observed/Predicted Series

Computes a structured set of fit-quality diagnostics comparing an
observed daily series to a model-predicted daily series, decomposed
along three independent dimensions: **bias** (total scale, by year, and
by epidemic regime), **shape** (peak timing, peak magnitude, onset
slope, seasonal correlation, and a timing-normalised shape correlation),
and **variance** (coefficient-of-variation ratio and residual
autocorrelation). A PASS/WARN/FAIL scorecard summarises each dimension.

This is the metrics engine behind the active fit-diagnostic workflow
(see the `diagnose-fit` skill and
[`run_fit_sandbox()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_fit_sandbox.md)).
It is country-agnostic: it takes plain numeric vectors, so it can be
unit-tested without running a model.

## Usage

``` r
calc_fit_diagnostics(
  observed,
  predicted,
  dates,
  epidemic_threshold = NULL,
  smooth_window = 28L,
  na_rm = TRUE
)
```

## Arguments

- observed:

  Numeric vector; observed daily series (NA allowed for missing days).

- predicted:

  Numeric vector; model-predicted daily series, same length as
  `observed`.

- dates:

  Date vector (or coercible) aligned to `observed`/`predicted`.

- epidemic_threshold:

  Optional numeric; observed value at/above which a day is classed
  epidemic for the bias split. Default `NULL` (75th percentile of
  positive observed values).

- smooth_window:

  Integer; centred running-mean window (days) used for peak detection.
  Default `28`.

- na_rm:

  Logical; drop NA pairs in level metrics. Default `TRUE`.

## Value

A named list with elements:

- bias:

  list: `total`, `by_year` (named vector), `endemic`, `epidemic`.

- r2:

  list: `corr`, `sse`, `by_year` (named vector, corr method).

- shape:

  list: `peak_timing_error_by_year` (days; positive = model peaks
  later), `peak_magnitude_ratio_by_year` (pred/obs),
  `onset_slope_ratio`, `seasonal_corr`, `shape_corr`.

- variance:

  list: `cv_ratio` (pred CV / obs CV), `residual_autocorr_lag7`.

- scorecard:

  named character vector: `bias`, `peak_timing`, `peak_shape`,
  `variance`, each "PASS"\|"WARN"\|"FAIL".

## Details

Diagnose each dimension in order. Bias is addressed first because shape
and variance diagnostics are confounded when the total scale is wrong.
Reuses
[`calc_model_R2()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_R2.md)
(corr and SSE) and
[`calc_bias_ratio()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_bias_ratio.md)
for the level metrics.

The endemic/epidemic bias split is a pragmatic *observed-magnitude*
proxy: days whose observed value is at or above `epidemic_threshold` are
treated as epidemic, the rest endemic. When `epidemic_threshold` is
`NULL` the threshold defaults to the 75th percentile of positive
observed values. This is a diagnostic heuristic, not the model's
internal regime classifier.

## See also

[`run_fit_sandbox()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_fit_sandbox.md),
[`calc_model_R2()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_R2.md),
[`calc_bias_ratio()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_bias_ratio.md)
