# Optimize Ensemble Subset Size

Post-ensemble optimization that evaluates every possible top-N subset
(ranked by likelihood) and selects the N that maximizes prediction
quality. Produces a separate `mosaic_ensemble` object at the optimal
subset size, leaving the original ensemble untouched.

For each candidate size N (from `min_n` to the full ensemble), the
function re-computes Gibbs weights within the top-N subset, re-computes
weighted median predictions from the 4D arrays, and scores with the
selected objective function.

## Usage

``` r
optimize_ensemble_subset(
  ensemble,
  likelihoods,
  seeds = NULL,
  min_n = 30L,
  objective = c("mae", "r2_bias", "wis"),
  central_method = "median",
  verbose = TRUE
)
```

## Arguments

- ensemble:

  A `mosaic_ensemble` object returned by
  [`calc_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md).

- likelihoods:

  Numeric vector of log-likelihoods, one per parameter set in the
  ensemble. Must have length `ensemble$n_param_sets`.

- seeds:

  Optional numeric/integer vector of simulation seeds aligned with
  `likelihoods`. When supplied, the function returns `optimal_seeds` so
  callers can map the optimized subset back to the original
  `samples.parquet` rows without re-deriving the internal sort.
  Separately, if `ensemble$seeds` is present (the per-member seeds
  carried by `calc_model_ensemble`, aligned with `cases_array` by
  construction), it is carried through the same sort/slice and exposed
  as `ensemble_optimized$seeds` for cases_array-aligned consumers such
  as medoid selection. This argument and `ensemble$seeds` serve
  different roles and are kept independent.

- min_n:

  Minimum subset size to evaluate. Default `30L` to guard against
  small-subset KDE degeneracy when the optimized subset drives posterior
  artifacts. Fox et al. (2024) report 4 as a statistical minimum for
  accuracy, but posterior density estimation needs larger N.

- objective:

  Scoring function: `"mae"` (default, normalized MAE), `"r2_bias"`
  (R-squared plus bias penalty), or `"wis"` (normalized Weighted
  Interval Score).

- central_method:

  Central tendency used to summarize each per-cell ensemble distribution
  for the `"mae"` and `"r2_bias"` objectives and for the recorded
  R^2/bias/MAE diagnostics: `"median"` (default, reproduces historical
  selection) or `"mean"` (weighted mean, unbiased for expected counts).
  Scalar or per-channel `c(cases=, deaths=)`. The `"wis"` objective is
  quantile-based and **unaffected** by this setting (its point forecast
  remains the weighted median). The default `"median"` here is
  deliberate – it preserves historical direct-call selection and the
  Tier-2 bit-for-bit parity guarantee;
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  passes the package default (`"mean"`) explicitly.

- verbose:

  Logical; if `TRUE`, emit progress messages.

## Value

An S3 object of class `mosaic_subset_optimization` containing:

- evaluation_table:

  Data frame with one row per N evaluated.

- optimal_n:

  Selected subset size.

- optimal_score:

  Score at optimal N.

- optimal_weights:

  Re-computed Gibbs weights for the optimal subset.

- optimal_indices:

  Integer indices into the original ensemble arrays.

- optimal_seeds:

  Simulation seeds of the optimal subset in likelihood-sorted order,
  aligned with `optimal_weights` (when `seeds` supplied), else `NULL`.

- ensemble_optimized:

  Complete `mosaic_ensemble` object at optimal N. Its `$seeds` field is
  the per-member seed aligned with its `cases_array` (member i \<-\>
  seedsi) when `ensemble$seeds` was present, else it falls back to
  `optimal_seeds`.

- stability_flag:

  TRUE if score profile was flat.

- diagnostics_n:

  Original diagnostics-selected N.

- diagnostics_score:

  Score at the diagnostics-selected N.

- objective:

  Which objective was used.

- central_method:

  Resolved per-channel central tendency used for `"mae"`/`"r2_bias"`
  scoring and the recorded diagnostics.

## References

Bracher J et al. (2021). Evaluating epidemic forecasts in an interval
format. *PLOS Computational Biology*, 17(2), e1008618.

Gneiting T & Raftery AE (2007). Strictly Proper Scoring Rules,
Prediction, and Estimation. *JASA*, 102(477), 359–378.
