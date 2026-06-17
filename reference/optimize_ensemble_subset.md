# Optimize Ensemble Subset Size

Post-ensemble optimization that evaluates top-N subsets (ranked by
likelihood) and selects the N that maximizes prediction quality.
Produces a separate `mosaic_ensemble` object at the optimal subset size,
leaving the original ensemble untouched.

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
  stride = 1L,
  cl = NULL,
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

- stride:

  Integer \>= 1. `1L` (default) evaluates every N in `min_n:max_n`
  (exhaustive, bit-identical parity path). `> 1L` enables a
  coarse-then-refine two-stage search (see Details). **Opt-in**; changes
  which N's are evaluated and may select a slightly different
  `optimal_n` than the exhaustive search.

- cl:

  Optional parallel cluster (from
  [`make_mosaic_cluster`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_mosaic_cluster.md)
  or
  [`parallel::makeCluster`](https://rdrr.io/r/parallel/makeCluster.html))
  used to split the per-cell evaluation kernel across workers. `NULL`
  (default) runs serially. When supplied, the result is bit-for-bit
  identical to the serial path. Must be a `"PSOCK"` cluster (FORK is
  unsafe at this point in the pipeline). For trivially small problems
  the function falls back to serial regardless of `cl`.

- verbose:

  Logical; if `TRUE`, emit progress messages.

## Value

An S3 object of class `mosaic_subset_optimization` containing:

- evaluation_table:

  Data frame with one row per N evaluated. With `stride > 1L` this
  contains only the coarse + refine N's, sorted by N, not every N in
  `min_n:max_n`.

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
  the per-member seed aligned with its `cases_array` (member-to-seed
  aligned) when `ensemble$seeds` was present, else it falls back to
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

## Details

The evaluation loop is organized *cell-outer*: each `[location, time]`
cell's finite predictions are gathered once and then scored across every
candidate N (the gather does not depend on N), filling a per-cell vector
of central tendencies. The default call path (`stride = 1L`,
`cl = NULL`) is exhaustive and serial and is guaranteed bit-for-bit
identical to the historical implementation (Tier-2 parity).

Two opt-in speed levers are available:

- **PSOCK parallelism** (`cl`): the `n_locations * n_time_points` cells
  are split across the supplied cluster's workers. The result is
  bit-for-bit identical to the serial path (each worker runs the same
  kernel on a disjoint block of cells; the master scatters the per-cell
  central/quantile values back into the same `[i, j]` positions). FORK
  clusters are unsafe here (native reticulate/numba threads are loaded
  by this post-calibration point); pass a `"PSOCK"` cluster.

- **Stride-then-refine** (`stride`): with `stride > 1L`, a coarse grid
  `seq(min_n, max_n, by = stride)` is evaluated first to locate the best
  N\*, then a refinement bracket `(N*-stride):(N*+stride)` is evaluated,
  and the selection logic runs over the combined (coarse + refine) rows.
  This is **not** bit-identical to the exhaustive path:
  `evaluation_table` contains only the evaluated N's (fewer rows), and
  the selected `optimal_n` may differ slightly from the exhaustive
  optimum if the score profile is non-monotone between coarse points.
  The trade is roughly an `O(stride)` reduction in the number of N's
  evaluated.

## References

Bracher J et al. (2021). Evaluating epidemic forecasts in an interval
format. *PLOS Computational Biology*, 17(2), e1008618.

Gneiting T & Raftery AE (2007). Strictly Proper Scoring Rules,
Prediction, and Estimation. *JASA*, 102(477), 359–378.

## See also

[`make_mosaic_cluster`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_mosaic_cluster.md)
for building a PSOCK cluster to pass to `cl`.
