# Build Complete MOSAIC Control Structure

Creates a complete control structure for
[`run_mosaic()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md).
This is the primary interface for configuring MOSAIC execution settings,
consolidating calibration strategy, parameter sampling, parallelization,
and output options.

**Parameters are organized in workflow order:**

1.  `calibration`: How to run (simulations, iterations, batch sizes)

2.  `sampling`: What to sample (which parameters to vary)

3.  `likelihood`: How to score model fit (likelihood components and
    weights)

4.  `targets`: When to stop (ESS convergence thresholds)

5.  `parallel`: Infrastructure (cores, cluster type)

6.  `io`: Output format (file format, compression)

7.  `paths`: File management (output directories, plots)

## Usage

``` r
mosaic_control_defaults(
  calibration = NULL,
  sampling = NULL,
  likelihood = NULL,
  targets = NULL,
  predictions = NULL,
  weights = NULL,
  parallel = NULL,
  io = NULL,
  paths = NULL,
  logging = NULL
)
```

## Arguments

- calibration:

  List of calibration settings. Default is:

  - `n_simulations`: NULL for auto mode, or integer for fixed mode

  - `n_iterations`: Number of stochastic engine iterations per parameter
    set (default: 3L)

  - `max_simulations_total`: Maximum total simulations across all phases
    (default: 100000L)

  - `batch_size_adaptive`: Simulations per batch in Phase 1 adaptive
    calibration (default: 500L)

  - `min_batches_adaptive`: Minimum Phase 1 batches before convergence
    check (default: 5L)

  - `max_batches_adaptive`: Maximum Phase 1 batches (default: 8L)

  - `max_batch_predictive`: Cap on each Phase 2 predictive batch
    (default: 10000L)

  - `max_batches_predictive`: Maximum Phase 2 predictive batches
    (default: 10L)

  - `target_r2_adaptive`: ESS regression R-squared target for Phase 1
    convergence (default: 0.90)

- sampling:

  List of parameter sampling flags (what to sample). Default is:

  - `sample_tau_i`: Sample the daily departure (travel) probability by
    location (default: TRUE)

  - `sample_mobility_gamma`: Sample the gravity-model distance-decay
    exponent (default: TRUE)

  - `sample_mobility_omega`: Sample the gravity-model population-scaling
    exponent (default: TRUE)

  - `sample_iota`: Sample the incubation rate, E to I (default: TRUE)

  - `sample_gamma_2`: Sample the asymptomatic recovery rate (default:
    TRUE; second-dose vaccine effectiveness is `sample_phi_2`)

  - `sample_alpha_1`: Sample within-metapop population mixing exponent
    (default: FALSE, PINNED)

  - `sample_alpha_2`: Sample frequency-dependence degree (default:
    FALSE; pinned, weakly identified)

  - ... (see `mosaic_control_defaults()` for complete list of 40 flags)

- likelihood:

  List of likelihood calculation settings (how to score model fit).
  Default is:

  - `weight_cases`: Weight for cases vs deaths (default: 1.0)

  - `weight_deaths`: Weight for deaths vs cases (default: 1.0)

  - `weight_wis`: WIS regularizer weight (default: 0, off). The 0.10
    suggested before v0.101.0 was tuned against the daily cases core,
    the default; against the weekly core (`cases_scoring = "weekly"`) a
    given weight weighs several times more (a median 4.8 times, range
    1.8 to 6.5, on the v0.100.1 national re-selection pools; more where
    the cases dispersion is small), so about 0.02 keeps that balance
    there. Not re-tuned since; check the fit before relying on it

  - `cases_scoring`: `"daily"` (default; one NB cell per day at the
    weekly k, the cell rule of v0.100.1 and earlier) or `"weekly"`
    (cases scored as NB on reporting-week totals). The daily rule is the
    default because the weekly rule fitted the cases worse in the
    v0.101.0 likelihood gate. Either rule runs at the dispersion this
    version estimates, so the default does not reproduce a v0.100.1 run:
    a cases fit with no estimate of its own, or clamped at the lower
    bound, now takes the panel trend, a config with `reported_tier`
    restricts the cases k and the deaths dispersion to observed weeks,
    the ensemble intervals are observation-level (weekly NB at the
    scored k under either rule) and the cases central line is the
    median. Matching a v0.100.1 likelihood also needs that run's
    `nb_k_cases` (its `nb_dispersion.csv`) and a config without
    `reported_tier`; resume refuses to pool with v0.100.1 simulations
    either way

  - ... (see `mosaic_control_defaults()` for complete list)

- targets:

  List of convergence targets (when to stop). Default is:

  - `ESS_param`: Target ESS per parameter (default: 100)

  - `ESS_param_prop`: Proportion of parameters meeting ESS (default:
    0.95)

  - `ESS_best`: Target for both subset size and ESS within subset
    (default: 100).

  - `A_best`: Target agreement index (default: 0.70). Lower values allow
    top sims to dominate.

  - `CVw_best`: Target CV of weights (default: 1.0). Higher values
    permit sharper discrimination.

  - `min_best_subset`: Smallest best-subset size searched (default: 30)

  - `max_best_subset`: Largest best-subset size searched (default: 1000)

  - `ESS_method`: ESS calculation method, "kish" or "perplexity"
    (default: "perplexity")

  - `ESS_marginal_method`: Per-parameter marginal ESS method, "kde" or
    "binned" (default: "kde")

  - `best_subset_weighting`: Best-subset posterior weights, "saturated"
    or "tempered" (default: "saturated"; "tempered" is sharper, not
    softer)

- predictions:

  List of prediction generation settings. Default is:

  - `n_iter_ensemble`: Stochastic runs per posterior parameter set in
    the weighted ensemble (default: 10L). Total ensemble sims = N
    parameter sets x `n_iter_ensemble`.

  - `n_iter_best`: Stochastic runs for the medoid single-config
    prediction plots (default: 100L). Applied identically to both
    models. These runs execute on an internal PSOCK cluster when
    `parallel$enable = TRUE`.

  - `optimize_subset`: Logical; when `TRUE`, the post-ensemble optimizer
    (MAE / WIS / R^2+bias) refines the best subset used for
    `posteriors.json`, `posterior_quantiles.csv`, and the ensemble
    object driving all downstream plots and metrics. The tier-selected
    subset is preserved in the `is_best_subset` / `weight_best` columns
    of `samples.parquet` for provenance; the optimized selection is
    written to new `is_best_subset_opt` / `weight_best_opt` columns.
    Default `TRUE` since v1.2.0 (`FALSE` before). When `FALSE`, the
    tier-selected subset is canonical and no `_opt` columns are written.
    The optimizer chooses among subsets of the simulated candidate
    ensemble, so the size it selects is between `optimize_min_n` and the
    tier-selected size. **Statistical note:** enabling this flag yields
    a posterior conditioned on ensemble predictive performance (MAE /
    WIS / R^2+bias on the training data) rather than a pure
    likelihood-weighted posterior.

  - `optimize_min_n`: Minimum subset size the optimizer may select
    (default `30L`, raised from `4L` to guard against KDE degeneracy in
    `posteriors.json`).

  - `optimize_objective`: Objective function, `"mae"` (default),
    `"r2_bias"`, or `"wis"`.

  - `optimize_stride`: Integer \>= 1 (default `3L`). `1L` evaluates
    every candidate subset size in `min_n:max_n` (exhaustive,
    bit-identical to the historical search). `> 1L` runs a
    coarse-then-refine search that evaluates a strided grid first and
    then refines around the best point. The default `3L` gives roughly a
    3x speedup with negligible accuracy cost on the smooth `score(N)`
    curve (it re-examines a +/- 3 window around the coarse winner);
    raise it (`5L`/`10L`/`25L`) for more speed, or set `1L` to force the
    exhaustive search. Note it may select a slightly different
    `optimal_n` than exhaustive and records only the evaluated N's in
    the diagnostics table. (The
    [`optimize_ensemble_subset`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/optimize_ensemble_subset.md)
    function's own `stride` default remains `1L` to preserve
    bit-identicality.)

  - `central_method`: Ensemble central tendency, `"mean"` (the expected
    count, which never collapses to zero on sparse deaths) or `"median"`
    (the typical trajectory, robust to a few explosive members). Scalar
    or per-channel `c(cases=, deaths=)`; default
    `c(cases = "median", deaths = "mean")` since v0.101.0 (both mean
    from v0.98.0 to v0.100.x, both median from v0.46.1 to v0.97.x).
    Either way the line is a summary of the engine-level member
    trajectories; the intervals are observation-level. Governs the
    prediction trajectory + plots, the canonical `*_ensemble` R^2/bias
    metrics, the medoid target, and the subset-selection objective
    consistently.

  The number of parameter sets in the ensemble is determined by the best
  subset (all sims with non-zero importance weights).

- weights:

  List of importance-weight settings. Default is:

  - `floor`: Minimum weight to prevent underflow (default: 1e-15).

  - `iqr_multiplier`: Tukey IQR outlier-detection multiplier (default:
    1.5; use 3.0 for extreme outliers only).

- parallel:

  List of parallelization settings (infrastructure). Default is:

  - `enable`: Enable parallel execution (default: FALSE)

  - `n_cores`: Number of simulation worker processes (default: 1L)

  - `type`: Cluster type, "PSOCK" or "FORK" (default: "PSOCK")

  - `progress`: Show progress bar (default: TRUE)

- io:

  List of I/O settings (output format). Default is:

  - `format`: Output format; only "parquet" is supported ("csv" is
    coerced to "parquet" with a warning)

  - `compression`: Compression algorithm (default: "zstd")

  - `compression_level`: Compression level (default: 3L)

  - `persist_ensemble_arrays`: Retain the dense 4-D arrays – the
    observation-level `cases_array`/`deaths_array` and the engine-level
    `cases_engine_array`/`deaths_engine_array` – in the persisted
    ensemble RDS files (`ensemble_candidate.rds`,
    `ensemble_optimized.rds`, `subset_opt.rds`, `medoid_ensemble.rds`).
    Default `FALSE` strips the arrays at save time so the on-disk
    artifacts are small (~tens of KB); every light field (central
    tendencies, envelopes, weights, seeds, obs, metadata) is preserved
    and all standard consumers (plotting, OCV, rolling CV) work
    unchanged. Set `TRUE` for a re-analysable raw archive (e.g.
    re-running subset optimization or the R_eff posterior-resimulation
    CI path from the saved file). The in-memory object used during the
    run is never affected (default: `FALSE`).

- paths:

  List of path and output settings (file management). Default is:

  - `clean_output`: Remove output directory if exists (default: FALSE)

  - `plots`: Generate diagnostic plots (default: TRUE)

- logging:

  List of logging settings. Default is:

  - `verbose`: Enable detailed progress messages in sub-functions
    (default: FALSE).

## Value

A complete control list suitable for passing to
[`run_mosaic()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md).

## Examples

``` r
# Default control settings
ctrl <- mosaic_control_defaults()

# Quick parallel run with 8 cores
ctrl <- mosaic_control_defaults(
  parallel = list(enable = TRUE, n_cores = 8)
)

# Fixed mode with 5000 simulations, 5 iterations each
ctrl <- mosaic_control_defaults(
  calibration = list(
    n_simulations = 5000,
    n_iterations = 5
  )
)

# Auto mode with custom settings
ctrl <- mosaic_control_defaults(
  calibration = list(
    n_simulations = NULL,  # NULL = auto mode
    n_iterations = 3,
    max_simulations_total = 50000,
    batch_size_adaptive = 1000
  ),
  parallel = list(enable = TRUE, n_cores = 16)
)

# Only sample specific parameters
ctrl <- mosaic_control_defaults(
  sampling = list(
    sample_tau_i = TRUE,
    sample_mobility_gamma = FALSE,
    sample_mobility_omega = FALSE,
    sample_beta_j0_tot = TRUE,
    sample_iota = FALSE,
    sample_gamma_2 = FALSE,
    sample_alpha_1 = FALSE
  )
)

# Enable WIS regularizer and peak timing
ctrl <- mosaic_control_defaults(
  likelihood = list(
    weight_wis = 0.02,
    weight_peak_timing = 0.25
  )
)

# Use perplexity method for ESS calculations
ctrl <- mosaic_control_defaults(
  targets = list(ESS_method = "perplexity")
)

# Full workflow configuration (demonstrates logical order)
ctrl <- mosaic_control_defaults(
  calibration = list(n_simulations = NULL, n_iterations = 3),      # How to run
  sampling = list(sample_tau_i = TRUE, sample_beta_j0_tot = TRUE),  # What to sample
  likelihood = list(weight_wis = 0.02, weight_cases = 1.0),        # How to score
  targets = list(ESS_param = 100, ESS_param_prop = 0.95),          # When to stop
  parallel = list(enable = TRUE, n_cores = 16),                    # Infrastructure
  io = mosaic_io_presets("default"),                               # Output format
  paths = list(clean_output = FALSE, plots = TRUE)                 # File management
)
```
