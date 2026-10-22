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

4.  `targets`: The posterior, the ensemble and when to stop (convergence
    status)

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

  - `n_simulations`: NULL for auto (adaptive) mode, or integer for fixed
    mode

  - `n_iterations`: Number of stochastic engine iterations per parameter
    set (default: 3L)

  - `max_simulations_total`: Budget cap of auto mode (default: 150000L).
    A run that reaches it before the convergence status is green ends
    with the `budget_cap_reached` flag

  - `batch_size_adaptive`: Simulations per batch until the minimum run
    length is met, and the fallback size afterwards (default: 500L)

  - `max_batch_predictive`: Cap on each projected batch (default:
    10000L)

  - `min_batches_adaptive`: Minimum batches before any stop or
    projection (default: 5L)

  - `min_draws`: Minimum draws before any stop or projection (default:
    5000L)

  Auto mode computes the convergence status
  ([`calc_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md))
  on every draw so far after each batch, on the members of
  `posterior_weighting`, and stops when its sampling checks (ensemble
  ESS, Monte Carlo error, k-hat) are green at two consecutive checks and
  both minimum run lengths are met; informativeness never blocks the
  stop. Once the minimum is met, batches are sized from the projection N
  x ESS_all target / ESS_all (converted to simulations at the success
  rate) while the ESS growth slope is at least 0.8, and otherwise grow
  geometrically (a quarter of the draws so far, between
  `batch_size_adaptive` and `max_batch_predictive`), so a heavy-tailed
  run reaches a 150,000 cap in about 20 checks after the minimum run. In
  parallel each batch is rounded up to a multiple of workers x
  `io$shard_batch_size`, never beyond the budget left.
  `max_batches_adaptive`, `max_batches_predictive` and
  `target_r2_adaptive` were retired in 1.4.0 (warned and ignored when
  set).

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

  List of posterior and convergence settings. Default is:

  - `posterior_weighting`: The ensemble members, from which the
    parameter posterior and the predictions are built (default:
    `"bestfit"`). `"bestfit"`: the draws in descending log-likelihood
    until the Kish ESS of their coarsened weights reaches
    `best_subset_ratio` x ESS_all, bounded by `min_ensemble_members` /
    `max_ensemble_members`, carrying those weights. `"coarsened"` (the
    1.3.0 default): a systematic resample of K = clamp(round(ESS_all),
    min, max) draws weighted by count / K. `"saturated"` (pre-1.3.0) and
    `"tempered"`: the tier-searched best subset (with `ESS_best` meaning
    the tier target, 100 unless set, and `A_best` 0.70, `CVw_best` 1.0,
    `min_best_subset`, `max_best_subset`). Formerly
    `best_subset_weighting` (accepted with a deprecation warning).

  - `coarsening_alpha`: Effective number of independent observations of
    the coarsened posterior, \\\eta = \alpha / n\_{obs}\\ with
    \\n\_{obs}\\ the scored observation cells (default: 10). See
    [`calc_weight_health`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_weight_health.md).

  - `best_subset_ratio`: Share of ESS_all the best-fit ensemble holds
    (default: 0.2).

  - `ESS_best`: Required ensemble ESS (default: 200; 100 under the
    legacy schemes). The convergence status is green at or above it, and
    auto mode simulates towards ESS_all =
    `ESS_best / best_subset_ratio`. Its meaning changed in 1.4.0 (it was
    the tier-search target in 1.3.x): an explicit value is honoured
    under every scheme, with a warning when it differs from the scheme's
    default.

  - `min_draws` (in `calibration`) above `max_simulations_total` is
    clamped to it with a warning.

  - `min_ensemble_members`, `max_ensemble_members`: Bounds on the
    ensemble size (default: 30, 1000; set the maximum lower for
    continental runs: the ensemble arrays grow with it).

  - `status_thresholds`: Bands of the convergence status (default
    `list(ess_yellow_frac = 0.5, mce_green = 0.10, mce_yellow = 0.15, khat_green = 0.5, khat_yellow = 0.7, slope_stall = 0.8, flat_ess_frac = 0.01)`;
    `flat_ess_frac` is the untempered-ESS share of the draws at or above
    which the likelihood counts as flat); see
    [`calc_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md).

  - `min_best_subset`, `max_best_subset`: Bounds of the legacy tier
    search (default: 30, 1000); ignored (with a warning when set) under
    `"bestfit"` and `"coarsened"`.

  - `ESS_method`: ESS formula, "kish" or "perplexity", of the legacy
    schemes' ESS_B / A / CVw, the subset optimiser and the \\\eta = 1\\
    likelihood-concentration numbers (default: "perplexity").

  Deprecated in 1.4.0 (warned and ignored when set): `ESS_param`,
  `ESS_param_prop` and `batch_ratio_max` (the per-parameter-ESS stopping
  rule, replaced by the convergence status); `A_best` and `CVw_best`
  outside the legacy schemes. `ESS_marginal_method` was retired in
  1.3.0.

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
    (MAE / WIS / R^2+bias) refines the ensemble members used for
    `posteriors.json`, `posterior_quantiles.csv`, and the ensemble
    object driving all downstream plots and metrics, and the convergence
    status is then assessed on the optimised subset. The members are
    preserved in the `is_best_subset` / `weight_best` columns of
    `samples.parquet` for provenance; the optimized selection is written
    to new `is_best_subset_opt` / `weight_best_opt` columns. Default
    `FALSE` (it was `TRUE` in v1.2.x; reverted in 1.3.0 because, under
    the coarsened posterior, the optimizer re-truncates the members by
    their fit to the training data). An explicit `TRUE` is honoured
    under every weighting scheme. When `FALSE`, the ensemble members
    (`is_best_subset` / `weight_best`) are canonical and no `_opt`
    columns are written. The optimizer chooses among subsets of the
    simulated candidate ensemble, so the size it selects is between
    `optimize_min_n` and the number of members. **Statistical note:**
    enabling this flag yields a posterior conditioned on ensemble
    predictive performance (MAE / WIS / R^2+bias on the training data)
    rather than a pure likelihood-weighted posterior.

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

  - `iqr_multiplier`: Tukey lower-fence multiplier of the legacy schemes
    (default: 1.5; use 3.0 for extreme outliers only).

  `floor` is deprecated (1.4.0; warned and ignored).

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
    re-running subset optimization or the R_eff posterior CI from the
    saved file). With `TRUE` the candidate ensemble also captures its
    members' R_eff inputs (three more arrays of the same size,
    `reff_capture`; MOSAIC 1.5.0), so
    `add_reproductive_numbers(recompute_ci = TRUE)` does not re-simulate
    them. The in-memory object used during the run is never affected
    (default: `FALSE`).

  `verbose_weights` is deprecated (1.4.0; it had no effect).

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

# The 1.3.0 resampled ensemble instead of the best-fit default
ctrl <- mosaic_control_defaults(
  targets = list(posterior_weighting = "coarsened")
)

# Full workflow configuration (demonstrates logical order)
ctrl <- mosaic_control_defaults(
  calibration = list(n_simulations = NULL, n_iterations = 3),      # How to run
  sampling = list(sample_tau_i = TRUE, sample_beta_j0_tot = TRUE),  # What to sample
  likelihood = list(weight_wis = 0.02, weight_cases = 1.0),        # How to score
  targets = list(ESS_best = 200, best_subset_ratio = 0.2),         # When to stop
  parallel = list(enable = TRUE, n_cores = 16),                    # Infrastructure
  io = mosaic_io_presets("default"),                               # Output format
  paths = list(clean_output = FALSE, plots = TRUE)                 # File management
)
```
