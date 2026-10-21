# Calculate Batch Size for Bookend Strategy (deprecated)

**Deprecated since MOSAIC 1.4.0.**
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
no longer calls it: the adaptive mode sizes its batches from the
projection \\N \times ESS\_{all}^{target} / ESS\_{all}\\ of the
convergence status
([`calc_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md)),
with geometric growth when the projection is not valid. The function
keeps its pre-1.4.0 signature and behaviour for one release and warns
once per session when called.

## Usage

``` r
calc_bookend_batch_size(
  ess_history,
  target_ess,
  max_total_sims,
  target_r_squared = 0.95
)
```

## Arguments

- ess_history:

  ESS measurements from calibration phase

- target_ess:

  Target ESS value

- max_total_sims:

  Maximum total simulations allowed

- target_r_squared:

  Target R-squared for ESS regression (default: 0.95)

## Value

List with batch size recommendation. `phase` is one of `"complete"`,
`"low_confidence"`, `"no_progress"` or `"predictive"`; `batch_size` is
positive only for `"predictive"`. `model` is `"linear"` or `"sqrt"` and
`r_squared_linear` / `r_squared_sqrt` report both fits when a fit was
made.

## Details

Predicts the number of simulations needed to reach a target ESS based on
the observed ESS trajectory. Fits linear and sqrt models to the
cumulative (n_sims, threshold_ESS) data and predicts from whichever has
the higher R-squared (linear on a tie), through the same helper as the
auto-mode calibration phase. Linear is the expected shape: the ESS the
stopping rule tracks is the function-specific ESS of a fixed posterior
(bounded by its Kish ESS), and for \\n\\ independent prior draws the
Kish ESS of fixed importance weights grows as \\n \\ (E w)^2 / E w^2\\,
linearly in \\n\\. Replaying the 2026-10 national runs, the
coarsened-posterior Kish ESS was linear in the number of draws
(R-squared 0.995-1.000 against 0.955-0.978 for sqrt). Before MOSAIC
1.3.0 sqrt was the default and linear had to beat it by 0.03, which
picked sqrt even at a linear R-squared of 1.000. A log model is also
fitted, but only as a diagnostic (a warning when it fits best); it is
never used for prediction.

The extrapolation is only defined when the chosen model's ESS increases
with the number of simulations. When its slope is zero or negative the
target is unreachable on the fitted trajectory, and the function returns
`phase = "no_progress"` with `batch_size = 0` instead of squaring a
negative root into a positive requirement. The ESS can fall as draws are
added: a much better draw concentrates the weights. A fit below
`target_r_squared` returns `phase = "low_confidence"` with
`batch_size = 0` as well. In both cases
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
runs a batch of `control$calibration$batch_size_adaptive` instead,
capped by `max_batch_predictive` and `max_simulations_total`, so a
non-monotone trajectory neither stalls the run nor sizes a batch from a
meaningless extrapolation.
