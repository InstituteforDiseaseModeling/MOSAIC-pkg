# Pre-fit and freeze the environmental-suitability (psi) cache for rolling CV

Fits the environmental-suitability (psi) LSTM once per rolling-origin
cutoff `T` (with `fit_date_stop = T`) and freezes each fit's daily psi
prediction into a per-cutoff CSV under `dir_cache`. The frozen cache is
consumed by
[`run_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md)
via its `psi_cache` argument, so the (non-deterministic, expensive) psi
fit is run exactly once per cutoff and the calibration backtest reuses
the identical frozen psi across reruns.

## Usage

``` r
prefit_rolling_cv_psi(
  PATHS,
  cutoffs,
  est_suitability_spec = list(),
  pred_date_start,
  pred_date_stop,
  dir_cache,
  verbose = TRUE
)
```

## Arguments

- PATHS:

  Path list from
  [`get_paths`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).

- cutoffs:

  Date or character vector of rolling-origin cutoffs.

- est_suitability_spec:

  Named list of *modeling* arguments forwarded to
  [`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
  (e.g. `architecture`, `feature_set`, `response_var`, `bias_correct`,
  and the lstm_v2 `arch_control` list, which carries
  `n_seeds`/`parallel_seeds`). Date keys are ignored (harness-owned)
  with a warning.

- pred_date_start, pred_date_stop:

  Prediction window (Date/character) used for every cutoff fit. Held
  fixed so all frozen CSVs share one date grid.

- dir_cache:

  Directory to hold the frozen `psi_<T>.csv` files and the
  `psi_manifest.json` (created if needed).

- verbose:

  Logical (default TRUE).

## Value

Invisibly, the manifest list (also written to
`dir_cache/psi_manifest.json`). Side effects: one `psi_<T>.csv` per
fitted cutoff plus the manifest under `dir_cache`.

## Details

**Leakage discipline.** Each cutoff's fit uses `fit_date_stop = T` (no
observation after `T` informs the psi representation); the prediction
window (`pred_date_start`/`pred_date_stop`) is harness-owned and held
fixed across cutoffs so every frozen CSV spans the same grid.

**Leak-free v7.4 hazard panel (opt-in).** When the RAW
`est_suitability_spec` requests `feature_set = "v7.4"`, the four imputed
hazard channels (flood/cyclone/drought probabilities) would LEAK if
their GAMs were fit once on the full series: a fold's covariates would
depend on outcomes in that fold's OOS future. To prevent this, for each
cutoff `T` this function rebuilds a *leak-free* panel via
[`compile_suitability_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/compile_suitability_data.md)`(gam_train_stop = T, include_flood_prob = TRUE)`
– every hazard GAM is fit only on rows with `date <= T` and then
predicts every row – writes it to `dir_cache/panel_v74_<T>.csv`, and
fits psi via
`est_suitability(source_csv = <panel>, feature_set = "v7.4", fit_date_stop = T)`.
The panel is compiled ONCE per cutoff and reused across the whole
\\\ge\\10-seed psi ensemble (the 3 GAMs are not refit per seed). When
v7.4 is not requested the DEFAULT path is byte-unchanged: psi is fit
from the canonical panel with no `source_csv` and no `gam_train_stop`
(v1/v7.3 behaviour).

**Panel-window alignment (v7.4).** The leak-free panel's compile/GAM-fit
window is deliberately aligned to the psi-LSTM *training* window
\\\[\code{fit_date_start}, T\]\\, NOT an arbitrary-wide (e.g.
2000-\>2027) span. Imputed hazard-probability magnitudes dilute when the
compile window is wider than the LSTM fit window (more all-zero rows
shrink the same event labels), so the hazard probabilities the LSTM
ingests must be computed over the same rows the LSTM trains on. The
compile `date_start` is therefore resolved from the spec's LSTM
`fit_date_start` (`est_suitability_spec$arch_control$fit_date_start`,
default `"2015-01-01"`) and `date_stop` from the cutoff `T`. This
resolved window is folded into the cache spec-hash (below) so a panel
built over a different window cannot be silently reused.

**Atomic cache (concurrency-safe).**
[`est_suitability()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
writes a single global `MODEL_INPUT/pred_psi_suitability_day.csv`; this
function copies that file into `dir_cache/psi_<T>.csv` via a tempfile +
`file.rename` so a partially-written cache file can never be observed.

**Cache key (spec hash).** For each cutoff a `spec_hash` is computed
over `list(fit_date_stop = T, est_suitability_spec)` where the resolved
spec includes `arch_control$n_seeds`. The number of pooled seeds is part
of the key: refitting with a different seed count invalidates the cache.
When the spec requests `feature_set = "v7.4"`, a `v74_panel` block
(`leakfree = TRUE`, resolved `compile_date_start`, and
`gam_train_stop = T`) is also folded into the hash, so a frozen psi
built from a v7.3 panel – or from a v7.4 panel compiled over a different
window – cannot be silently reused. `run_rolling_cv(psi_cache=)`
recomputes the same hash from its own `est_suitability_spec` and
**hard-errors** on a mismatch.

**Idempotent / resume.** A cutoff is skipped when its `psi_<T>.csv`
exists and the manifest already records a matching `spec_hash` for that
cutoff; rerunning therefore only fits the missing/changed cutoffs.

## See also

[`run_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md),
[`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
