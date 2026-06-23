---
name: dask-worker-likelihood-v016-parity
description: laser-cholera v0.16.0 weights_obs port + Dask worker _score_window_likelihood parity traps (loc_idx peak-sourcing, WIS np.sum NaN engine bug, stale local repo vs installed wheel)
metadata:
  type: project
---

Review of MOSAIC v0.48.0 (laser-cholera v0.16.0 integration): per-cell obs weights
on the Dask path + dual-mode alpha. Durable traps found.

**Stale local laser-cholera checkout vs installed wheel.** `/Users/johngiles/MOSAIC/laser-cholera`
on disk was at tag **v0.13.1** while `~/.virtualenvs/r-mosaic` had **v0.16.0** installed.
Always verify the engine contract against the INSTALLED source
(`~/.virtualenvs/r-mosaic/lib/python3.12/site-packages/laser/cholera/...`), not the
read-only repo checkout — they drift. The r-mosaic env is a **conda** env, not a venv:
use `Sys.setenv(RETICULATE_PYTHON=".../r-mosaic/bin/python")`, NOT `use_virtualenv()`
(errors "not a Python virtualenv").

**loc_idx peak-sourcing trap (the real bug in the rewrite).** v0.16.0 Python
`calc_model_likelihood` matches `epidemic_peaks` rows by an integer `loc_idx` column
(`getattr(row,"loc_idx",None)` -> None -> row silently dropped). The raw
`config["epidemic_peaks"]` scattered to workers carries only `iso_code`/`peak_date`
(`.filter_epidemic_peaks()` does NOT add loc_idx). The engine's `params.py`
(`dict_to_propertysetex`, ~line 582-585) appends `loc_idx` to `model.params.epidemic_peaks`
at model construction — so the **analyzer** path (reads `model.params`) is fine, but the
worker's `_score_window_likelihood` read the raw `config` dict and dropped ALL peaks ->
every peak/cumulative/peak-magnitude shape term collapsed to 0 vs the local R scorer
(which dispatches by iso_code). FIX: source peaks from `model.params["epidemic_peaks"]`
in the worker, not `config`. This is the same class as CLAUDE.md Lesson #12(b). Shape
weights default 0 so it was latent. Same trap as the local-vs-Dask peak divergence —
the engine enriches loc_idx only on `model.params`, never on the bare config dict.

**WIS np.sum NaN engine bug (NOT fixable in MOSAIC — read-only engine).** v0.16.0
`compute_wis_parametric_row` uses `np.sum(np.abs(y-q_med)*w_use)` (lines ~589/612), NOT
`np.nansum`. A single NA obs -> `NaN*0 = NaN` poisons the whole WIS row -> NaN -> term
dropped engine-side. R's `.compute_wis_parametric_row` uses `na.rm=TRUE` and keeps it.
So local R and ANY Dask path (analyzer + worker) diverge on WIS whenever `weight_wis>0`
over gappy surveillance data. Cumulative correctly uses `np.nansum`; only WIS is broken.
Belongs in laser-cholera. WIS defaults to 0 so latent.

**What IS parity-correct (verified to ~1e-11):** NB core, peak-timing, peak-magnitude,
cumulative all match R local exactly with per-cell weights + burn-in slice (s>1) +
deaths-prefix zeroing. The deaths-prefix NaN-obs -> zero-weight swap is NB-equivalent AND
shape-correct (shape terms see the spike-free sliced region either way). The analyzer does
NOT pass weights_obs_* (analyzer.py ~line 59-87 omits them), so the worker correctly
recomputes when weights non-trivial OR idx>1; trivial+no-slice keeps model.log_likelihood.

**Dual-mode alpha:** scalar vs uniform-vector alpha give byte-identical run_LASER output;
hetero-vector runs without error. Default sampling keeps alpha scalar; `auto_unbox=TRUE`
in the Dask sampled-params JSON serializes scalar alpha as a JSON number (not a length-1
list). `alpha_1/alpha_2` are NOT in the worker `_VECTOR_FIELDS` (stay Python float) and the
engine's dict_to_propertysetex coerces list/ndarray->float32. `convert_config_to_matrix`
expands a vector alpha to indexed cols (`alpha_1_1`...) not iso-suffixed, but never
truncates to length-1 — moot since calibration alpha is always scalar.

**postca/ensemble Dask path** (`run_laser_postca`) returns only reported_cases/deaths;
ensemble scoring/R2 is in R via calc_model_ensemble. `_score_window_likelihood` is the
ONLY Dask likelihood path. Regression test: `test-dask_worker_score_window_parity.R`
(engine-backed, skips like test-calc_model_likelihood_python_parity).

**v0.16.0 ALSO aligned the zero-prediction cumulative penalty** (est==0 & obs>0 ->
-obs*log(1e6)). Pre-v0.16 Python was ~1.78x more negative than R; v0.16.0's verbatim
port made them exact-parity (ratio 1.0, ~1e-12). `test-calc_model_likelihood_python_parity.R`
test #7 had PINNED the 1.78 ratio with "if engine aligns -> switch to expect_equal";
the v0.16.0 bump tripped it (the only suite failure post-integration) -> updated to
exact-parity assertion. Lesson: a deliberately-pinned divergence test IS the early-warning
that an engine upgrade closed the gap; honor its instruction rather than re-pinning.

**Scale-up foot-gun (flagged, not a bug): burn_in_days=30 is now the DEFAULT** (v0.47.3)
AND config_default v4.3 ships non-trivial per-cell weights -> EVERY production sim now
takes the worker's `_score_window_likelihood` recompute branch (and the local slice).
Overhead measured ~0.2% of a sim (1.2ms vs 768ms) -> negligible. BUT
`.mosaic_resolve_score_window` clamps idx to n_time with NO warning, so on a SHORT
window (n_time < ~30) the 30-day burn-in eats the whole series -> min_obs_for_likelihood
gate yields 0-contribution -> all sims score equal -> calibration silently does nothing.
Safe for the default 1398-day (2023-2026) window; only bites outbreak/short-window sims
that forget to set burn_in_days=0L. Candidate: a one-time warning when
(n_time - max(idx_cases,idx_deaths)) < min_obs_for_likelihood. Scoring semantics are
disease-modeler/statistician territory, so propose-don't-impose.

Related: [[cairo_pdf_glyph_trap]] (installed-capability-lies pattern), CLAUDE.md Lesson #12.
