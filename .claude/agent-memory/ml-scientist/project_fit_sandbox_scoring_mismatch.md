---
name: fit-sandbox-scoring-mismatch
description: run_fit_sandbox() scores with no burn-in, no observed-NA mask and no likelihood, so its bias/R2 disagree with the calibration path by 27%/0.39 on the identical simulation — do not rank psi or bias levers with it unaddressed
metadata:
  type: project
---

Measured 2026-09-16 on MOSAIC v0.85.0, shipped `config_default`, seed 42, one identical simulation:

| scoring | bias (cases) | R² (cases) |
|---|---|---|
| `run_fit_sandbox()` as shipped | **2.136** | **0.380** |
| + predictions masked to observed cells | 1.704 | 0.546 |
| + burn-in 30 (the calibration scored window) | **1.680** | **0.771** |

**Why:** three divergences from `run_MOSAIC()`'s scoring, all making the model look worse.
1. **No burn-in.** `control$likelihood$burn_in_days` defaults to 30 and the run log confirms
   `Scored window: cases from step 31 ... (of 3322)`. The sandbox scores from tick 1, so it eats the
   IC discharge spike — predicted day-2 cases 15,604 against 310 observed.
2. **No scored-cell mask.** `run_MOSAIC.R:2765-2777` masks predictions with
   `ensemble$artifact_mask` before `calc_model_R2`/`calc_bias_ratio`. The sandbox aggregates
   observed with `colSums(na.rm = TRUE)` — every missing surveillance cell becomes a 0 — while
   summing predictions over all locations. **34.0% of `config_default$reported_cases` cells are NA**
   and 3,094 of 3,322 timesteps have a partial-NA panel, so **20.2% of predicted cases are compared
   against a zero**.
3. **No likelihood.** It never calls `calc_model_likelihood()`, so it also ignores
   `reported_cases_weight`/`reported_deaths_weight`, the NB dispersion floor and the shape terms
   that calibration actually optimises.

**How to apply:** when using `run_fit_sandbox()` (the `diagnose-fit` harness) to rank a psi variant
or a bias lever, recompute the metrics yourself from `res$predictions`: drop the first
`burn_in_days` rows and mask predicted cells to where observed is finite. Treat the harness's own
`metrics$r2_cases`/`bias_cases` as a *relative* signal between arms at best, never as the number the
calibration objective sees. This is the harness behind the earlier bias-lever sweeps, so those
rankings inherit the mismatch.

Scripts: `claude/review_v084/scratch/ENV/t06_sandbox_vs_calibration.R`, `t07_sandbox_namask.R`.
Related: [[psi-artefact-provenance-v077]] (if the psi you are diagnosing came from the CSV rather
than the config, you have a second, independent mismatch).
