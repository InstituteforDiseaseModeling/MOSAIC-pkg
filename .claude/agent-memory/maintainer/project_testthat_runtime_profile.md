---
name: testthat-runtime-profile
description: Per-file testthat timing profile (2026-06-19) — where the suite's wall-clock actually goes and the dominant slow patterns
metadata:
  type: project
---

Full testthat suite wall-clock = ~153s across 95 files (profiled 2026-06-19, v0.45.x).
Harness: claude/time_tests.R loops files + system.time(test_file). Reusable.

**Surprise:** the expensive LASER/keras/dask integration tests all skip cleanly and cost ~0
(lasik=0.02s, run_MOSAIC_integration=0.03s skip, python_parity=7 skips, suitability keras gated).
The runtime is dominated by PURE-R prior/sampling tests, NOT the engine.

Top offenders + root cause:
- test-impute_flood_probability.R 40s (26%): each call fits mgcv::bam() GAM on ~1040 synth rows;
  ~5 calls + diagnostics test does 3-fold year-blocked CV = ~8 GAM fits.
- test-sample_parameters_rho_deaths.R 16.5s: line 65 vapply(1:200, sample_parameters(seed=s)) to
  validate ONE scalar Beta draw. Each call = 301 params + 40×1398 psi_star calibration.
- test-est_zeta_ratio_prior.R 14.5s: 5 full calls at n_sim=10000/5000 + 4 artefacts (CSV+ggplot) each.
- test-est_suitability_dispatch.R 14.5s: deprecation-message tests (lines 29-54) are NOT mocked;
  they enter real est_suitability() which .onLoad-imports Python TF (TF present locally so no skip).
  7-iter loop at line 38. Pays TF import cost to assert a message emitted BEFORE the heavy path.
- test-sample_parameters_zeta.R 8.5s: lapply(1:100, sample_parameters).
- est_zeta_1/2_prior 4.6/2.8s: repeated full prior calls + artefact ggplots.
- plot_model_* / plot_rolling_cv 3-4s each: pure ggplot render + ggsave (15-17 plots).

**Dominant fixable pattern:** distributional-correctness loops calling the full sample_parameters()
N times to check one parameter's marginal. sample_parameters() is a heavy whole-config op (psi
calibration on 40×1398). Better: sample the Beta directly, or expose/test the per-param sampler, or
cache one big draw. See [[reviewer-checklist]].
