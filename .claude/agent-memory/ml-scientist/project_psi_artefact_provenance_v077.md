---
name: psi-artefact-provenance-v077
description: config_default$psi_jt does NOT match the shipped pred_psi_suitability_day.csv (median per-country r=0.46); psi run manifest records no source_csv/timesteps/versions; shipped fit used n_seeds=3; 9.4% of psi cells pinned at the 0.01 eps clamp
metadata:
  type: project
---

Verified 2026-09-16 against MOSAIC v0.85.0 (clean `R CMD INSTALL` of HEAD).

**1. The shipped default config's psi is not reproducible from the repo's psi artefact.**
`data-raw/make_config_default.R:497` builds `psi_jt` as
`acast(read.csv(MODEL_INPUT/pred_psi_suitability_day.csv), iso_code ~ date, value.var="psi",
fun.aggregate=mean)` with no later mutation. Re-running that on the committed CSV gives the right
shape (40×3322) but **125,887 of 132,880 cells differ**, max|diff| 0.992, 34/40 countries differ in
every cell. `.rda` and `inst/extdata/config_default.json` agree bit-for-bit (so the config is
internally consistent); the CSV is the odd one out — and both were committed in the SAME commit
(`43fd94647`, v0.77.0). Checked `pred_raw`/`pred_smooth`/`pred_bias_corrected` as alternative source
columns: all differ too. Median per-country Pearson **0.464** (GMB −0.03, GIN 0.0002, GHA 0.03);
per-country mean-psi ratio 0.14× (MLI) to 39× (GIN). Engine A/B at fixed seed (swap CSV psi into the
shipped config): total cases +1.6%, per country GNB **+59%**, GNQ **−31%**, MLI +22%, ZAF +22%.
**Practical consequence for us:** `plot_suitability_and_cases()`, `plot_psi_star_diagnostic()` and
`run_rolling_cv.R:252` all read the CSV — so any psi diagnosis of default-config behaviour is
reading a different psi from the one the simulation used. Always verify
`all.equal(acast(csv), config$psi_jt)` before attributing a fit artefact to psi.

**2. The psi run manifest has no provenance.** `psi_suitability_config.json` (written at
`run_rolling_cv_suitability.R:386-405`) has 18 keys and records **none** of: `source_csv` (the
parameter we added for the v7.4 / leak-free panels!), `timesteps`, `rw_subsample`,
`rw_step_months`/`rw_test_months`, `exclude_covariates`, `smooth_span`, `ensemble_logit_eps`,
`loss_kind`, MOSAIC version, git sha, TF/keras/numpy versions. `environment.yml` pins TF only as a
range (`>=2.16.0,<2.21`). Combined with lstm_v2's known cross-process non-determinism, a psi
artefact is unreconstructible in principle. This is exactly why the OCV-4 `timesteps` re-validation
baseline was ambiguous — see [[forecast-cv-ocv4-timesteps11-redteam]].

**3. The shipped psi used `n_seeds = 3` (seeds 11,22,33), below the production default of 5**
(`run_rolling_cv_suitability.R:161`) and well below the 10 used for the v4.0 build, which the config
description itself records as "bumped from 5 for ensemble stability on this load-bearing artifact".
A plausible partial explanation of (1) if the two artefacts are two independent 3-seed draws.

**4. The `ensemble_logit_eps = 0.01` clamp is doing a lot of work at the low end.**
`ensemble_suitability.R:285` clamps per-seed probabilities to `[0.01, 0.99]` BEFORE the logit LOESS,
and because every cross-seed aggregation is a quantile (median), the clamp survives to `psi`
exactly. **12,499 / 132,880 cells of `config_default$psi_jt` are exactly 0.01** (BWA 1,562 d,
ERI 172, GAB 2,470, GMB 2,963, GNQ 2,505, SEN 2,827); six countries end the series on a constant run
of 547–914 days. Second-order effect worth remembering: a clamped country fails
`calibrate_psi_predictions()`'s `min_pred_sd = 0.05` screen and falls back to **identity**, so the
per-country bias correction silently no-ops for exactly the most degenerate countries (it is only
counted in the `n_identity` warning, never attributed to the clamp). The per-capita
`target_D_rate_per_country_floored` response lives far below 1%, so a 1% floor is not obviously the
right eps for it.

**Side note:** the psi CSVs carry 41 duplicated `(iso_code, date)` keys, all on the ISO-week-1
anchors 2018-01-04 and 2024-01-04, with identical psi but *disagreeing* `cases`/`cases_binary`.
Benign for `psi_jt` (acast `fun.aggregate = mean` over identical values) but it breaks any row count
and any consumer reading `cases`. Likely an upstream weekly-panel ISO-week mapping issue.

Corrects/extends [[psi-G-redteam-v4]] (which covered the bias-correction corruption) and pairs with
[[psi-flat-tail-lstm-v2-unfixed]] (the 98-day fill tail in the same artefact).
