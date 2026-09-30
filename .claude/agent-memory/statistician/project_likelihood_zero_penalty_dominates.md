---
name: likelihood-zero-penalty-dominates
description: The -y*log(1e6) zero-prediction branch carries ~100% of MOSAIC's likelihood variance, sets the whole delta-AIC scale, and manufactures the deaths over-prediction bias (measured v0.90.3)
metadata:
  type: project
---

`calc_log_likelihood_negbin()`/`_poisson()` replace `log NB(y>0 | mu=0) = -Inf` with a flat
`-observed[i] * log(1e6)` (= -13.8155 per observed unit) at
`R/calc_log_likelihood_distributions.R:423-425` and `:677-681`, mirrored in the cumulative helper at
`R/calc_model_likelihood.R:543, :552`. It is **not a log-density** and appears nowhere in
`MOSAIC-docs/05-model-calibration.Rmd`.

**Measured (v0.90.3; ETH 25k + 40-loc 100k configs, reproduction exact to 0 over 5,500 ETH draws):**
- `Cov(component, total LL)/Var(total LL)`: zero-branch = **+100.1% to +100.8%** on random prior
  draws AND on the top-150 of the 40-loc run. The proper NB terms have **negative** covariance.
- Whole delta-AIC scale is the constant: the LL plateau ~-1.4536e6 on ETH is exactly
  `-(sum obs cases + deaths) * log(1e6) * renormalised weights`. Swap 1e6 -> 1e3 and every ΔAIC halves.
- Replacing it with `mu <- max(est, 0.5)`: LL spread sd 694,020 -> 82,600 (8.4x); score seed-noise
  SD 220.8 -> 48.4 (4.6x); signal-to-noise 6.11 -> 33.9 (5.5x).
- **It rewards over-prediction.** Across 19 locations: rho(log deaths-bias, deaths-NB) = **-0.98**,
  rho(log deaths-bias, zero-penalty) = **+0.90**, rho(log deaths-bias, total LL) = **+0.27**.
  Selecting the top 115 of 1,500 identical ETH sims: deaths bias **3.27** (production) vs **1.32**
  (mean floor); cases bias 1.98 vs 1.25. Production ETH reports bias_deaths 2.92.

**Why it matters:** the long-running "~2.9x deaths over-prediction" was chased through CFR priors,
mu_j_baseline re-anchoring, and the (1+epi) chain factor (see [[b2-cfr-chain-factor-diagnosis]] and
[[deaths-bias-cfr-target-drift]]). A large share of it is manufactured by this one branch. Check the
scoring rule before re-deriving a prior.

**Where it bit:** review_inference round, 2026-09-17. Report at
`MOSAIC-pkg/claude/review_inference/findings/LIKE.md`.

**Do NOT** "fix" it by dropping the offending cells — that makes the number of scored cells
draw-dependent so LLs stop being comparable across draws.
