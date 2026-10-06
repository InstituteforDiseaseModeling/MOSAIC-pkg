---
name: nb-dispersion-estimator-traps
description: est_nb_dispersion traps - collapse = theta ~1e-30 w/ same-order SE (rule se/max(theta,0.1)<1e-4); clamped 0.1 fits are censoring -> panel trend; near-bound fits (CI reaches 0.1, UGA v7.0 0.117) same mechanism (F4); k changes need an impl-tag bump (resolved k not in control.json); shrinkage inert
metadata:
  type: reference
---

Measured on config_default v6.0, scored window from step 46, on 2026-10-01. Scripts:
`MOSAIC-pkg/claude/stat_v0100/k_panel.R` and `k_smoother.R`; the collapse probe is
`claude/worktree_scratch/v0101-lik/v0101_lik/`.

- **Collapse signature (corrected).** k_min = 0.1 binds for CMR, UGA and ZAF only.
  - Their raw glm.nb theta is 3.6e-30, 5.6e-13 and 6.3e-30, and the SE is of the SAME order.
  - So se/theta is 0.06-0.13: ordinary, and relative-SE tests miss the collapse.
  - "Absurd SE" exists only relative to the reported bound (se/0.1 = 2e-30).
  - All three came through the theta.ml fallback, with fitted mu down to 2.2e-16 in runs of zero weeks.
  - The shipped rule (v0.101.0) is `se / max(theta, K_LO) < 1e-4`. A Cramer-Rao bound,
    se/k >= 1/sqrt(n k^2 trigamma(k)) >= 1/sqrt(2.64 n) for k <= 1, means only theta below ~2e-4 can trip it.
  - Identified fits have se/k of 0.11-0.61.
- **The estimate flips with the smoother.** Across trend_df_per_year 2/4/8/12: CMR 0.1/0.1/2.96/0.1,
  UGA 0.1/Inf/0.22/0.1. Bursty series and reporting dumps defeat the Farrington smooth mean.
- **Shrinkage is inert as implemented.** The precision weight gives degenerate SEs weight ~1 (the
  panel median own-weight is 0.989), and single-location runs never shrink.
  - v0.101.0 bypasses it: a no-estimate location takes the shipped panel trend `.NB_DISP_PANEL_TREND`,
    fitted by `.nb_disp_panel_trend_fit()`, at every scale.
- **The panel trend is flat, noisy and fragile.** Slope 0.143 +/- 0.176, residual SD 1.56 (v6.0 fit;
  the SHIPPED `.NB_DISP_PANEL_TREND` is the v6.1 refit: intercept -0.355, slope 0.220, sigma 1.19, n 22;
  takers BFA 0.89, CIV 0.98, ZAF 1.10, CMR 1.65, UGA 0.96).
  - At burn-in 45 the unidentified BFA k = 71 (SE 105) enters the fit and gives CMR/UGA/ZAF 1.41/1.00/1.20.
  - At burn-in 30, BFA is Poisson, the slope is 0.33, and the same three get 1.06/0.48/0.75.
  - Treat the trend as a prior centre (~1), and re-derive it after every config rebuild (a drift test enforces this).
- **Too few observed weeks is not Poisson.** With observed-only fitting, BFA (all cases imputed) and
  ZAF (14 observed cases against 1,152 in a reconstructed window) fail the minimum.
  - The Poisson gate must use all tiers; observed-insufficient means no estimate, so the panel trend.
  - Under Poisson, ZAF's flat window would become a hard shape constraint.
- **k is relative to the smoother even when identified.** From 2 to 8 df/yr: KEN 0.135 -> 0.24,
  ZMB 0.49 -> 1.31. The per-draw profile k of selected draws is KEN 0.12 and CMR 0.18.
- **Downstream.** The ensemble intervals omit this NB noise. Adding NB(k_run) takes tier-A coverage
  passes from 4/15 to 15/15. CMR is the exception (1.71x WIS) at its clamped k = 0.1.

- **A clamped fit is censoring, not a measurement (routed to the trend, integrate/v0101 c939095fc).**
  - UGA (v6.1): 10 non-zero of 84 observed weeks, mostly edges of outbreaks whose middles are tier 2.
    On synthetic series of that shape the fit returns 0.1 in 22-29/40 for true Poisson, k=1 AND k=5.
  - `use_panel` now includes `clamped_lower_bound` (cases only; deaths have no trend, so ZAF's clamped
    deaths k stays 0.1). status/k_raw keep describing the fit, panel_trend = TRUE the k used.
  - Removing a clamped location from shrinkage moves NO other location: the shrink trend (fit_ok) and
    mean_s2 already exclude clamped rows. Census over all suite scopes: only UGA, 0.100/0.100/0.105/0.108
    -> 0.964; every other row identical. This also makes F5 (s2 from the bound) moot for cases.
  - Level cost of a 2x error on UGA: daily 3.1 nats at k=0.1 vs 22 at 0.96; weekly 0.5 vs 4.6.
- **Near-bound fits are the same censoring (F4 ruling, 2026-10-03; recommended, acceptance pending).**
  - UGA on config_default v7.0 (2018 window, day 46): k_raw 0.117, se 0.024, log-scale 95% interval
    0.079-0.173 (z from the floor 0.77). Its 2019+ weeks alone clamp; the fully observed 2018 outbreak
    alone gives 1.36 (the trend gives 1.34). The low k comes from isolated tier-1 spikes between observed
    zeros (173 on 2020-12-28, 99 on 2025-07-07): likely batch reports. Drop one week and k is 0.198.
  - Synthetic check (`claude/v0103_uga_k_ruling/synthetic_power.R`): with UGA's observed pattern as the
    mean, k-hat is 0.11-0.13 for true k from 0.3 to Poisson, i.e. no power. With a 3-week-smoothed mean it
    ranks k but reads Poisson as 0.30. CMR/MOZ/KEN recover a 4-7x monotone range, yet Poisson truth still
    reads 1.0-1.7 there, so every k-hat is biased low by the smoother.
  - Rule in the patch (`claude/v0103_uga_k_ruling/F4_near_bound.patch`): status `near_lower_bound` when
    k*exp(-1.96*se/k) <= 0.1, censored like clamped (`.nb_disp_censored()`). It moves only UGA at v7.0 at
    both burn-ins; the next own fit is z 3.43 (CIV, b45) and 2.88 (COG, b30). At v6.2 nothing new moves.
  - Leaving UGA out of the trend fit re-derives the constants (int -0.512 -> -0.294, slope 0.257 -> 0.226,
    SD 1.04 -> 0.93, n 23 -> 22). Every trend taker moves with it: BFA 0.62 -> 0.77, ZWE 2.36 -> 2.49.
  - **v0.103.0 HEAD re-pasted v7.0 constants but kept the tag `R/v0.101.0+clamped_k_trend`.** Any constant
    change moves trend takers' k, so the tag must be bumped.
  - Level cost at 0.117 vs 1.34 (2x error, 2018 window): daily 13.5 vs 118 nats; weekly 2.2 vs 23.
  - Near-bound fixture: weekly rnbinom(150, mu = 8, size = 0.12), seed 2, sum 1078, k_raw 0.1179,
    se 0.0205.
- **Any change to how k is resolved needs a likelihood impl-tag bump.** Resume compares the MERGED
  control$likelihood (so a default flip such as cases_scoring is caught), but the resolved k and
  .nb_dispersion_table are private slots set after control.json is written. Only
  `.mosaic_likelihood_impl_version()` keeps shards scored at different k apart.
- A synthetic fixture that clamps deterministically: weekly rnbinom(150, mu = 8, size = 0.05) clamps in
  12/12 seeds with se/k 0.10-0.18 (not a collapse). A custom trend list(intercept = log(0.5), slope = 0.5)
  gives a hand-computable expected k = 0.5*sqrt(mean weekly).

See [[daily-vs-weekly-cases-scoring]], [[surveillance-reconstruction-traps]] and
[[weighting-stack-measured-v085]] (the retired nb_k_min = 3 floor bound everywhere).
