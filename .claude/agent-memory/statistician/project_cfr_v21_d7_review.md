---
name: cfr-v21-d7-laplace-review
description: CFR v2.1 D7 integrated-deaths review (2026-09-28, two rounds) - Laplace math exact; NB small-k level estimator biases deaths totals (ZMB 0.74x) -> quasi-Poisson fix; medoid config must use the medoid's own conditional CFR; in-sample deaths metrics are fitted
metadata:
  type: project
---
Review of `R/calc_log_likelihood_deaths_integrated.R` (worktree cfr-v21, v0.96.1). Scripts:
`claude/cfr_v21_review/stat/` (round 1) and `claude/cfr_v21_review/redteam/stat/` (round 2).

Verified exact: grad/Hessian vs finite differences (incl. weeks crossing a year boundary), Laplace
vs quadrature, engine/exposure/redraw alignment s = delta_reporting_cases + 1 (alternating-mu test).
1000 real fits: 0 non-converged, 0 non-PD at the mode, 0 false convergence.

Durable traps (why they matter):
- **NB level estimator is not total-matching under misfit.** The level score is
  sum (D - m)/(k + m) = 0; with weekly k ~ 0.5-1 it averages per-week ratios, so any path whose
  weekly shape differs from the data gets a biased CFR. ZMB in-sample deaths 0.74x (2024 0.69x),
  MOZ 1.09x, NGA/COD/ETH ~1.0. Poisson score = ratio of totals (1.00). NB with a
  deaths-given-cases k only halves the gap and that k is unstable (GHA 25,158, ZAF 83,431, NA for
  3 of 23). Fix recommended: quasi-Poisson (Poisson score, LL and curvature / phi), with phi the
  Pearson dispersion of observed weekly deaths ~ year + offset(log observed cases). sd_year is a
  minor lever (1.4 or 3 adds only 2-7 points).
- **The integrated CFR is path-conditional.** Writing the ensemble-median CFR into one member's
  config (config_medoid.json) is wrong. For the ZMB smoke run, re-simulating that config gives
  deaths at 0.42x observed, against 0.76x from the medoid's own conditional fit.
- **The location offset a is not separately identified from delta_y.** Its posterior SD of about
  0.26 is a prior partition. Report the information share of a + delta_y instead (0.8-0.99 for
  scored years).
- **In-sample deaths R2 and bias from the redraw are fitted quantities.** COD R2 goes from 0.0015
  with the engine at the prior CFR to 0.43 after the redraw; leave-one-year-out gives 0.01. Use
  t_cut or rolling CV for deaths validation.
- **The channel imbalance comes from the cases side.** The daily cases LL SD is 5-7x the weekly
  cases SD, because the weekly-estimated k is applied to daily cells. On a weekly footing, deaths
  and cases have comparable spread.
- **A pmax data-relative floor creates a kink.** 1-3% of fits stop with |grad| 0.02-0.1 (LL error
  <= 0.01). An additive background of the same size is smooth, with 0 false stops, and gives
  identical rankings.
- **Jensen mismatch in cfr_posterior prior_median** is <= 3e-4 at production mu_jt, which is
  negligible.
