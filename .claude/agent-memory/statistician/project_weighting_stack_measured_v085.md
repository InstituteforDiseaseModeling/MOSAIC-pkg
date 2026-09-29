---
name: weighting-stack-measured-v085
description: Measured behaviour of the MOSAIC weighting/convergence stack at v0.85.0 — nb_k_min binds and over-sharpens, adaptive-eta pins best:worst at the weight floor, three weight columns give three different posteriors, ess_marginal is likelihood-blind, and the cases-vs-deaths imbalance is far smaller than previously recorded
metadata:
  type: project
---

Measured 2026-09-16 during the v0.84 deep review (STAT-A), on
`/Users/johngiles/MOSAIC/output/psi_compare_{MOZ,KEN,ETH,COD,NGA}_nmme_3k/` (3000-sim
single-country runs, 2026-06-26) plus live `run_simulation()` calls from each run's
`config_medoid.json`. Scripts: `MOSAIC-pkg/claude/review_v084/scratch/STAT-A/exp*.R`.

**Why:** these five numbers are the ones that get re-derived from first principles every time
someone asks "is the posterior any good". They are counter-intuitive enough that reasoning
from the formulas gives the wrong answer.

**How to apply:** before attributing a fit problem to the likelihood's *terms*, check which of
these is actually in play. Re-measure rather than trust the numbers if the config version or
`nb_k_min`/`burn_in_days` defaults have moved.

1. **`nb_k_min = 3` binds on 8 of 10 channel/country pairs and the MoM estimate is 3-13x
   below it.** Cases MoM k: MOZ 0.228, KEN 0.667, ETH 1.367, COD 5.449, NGA 0.486. Deaths:
   MOZ 0.416, KEN **Inf** (→ Poisson branch, which bypasses `k_min` entirely), ETH 8.311,
   COD 1.376, NGA 0.558. So "weighted MoM dispersion" mostly does not run — the floor does.
   Cost: `dLL(+10% cases)` on MOZ is **-123.88 at k=3 vs -10.99 at k=0.226** (11.3x
   over-sharp). The roxygen rationale ("prevents collapse to a near-Poisson kernel for
   low-variance series") is **inverted**: low-variance → `Inf` → really is Poisson, unfloored;
   the floor only touches over-dispersed series and pushes them toward Poisson.

2. **The three weight columns are three different posteriors.** MOZ run, n_valid 2993:
   raw `exp(LL-max)` → 21 non-zero, ESS_kish **1.0000**; `weight_all` → ESS **2323** (77.6%
   of n); `weight_retained` → ESS **1.0** (one model at 99.05%, 2582 tied at the ΔAIC-25
   floor carrying 1.0%); `weight_best` → ESS **87.4** of 115, with **114 of 115 tied at the
   ΔAIC-4 floor carrying 93.9% of the mass**. The ensemble consumes `weight_best`, so it is
   very nearly an equal-weight average over the top-N draws — the likelihood supplies
   ranking, not magnitude.

3. **`.mosaic_calc_adaptive_gibbs_weights()`'s eta algebraically reduces to
   `-log(weight_floor)/max_delta_aic`** (because `actual_range == max_delta_aic` identically —
   `delta_aic` is measured from the best valid model). So best:worst is pinned at the floor
   whatever the data say, and **the sharpness is set by the single worst simulation**. On 500
   draws ~N(-3000,30): ESS 1.4; add one draw at -10,000 → ESS **488.6**; at -600,000 → 500.0.
   The logged `ESS (all)` is therefore not a health indicator.

4. **`ess_marginal` (the `ESS_param` convergence gate) does not see the likelihood.**
   Shuffling the `likelihood` column across parameter rows moves the median from 126.5 to
   **125.4** (0.9%). It is ~0.10-0.20·n (39.9/75.6/126.5/210.4/285.7 at n =
   200/500/1000/2000/2993), so `ESS_param = 100` is a sample-size gate crossed at n≈800-1000.
   With genuinely uniform weights (where ESS should be n=1000) it returns **572.8**, so it is
   not on the scale of its own threshold. It *is* invariant to linear reparameterisation.

5. **Cases-vs-deaths imbalance is much smaller than the "15-210x" figure in earlier notes.**
   At v0.85.0 with per-cell obs weights and `burn_in_days = 30`: |LL_cases|/|LL_deaths| =
   1.6-2.3x. The operationally relevant measure is the sensitivity ratio
   `dLL(+10% cases)/dLL(+10% deaths)` = **MOZ 13.3, KEN 1.3, ETH 0.7, COD 5.1, NGA 1.8** —
   country-dependent, and **deaths-dominated for ETH**. Driver is which `k` each channel
   lands on (item 1), not `weight_cases`/`weight_deaths`. Supersedes the blanket claim in
   [[deaths-bias-cfr-target-drift]] that deaths never scores.

Two structural splits worth remembering: the best-subset **size** is chosen by *rescaled*
Gibbs weights (`eta = 0.5*4/range`, `grid_search_best_subset.R`) while the stored
`weight_best` and the reported `ESS_B` use *truncated* ones (`pmin(Δ,4)`, `eta = 0.5`,
`run_MOSAIC.R`) — MOZ: selection perplexity-ESS 100.64 vs reported 107.48. And the shape-term
scale is `N_obs/N_component` (`N_obs` = finite-obs count, 56-84% of T), **not** the `T`,
`T/4`, `T/5`-free form printed in the roxygen header and CLAUDE.md.

Related: [[artifact-mask-scoring]] — the `deaths_final` half of that mask is now a relic; the
0.16.1 oracle fixture and the R engine both write the final deaths column (no trailing
structural zero). The real deaths artifact is `delta_reporting_deaths` **leading** zeros
(5 in the replay fixture, 16 in the MOZ medoid config).
