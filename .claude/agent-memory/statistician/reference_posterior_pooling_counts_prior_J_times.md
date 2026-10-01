---
name: posterior-pooling-counts-prior-J-times
description: Inverse-variance pooling of J posteriors that share one prior counts the prior J times and invents precision (eastern_eth 2026-09-18 pooled globals at 0.42-0.81x prior width from national posteriors at 0.84-1.08x); use the consensus formula or don't pool
metadata:
  type: reference
---
**Invariant.** p(theta | y_1..y_J) is proportional to p(theta)^(1-J) * prod_j p(theta | y_j). A precision-weighted (inverse-variance) pool of J national posteriors that share one prior is prod_j p(theta | y_j), which carries the prior J times. The correct Gaussian-natural-parameter consensus is: precision = sum_j prec_j - (J-1)*prec_0, and the same for precision*mean. Temper it by 1/f if the pooled result is then re-used against the same data. When every posterior equals the prior, the consensus returns the prior (a fixed point); the naive pool does not.

**Where it bit.** claude/regional_nmme/02_assemble_warmstart_regional.R (POOL_GLOBALS, 2026-06-28) pooled member national globals. Measured on eastern_eth (5 members): national posterior/prior 95% width ratios were 0.84-1.08 (median per parameter), but the pooled priors were 0.42-0.81 of the prior width (median 0.53, about 1/sqrt(5)). delta_reporting_cases: every national posterior equalled the prior (1.000), pooled 0.617. The 2026-09-18 regional fits ran on those priors.

**Production context.** National posteriors (27 runs, 2026-09-18) are approximately the prior for every global (width ratio 0.80-1.04) and for most location parameters. Only beta_j0_tot (0.30), psi_star_a/k (0.40) and psi_star_b (0.75) are identified.

**How to apply.** Any stage-2/warm-start or meta-analytic combination of per-unit posteriors must divide out the shared prior, or keep the prior. The v0.100 warm start keeps globals at base ([[warmstart-v0100-design]]).
